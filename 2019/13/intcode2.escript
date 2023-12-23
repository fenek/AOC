#!/usr/bin/env/escript

-mode(compile).

%-define(DBG, 1).

main([Filename]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = re:replace(Input0, <<"\\n">>, <<>>, [{return, binary}, global]),
    [_ToReplace | RCodes] = binary:split(Input, <<",">>, [global]),

    Intcode = spawn_intcode([<<"2">> | RCodes], self(), self()),

    Renderer = case code:add_path("cecho/_build/default/lib/cecho/ebin") of
                   true ->
                       logger:remove_handler(default),
                       {ok, [_ | _]} = application:ensure_all_started(cecho),
                       timer:sleep(100),
                       cecho:move(0, 0),
                       cecho:cbreak(),
                       cecho:noecho(),
                       put(window, cecho:newwin(40, 60, 0, 0)),
                       draw(-1, 0, 0),
                       cecho:wrefresh(get(window)),
                       fun draw/3;
                   _ ->
                       fun print_score/3
               end,

    game_loop(#{ renderer => Renderer }, Intcode).

game_loop(#{ renderer := Renderer } = Game, Intcode) ->
    timer:sleep(1),
    dbg("Ball: ~p\tPaddle: ~p\tScore: ~p~n", [maps:get(ball, Game, undefined),
                                              maps:get(paddle, Game, undefined),
                                              maps:get(score, Game, undefined)]),
    case read(Intcode) of
        halt ->
            Game;
        input_request ->
            #{ paddle := {PaddleX, _}, ball := {BallX, _} } = Game,
            tilt_joystick(PaddleX, BallX, Intcode),
            game_loop(Game, Intcode);
        -1 ->
            0 = read(Intcode),
            Score = read(Intcode),
            Renderer(-1, 0, Score),
            game_loop(Game#{ score => Score }, Intcode);
        X ->
            Y = read(Intcode),
            Type = read(Intcode),
            NGame = case Type of
                3 -> Game#{ paddle => {X, Y} };
                4 -> Game#{ ball => {X, Y} };
                _Other -> Game
            end,
            Renderer(X, Y, Type),
            game_loop(NGame, Intcode)
    end.

tilt_joystick(PaddleX, BallX, Intcode) when BallX < PaddleX ->
    dbg("   Tilt: left~n"),
    write(Intcode, -1);
tilt_joystick(PaddleX, BallX, Intcode) when BallX > PaddleX ->
    dbg("   Tilt: right~n"),
    write(Intcode, 1);
tilt_joystick(_PaddleX, _BallX, Intcode) ->
    dbg("   Tilt: none~n"),
    write(Intcode, 0).

draw(-1, 0, Score) ->
    Window = get(window),
    cecho:mvwaddstr(Window, 0, 0, "                         "),
    cecho:mvwaddstr(Window, 0, 0, integer_to_list(Score));
draw(X, Y, Type) ->
    ToPrint = case Type of
                  0 -> $\s; % empty
                  1 -> $#; % wall
                  2 -> $X; % block
                  3 -> $=; % paddle
                  4 -> $O % ball
              end,
    cecho:mvwaddch(get(window), Y + 1, X, ToPrint),
    cecho:wrefresh(get(window)).

print_score(-1, 0, Score) ->
    io:format("~p\t", [Score]);
print_score(_, _, _) ->
    ignore.

%% ----------------------------------------------------------------
%% INTCODE IMPLEMENTATION
%% ----------------------------------------------------------------

write({Pid, _Ref}, Int) ->
    Pid ! {input, self(), Int}.

read({Pid, _Ref}) ->
    receive
        {'DOWN', _Ref, process, Pid, normal} ->
            halt;
        {'DOWN', _Ref, process, Pid, Reason} ->
            exit({crash, {Pid, Reason}});
        {input, Pid, Int} ->
            Int;
        {input_request, Pid} ->
            input_request
    end.

spawn_intcode(Codes, Input, Output) ->
    spawn_monitor(fun() -> init_intloop(Codes, Input, Output) end).

init_intloop(Codes, Input, Output) ->
    put(relative_base, 0),
    put(input, Input),
    put(output, Output),

    store_program(0, Codes),
    
    intloop(0).

store_program(_N, []) ->
    %dbg("LOADED ~p~n", [N-1]),
    ok;
store_program(N, [CodeBin | RProgram]) ->
    store(N, binary_to_integer(CodeBin)),
    store_program(N + 1, RProgram).

intloop(IP) ->
    Opcode = load(IP),
    %dbg("~p: ~p~n", [IP, Opcode]),
    case Opcode rem 100 of
        1 ->
            addition(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        2 ->
            multiplication(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        3 -> 
            input(dest(Opcode, 1, IP)),
            intloop(IP + 2);
        4 ->
            output(param(Opcode, 1, IP)),
            intloop(IP + 2);
        5 ->
            intloop(jump_if_true(param(Opcode, 1, IP), param(Opcode, 2, IP), IP));
        6 ->
            intloop(jump_if_false(param(Opcode, 1, IP), param(Opcode, 2, IP), IP));
        7 ->
            less_than(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        8 ->
            equals(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        9 ->
            update_relative_base(param(Opcode, 1, IP)),
            intloop(IP + 2);
        99 ->
            ok;
        _ ->
            exit({unknown_opcode, IP, Opcode})
    end.

param(Opcode, ParamPos, IP) ->
    Val =
    case param_mode(Opcode, ParamPos) of
        0 -> load(load(IP + ParamPos));
        1 -> load(IP + ParamPos);
        2 -> load(get(relative_base) + load(IP + ParamPos))
    end,
    %dbg("PARAM VAL ~p~n", [Val]),
    Val.

param_mode(Opcode, ParamPos) ->
    Mode = (Opcode div round(math:pow(10, 1 + ParamPos))) rem 10,
    %dbg("MODE ~p, ~p: ~p~n", [Opcode, ParamPos, Mode]),
    Mode.

dest(Opcode, ParamPos, IP) ->
    case param_mode(Opcode, ParamPos) of
        2 -> get(relative_base) + load(IP + ParamPos);
        _ -> load(IP + ParamPos)
    end.

addition(Int1, Int2, Dest) ->
    store(Dest, Int1 + Int2).

multiplication(Int1, Int2, Dest) ->
    store(Dest, Int1 * Int2).

input(Dest) ->
    InputSrc = get(input),
    InputSrc ! {input_request, self()},
    receive
        {input, InputSrc, Value} ->
            store(Dest, Value)
    end.

output(Value) ->
    get(output) ! {input, self(), Value}.

jump_if_true(0, _, IP) ->
    IP + 3;
jump_if_true(_, NewIP, _) ->
    NewIP.

jump_if_false(0, NewIP, _) ->
    NewIP;
jump_if_false(_, _, IP) ->
    IP + 3.

less_than(Int1, Int2, Dest) when Int1 < Int2 ->
    store(Dest, 1);
less_than(_, _, Dest) ->
    store(Dest, 0).

equals(Int, Int, Dest) ->
    store(Dest, 1);
equals(_, _, Dest) ->
    store(Dest, 0).

update_relative_base(Value) ->
    put(relative_base, get(relative_base) + Value).

store(Pos, Value) ->
    put(Pos, Value).

load(Pos) ->
    Loaded = case get(Pos) of
                 undefined -> 0;
                 Loaded0 -> Loaded0
             end,
    Loaded.

-ifdef(DBG).
dbg(Fmt) -> io:format(Fmt, []).
dbg(Fmt, Vals) -> io:format(Fmt, Vals).
-else.
dbg(_) -> ok.
dbg(_, _) -> ok.
-endif.

