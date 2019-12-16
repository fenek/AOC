#!/usr/bin/env/escript

-mode(compile).

%-define(DBG, 1).

main([Filename, Part]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = re:replace(Input0, <<"\\n">>, <<>>, [{return, binary}, global]),
    Codes = binary:split(Input, <<",">>, [global]),

    Intcode = spawn_intcode(Codes, self(), self()),

    RobotState0 = #{ pos => {0, 0},
                     dir => 0, % 0 = up, 1 = right, 2 = down, 3 = left
                     white_tiles => sets:new(),
                     ever_painted => sets:new() },

    RobotState = case Part of
                     "1" ->
                         RobotState0;
                     "2" ->
                         WT0 = maps:get(white_tiles, RobotState0),
                         RobotState0#{ white_tiles := sets:add_element({0, 0}, WT0) }
                 end,

    FinalState= robot_loop(RobotState, Intcode),
    
    io:format("~p~n", [sets:size(maps:get(ever_painted, FinalState))]),

    WT = maps:get(white_tiles, FinalState),

    print(WT).

print(WT) ->
    {XMin, YMin, XMax, YMax} = find_dimensions(WT),

    lists:foreach(fun(Y) ->
                          lists:foreach(fun(X) ->
                                                case sets:is_element({X, Y}, WT) of
                                                    true -> io:format("#", []);
                                                    false -> io:format(" ", [])
                                                end
                                        end, lists:seq(XMin, XMax)),
                          io:format("~n", [])
                  end, lists:seq(YMin, YMax)).

find_dimensions(WhiteTiles) ->
    sets:fold(fun(Point, Bounds) ->
                      apply_point(Point, Bounds)
              end, {0, 0, 0, 0}, WhiteTiles).

apply_point({X, Y}, {UpperX, LeftY, BottomX, RightY}) ->
    {
     case X < UpperX of
         true -> X;
         false -> UpperX
     end,
     case Y < LeftY of
         true -> Y;
         false -> LeftY
     end,
     case X > BottomX of
         true -> X;
         false -> BottomX
     end,
     case Y > RightY of
         true -> Y;
         false -> RightY
     end
    }.

robot_loop(RobotState, Intcode) ->
    write(Intcode, color(RobotState)),
    case read(Intcode) of
        halt ->
            RobotState;
        NewColor ->
            case read(Intcode) of
                halt ->
                    RobotState;
                Turn ->
                    robot_loop(move(turn(paint(RobotState, NewColor), Turn)), Intcode)
            end
    end.

color(#{ pos := Pos, white_tiles := WhiteTiles }) ->
    case sets:is_element(Pos, WhiteTiles) of
        true -> 1;
        false -> 0
    end.

paint(#{ pos := Pos, white_tiles := WhiteTiles, ever_painted := EverPainted } = State, Color) ->
   State1 = State#{ ever_painted := sets:add_element(Pos, EverPainted) },
   case Color of
       0 -> State1#{ white_tiles := sets:del_element(Pos, WhiteTiles) };
       1 -> State1#{ white_tiles := sets:add_element(Pos, WhiteTiles) }
   end.

turn(#{ dir := 0 } = State, 0) -> State#{ dir := 3 };
turn(#{ dir := Dir } = State, 0) -> State#{ dir := Dir - 1 };
turn(#{ dir := Dir } = State, 1) -> State#{ dir := (Dir + 1) rem 4 }.

move(#{ dir := 0, pos := {X, Y} } = State) -> State#{ pos := {X, Y - 1} };
move(#{ dir := 1, pos := {X, Y} } = State) -> State#{ pos := {X + 1, Y} };
move(#{ dir := 2, pos := {X, Y} } = State) -> State#{ pos := {X, Y + 1} };
move(#{ dir := 3, pos := {X, Y} } = State) -> State#{ pos := {X - 1, Y} }.

%% ----------------------------------------------------------------
%% INTCODE IMPLEMENTATION
%% ----------------------------------------------------------------

write({Pid, _Ref}, Int) ->
    Pid ! {input, self(), Int}.

read({Pid, _Ref}) ->
    receive
        {'DOWN', _Ref, process, Pid, Info} ->
            io:format("~p stopped: ~p~n", [Pid, Info]),
            halt;
        {input, Pid, Int} ->
            Int
    end.

spawn_intcode(Codes, Input, Output) ->
    spawn_monitor(fun() -> init_intloop(Codes, Input, Output) end).

init_intloop(Codes, Input, Output) ->
    put(relative_base, 0),
    put(input, Input),
    put(output, Output),

    store_program(0, Codes),
    
    intloop(0).

store_program(N, []) ->
    dbg("LOADED ~p~n", [N-1]),
    ok;
store_program(N, [CodeBin | RProgram]) ->
    store(N, binary_to_integer(CodeBin)),
    store_program(N + 1, RProgram).

intloop(IP) ->
    Opcode = load(IP),
    dbg("~p: ~p~n", [IP, Opcode]),
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
dbg(Fmt, Vals) -> io:format(Fmt, Vals).
-else.
dbg(_, _) -> ok.
-endif.

