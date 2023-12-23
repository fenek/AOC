#!/usr/bin/env/escript

-mode(compile).

main([Filename]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = re:replace(Input0, <<"\\n">>, <<>>, [{return, binary}, global]),
    Codes = binary:split(Input, <<",">>, [global]),
    
    Program = load_program(0, Codes, #{}),

    start_amplifiers_for_all_phases([0, 1, 2, 3, 4], [], Program),

    % 120 - number of permutations of 5-element list
    Best = collect_and_get_best(120, 0),

    io:format("~p~n~n", [Best]).

collect_and_get_best(0, Best) ->
    Best;
collect_and_get_best(N, Best) ->
    io:format("Collecting ~p, best: ~p~n", [N, Best]),
    receive
        {input, Thrust} ->
            case Thrust > Best of
                true -> collect_and_get_best(N - 1, Thrust);
                false -> collect_and_get_best(N - 1, Best)
            end
    end.

start_amplifiers_for_all_phases([], Current, Program) ->
    [ APid | _ ] = Amplifiers = start_amplifiers(Program),
    set_phases(Current, Amplifiers),
    send(APid, 0);
start_amplifiers_for_all_phases(Phases, Current, Program) ->
    lists:foreach(fun(Phase) ->
                          start_amplifiers_for_all_phases(Phases -- [Phase],
                                                          [Phase | Current], Program)
                  end, Phases).

start_amplifiers(Program) ->
    lists:foldl(fun(N, [ OutputPid | _ ] = Amplifiers0) ->
                        [spawn(fun() -> amplifier(N, OutputPid, Program) end)
                         | Amplifiers0]
                end, [self()], "EDCBA").

set_phases([], _) ->
    ok;
set_phases([Phase | RPhases], [Amp | RAmps]) ->
    send(Amp, Phase),
    set_phases(RPhases, RAmps).

send(Pid, Value) ->
    Pid ! {input, Value}.

load_program(_, [], Program) ->
    Program;
load_program(N, [CodeBin | RProgram], Program0) ->
    Program1 = store(N, binary_to_integer(CodeBin), Program0),
    load_program(N + 1, RProgram, Program1).

amplifier(_N, OutputPid, Program) ->
%    io:format("~p started~n", [N]),
    put(program, Program),
    put(outpid, OutputPid),
    intloop(0).
%    io:format("~p finished~n", [N]).

intloop(IP) ->
    Opcode = load(IP),
    case Opcode rem 100 of
        1 ->
            addition(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(3, IP)),
            intloop(IP + 4);
        2 ->
            multiplication(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(3, IP)),
            intloop(IP + 4);
        3 -> 
            input(dest(1, IP)),
            intloop(IP + 2);
        4 ->
            output(param(Opcode, 1, IP)),
            intloop(IP + 2);
        5 ->
            intloop(jump_if_true(param(Opcode, 1, IP), param(Opcode, 2, IP), IP));
        6 ->
            intloop(jump_if_false(param(Opcode, 1, IP), param(Opcode, 2, IP), IP));
        7 ->
            less_than(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(3, IP)),
            intloop(IP + 4);
        8 ->
            equals(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(3, IP)),
            intloop(IP + 4);
        99 ->
            ok;
        _ ->
            exit({unknown_opcode, IP, Opcode})
    end.

param(Opcode, ParamPos, IP) ->
    ValuePos = case (Opcode div round(math:pow(10, 1 + ParamPos))) rem 10 of
                   0 -> load(IP + ParamPos);
                   1 -> IP + ParamPos
               end,
    load(ValuePos).

dest(ParamPos, IP) ->
    load(ParamPos + IP).

addition(Int1, Int2, Dest) ->
    store(Dest, Int1 + Int2).

multiplication(Int1, Int2, Dest) ->
    store(Dest, Int1 * Int2).

input(Dest) ->
    receive
        {input, Value} ->
            store(Dest, Value)
    end.

output(Value) ->
    get(outpid) ! {input, Value}.

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

store(Pos, Value) ->
    put(program, store(Pos, Value, get(program))).

load(Pos) ->
    load(Pos, get(program)).

store(Pos, Value, Program) ->
    Program#{ Pos => Value }.

load(Pos, Program) ->
    maps:get(Pos, Program).

