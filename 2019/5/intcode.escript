#!/usr/bin/env/escript

-mode(compile).

main([Filename]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = re:replace(Input0, <<"\\n">>, <<>>, [{return, binary}, global]),
    Codes = binary:split(Input, <<",">>, [global]),
    ets:new(program, [named_table]),
    store_program(0, Codes),

    intloop(0),
    io:format("~n~n", []).

store_program(_, []) ->
    ok;
store_program(N, [CodeBin | RProgram]) ->
    store(N, binary_to_integer(CodeBin)),
    store_program(N + 1, RProgram).

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
    Input = io:get_line("Input: "),
    ToStore = list_to_integer(string:trim(Input, both, "\n")),
    store(Dest, ToStore).

output(Value) ->
    io:format("~p  ", [Value]).

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
    ets:insert(program, {Pos, Value}).

load(Pos) ->
    [{_, Value}] = ets:lookup(program, Pos),
    Value.

