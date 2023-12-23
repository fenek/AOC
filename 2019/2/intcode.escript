#!/usr/bin/env/escript

main([Filename, Task]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = re:replace(Input0, <<"\\n">>, <<>>, [{return, binary}, global]),
    Codes = binary:split(Input, <<",">>, [global]),
    ets:new(program, [named_table]),
    store_program(0, Codes),

    case Task of
        "1" ->
            ets:insert(program, {1, 12}),
            ets:insert(program, {2, 2}),
            intloop(0),
            io:format("~p~n~n", [element(2, hd(ets:lookup(program, 0)))]);
        "2" ->
            Backup = ets:tab2list(program),
            {Noun, Verb} = solve([{Noun0, Verb0} || Noun0 <- lists:seq(1, 100),
                                                    Verb0 <- lists:seq(1, 100) ], Backup),
            io:format("~p~n~n", [100 * Noun + Verb])
    end.

solve([], _) ->
    exit(no_solution);
solve([{Noun, Verb} | R], Backup) ->
    ets:delete_all_objects(program),
    ets:insert(program, Backup),
    ets:insert(program, {1, Noun}),
    ets:insert(program, {2, Verb}),
    intloop(0),
    case ets:lookup(program, 0) of
        [{0, 19690720}] -> {Noun, Verb};
        _ -> solve(R, Backup)
    end.

store_program(_, []) ->
    ok;
store_program(N, [CodeBin | RProgram]) ->
    ets:insert(program, {N, binary_to_integer(CodeBin)}),
    store_program(N + 1, RProgram).

intloop(Position) ->
    case ets:lookup(program, Position) of
        [{_, 1}] -> addition(params(Position)), intloop(Position+4);
        [{_, 2}] -> multiplication(params(Position)), intloop(Position+4);
        [{_, 99}] -> ok;
        [{_, Opcode}] -> exit({unknown_opcode, Opcode})
    end.

params(Position) ->
    [{_, Pos1}] = ets:lookup(program, Position+1),
    [{_, Pos2}] = ets:lookup(program, Position+2),
    [{_, Int1}] = ets:lookup(program, Pos1),
    [{_, Int2}] = ets:lookup(program, Pos2),
    [{_, Dest}] = ets:lookup(program, Position+3),
    {Int1, Int2, Dest}.

addition({Int1, Int2, Dest}) ->
    ets:insert(program, {Dest, Int1 + Int2}).

multiplication({Int1, Int2, Dest}) ->
    ets:insert(program, {Dest, Int1 * Int2}).

