#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    Numbers = [ binary_to_integer(Bin) || Bin <- Lines, Bin /= <<>> ],
    Set = sets:from_list(Numbers),
    find(Numbers, Set).

find([N | R], Set) ->
    case sets:is_element(2020-N, Set) of
        true ->
            io:format("~p~n~p~n~p~n", [N, 2020-N, N*(2020-N)]);
        false ->
            find(R, Set)
    end.
