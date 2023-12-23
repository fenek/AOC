#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    {Depth, Horizontal} = compute(Lines, 0, 0),
    io:format("~p~n~p~n~p~n", [Depth, Horizontal, Depth*Horizontal]).

compute([<<"up ", N/binary>> | R], Depth, Horizontal) ->
    compute(R, Depth - binary_to_integer(N), Horizontal);
compute([<<"down ", N/binary>> | R], Depth, Horizontal) ->
    compute(R, Depth + binary_to_integer(N), Horizontal);
compute([<<"forward ", N/binary>> | R], Depth, Horizontal) ->
    compute(R, Depth, Horizontal + binary_to_integer(N));
compute([_ | R], Depth, Horizontal) ->
    compute(R, Depth, Horizontal);
compute([], Depth, Horizontal) ->
    {Depth, Horizontal}.
