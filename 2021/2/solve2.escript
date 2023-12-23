#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    {Depth, Horizontal} = compute(Lines, 0, 0, 0),
    io:format("~p~n~p~n~p~n", [Depth, Horizontal, Depth*Horizontal]).

compute([<<"up ", N/binary>> | R], Aim, Depth, Horizontal) ->
    compute(R, Aim - binary_to_integer(N), Depth, Horizontal);
compute([<<"down ", N/binary>> | R], Aim, Depth, Horizontal) ->
    compute(R, Aim + binary_to_integer(N), Depth, Horizontal);
compute([<<"forward ", N/binary>> | R], Aim, Depth, Horizontal) ->
    Forward = binary_to_integer(N),
    compute(R, Aim, Depth + Forward*Aim, Horizontal + Forward);
compute([_ | R], Aim, Depth, Horizontal) ->
    compute(R, Aim, Depth, Horizontal);
compute([], _Aim, Depth, Horizontal) ->
    {Depth, Horizontal}.
