#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    OceanFloor = lists:foldl(fun apply_input/2, #{}, Lines),
    io:format("~p~n", [length(lists:filter(fun(C) -> C > 1 end, maps:values(OceanFloor)))]).

apply_input(<<>>, OF) ->
    OF;
apply_input(Line, OF0) ->
    [From, To] = binary:split(Line, <<" -> ">>),
    XY1 = binary:split(From, <<",">>),
    XY2 = binary:split(To, <<",">>),
    [X1, Y1, X2, Y2] = bulk_b2i(XY1 ++ XY2),
    CoordsSeq = coords_seq(X1, Y1, X2, Y2),
    lists:foldl(
      fun(Coords, OFAcc) ->
              C = maps:get(Coords, OFAcc, 0),
              OFAcc#{ Coords => C + 1 }
      end, OF0, CoordsSeq).

coords_seq(X, Y1, X, Y2) ->
    [ {X, Y} || Y <- lists:seq(min(Y1, Y2), max(Y1, Y2)) ];
coords_seq(X1, Y, X2, Y) ->
    [ {X, Y} || X <- lists:seq(min(X1, X2), max(X1, X2)) ];
coords_seq(X1, Y1, X2, Y2) ->
    lists:zip(
      lists:seq(X1, X2, case X2 > X1 of true -> 1; false -> -1 end),
      lists:seq(Y1, Y2, case Y2 > Y1 of true -> 1; false -> -1 end)).

bulk_b2i(List) ->
    [ binary_to_integer(B) || B <- List ].

