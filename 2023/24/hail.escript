#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve(Lines) ->
    Hailstones = parse(Lines),
    %collide_hailstones(Hailstones, 0, 7, 27).
    collide_hailstones(Hailstones, 0, 200000000000000, 400000000000000).

collide_hailstones([_], Collisions, _, _) ->
    Collisions;
collide_hailstones([Hailstone1 | RStones], Collisions, Min, Max) ->
    NCollisions = lists:foldl(fun(Hailstone2, Sum) ->
                                      case stones_collide(Hailstone1, Hailstone2, Min, Max) of
                                          true -> Sum + 1;
                                          false -> Sum
                                      end
                              end, Collisions, RStones),
    collide_hailstones(RStones, NCollisions, Min, Max).

stones_collide({{PX1, PY1, _PZ1}, {VX1, VY1, _VZ1}},
               {{PX2, PY2, _PZ2}, {VX2, VY2, _VZ2}}, Min, Max) ->
    A1 = VX1,
    B1 = -1 * VX2,
    C1 = PX2 - PX1,
    A2 = VY1,
    B2 = -1 * VY2,
    C2 = PY2 - PY1,
    case A1 * B2 - B1 * A2 of
        0 ->
            false; % parallel
        W ->
            Wx = C1 * B2 - B1 * C2,
            Wy = A1 * C2 - C1 * A2,
            Coeff1 = Wx / W,
            Coeff2 = Wy / W,
            X = Coeff1 * VX1 + PX1,
            Y = Coeff1 * VY1 + PY1,
            X =< Max andalso X >= Min andalso Y =< Max andalso Y >= Min
            andalso Coeff1 > 0 andalso Coeff2 > 0
    end.

parse(Lines) ->
    lists:map(fun parse_hail/1, Lines).

parse_hail(Bin) ->
    {PX, RBin1} = parse_int(Bin),
    {PY, RBin2} = parse_int(RBin1),
    {PZ, RBin3} = parse_int(RBin2),
    {VX, RBin4} = parse_int(RBin3),
    {VY, RBin5} = parse_int(RBin4),
    {VZ, <<>>} = parse_int(RBin5),
    {{PX, PY, PZ}, {VX, VY, VZ}}.

parse_int(<<$-, RBin/binary>>) ->
    {Int, Bin} = parse_int(RBin),
    {-1 * Int, Bin};
parse_int(<<NonDigit:8, RBin/binary>>) when NonDigit < $0; NonDigit > $9 ->
    parse_int(RBin);
parse_int(Bin) ->
    parse_int(Bin, 0).

parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

