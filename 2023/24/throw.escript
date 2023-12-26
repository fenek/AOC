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
    [{{X1, _, Z1}, {VX1, _, VZ1}},
     {{X2, _, Z2}, {VX2, _, VZ2}} | _] = Hailstones = parse(Lines),
    Matrix = hailstones_to_matrix(lists:sublist(Hailstones, 5)),
    [XS, YS, VXS, _VYS] = gauss_elimination(Matrix),
    T1 = (X1 - XS) / (VXS - VX1),
    T2 = (X2 - XS) / (VXS - VX2),
    [ZS, _VZS] = gauss_elimination([[1, T1, Z1 + T1 * VZ1],
                                    [1, T2, Z2 + T2 * VZ2]]),
    XS + YS + ZS.

hailstones_to_matrix([_]) ->
    [];
hailstones_to_matrix([{{X1, Y1, _Z1}, {VX1, VY1, _VZ1}},
                      {{X2, Y2, _Z2}, {VX2, VY2, _VZ2}} = H2 | RHailstones]) ->
    [ [VY1 - VY2, VX2 - VX1, Y2 - Y1, X1 - X2, VX2 * Y2 - X2 * VY2 - VX1 * Y1 + X1 * VY1]
      | hailstones_to_matrix([H2 | RHailstones]) ].

gauss_elimination(Matrix) ->
    Triangular = gauss_elimination(Matrix, 1),
    lists:map(fun erlang:round/1, solve_matrix(lists:reverse(Triangular), [])).

solve_matrix([Row | RRows], Known) ->
    [Last | RElems] = lists:reverse(Row),
    Next = solve_next(Last, RElems, Known),
    solve_matrix(RRows, Known ++ [Next]);
solve_matrix([], Known) ->
    lists:reverse(Known).

solve_next(Sum, [NextElem | _], []) ->
    Sum / NextElem;
solve_next(Sum, [NextElem | RElems], [NextKnown | RKnown]) ->
    solve_next(Sum - NextElem * NextKnown, RElems, RKnown).

gauss_elimination([Vector], _NthCol) ->
    [Vector];
gauss_elimination([Vector1 | RMatrix], NthCol) ->
    Vector1Nth = lists:nth(NthCol, Vector1),
    NewRMatrix = lists:map(
                   fun(Vector2) ->
                           Vector2Nth = lists:nth(NthCol, Vector2),
                           Coeff = Vector2Nth / Vector1Nth,
                           add_vec(Vector2, [ -1 * Coeff * Elem || Elem <- Vector1 ])
                   end, RMatrix),
    [Vector1 | gauss_elimination(NewRMatrix, NthCol + 1)].

add_vec([], []) -> [];
add_vec([A | R1], [B | R2]) -> [A + B | add_vec(R1, R2)].

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

