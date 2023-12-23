#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    [First | _] = Lines0 = binary:split(Data, <<"\n">>, [global]),
    Lines = [ L || L <- Lines0, L /= <<>> ],
    {Ones, LinesCount} = count(Lines, lists:duplicate(byte_size(First), 0), 0),
    Gamma = gamma(Ones, LinesCount, 0),
    Epsilon = epsilon(Ones, LinesCount, 0),

    io:format("~p~n~p~n", [Ones, LinesCount]),
    io:format("~p, ~p~n~p~n", [Gamma, Epsilon, Gamma * Epsilon]).

count([Line | R], Ones, LC) ->
    count(R, count_ones(Line, Ones), LC + 1);
count([], Ones, LC) ->
    {Ones, LC}.

count_ones(<<Ch:8, RBin/binary>>, [C | ROnes]) ->
    [C + Ch - $0 | count_ones(RBin, ROnes)];
count_ones(<<>>, []) ->
    [].

gamma([OneC | ROnes], LinesC, Acc) when OneC * 2 > LinesC -> gamma(ROnes, LinesC, Acc bsl 1 + 1);
gamma([_ | ROnes], LinesC, Acc) -> gamma(ROnes, LinesC, Acc bsl 1);
gamma([], _, Acc) -> Acc.

epsilon([OneC | ROnes], LinesC, Acc) when OneC * 2 > LinesC -> epsilon(ROnes, LinesC, Acc bsl 1);
epsilon([_ | ROnes], LinesC, Acc) -> epsilon(ROnes, LinesC, Acc bsl 1 + 1);
epsilon([], _, Acc) -> Acc.

