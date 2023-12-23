#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    Lines0 = binary:split(Data, <<"\n">>, [global]),
    Lines = [ L || L <- Lines0, L /= <<>> ],

    Oxygen = oxygen(Lines, 0),
    CO2 = co2(Lines, 0),

    io:format("~p, ~p~n~p~n", [Oxygen, CO2, Oxygen * CO2]).

count([Line | R], Ones, LC) ->
    count(R, count_ones(Line, Ones), LC + 1);
count([], Ones, LC) ->
    {Ones, LC}.

count_ones(<<Ch:8, RBin/binary>>, [C | ROnes]) ->
    [C + Ch - $0 | count_ones(RBin, ROnes)];
count_ones(<<>>, []) ->
    [].

oxygen([Line], _Pos) ->
    binary_to_integer(Line, 2);
oxygen([First | _] = Lines, Pos) ->
    {Ones, LC} = count(Lines, lists:duplicate(byte_size(First), 0), 0),
    OneC = lists:nth(Pos + 1, Ones),
    NLines = lists:filter(fun(Line) ->
                                  not ((binary:at(Line, Pos) == $1) xor (OneC * 2 >= LC))
                          end, Lines),
    oxygen(NLines, Pos + 1).

co2([Line], _Pos) ->
    binary_to_integer(Line, 2);
co2([First | _] = Lines, Pos) ->
    {Ones, LC} = count(Lines, lists:duplicate(byte_size(First), 0), 0),
    OneC = lists:nth(Pos + 1, Ones),
    NLines = lists:filter(fun(Line) ->
                                  (binary:at(Line, Pos) == $1) xor (OneC * 2 >= LC)
                          end, Lines),
    co2(NLines, Pos + 1).

