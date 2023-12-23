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

solve([Line | _] = Lines) ->
    {Map, EmptyRows, EmptyColumns} = parse(Lines),
    XMapping = expand(byte_size(Line), EmptyColumns),
    YMapping = expand(length(Lines), EmptyRows),
    distances(Map, XMapping, YMapping).

distances(Map, XMap, YMap) ->
    distances(Map, XMap, YMap, 1, 0).

distances(#{last_galaxy_number := Galaxy}, _XMap, _YMap, Galaxy, Sum) ->
    Sum;
distances(#{last_galaxy_number := LastGalaxy} = Map, XMap, YMap, Galaxy, Sum) ->
    NewSum = lists:foldl(fun(OtherGalaxy, SumAcc) ->
                                 SumAcc + distance(Galaxy, OtherGalaxy, Map, XMap, YMap)
                         end, Sum, lists:seq(Galaxy + 1, LastGalaxy)),
    distances(Map, XMap, YMap, Galaxy + 1, NewSum).

distance(G1, G2, Map, XMap, YMap) ->
    #{ G1 := {OldX1, OldY1}, G2 := {OldX2, OldY2} } = Map,
    #{ OldX1 := X1, OldX2 := X2 } = XMap,
    #{ OldY1 := Y1, OldY2 := Y2 } = YMap,
    abs(Y2 - Y1) + abs(X2 - X1).

expand(Max, Empty) ->
    expand(1, Max, #{}, 0, Empty).

expand(N, Max, Mapping, _Shift, []) when N > Max ->
    Mapping;
expand(N, Max, Mapping, Shift, [N | REmpty]) ->
    expand(N + 1, Max, Mapping, Shift + 1, REmpty);
expand(N, Max, Mapping, Shift, Empty) ->
    expand(N + 1, Max, Mapping#{ N => N + Shift }, Shift, Empty).

parse([Line | _] = Lines) ->
    parse(Lines, #{}, 1, [], sets:from_list(lists:seq(1, byte_size(Line)), [{version, 2}])).

parse([Line | RLines], Map, Y, EmptyRows, EmptyColumns) ->
    case parse_line(Line, Map, Y) of
        {[], _} ->
            parse(RLines, Map, Y + 1, [Y | EmptyRows], EmptyColumns);
        {NewXs, NewMap} ->
            NewEmptyColumns = sets:subtract(EmptyColumns, sets:from_list(NewXs, [{version, 2}])),
            parse(RLines, NewMap, Y + 1, EmptyRows, NewEmptyColumns)
    end;
parse([], Map, _, EmptyRows, EmptyColumns) ->
    {Map, lists:reverse(EmptyRows), lists:sort(sets:to_list(EmptyColumns))}.

parse_line(Line, Map, Y) ->
    parse_line(Line, Map, 1, [], Y).

parse_line(<<$., R/binary>>, Map, X, NewXs, Y) ->
    parse_line(R, Map, X + 1, NewXs, Y);
parse_line(<<$#, R/binary>>, Map, X, NewXs, Y) ->
    Next = maps:get(last_galaxy_number, Map, 0) + 1,
    parse_line(R, Map#{ last_galaxy_number => Next, Next => {X, Y} }, X + 1, [X | NewXs], Y);
parse_line(<<>>, Map, _X, NewXs, _Y) ->
    {NewXs, Map}.

