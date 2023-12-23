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
    MaxX = byte_size(Line),
    {Movable, Static} = parse(Lines),
    lists:foldl(fun(X, Sum) ->
                        MovableCol = maps:get(X, Movable, []),
                        StaticCol = maps:get(X, Static, []),
                        Sum + solve(MovableCol, 0, length(Lines), StaticCol, 0)
                end, 0, lists:seq(1, MaxX)).

solve([Movable | _] = Movables, LastRock, RowCount, [Static | RStatic], Sum)
  when Static < Movable, Static > LastRock ->
    solve(Movables, Static, RowCount, RStatic, Sum);
solve([_Movable | RMovables], LastRock, RowCount, Static, Sum) ->
    NewMovable = LastRock + 1,
    Load = calc_load(NewMovable, RowCount),
    solve(RMovables, NewMovable, RowCount, Static, Sum + Load);
solve([], _LR, _RC, _S, Sum) ->
    Sum.

calc_load(Movable, RowCount) ->
    RowCount - Movable + 1.

parse(Lines) ->
    {_, Movable, Static} = lists:foldl(fun parse/2, {1, #{}, #{}}, Lines),
    {reverse_lists_in_map(Movable), reverse_lists_in_map(Static)}.

parse(Line, {Y, Movable0, Static0}) ->
    {Movable, Static} = parse_row(Line, 1, Y, Movable0, Static0),
    {Y + 1, Movable, Static}.

parse_row(<<$O, RBin/binary>>, X, Y, Movable0, Static0) ->
    parse_row(RBin, X + 1, Y, Movable0#{ X => [ Y | maps:get(X, Movable0, []) ] }, Static0);
parse_row(<<$#, RBin/binary>>, X, Y, Movable0, Static0) ->
    parse_row(RBin, X + 1, Y, Movable0, Static0#{ X => [ Y | maps:get(X, Static0, []) ] });
parse_row(<<$., RBin/binary>>, X, Y, Movable, Static) ->
    parse_row(RBin, X + 1, Y, Movable, Static);
parse_row(<<>>, _, _, Movable, Static) ->
    {Movable, Static}.

reverse_lists_in_map(ListsMap) ->
    maps:map(fun(_K, List) -> lists:reverse(List) end, ListsMap).
