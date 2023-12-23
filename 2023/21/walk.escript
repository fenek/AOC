#!/usr/bin/env escript

-mode(compile).

-export([solve/2]).

main([Filename, Steps]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines, list_to_integer(Steps)])
                                     || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve(Lines, Steps) ->
    #{ start := Start } = Garden = parse(Lines),
    NextTiles = walk(sets:from_list([Start], [{version, 2}]), Garden, Steps),
    sets:size(NextTiles).

walk(CurrentTiles, _Garden, 0) ->
    CurrentTiles;
walk(CurrentTiles, Garden, StepsLeft) ->
    walk(next_tiles(CurrentTiles, Garden), Garden, StepsLeft - 1).

next_tiles(CurrentTiles, Garden) ->
    sets:fold(fun(CurrTile, NextTiles0) ->
                      lists:foldl(fun(NextTile, NextTiles1) ->
                                          case maps:is_key(NextTile, Garden) of
                                              true -> sets:add_element(NextTile, NextTiles1);
                                              false -> NextTiles1
                                          end
                                  end, NextTiles0, neighbours(CurrTile))
              end, sets:new([{version, 2}]), CurrentTiles).

neighbours({X, Y}) -> [{X + 1, Y},  {X - 1, Y}, {X, Y + 1}, {X, Y - 1}].

parse(Lines) ->
    lists:foldl(fun({Y, Line}, Garden) ->
                        parse_line(Line, Y, Garden)
                end, #{}, lists:zip(lists:seq(1, length(Lines)), Lines)).

parse_line(Line, Y, Garden) ->
    parse_line(Line, 1, Y, Garden).

parse_line(<<$., RBin/binary>>, X, Y, Garden) ->
    parse_line(RBin, X + 1, Y, Garden#{ {X, Y} => walkable });
parse_line(<<$#, RBin/binary>>, X, Y, Garden) ->
    parse_line(RBin, X + 1, Y, Garden);
parse_line(<<$S, RBin/binary>>, X, Y, Garden) ->
    parse_line(RBin, X + 1, Y, Garden#{ {X, Y} => walkable, start => {X, Y} });
parse_line(<<>>, _X, _Y, Garden) ->
    Garden.

