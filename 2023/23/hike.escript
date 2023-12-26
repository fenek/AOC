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
    Initial = {2, 1},
    {_, Map} = parse(Lines),
    Paths = hike([new_path(Initial)], [], Map),
    lists:max([ sets:size(Path) - 1 || Path <- Paths ]).

hike(Paths, Finished, Map) ->
    hike(Paths, Finished, [], Map).

hike([{{_, MaxY}, Visited} | RPaths], Finished, NewPaths, #{ max_y := MaxY } = Map) ->
    hike(RPaths, [Visited | Finished], NewPaths, Map);
hike([{Position, Visited} | RPaths], Finished, NewPaths0, Map) ->
    Neighbours1 = neighbours(Position),
    Neighbours2 = lists:filter(fun(Neighbour) ->
                                       validate(Position, Neighbour, Map)
                                       andalso
                                       not sets:is_element(Neighbour, Visited)
                               end, Neighbours1),
    NewPaths = lists:foldl(fun(NextStep, NewPaths) ->
                                   [{NextStep, sets:add_element(NextStep, Visited)} | NewPaths]
                           end, NewPaths0, Neighbours2),
    hike(RPaths, Finished, NewPaths, Map);
hike([], Finished, [], _Map) ->
    Finished;
hike([], Finished, NewPaths, Map) ->
    hike(NewPaths, Finished, [], Map).

neighbours({X, Y}) ->
    [{X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}].

validate(PrevPosition, Position, Map) ->
    case maps:get(Position, Map, undefined) of
        {only_from, PrevPosition} ->
            true;
        walkable ->
            true;
        _ ->
            false
    end.

new_path(Initial) -> {Initial, sets:from_list([Initial], [{version, 2}])}.

parse([Line0 | _] = Lines) ->
    lists:foldl(fun(Line, {Y, Map}) -> {Y + 1, parse_line(Line, Y, Map)} end,
                {1, #{ max_x => byte_size(Line0), max_y => length(Lines) }}, Lines).

parse_line(Line, Y, Map) ->
    parse_line(Line, 1, Y, Map).
parse_line(<<$#, RBin/binary>>, X, Y, Map) ->
    parse_line(RBin, X + 1, Y, Map);
parse_line(<<$., RBin/binary>>, X, Y, Map) ->
    parse_line(RBin, X + 1, Y, Map#{ {X, Y} => walkable });
parse_line(<<Slope:8, RBin/binary>>, X, Y, Map) ->
    parse_line(RBin, X + 1, Y, Map#{ {X, Y} => parse_slope(Slope, X, Y) });
parse_line(<<>>, _, _, Map) ->
    Map.

parse_slope($>, X, Y) -> {only_from, {X - 1, Y}};
parse_slope($<, X, Y) -> {only_from, {X + 1, Y}};
parse_slope($^, X, Y) -> {only_from, {X, Y + 1}};
parse_slope($v, X, Y) -> {only_from, {X, Y - 1}}.

