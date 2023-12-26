#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 10) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 10, lists:max(Timings)]).

solve(Lines) ->
    {_, #{ max_y := MaxY } = Map} = parse(Lines),
    [{InitialX, _} = Initial] = maps:keys(maps:filter(
                                            fun({_, Y}, _) -> Y == 1; (_, _) -> false end, Map)),
    [Exit] = maps:keys(maps:filter(
                         fun({_, Y}, _) -> Y == MaxY; (_, _) -> false end, Map)),
    Vertices = vertices([Initial], sets:from_list([{InitialX, 0}, Initial], [{version, 2}]),
                        [Initial], Map),

    Distances = distances([Exit | Vertices], #{}, Map),
    all_paths([{[Initial], Initial, 0}], 0, Exit, Distances).

all_paths([{_Visited, Exit, Dist} | RPaths], Max, Exit, Distances) ->
    all_paths(RPaths, max(Max, Dist), Exit, Distances);
all_paths([{Visited, Last, Total} | RPaths], Max, Exit, Distances) ->
    NextPaths = lists:filtermap(
                  fun({Next, Distance}) ->
                          case lists:member(Next, Visited) of
                              true -> false;
                              false -> {true, {[Next | Visited], Next, Total + Distance}}
                          end
                  end, maps:get(Last, Distances)),
    all_paths(NextPaths ++ RPaths, Max, Exit, Distances);
all_paths([], Max, _Exit, _Distances) ->
    Max.

distances([Vertex | RVertices], Edges, Map) ->
    ValidNeighbours = filter_by_map(neighbours(Vertex), Map),
    NEdges = lists:foldl(
               fun(Neighbour, EdgesAcc) ->
                       case trace(Vertex, Neighbour, 1, Map) of
                           undefined ->
                               EdgesAcc;
                           {Distance, NextVertex} ->
                               VertexDistances = lists:usort(
                                                   [{NextVertex, Distance}
                                                    | maps:get(Vertex, EdgesAcc, [])]),
                               NextVertexDistances = lists:usort(
                                                       [{Vertex, Distance}
                                                        | maps:get(NextVertex, EdgesAcc, [])]),
                               EdgesAcc#{ Vertex => VertexDistances,
                                          NextVertex => NextVertexDistances }
                       end
               end, Edges, ValidNeighbours),
    distances(RVertices, NEdges, Map);
distances([], Edges, _) ->
    Edges.

trace(Prev, Curr, Dist, Map) ->
    case filter_by_map(neighbours(Curr) -- [Prev], Map) of
        [] ->
            undefined;
        [Next] ->
            trace(Curr, Next, Dist + 1, Map);
        _Many ->
            {Dist, Curr}
    end.

vertices([], _, Vertices, _) ->
    Vertices;
vertices(Positions, Visited, Vertices, Map) ->
    {NewPositions, NewVisited, NewVertices} =
    lists:foldl(fun(Position, {PositionsAcc, VisitedAcc, VerticesAcc}) ->
                        ValidNeighbours = filter_by_map(neighbours(Position), Map),
                        NPositionsAcc = filter_by_visited(ValidNeighbours, VisitedAcc) ++ PositionsAcc,
                        NVisitedAcc = lists:foldl(fun sets:add_element/2, VisitedAcc, ValidNeighbours),
                        NVerticesAcc = case length(ValidNeighbours) >= 3 of
                                           true ->
                                               [Position | VerticesAcc];
                                           false ->
                                               VerticesAcc
                                       end,
                        {NPositionsAcc, NVisitedAcc, NVerticesAcc}
                end, {[], Visited, Vertices}, Positions),
    vertices(lists:usort(NewPositions), NewVisited, NewVertices, Map).

neighbours({X, Y}) ->
    [{X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}].

filter_by_map(Locations, Map) ->
    lists:filter(fun(Loc) -> maps:is_key(Loc, Map) end, Locations).

filter_by_visited(Locations, Visited) ->
    lists:filter(fun(Loc) -> not sets:is_element(Loc, Visited) end, Locations).

parse([Line0 | _] = Lines) ->
    lists:foldl(fun(Line, {Y, Map}) -> {Y + 1, parse_line(Line, Y, Map)} end,
                {1, #{ max_x => byte_size(Line0), max_y => length(Lines) }}, Lines).

parse_line(Line, Y, Map) ->
    parse_line(Line, 1, Y, Map).
parse_line(<<$#, RBin/binary>>, X, Y, Map) ->
    parse_line(RBin, X + 1, Y, Map);
parse_line(<<_SlopeOrWalkable:8, RBin/binary>>, X, Y, Map) ->
    parse_line(RBin, X + 1, Y, Map#{ {X, Y} => walkable });
parse_line(<<>>, _, _, Map) ->
    Map.
