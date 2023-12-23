#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).

-define(MINMOVE, 4).
-define(MAXMOVE, 10).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve([Line | _] = Lines) ->
    FinalX = byte_size(Line),
    FinalY = length(Lines),
    Heatmap = parse(Lines),
    PQ = pq_insert(0, {1, 1, 1, 0, 0}, pq_new()),
    Dists = dijkstra(Heatmap, PQ, #{}, sets:new([{version, 2}]), FinalX, FinalY),
    maps:fold(fun({X, Y, _, _, _}, Dist, Min) when X == FinalX, Y == FinalY, Dist < Min -> Dist;
                 (_, _, Min) -> Min end, infinity, Dists).


dijkstra(Heatmap, Q, Dist, Visited, MX, MY) ->
    case pq_empty(Q) of
        true ->
            Dist;
        false ->
            {PrevDist, U, NQ} = pq_next(Q),
            case sets:is_element(U, Visited) of
                true ->
                    dijkstra(Heatmap, NQ, Dist, Visited, MX, MY);
                false ->
                    Neighbours = neighbours(U, MX, MY),
                    process_neighbours(Heatmap, Neighbours, NQ, PrevDist, Dist,
                                       sets:add_element(U, Visited), MX, MY)
            end
    end.

process_neighbours(Heatmap, [{NX, NY, _DX, _DY, _Steps} = N | RNeighbours], Q0,
                   PrevDist, Dist0, Visited, MX, MY) ->
    case sets:is_element(N, Visited) of
        true ->
            process_neighbours(Heatmap, RNeighbours, Q0, PrevDist, Dist0, Visited, MX, MY);
        false ->
            #{ {NX, NY} := HeatN } = Heatmap,
            DistN = PrevDist + HeatN,
            OldDistN = maps:get(N, Dist0, infinity),
            case DistN < OldDistN of
                true ->
                    Dist = Dist0#{ N => DistN },
                    Q = pq_update(OldDistN, DistN, N, Q0),
                    process_neighbours(Heatmap, RNeighbours, Q, PrevDist, Dist, Visited, MX, MY);
                false ->
                    process_neighbours(Heatmap, RNeighbours, Q0, PrevDist, Dist0, Visited, MX, MY)
            end
    end;
process_neighbours(Heatmap, [], Q, _PrevDist, Dist, Visited, MX, MY) ->
    dijkstra(Heatmap, Q, Dist, Visited, MX, MY).

neighbours({X, Y, DX, DY, Steps}, MX, MY) when Steps < ?MINMOVE ->
    filter_neighbours([forward(X, Y, DX, DY, Steps)], MX, MY);
neighbours({X, Y, DX, DY, Steps}, MX, MY) when Steps == ?MAXMOVE ->
    filter_neighbours(turns(X, Y, DX, DY), MX, MY);
neighbours({X, Y, DX, DY, Steps}, MX, MY) ->
    filter_neighbours([forward(X, Y, DX, DY, Steps) | turns(X, Y, DX, DY)], MX, MY).

filter_neighbours(Vs, MX, MY) ->
    lists:filter(fun({NX, NY, _DX, _DY, _NSteps}) ->
                         NX >= 1 andalso NX =< MX andalso NY >= 1 andalso NY =< MY
                 end, Vs).

forward(X, Y, DX, DY, Steps) ->
    {X + DX, Y + DY, DX, DY, Steps + 1}.

turns(X, Y, 0, _) -> [{X - 1, Y, -1, 0, 1}, {X + 1, Y, 1, 0, 1}];
turns(X, Y, _, 0) -> [{X, Y - 1, 0, -1, 1}, {X, Y + 1, 0, 1, 1}].

pq_new() ->
    gb_trees:empty().

pq_insert(Weight, Object, PQ) ->
    case gb_trees:lookup(Weight, PQ) of
        none ->
            gb_trees:insert(Weight, [Object], PQ);
        {value, Objs} ->
            gb_trees:update(Weight, [Object | Objs], PQ)
    end.

pq_update(OldWeight, Weight, Object, PQ) ->
    case gb_trees:take_any(OldWeight, PQ) of
        error ->
            pq_insert(Weight, Object, PQ);
        {OldObjs, PQ2} ->
            PQ3 = case OldObjs =:= [Object] of
                      true -> PQ2;
                      false -> gb_trees:update(OldWeight, lists:delete(Object, OldObjs), PQ2)
                  end,
            pq_insert(Weight, Object, PQ3)
    end.

pq_empty(PQ) ->
    gb_trees:is_empty(PQ).

pq_next(PQ) ->
    case gb_trees:smallest(PQ) of
        {Weight, [Object]} ->
            {Weight, Object, gb_trees:delete(Weight, PQ)};
        {Weight, [Object | Objs]} ->
            {Weight, Object, gb_trees:update(Weight, Objs, PQ)}
    end.

parse(Lines) ->
    {_, Heatmap} = lists:foldl(fun parse_line/2, {1, #{}}, Lines),
    Heatmap.

parse_line(Line, {Y, Heatmap}) ->
    parse_line(Line, 1, Y, Heatmap).
parse_line(<<WeightCh:8, RBin/binary>>, X, Y, Heatmap) ->
    parse_line(RBin, X + 1, Y, Heatmap#{ {X, Y} => WeightCh - $0 });
parse_line(<<>>, _X, Y, Heatmap) ->
    {Y + 1, Heatmap}.
