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
    FinalX = byte_size(Line),
    FinalY = length(Lines),
    Heatmap = parse(Lines),
    Dists = dijkstra(Heatmap, [{0, {1, 1, 1, 0, 0}}], #{}, sets:new([{version, 2}]), FinalX, FinalY),
    maps:fold(fun({X, Y, _, _, _}, Dist, Min) when X == FinalX, Y == FinalY, Dist < Min -> Dist;
                 (_, _, Min) -> Min end, infinity, Dists).


dijkstra(Heatmap, [{PrevDist, U} | RQ], Dist, Visited, MX, MY) ->
    Neighbours = neighbours(U, MX, MY),
    process_neighbours(Heatmap, Neighbours, RQ, PrevDist, Dist,
                       sets:add_element(U, Visited), MX, MY);
dijkstra(_H, [], Dist, _V, _MX, _MY) ->
    Dist.

process_neighbours(Heatmap, [{NX, NY, _DX, _DY, _Steps} = N | RNeighbours], Q0,
                   PrevDist, Dist0, Visited, MX, MY) ->
    case sets:is_element(N, Visited) of
        true ->
            process_neighbours(Heatmap, RNeighbours, Q0, PrevDist, Dist0, Visited, MX, MY);
        false ->
            #{ {NX, NY} := HeatN } = Heatmap,
            DistN = PrevDist + HeatN,
            case DistN < maps:get(N, Dist0, infinity) of
                true ->
                    Dist = Dist0#{ N => DistN },
                    Q = lists:keystore(N, 2, Q0, {DistN, N}),
                    process_neighbours(Heatmap, RNeighbours, Q, PrevDist, Dist, Visited, MX, MY);
                false ->
                    process_neighbours(Heatmap, RNeighbours, Q0, PrevDist, Dist0, Visited, MX, MY)
            end
    end;
process_neighbours(Heatmap, [], Q, _PrevDist, Dist, Visited, MX, MY) ->
    dijkstra(Heatmap, lists:keysort(1, Q), Dist, Visited, MX, MY).

neighbours({X, Y, DX, DY, Steps}, MX, MY) ->
    lists:filter(fun({NX, NY, _DX, _DY, NSteps}) ->
                         NX >= 1 andalso NX =< MX andalso NY >= 1 andalso NY =< MY
                         andalso NSteps =< 3
                 end, [{X + DX, Y + DY, DX, DY, Steps + 1} | turns(X, Y, DX, DY)]).

turns(X, Y, 0, _) -> [{X - 1, Y, -1, 0, 1}, {X + 1, Y, 1, 0, 1}];
turns(X, Y, _, 0) -> [{X, Y - 1, 0, -1, 1}, {X, Y + 1, 0, 1, 1}].

parse(Lines) ->
    {_, Heatmap} = lists:foldl(fun parse_line/2, {1, #{}}, Lines),
    Heatmap.

parse_line(Line, {Y, Heatmap}) ->
    parse_line(Line, 1, Y, Heatmap).
parse_line(<<WeightCh:8, RBin/binary>>, X, Y, Heatmap) ->
    parse_line(RBin, X + 1, Y, Heatmap#{ {X, Y} => WeightCh - $0 });
parse_line(<<>>, _X, Y, Heatmap) ->
    {Y + 1, Heatmap}.
