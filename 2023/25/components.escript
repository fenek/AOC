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

solve([<<StarterBin:3/binary, _/binary>> | _] = Lines) ->
    Vertex1 = atomize(StarterBin),
    Connections = parse(Lines),
    Heat = lists:foldl(
             fun(Vertex, Heat) ->
                     element(2, bfs_heat(Vertex, Heat, Connections))
             end, #{}, maps:keys(Connections)),
    CandidateEdges = candidate_edges(lists:reverse(lists:keysort(2, maps:to_list(Heat)))),
    try_combinations(Vertex1, edges_combinations(CandidateEdges), Connections).

try_combinations(ProbeVertex, [EdgeSet | REdges], Connections) ->
    NConnections = remove_edges(EdgeSet, Connections),
    case {bfs(ProbeVertex, NConnections), maps:size(Connections)} of
        {Size, Size} ->
            try_combinations(ProbeVertex, REdges, Connections);
        {PartitionSize, TotalSize} ->
            PartitionSize * (TotalSize - PartitionSize)
    end.

remove_edges([{V1, V2} | REdges], Connections) ->
    remove_edges(REdges, remove_edge(V1, V2, remove_edge(V2, V1, Connections)));
remove_edges([], Connections) ->
    Connections.

remove_edge(From, To, Connections) ->
    Connections#{ From => lists:delete(To, maps:get(From, Connections)) }.

edges_combinations(Edges) ->
    lists:usort([ lists:sort([E1, E2, E3]) || E1 <- Edges, E2 <- Edges, E3 <- Edges,
                                              E1 /= E2, E2 /= E3, E1 /= E3 ]).

candidate_edges([Edge | HeatList]) ->
    candidate_edges(HeatList, [Edge]).
candidate_edges([{_, V} = Edge | REdges], [{_, V} | _] = Acc) ->
    candidate_edges(REdges, [Edge | Acc]);
candidate_edges([Edge | REdges], Acc) when length(Acc) < 3 ->
    candidate_edges(REdges, [Edge | Acc]);
candidate_edges(_, Acc) ->
    [ element(1, E) || E <- Acc ].

bfs(Vertex, Connections) ->
    {Size, _} = bfs_heat(Vertex, #{}, Connections),
    Size.

bfs_heat(Vertex, Heat, Connections) ->
    bfs_heat([Vertex], sets:from_list([Vertex], [{version, 2}]), Heat, Connections).

bfs_heat([], Visited, Heat, _) ->
    {sets:size(Visited), Heat};
bfs_heat(Curr, Visited, Heat, Connections) ->
    NextEdgesMap = maps:map(fun(_K, Tos) ->
                                    lists:filter(fun(To) -> not sets:is_element(To, Visited) end, Tos)
                            end, maps:with(Curr, Connections)),
    Next = lists:usort(lists:flatten(maps:values(NextEdgesMap))),
    NextEdges = maps:fold(
                  fun(From, Tos, Edges) ->
                          [ make_edge(From, To) || To <- Tos ] ++ Edges
                  end, [], NextEdgesMap),
    bfs_heat(Next, sets:union(Visited, sets:from_list(Next, [{version, 2}])),
             bump_heat(NextEdges, Heat), Connections).

bump_heat(Edges, Heat) ->
    lists:foldl(fun(Edge, HeatAcc) ->
                        HeatAcc#{ Edge => maps:get(Edge, HeatAcc, 0) + 1}
                end, Heat, Edges).

make_edge(V1, V2) when V1 < V2 -> {V1, V2};
make_edge(V1, V2) -> {V2, V1}.

parse(Lines) ->
    lists:foldl(fun parse_line/2, #{}, Lines).

parse_line(Line, Connections) ->
    [Component1, <<" ", ComponentsBin/binary>>] = binary:split(Line, <<":">>),
    Components = binary:split(ComponentsBin, <<" ">>, [global]),
    connect(atomize(Component1), atomize(Components), Connections).

connect(Component, Components, Connections0) ->
    Connections1 = add_connections(Component, Components, Connections0),
    lists:foldl(fun(From, ConnectionsAcc) ->
                        add_connections(From, [Component], ConnectionsAcc)
                end, Connections1, Components).

add_connections(From, ToList, Connections) ->
    FromConnections = maps:get(From, Connections, []),
    Connections#{ From => lists:usort(ToList ++ FromConnections) }.

atomize(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, latin1);
atomize(List) ->
    lists:map(fun atomize/1, List).
