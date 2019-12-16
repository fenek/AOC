#!/usr/bin/env escript

-mode(compile).

main([Filename, Part]) ->
    {ok, Input} = file:read_file(Filename),
    OrbitsData = binary:split(Input, <<"\n">>, [global]),
    
    Orbits = load_orbits(OrbitsData),

    case Part of
        "1" ->
            Count = count_orbits(<<"COM">>, Orbits, 0),
            io:format("~p~n~n", [Count]);
        "2" ->
            PathToYou = path_to(<<"YOU">>, Orbits),
            PathToSan = path_to(<<"SAN">>, Orbits),
            {YouBranch, SanBranch} = discard_common_prefix(PathToYou, PathToSan),
            % JumpsToCommonObjects = number of objects in YouBranch
            %                        minus YOU and minus current object
            %                        and plus the common object that got discarded by
            %                        discard_common_prefix/2
            JumpsToCommonObject = length(YouBranch) - 2 + 1,
            % JumpsToSantaObject = number of objects in SanBranch
            %                      minus SAN himself
            JumpsToSantaObject = length(SanBranch) - 1,
            io:format("~p~n~n", [JumpsToCommonObject + JumpsToSantaObject])
    end.

load_orbits(OrbitsData) ->
    lists:foldl(fun(<<>>, Orbits0) ->
                        Orbits0;
                   (OrbitIn, Orbits0) ->
                        [Centre, Orbiter] = binary:split(OrbitIn, <<")">>),
                        Orbiters = maps:get(Centre, Orbits0, []),
                        Orbits0#{ Centre => [Orbiter | Orbiters] }
                end, #{}, OrbitsData).

count_orbits(Centre, Orbits, Depth) ->
    lists:foldl(fun(Centre0, Count0) ->
                        Count0 + count_orbits(Centre0, Orbits, Depth + 1)
                end, Depth, maps:get(Centre, Orbits, [])).

path_to(Target, Orbits) ->
    path_to(Target, <<"COM">>, Orbits).

path_to(Target, Target, _Orbits) ->
    [Target];
path_to(Target, Object, Orbits) ->
    Orbiters = maps:get(Object, Orbits, []),
    case check_orbiters(Target, Orbiters, Orbits) of
        undefined ->
            undefined;
        Path ->
            [Object | Path]
    end.

check_orbiters(_, [], _) ->
    undefined;
check_orbiters(Target, [Orbiter | ROrbiters], Orbits) ->
    case path_to(Target, Orbiter, Orbits) of
        undefined ->
            check_orbiters(Target, ROrbiters, Orbits);
        Path ->
            Path
    end.

discard_common_prefix([Same | R1], [Same | R2]) ->
    discard_common_prefix(R1, R2);
discard_common_prefix(L1, L2) ->
    {L1, L2}.

