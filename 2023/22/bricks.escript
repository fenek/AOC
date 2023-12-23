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

%% Deps :: #{ ID := {SupportedBy :: [], Supported :: []} }

solve(Lines) ->
    BricksSnapshot = parse(Lines),
    Bricks = lists:sort(fun({{_, _, Z1}, _}, {{_, _, Z2}, _}) -> Z1 =< Z2 end, BricksSnapshot),
    {_Space, Deps} = lists:foldl(fun place_brick/2, {#{}, #{}},
                                 lists:zip(lists:seq(1, length(Bricks)), Bricks)),
    count_disintegrable(Deps).

count_disintegrable(Deps) ->
    maps:fold(fun(_ID, {_, Supported}, Count) ->
                      case disintegrable(Supported, Deps) of
                          true -> Count + 1;
                          false -> Count
                      end
              end, 0, Deps).

disintegrable(Supported, Deps) ->
    lists:all(fun(SupportedID) ->
                      {SupportedBy, _} = maps:get(SupportedID, Deps),
                      length(SupportedBy) > 1
              end, Supported).

place_brick({ID, {Corner1, Corner2}}, {Space, Deps}) ->
    {Z, SupportedBy} = find_z(Corner1, Corner2, Space),
    {update_space(Space, Corner1, Corner2, Z, ID), update_deps(ID, SupportedBy, Deps)}.

find_z({X1, Y1, _BottomZ}, {X2, Y2, _}, Space) ->
    ViewFromTop = maps:values(maps:with(rect(X1, Y1, X2, Y2), Space)),
    SortedByZDesc = lists:reverse(lists:keysort(1, ViewFromTop)),
    case SortedByZDesc of
        [] ->
            {0, []};
        [{TopZ, _} | _] ->
            Supporters = lists:foldl(fun({Z, Supporter}, SupportersAcc) when Z == TopZ ->
                                             [Supporter | SupportersAcc];
                                        (_, SupportersAcc) ->
                                             SupportersAcc
                                     end, [], SortedByZDesc),
            {TopZ, lists:usort(Supporters)}
    end.

update_space(Space, {X1, Y1, Z1}, {X2, Y2, Z2}, ZOffset, ID) ->
    TopZ = ZOffset + Z2 - Z1 + 1,
    lists:foldl(fun({X, Y}, SpaceAcc) ->
                        SpaceAcc#{ {X, Y} => {TopZ, ID} }
                end, Space, rect(X1, Y1, X2, Y2)).

rect(X1, Y1, X2, Y2) ->
    [ {X, Y} || X <- lists:seq(X1, X2), Y <- lists:seq(Y1, Y2) ].

update_deps(ID, SupportedBy, Deps) ->
    lists:foldl(fun(Supporter, DepsAcc) ->
                        {SupporterSupportedBy, SupporterSupported} = maps:get(Supporter, DepsAcc),
                        DepsAcc#{ Supporter => {SupporterSupportedBy, [ID | SupporterSupported]} }
                end, Deps#{ ID => {SupportedBy, []} }, SupportedBy).

parse(Lines) ->
    lists:map(fun parse_line/1, Lines).

parse_line(Line) ->
    {Corner1, <<"~", RBin1/binary>>} = parse_triplet(Line),
    {Corner2, <<>>} = parse_triplet(RBin1),
    {Corner1, Corner2}.

parse_triplet(Bin) ->
    {A, <<",", RBin1/binary>>} = parse_int(Bin),
    {B, <<",", RBin2/binary>>} = parse_int(RBin1),
    {C, RBin3} = parse_int(RBin2),
    {{A, B, C}, RBin3}.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

