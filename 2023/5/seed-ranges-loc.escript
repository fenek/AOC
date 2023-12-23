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
    parse(Lines).

parse([SeedsLine | MapsLines]) ->
    Seeds = parse_seeds(SeedsLine),
    {Maps, _} = lists:foldl(fun parse_map/2, {#{}, undefined}, MapsLines),
    solve(Seeds, sort_maps(Maps)).

solve(Seeds, Maps) ->
    map(Seeds, Maps, seed).

map(Locations, _Maps, location) ->
    lists:min([ element(1, T) || T <- Locations ]);
map(Objects, Maps, Type) ->
    #{ Type := Map, {Type, to} := NextType } = Maps,
    % It would be so nice to have Elixir's pipe operator in Erlang...
    map(map_objects(merge_overlaps(lists:keysort(1, Objects)), Map), Maps, NextType).

merge_overlaps([{A, B}, {C, D} | R]) when B >= C ->
    merge_overlaps([{A, max(B, D)} | R]);
merge_overlaps([I | R]) ->
    [I | merge_overlaps(R)];
merge_overlaps([]) ->
    [].

% Assumption: Objects and Map are sorted by the beggining of the intervals.

% No more objects.
map_objects([], _) ->
    [];
% No more mapping, all remaining objects are mapped with zero offset.
map_objects(Objects, []) ->
    Objects;
%                    |  object  |
% |  mapping src  |
% Object is entirely after the mapping, so we can discard the latter - no more objects
% will match it.
map_objects([{Start, _} | _] = Objects, [{_, SrcEnd, _} | RMap])
  when Start > SrcEnd ->
    map_objects(Objects, RMap);
% |  object  |
%               |  mapping src  |
% Object is entirely before the next mapping, it is copied with 0 offset.
map_objects([{_, End} = Object | RObjects], [{SrcStart, _, _} | _] = Map)
  when End < SrcStart ->
    [Object | map_objects(RObjects, Map)];
% |  object  ...
%       |  mapping src  |
% Object's Start is before Mapping Src Start but thanks to the previous clause we know
% that the end of the object is after Mapping Src Start. We split the object in two:
% {ObjStart, MappingStart - 1} and {MappingStart, ObjEnd} and readd them to the queue.
% The first subobject will be handled by the previous clause, the second subobject
% will be processed by one of the following ones.
map_objects([{Start, End} | RObjects], [{SrcStart, _, _} | _] = Map)
  when Start < SrcStart ->
    map_objects([{Start, SrcStart - 1}, {SrcStart, End} | RObjects], Map);
%     |      object      |
% |  mapping src  |
% Thanks to the previous clauses, we know that the object begins somewhere inside
% the Mapping and ends outside of it. We slice it into two new objects:
% one that is entirely inside the mapping now and the other that is entirely after the mapping.
map_objects([{Start, End} | RObjects], [{_, SrcEnd, _} | _] = Map)
  when End > SrcEnd ->
    map_objects([{Start, SrcEnd}, {SrcEnd + 1, End} | RObjects], Map);
%     |  object  |
% |  mapping src  |
% After all these slices and checks we know that the last remaining possibility
% is an object that entirely belongs to the mapping.
% "when" here is redundant but I've added it as a part of the debugging process. :)
map_objects([{Start, End} | RObjects], [{SrcStart, SrcEnd, Offset} | _] = Map)
  when Start >= SrcStart, End =< SrcEnd ->
    [{Start + Offset, End + Offset} | map_objects(RObjects, Map)].

%% ====================== PARSING ============================

sort_maps(Map) ->
    maps:map(fun({_Type, to}, Val) -> Val;
                (_Type, Val) -> lists:keysort(1, Val) end, Map).

parse_seeds(<<"seeds: ", SeedsBin/binary>>) ->
    make_seed_ranges(parse_ints(SeedsBin)).

make_seed_ranges([]) ->
    [];
make_seed_ranges([SBegin, Length | RSeeds]) ->
    [{SBegin, SBegin + Length - 1} | make_seed_ranges(RSeeds)].

parse_map(<<Digit:8, _/binary>> = Line, {Maps, Current}) when Digit >= $0, Digit =< $9 ->
    [DestStart, SrcStart, Length] = parse_ints(Line),
    Mapping = {SrcStart,
               SrcStart + Length - 1, % SrcEnd
               DestStart - SrcStart}, % Offset
    {Maps#{ Current => [Mapping | maps:get(Current, Maps)] }, Current};
parse_map(MapTypeLine, {Maps, _Current}) ->
    [FromBin, <<"to-", Rest/binary>>] = binary:split(MapTypeLine, <<"-">>),
    [ToBin, _] = binary:split(Rest, <<" ">>),
    From = binary_to_atom(FromBin),
    To = binary_to_atom(ToBin),
    {Maps#{ {From, to} => To, From => [] }, From}.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

parse_ints(Bin) ->
    parse_ints(Bin, []).
parse_ints(<<>>, Acc) ->
    lists:reverse(Acc);
parse_ints(<<" ", Bin/binary>>, Acc) ->
    parse_ints(Bin, Acc);
parse_ints(Bin, Acc) ->
    {Int, Rest} = parse_int(Bin),
    parse_ints(Rest, [Int | Acc]).
