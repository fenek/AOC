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
    lists:min(solve(Seeds, Maps)).

solve(Seeds, Maps) ->
    [ map(Seed, Maps, seed) || Seed <- Seeds ].

map(Location, _Maps, location) ->
    Location;
map(Object, Maps, Type) ->
    #{ Type := Map, {Type, to} := NextType } = Maps,
    map(map_object(Object, Map), Maps, NextType).

map_object(Object, []) ->
    Object;
map_object(Object, [{SrcStart, Length, DestStart} | _])
  when Object >= SrcStart, Object < SrcStart + Length ->
    DestStart + Object - SrcStart;
map_object(Object, [_ | RMap]) ->
    map_object(Object, RMap).

parse_seeds(<<"seeds: ", SeedsBin/binary>>) ->
    parse_ints(SeedsBin).

parse_map(<<Digit:8, _/binary>> = Line, {Maps, Current}) when Digit >= $0, Digit =< $9 ->
    [DestStart, SrcStart, Length] = parse_ints(Line),
    {Maps#{ Current => [{SrcStart, Length, DestStart} | maps:get(Current, Maps)] }, Current};
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
