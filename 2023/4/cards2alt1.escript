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
    Cards = lists:map(fun parse_card/1, Lines),
    {Sum, _} = lists:foldl(fun process_card/2, {0, lists:duplicate(length(Cards), 1)}, Cards),
    Sum.

parse_card(Line) ->
    {match, [_CardId | AllNumbers]} = re:run(Line, "\\d+", [global, {capture, all, binary}]),
    {WinningNumbers, Numbers} = lists:split(10, AllNumbers),
    calculate_points(to_ints(WinningNumbers), to_ints(Numbers)).

to_ints(IntBins) ->
    [ binary_to_integer(IntBin) || [IntBin] <- IntBins ].

calculate_points(Winning, Numbers) ->
    sets:size(sets:intersection(sets:from_list(Winning), sets:from_list(Numbers))).

process_card(Value, {Sum, [Count | RCounts]}) ->
    {Sum + Count, bump_counts(Count, Value, RCounts)}.

bump_counts(_BumpValue, _Num, []) ->
    [];
bump_counts(_BumpValue, 0, Counts) ->
    Counts;
bump_counts(BumpValue, Num, [C | RCounts]) ->
    [C + BumpValue | bump_counts(BumpValue, Num - 1, RCounts)].

