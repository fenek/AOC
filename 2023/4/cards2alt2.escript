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

parse_card(<<"Card", Rest1/binary>>) ->
    {CardID, <<":", Rest2/binary>>} = parse_int(skip_spaces(Rest1)),
    {WinningNumbers, Rest3} = parse_ints(Rest2),
    {Numbers, <<>>} = parse_ints(Rest3),
    {CardID, calculate_points(WinningNumbers, Numbers)}.

skip_spaces(<<" ", R/binary>>) -> skip_spaces(R);
skip_spaces(Bin) -> Bin.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

parse_ints(Bin) ->
    parse_ints(Bin, []).
parse_ints(<<>>, Acc) ->
    {Acc, <<>>};
parse_ints(<<" | ", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_ints(<<" ", Bin/binary>>, Acc) ->
    parse_ints(Bin, Acc);
parse_ints(Bin, Acc) ->
    {Int, Rest} = parse_int(Bin),
    parse_ints(Rest, [Int | Acc]).

calculate_points(Winning, Numbers) ->
    sets:size(sets:intersection(sets:from_list(Winning, [{version, 2}]),
                                sets:from_list(Numbers, [{version, 2}]))).

process_card({_, Value}, {Sum, [Count | RCounts]}) ->
    {Sum + Count, bump_counts(Count, Value, RCounts)}.

bump_counts(_BumpValue, _Num, []) ->
    [];
bump_counts(_BumpValue, 0, Counts) ->
    Counts;
bump_counts(BumpValue, Num, [C | RCounts]) ->
    [C + BumpValue | bump_counts(BumpValue, Num - 1, RCounts)].

