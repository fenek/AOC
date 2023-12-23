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
    Hands = parse(Lines),
    {_, Result} = lists:foldl(fun({_, _, Bid}, {N, Sum}) ->
                                      {N + 1, Sum + Bid * N}
                              end, {1, 0}, lists:sort(fun compare_hands/2, Hands)),
    Result.

compare_hands({Type, HandA, _}, {Type, HandB, _}) ->
    HandA < HandB;
compare_hands({TypeA, _, _}, {TypeB, _, _}) ->
    TypeA < TypeB.

parse(Lines) ->
    lists:map(fun parse_line/1, Lines).

parse_line(<<HandBin:5/binary, " ", BidBin/binary>>) ->
    {Bid, <<>>} = parse_int(BidBin),
    Hand = parse_hand(HandBin),
    Type = get_hand_type(Hand),
    {Type, Hand, Bid}.

get_hand_type(Hand) ->
    Counts = lists:foldl(fun(Card, Counts) ->
                                 Counts#{ Card => maps:get(Card, Counts, 0) + 1 }
                         end, #{}, Hand),
    get_type(lists:reverse(lists:sort(maps:values(Counts)))).

get_type([5]) -> 7; % five of a kind
get_type([4, 1]) -> 6; % four of a kind
get_type([3, 2]) -> 5; % full house
get_type([3 | _]) -> 4; % three of a kind
get_type([2, 2, 1]) -> 3; % two pairs
get_type([2 | _]) -> 2; % pair
get_type(_) -> 1. % high card

parse_hand(<<>>) ->
    [];
parse_hand(<<Card:8, R/binary>>) ->
    [card_to_int(Card) | parse_hand(R)].

card_to_int($T) -> 10;
card_to_int($J) -> 11;
card_to_int($Q) -> 12;
card_to_int($K) -> 13;
card_to_int($A) -> 14;
card_to_int(Num) -> Num - $0.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

