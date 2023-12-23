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
    lists:foldl(fun(Card, Sum) -> Sum + calculate_points(Card) end, 0, Cards).

parse_card(<<"Card", Rest1/binary>>) ->
    {CardID, <<":", Rest2/binary>>} = parse_int(skip_spaces(Rest1)),
    {WinningNumbers, Rest3} = parse_ints(Rest2),
    {Numbers, <<>>} = parse_ints(Rest3),
    {CardID, WinningNumbers, Numbers}.

skip_spaces(<<" ", R/binary>>) ->
    skip_spaces(R);
skip_spaces(Bin) ->
    Bin.

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

calculate_points({_, Winning, Numbers}) ->
    lists:foldl(fun(Num, Sum) ->
                        case lists:member(Num, Winning) of
                            true -> max(1, Sum * 2);
                            false -> Sum
                        end
                end, 0, Numbers).

