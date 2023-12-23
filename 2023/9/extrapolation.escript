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
    lists:sum(lists:map(fun get_next_value/1, parse(Lines))).

get_next_value([V2 | _] = Values) ->
    V2 + get_next_delta(Values).

get_next_delta(Values) ->
    case get_deltas(Values) of
        {_, true} ->
            0;
        {Deltas, _} ->
            get_next_value(Deltas)
    end.

get_deltas(Values) ->
    get_deltas(Values, [], true).
get_deltas([V2, V1 | R], Deltas, AllZeros) ->
    Delta = V2 - V1,
    get_deltas([V1 | R], [Delta | Deltas], AllZeros and (Delta == 0));
get_deltas([_], Deltas, AllZeros) ->
    {lists:reverse(Deltas), AllZeros}.

parse(Lines) ->
    lists:map(fun parse_ints/1, Lines).

parse_ints(Bin) ->
    parse_ints(Bin, []).
parse_ints(<<>>, Acc) ->
    Acc; % Deliberately left inversed!
parse_ints(<<" ", Bin/binary>>, Acc) ->
    parse_ints(Bin, Acc);
parse_ints(Bin, Acc) ->
    {Int, Rest} = parse_int(Bin),
    parse_ints(Rest, [Int | Acc]).

parse_int(Bin) ->
    parse_int(Bin, 0, 1).
parse_int(<<Digit:8, Rest/binary>>, Acc, Sign) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0, Sign);
parse_int(<<$-, Rest/binary>>, Acc, _Sign) ->
    parse_int(Rest, Acc, -1);
parse_int(Bin, Acc, Sign) ->
    {Acc * Sign, Bin}.

