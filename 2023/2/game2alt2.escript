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
    lists:foldl(fun parse/2, 0, Lines).

parse(Line, PowerSum) ->
    {_ID, Power} = parse_game(Line),
    Power + PowerSum.

parse_game(<<"Game ", R/binary>>) ->
    {ID, <<": ", R2/binary>>} = parse_int(R),
    {ID, parse_draws(R2)}.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, R/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(R, Acc * 10 + Digit - $0);
parse_int(R, Int) ->
    {Int, R}.

parse_draws(Bin) ->
    parse_draw(Bin, 0, 0, 0).
parse_draw(<<", ", Rest/binary>>, MaxR, MaxG, MaxB) ->
    parse_draw(Rest, MaxR, MaxG, MaxB);
parse_draw(<<"; ", Rest/binary>>, MaxR, MaxG, MaxB) ->
    parse_draw(Rest, MaxR, MaxG, MaxB);
parse_draw(<<>>, MaxR, MaxG, MaxB) ->
    MaxR * MaxB * MaxG;
parse_draw(Bin, MaxR, MaxG, MaxB) ->
    {Count, Rest} = parse_int(Bin),
    case parse_type(Rest) of
        {red, Rest2} -> parse_draw(Rest2, max(Count, MaxR), MaxG, MaxB);
        {green, Rest2} -> parse_draw(Rest2, MaxR, max(Count, MaxG), MaxB);
        {blue, Rest2} -> parse_draw(Rest2, MaxR, MaxG, max(Count, MaxB))
    end.

parse_type(<<" red", Rest/binary>>) -> {red, Rest};
parse_type(<<" green", Rest/binary>>) -> {green, Rest};
parse_type(<<" blue", Rest/binary>>) -> {blue, Rest}.

