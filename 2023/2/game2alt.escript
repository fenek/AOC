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
    Games = lists:foldl(fun parse/2, #{}, Lines),
    lists:sum(compute_powers(maps:values(Games))).

parse(Line, GamesAcc) ->
    {ID, Draws} = parse_game(Line),
    GamesAcc#{ ID => Draws }.

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
    parse_draw(Bin, 0, 0, 0, []).
parse_draw(<<", ", Rest/binary>>, R, G, B, Draws) ->
    parse_draw(Rest, R, G, B, Draws);
parse_draw(<<"; ", Rest/binary>>, R, G, B, Draws) ->
    parse_draw(Rest, 0, 0, 0, [{R, G, B} | Draws]);
parse_draw(<<>>, R, G, B, Draws) ->
    [{R, G, B} | Draws];
parse_draw(Bin, R, G, B, Draws) ->
    {Count, Rest} = parse_int(Bin),
    case parse_type(Rest) of
        {red, Rest2} -> parse_draw(Rest2, Count, G, B, Draws);
        {green, Rest2} -> parse_draw(Rest2, R, Count, B, Draws);
        {blue, Rest2} -> parse_draw(Rest2, R, G, Count, Draws)
    end.

parse_type(<<" red", Rest/binary>>) -> {red, Rest};
parse_type(<<" green", Rest/binary>>) -> {green, Rest};
parse_type(<<" blue", Rest/binary>>) -> {blue, Rest}.

compute_powers(DrawsGroups) ->
    [ compute_power(Draws) || Draws <- DrawsGroups ].

compute_power(Draws) ->
    tuples_max(1, Draws) * tuples_max(2, Draws) * tuples_max(3, Draws).

tuples_max(Nth, TuplesList) ->
    lists:max([ element(Nth, Tuple) || Tuple <- TuplesList ]).

