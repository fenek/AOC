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
    calc(parse(Lines)).

parse([<<"Time:", TimeLine/binary>>, <<"Distance:", DistanceLine/binary>>]) ->
    Times = [parse_int(TimeLine)],
    Distances = [parse_int(DistanceLine)],
    lists:zip(Times, Distances).

calc(Races) ->
    lists:foldl(fun({Time, Distance}, Result) -> calc(Time, Distance) * Result end, 1, Races).

calc(T, D) ->
    X1 = trunc(math:floor((-T - math:sqrt(T * T - 4 * D)) / -2)),
    X2 = trunc(math:ceil((-T + math:sqrt(T * T - 4 * D)) / -2)),
    sanitize(T, D, X1, X1 - 1) - sanitize(T, D, X2, X2 + 1) + 1.

sanitize(T, D, V1, V2) ->
    case (T-V1) * V1 == D of
        true -> V2;
        false -> V1
    end.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<>>, Acc) ->
    Acc;
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(<<_:8, Bin/binary>>, Acc) ->
    parse_int(Bin, Acc).
