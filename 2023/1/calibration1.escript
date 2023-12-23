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
    lists:foldl(fun(Line, Sum) -> Sum + parse(Line) end, 0, Lines).

parse(Line) ->
    parse(Line, undefined, undefined).

parse(<<>>, First, undefined) ->
    parse(<<>>, First, First);
parse(<<>>, First, Last) ->
    (First - $0) * 10 + Last - $0;
parse(<<Ch:8, Rest/binary>>, First, Last) when Ch < $0; Ch > $9 ->
    parse(Rest, First, Last);
parse(<<Digit:8, Rest/binary>>, undefined, _) ->
    parse(Rest, Digit, undefined);
parse(<<Digit:8, Rest/binary>>, First, _) ->
    parse(Rest, First, Digit).

