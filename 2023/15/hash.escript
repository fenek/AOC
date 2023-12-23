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
    Steps = parse(Lines),
    lists:foldl(fun(Step, Sum) -> Sum + hash(Step) end, 0, Steps).

hash(Bin) ->
    hash(Bin, 0).
hash(<<Ch:8, RBin/binary>>, Acc) ->
    hash(RBin, ((Acc + Ch) * 17) rem 256);
hash(<<>>, Acc) ->
    Acc.

parse([Line]) ->
    binary:split(Line, <<",">>, [global]).

