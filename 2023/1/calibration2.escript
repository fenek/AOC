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

parse(<<>>, First, undefined) -> parse(<<>>, First, First);
parse(<<>>, First, Last) -> First * 10 + Last;
parse(<<"one", _/binary>> = Bin, First, _Last) -> store(1, Bin, First);
parse(<<"two", _/binary>> = Bin, First, _Last) -> store(2, Bin, First);
parse(<<"three", _/binary>> = Bin, First, _Last) -> store(3, Bin, First);
parse(<<"four", _/binary>> = Bin, First, _Last) -> store(4, Bin, First);
parse(<<"five", _/binary>> = Bin, First, _Last) -> store(5, Bin, First);
parse(<<"six", _/binary>> = Bin, First, _Last) -> store(6, Bin, First);
parse(<<"seven", _/binary>> = Bin, First, _Last) -> store(7, Bin, First);
parse(<<"eight", _/binary>> = Bin, First, _Last) -> store(8, Bin, First);
parse(<<"nine", _/binary>> = Bin, First, _Last) -> store(9, Bin, First);
parse(<<Ch:8, Rest/binary>>, First, Last) when Ch < $1; Ch > $9 -> parse(Rest, First, Last);
parse(<<Digit:8, _/binary>> = Bin, First, _Last) -> store(Digit - $0, Bin, First).

store(Number, <<_:8, Rest/binary>>, undefined) -> parse(Rest, Number, undefined);
store(Number, <<_:8, Rest/binary>>, First) -> parse(Rest, First, Number).
