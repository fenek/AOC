#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    Res = lists:foldl(fun(Line, Sum) ->
                              case validate(Line) of
                                  true -> Sum + 1;
                                  false -> Sum
                              end
                      end, 0, Lines),
    io:format("~p~n", [Res]).

validate(<<>>) ->
    false;
validate(Line) ->
    [Range, <<Letter:8, $:>>, Passwd] = binary:split(Line, <<" ">>, [global]),
    [MinBin, MaxBin] = binary:split(Range, <<"-">>),
    Min = btoi(MinBin),
    Max = btoi(MaxBin),
    Count = count(Passwd, Letter),
    Count >= Min andalso Count =< Max.

count(<<>>, _) -> 0;
count(<<Letter:8, R/binary>>, Letter) -> 1 + count(R, Letter);
count(<<_:8, R/binary>>, Letter) -> count(R, Letter).

btoi(Bin) -> binary_to_integer(Bin).
