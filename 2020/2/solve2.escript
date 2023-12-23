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
    [Pos1Bin, Pos2Bin] = binary:split(Range, <<"-">>),
    Pos1 = btoi(Pos1Bin),
    Pos2 = btoi(Pos2Bin),
    (binary:at(Passwd, Pos1 - 1) == Letter) xor (binary:at(Passwd, Pos2 - 1) == Letter).

btoi(B) -> binary_to_integer(B).
