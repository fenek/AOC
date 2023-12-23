#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, InputBin} = file:read_file("input"),
    LinesBin = binary:split(InputBin, <<"\n">>, [global]),
    io:format("~p~n", [count_increases(LinesBin, infinity, 0)]).

count_increases([], _, Sum) ->
    Sum;
count_increases([<<>> | R], Comp, Sum) ->
    count_increases(R, Comp, Sum);
count_increases([Bin | R], Comp, Sum) ->
    case binary_to_integer(Bin) of
        Int when Int > Comp ->
            count_increases(R, Int, Sum + 1);
        Int ->
            count_increases(R, Int, Sum)
    end.
