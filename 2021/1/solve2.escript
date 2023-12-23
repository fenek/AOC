#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, InputBin} = file:read_file("input"),
    LinesBin = binary:split(InputBin, <<"\n">>, [global]),
    Data = [ binary_to_integer(Bin) || Bin <- LinesBin, Bin /= <<>> ],
    io:format("~p~n", [count_increases(Data, 0)]).

count_increases([], Sum) ->
    Sum;
count_increases([_ | R] = Data, Sum) ->
    case {sum3(Data), sum3(R)} of
        {A, B} when is_integer(A), is_integer(B), B > A ->
            count_increases(R, Sum + 1);
        _ ->
            count_increases(R, Sum)
    end.

sum3([X, Y, Z | _]) -> X + Y + Z;
sum3(_) -> undefined.

