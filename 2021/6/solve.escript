#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data0} = file:read_file("input"),
    Data = string:trim(Data0),
    Countdowns = binary:split(Data, <<",">>, [global]),
    Reg0 = initial_register(Countdowns),
    io:format("~p~n~p~n", [lists:sum(maps:values(sim(Reg0, 80))),
                           lists:sum(maps:values(sim(Reg0, 256)))]).

sim(Reg, 0) ->
    Reg;
sim(Reg0, Steps) ->
    ToGiveBirth = maps:get(0, Reg0, 0),
    #{ 6 := Sixes } = Reg1 = lists:foldl(
                               fun(C, RegAcc) ->
                                       RegAcc#{ C-1 => maps:get(C, RegAcc, 0) }
                               end, Reg0, lists:seq(1, 8)),
    sim(Reg1#{ 6 => Sixes + ToGiveBirth, 8 => ToGiveBirth }, Steps - 1).

initial_register(Countdowns) ->
    lists:foldl(fun(<<>>, Acc) -> Acc;
                   (CBin, Acc) ->
                        C = binary_to_integer(CBin),
                        Acc#{ C => maps:get(C, Acc, 0) + 1 }
                end, #{}, Countdowns).
