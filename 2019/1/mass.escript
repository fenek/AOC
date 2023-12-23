#!/bin/usr/env escript

-mode(compile).

main([Filename, Part]) ->
    {ok, Input} = file:read_file(Filename),
    MassesBin = binary:split(Input, <<"\n">>, [global]),
    CalcFuelFun = case Part of
                      "1" -> fun calc_fuel_simple/1;
                      "2" -> fun calc_fuel_recursive/1
                  end,
    Answer = lists:foldl(fun (<<>>, Acc) ->
                                 Acc;
                             (MassBin, Acc) ->
                                 Mass = binary_to_integer(MassBin),
                                 Acc + CalcFuelFun(Mass)
                         end, 0, MassesBin),

    io:format("~p~n~n", [Answer]).

calc_fuel_simple(Mass) ->
    Mass div 3 - 2.

calc_fuel_recursive(Mass) ->
    case Mass div 3 - 2 of
        CarriedByGoodWill when CarriedByGoodWill < 1 -> 0;
        MoarFuel -> MoarFuel + calc_fuel_recursive(MoarFuel)
    end.

