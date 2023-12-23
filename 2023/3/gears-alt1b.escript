#!/usr/bin/env escript

% Alternative 1: Use regular expressions instead of scan

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
    {ok, ReInt} = re:compile("\\d+"),
    put(re_int, ReInt),
    {ok, ReParts} = re:compile("[^\\d.]"),
    put(re_parts, ReParts),
    {_, Numbers, Parts} = lists:foldl(fun parse/2, {1, #{}, #{}}, Lines),
    sum_gears_ratios(get_possible_gears(Numbers, Parts)).

parse(Line, {Y, Numbers0, Parts0}) ->
    Numbers = case re:run(Line, get(re_int), [global]) of
                  nomatch ->
                      Numbers0;
                  {match, NumbersCoords} ->
                      lists:foldl(
                        fun([{Offset, Length}], NumbersAcc) ->
                                Number = binary_to_integer(binary:part(Line, Offset, Length)),
                                NumbersAcc#{ {Offset + 1, Y} => {Number, Length} }
                        end, Numbers0, NumbersCoords)
              end,
    Parts = case re:run(Line, get(re_parts), [global]) of
                nomatch ->
                    Parts0;
                {match, PartsCoords} ->
                    lists:foldl(
                      fun([{Offset, _}], PartsAcc) ->
                              PartsAcc#{ {Offset + 1, Y} => binary:at(Line, Offset) }
                      end, Parts0, PartsCoords)
            end,
    {Y + 1, Numbers, Parts}.

get_possible_gears(Numbers, Parts) ->
    MaybeGears0 = maps:filtermap(fun(_, $*) -> {true, []};
                                    (_, _) -> false end, Parts),
    maps:fold(fun(Coords, {Number, Length}, MaybeGears) ->
                apply_number(get_adjacent_parts(Coords, Length, MaybeGears), Number, MaybeGears)
              end, MaybeGears0, Numbers).

get_adjacent_parts(NumCoords, Length, Parts) ->
    lists:filter(fun(Coords) -> maps:is_key(Coords, Parts) end,
                 generate_adjacent(NumCoords, Length)).

generate_adjacent({NumX, NumY}, Length) ->
    [ {X, Y} || X <- lists:seq(NumX - 1, NumX + Length), Y <- lists:seq(NumY - 1, NumY + 1) ].

apply_number(CoordsList, Number, Parts0) ->
    lists:foldl(fun(Coords, Parts) ->
                    Parts#{ Coords := [ Number | maps:get(Coords, Parts) ] }
                end, Parts0, CoordsList).

sum_gears_ratios(MaybeGears) ->
    maps:fold(fun(_, [N1, N2], Sum) -> Sum + N1 * N2;
                 (_, _, Sum) -> Sum end, 0, MaybeGears).

