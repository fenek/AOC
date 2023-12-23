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
    Games = lists:foldl(fun parse/2, #{}, Lines),
    lists:sum(compute_powers(maps:values(Games))).

parse(Line, GamesAcc) ->
    {ID, Draws} = parse_game(Line),
    GamesAcc#{ ID => Draws }.

parse_game(Line) ->
    [<<"Game ", IDBin/binary>>, DrawsLine] = binary:split(Line, <<": ">>),
    DrawsRaw = binary:split(DrawsLine, <<"; ">>, [global]),
    Draws = lists:map(fun parse_draw/1, DrawsRaw),
    {binary_to_integer(IDBin), Draws}.

parse_draw(DrawRaw) ->
    TypesCountsRaw = binary:split(DrawRaw, <<", ">>, [global]),
    lists:foldl(fun(TypeCountRaw, {R, G, B}) ->
                        [CountBin, Type] = binary:split(TypeCountRaw, <<" ">>),
                        Count = binary_to_integer(CountBin),
                        case Type of
                            <<"red">> -> {Count, G, B};
                            <<"green">> -> {R, Count, B};
                            <<"blue">> -> {R, G, Count}
                        end
                end, {0, 0, 0}, TypesCountsRaw).

compute_powers(DrawsGroups) ->
    [ compute_power(Draws) || Draws <- DrawsGroups ].

compute_power(Draws) ->
    tuples_max(1, Draws) * tuples_max(2, Draws) * tuples_max(3, Draws).

tuples_max(Nth, TuplesList) ->
    lists:max([ element(Nth, Tuple) || Tuple <- TuplesList ]).

