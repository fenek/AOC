#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
    {ok, Input} = file:read_file(Filename),

    Rows = binary:split(string:trim(Input, both, "\n"), <<"\n">>, [global]),

    Chart = load_chart(Rows),

    DetectedByBest = find_best_detection(Chart),

    io:format("~p~n", [DetectedByBest]).

load_chart(Rows) ->
    load_chart(Rows, 0, sets:new()).

load_chart([], _, Chart) ->
    Chart;
load_chart([Row | RRows], Y, Chart0) ->
    load_chart(RRows, Y + 1, load_row(Row, Y, Chart0)).

load_row(Row, Y, Chart) ->
    load_row(Row, 0, Y, Chart).

load_row(<<>>, _X, _Y, Chart) ->
    Chart;
load_row(<<$#, RRow/binary>>, X, Y, Chart0) ->
    load_row(RRow, X + 1, Y, sets:add_element({X, Y}, Chart0));
load_row(<<_, RRow/binary>>, X, Y, Chart0) ->
    load_row(RRow, X + 1, Y, Chart0).

find_best_detection(Chart) ->
    sets:fold(fun(Candidate, BestDetection) ->
                      case detect(Candidate, sets:del_element(Candidate, Chart)) of
                          NewBest when NewBest > BestDetection -> NewBest;
                          _ -> BestDetection
                      end
              end, 0, Chart).

detect(Candidate, Chart) ->
    Vectors = sets:fold(fun(Object, Vectors0) ->
                                sets:add_element(vector(Candidate, Object), Vectors0)
                        end, sets:new(), Chart),
    io:format("~p: ~p~n", [Candidate, sets:to_list(Vectors)]),
    sets:size(Vectors).

vector({X1, Y1}, {X2, Y2}) ->
    reduce(X2 - X1, Y2 - Y1).

reduce(DX, DY) ->
    reduce(DX, DY, 2).

reduce(DX, DY, Divisor) when Divisor > abs(DX), Divisor > abs(DY) ->
    {DX, DY};
reduce(DX, DY, Divisor) when DX rem Divisor == 0, DY rem Divisor == 0 ->
    reduce(DX div Divisor, DY div Divisor);
reduce(DX, DY, Divisor) ->
    reduce(DX, DY, Divisor + 1).

