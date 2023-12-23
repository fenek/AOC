#!/usr/bin/env escript

-mode(compile).

main([Filename, Mode]) ->
    {ok, Input} = file:read_file(Filename),

    [SomeRow | _] = Rows = binary:split(string:trim(Input, both, "\n"), <<"\n">>, [global]),

    XMax = byte_size(SomeRow) - 1,
    YMax = length(Rows) - 1,

    Chart = load_chart(Rows),

    {Coords, Vectors, BestCount} = find_best_detection(Chart),

    case Mode of
        "seek" ->
            io:format("~p~n", [BestCount]);
        "destroy" ->
            AngVecList = make_ang_vec_list(Vectors),
            VectorOrder = lists:keysort(1, AngVecList),
            io:format("~p\t", [sets:size(Chart)]),
            DestroyedList = eradicate(Chart, Coords, VectorOrder, XMax, YMax, []),
            {X, Y} = lists:nth(200, lists:reverse(DestroyedList)),
            io:format("~n~p~n~n", [X*100+Y])
    end.

eradicate(Chart0, Coords, VectorOrder, XMax, YMax, DestroyedList0) ->
    {Chart, DestroyedList} = sweep(Chart0, Coords, VectorOrder, XMax, YMax, DestroyedList0),
    io:format("~p\t", [sets:size(Chart)]),
    case sets:size(Chart) of
        1 -> % because the asteroid we're on won't be destroyed
            DestroyedList;
        _ ->
            eradicate(Chart, Coords, VectorOrder, XMax, YMax, DestroyedList)
    end.

sweep(Chart, Coords, VectorOrder, XMax, YMax, DestroyedList) ->
    lists:foldl(fun({_, Vector}, {Chart0, DestroyedList0} = Acc) ->
                        case shoot(Coords, Vector, Chart0, XMax, YMax) of
                            false -> Acc;
                            {NCharts, Shot} -> {NCharts, [Shot | DestroyedList0]}
                        end
                end, {Chart, DestroyedList}, VectorOrder).

shoot(Coords, Vector, Chart0, XMax, YMax) ->
    shoot(Coords, Vector, Vector, Chart0, XMax, YMax).

shoot(Coords, Vector, BaseVector, Chart0, XMax, YMax) ->
    case sum_2d(Coords, Vector) of
        {X, Y} when X < 0; Y < 0; X > XMax; Y > YMax ->
            false;
        Target ->
            case sets:is_element(Target, Chart0) of
                true ->
                    {sets:del_element(Target, Chart0), Target};
                false ->
                    shoot(Coords, sum_2d(Vector, BaseVector), BaseVector, Chart0, XMax, YMax)
            end
    end.

sum_2d({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.

make_ang_vec_list(Vectors) ->
    sets:fold(fun(Vector, List0) ->
                      [ {angle(Vector), Vector} | List0 ]
              end, [], Vectors).

angle({X, _Y} = Vector) when X >= 0 ->
    angle180(Vector);
angle(Vector) ->
    2 * math:pi() - angle180(Vector).

angle180({X, Y}) ->
    math:acos(-Y / math:sqrt(X*X + Y*Y)).

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
    sets:fold(fun(Candidate, {_BCoords, _BVectors, BCount} = Best) ->
                      Vectors = detect(Candidate, sets:del_element(Candidate, Chart)),
                      case sets:size(Vectors) of
                          Count when Count > BCount -> {Candidate, Vectors, Count};
                          _ -> Best
                      end
              end, {undefined, undefined, -1}, Chart).

detect(Candidate, Chart) ->
    Vectors = sets:fold(fun(Object, Vectors0) ->
                                sets:add_element(vector(Candidate, Object), Vectors0)
                        end, sets:new(), Chart),
    %io:format("~p: ~p~n", [Candidate, sets:to_list(Vectors)]),
    Vectors.

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

