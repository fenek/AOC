#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ begin
                                         io:format("Attempt ~w~n", [N]),
                                         timer:tc(?MODULE, solve, [Lines])
                                     end || N <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve([Line | _] = Lines) ->
    ets:new(memo, [named_table]),
    RowCount = length(Lines),
    ColCount = byte_size(Line),
    {MovablesCols, StaticsCols} = parse(Lines),
    Res = calc_load(memoized_cycle(MovablesCols, StaticsCols,
                                   RowCount, ColCount, 1_000_000_000), RowCount),
    ets:delete(memo),
    Res.

calc_load(MovablesCols, RowCount) ->
    lists:foldl(
      fun(Movables, Sum1) ->
              lists:foldl(fun(Y, Sum2) -> Sum2 + RowCount - Y + 1 end, Sum1, Movables)
      end, 0, maps:values(MovablesCols)).

memoized_cycle(MovablesCols, StaticsCols, RowCount, ColCount, MaxCycles) ->
    memoized_cycle(MovablesCols, StaticsCols, RowCount, ColCount, false, 0, MaxCycles).

memoized_cycle(MovablesCols, _SC, _RC, _CC, _LF, MaxCycles, MaxCycles) ->
    MovablesCols;
memoized_cycle(MovablesCols, StaticsCols, RowCount, ColCount, LoopFound, Cycle, MaxCycles) ->
    case get_next(MovablesCols) of
        undefined ->
            NewMovablesCols = cycle(MovablesCols, StaticsCols, RowCount, ColCount),
            put_next(MovablesCols, {NewMovablesCols, Cycle}),
            memoized_cycle(NewMovablesCols, StaticsCols, RowCount, ColCount, false, Cycle + 1, MaxCycles);
        {NewMovablesCols, _} when LoopFound ->
            memoized_cycle(NewMovablesCols, StaticsCols, RowCount, ColCount, true, Cycle + 1, MaxCycles);
        {NewMovablesCols, LoopStart} ->
            LoopLength = Cycle - LoopStart,
            ActuallyLeft = (MaxCycles - LoopStart) rem LoopLength,
            memoized_cycle(NewMovablesCols, StaticsCols, RowCount, ColCount, true, 1, ActuallyLeft)
    end.

get_next(MCs) ->
    case ets:lookup(memo, MCs) of
        [] -> undefined;
        [{_, Out}] -> Out
    end.

put_next(MCs, Next) ->
    ets:insert(memo, {MCs, Next}).

cycle(MovablesCols, StaticsCols, RowCount, ColCount) ->
    cycle(MovablesCols, StaticsCols, RowCount, ColCount, 4).

cycle(MovablesCols, _StaticsCols, _RowCount, _ColCount, 0) ->
    MovablesCols;
cycle(MovablesCols, StaticsCols, RowCount, ColCount, Left) ->
    NewMovablesCols = reverse_map_of_lists(tilt_and_rotate(MovablesCols, StaticsCols, RowCount, ColCount)),
    NewStaticsCols = reverse_map_of_lists(rot_statics(StaticsCols, RowCount, ColCount)),
    cycle(NewMovablesCols, NewStaticsCols, ColCount, RowCount, Left - 1).

rot_statics(StaticsCols, RowCount, ColCount) ->
    lists:foldl(fun(ColNum, RotatedAcc0) ->
                        Statics = maps:get(ColNum, StaticsCols),
                        lists:foldl(fun(Static, RotatedAcc1) ->
                                            RotatedLoc = RowCount - Static + 1,
                                            RotatedAcc1#{ RotatedLoc => [ ColNum | maps:get(RotatedLoc, RotatedAcc1) ] }
                                    end, RotatedAcc0, Statics)
                end, init_map(RowCount), lists:seq(1, ColCount)).

tilt_and_rotate(MovablesCols, StaticsCols, RowCount, ColCount) ->
    lists:foldl(fun(ColNum, RotatedAcc) ->
                        Movables = maps:get(ColNum, MovablesCols),
                        Statics = maps:get(ColNum, StaticsCols),
                        tilt_and_rotate(Movables, 0, Statics, RotatedAcc, RowCount, ColNum)
                end, init_map(RowCount), lists:seq(1, ColCount)).

tilt_and_rotate([Movable | _] = Movables, LastRock, [Static | RStatics], Rotated, RowCount, ColNum)
  when Static > LastRock, Movable > Static ->
    tilt_and_rotate(Movables, Static, RStatics, Rotated, RowCount, ColNum);
tilt_and_rotate([_ | RMovables], LastRock, Statics, Rotated, RowCount, ColNum) ->
    NewMovableLoc = LastRock + 1,
    RotatedLoc = RowCount - NewMovableLoc + 1,
    NewRotated = Rotated#{ RotatedLoc => [ColNum | maps:get(RotatedLoc, Rotated)] },
    tilt_and_rotate(RMovables, NewMovableLoc, Statics, NewRotated, RowCount, ColNum);
tilt_and_rotate([], _LR, _S, Rotated, _RC, _CN) ->
    Rotated.

reverse_map_of_lists(Map) ->
    maps:map(fun(_K, L) -> lists:reverse(L) end, Map).

parse([Line | _] = Lines) ->
    Len = byte_size(Line),
    {_, Movable, Static} = lists:foldl(fun parse/2, {1, init_map(Len), init_map(Len)}, Lines),
    {reverse_lists_in_map(Movable), reverse_lists_in_map(Static)}.

init_map(Len) ->
    maps:from_list([ {N, []} || N <- lists:seq(1, Len) ]).

parse(Line, {Y, Movable0, Static0}) ->
    {Movable, Static} = parse_row(Line, 1, Y, Movable0, Static0),
    {Y + 1, Movable, Static}.

parse_row(<<$O, RBin/binary>>, X, Y, Movable0, Static0) ->
    parse_row(RBin, X + 1, Y, Movable0#{ X => [ Y | maps:get(X, Movable0) ] }, Static0);
parse_row(<<$#, RBin/binary>>, X, Y, Movable0, Static0) ->
    parse_row(RBin, X + 1, Y, Movable0, Static0#{ X => [ Y | maps:get(X, Static0) ] });
parse_row(<<$., RBin/binary>>, X, Y, Movable, Static) ->
    parse_row(RBin, X + 1, Y, Movable, Static);
parse_row(<<>>, _, _, Movable, Static) ->
    {Movable, Static}.

reverse_lists_in_map(ListsMap) ->
    maps:map(fun(_K, List) -> lists:reverse(List) end, ListsMap).
