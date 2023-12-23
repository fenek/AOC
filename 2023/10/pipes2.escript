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
    ets:new(pipes_map, [named_table]),
    parse(Lines, 1),
    ets:new(pipes_shapes, [named_table]),
    ets:insert(pipes_shapes, ets:tab2list(pipes_map)),
    Result = solve_pipes(),
    ets:delete(pipes_map),
    ets:delete(pipes_shapes),
    Result.

solve_pipes() ->
    [{_, Coords}] = ets:lookup(pipes_map, start),
    ets:insert(pipes_map, {Coords, 0}),
    {_, InitialPaths} = move([ [{Coords, Dir}] || Dir <- [up, down, left, right] ]),
    LongestLoop = get_longest_loop(InitialPaths, 1, []),
    update_start_type(LongestLoop),
    LoopMap = paths_to_map(LongestLoop),
    inner_area(LoopMap).

inner_area(LoopMap) ->
    lists:foldl(fun(X, Sum) ->
                        YsWTypes = lists:map(fun(Y) ->
                                                     [{_, Type}] = ets:lookup(pipes_shapes, {X, Y}),
                                                     {Y, Type}
                                             end, maps:get(X, LoopMap)),
                        Sum + inner_area_slice(YsWTypes)
                end, 0, lists:sort(maps:keys(LoopMap))).

inner_area_slice(Ys) ->
    SortedYs = lists:usort(Ys),
    %io:format("~w~n", [SortedYs]),
    outside(SortedYs, 0).

outside([{_, _} = Pipe, {_, updown} | RY], Sum) ->
    outside([Pipe | RY], Sum);
outside([{_, T1}, {_, T2} = Pipe2 | RY], Sum)
  when (T1 == downleft andalso T2 == upright) orelse (T1 == downright andalso T2 == upleft) ->
    inside([Pipe2 | RY], Sum);
outside([{_, T1}, {_, T2} | RY], Sum)
  when (T1 == downleft andalso T2 == upleft) orelse (T1 == downright andalso T2 == upright) ->
    outside(RY, Sum);
outside([{Y, leftright} | RY], Sum) ->
    inside([{Y, border} | RY], Sum);
outside([], Sum) ->
    Sum.

inside([{_, _} = Pipe, {_, updown} | RY], Sum) ->
    inside([Pipe | RY], Sum);
inside([{_, T1}, {_, T2} | RY], Sum)
  when (T1 == downleft andalso T2 == upright) orelse (T1 == downright andalso T2 == upleft) ->
    outside(RY, Sum);
inside([{_, T1}, {_, T2} = Pipe2 | RY], Sum)
  when (T1 == downleft andalso T2 == upleft) orelse (T1 == downright andalso T2 == upright) ->
    inside([Pipe2 | RY], Sum);
inside([{_, leftright} | RY], Sum) ->
    outside(RY, Sum);
inside([{Y1, _}, {Y2, _} = Pipe2 | RY], Sum) ->
    inside([Pipe2 | RY], Sum + Y2 - Y1 - 1).

paths_to_map(Paths) ->
    lists:foldl(fun(Path, Map) ->
                        lists:foldl(fun index_coords/2, Map, Path)
                end, #{}, Paths).

index_coords({X, Y}, Map) ->
    Map#{ X => [ Y | maps:get(X, Map, [])] }.

update_start_type([Path1, Path2]) ->
    [{SX, SY} = StartCoords, {X1, Y1} | _] = lists:reverse(Path1),
    [StartCoords, {X2, Y2} | _] = lists:reverse(Path2),
    Type = type_from_adjacent(SX, SY, X1, Y1, X2, Y2),
    ets:insert(pipes_shapes, {StartCoords, Type}).

type_from_adjacent(SX, SY, X1, Y1, X2, Y2) ->
    case {relative_pos(SX, SY, X1, Y1), relative_pos(SX, SY, X2, Y2)} of
        {"down", "up"} ->
            updown;
        {Pos1, Pos2} when Pos1 == "up"; Pos1 == "down" ->
            list_to_atom(Pos1 ++ Pos2);
        {Pos1, Pos2} ->
            list_to_atom(Pos2 ++ Pos1)
    end.

relative_pos(SX, SY, SX, Y) when Y < SY -> "up";
relative_pos(SX, _SY, SX, _Y) -> "down";
relative_pos(SX, SY, X, SY) when X < SX -> "left";
relative_pos(_SX, SY, _X, SY) -> "right".

get_longest_loop([], _Distance, LongestLooping) ->
    LongestLooping;
get_longest_loop(PathsList, Distance, LongestLooping) ->
    lists:foreach(fun([{Coords, _Dir} | _]) -> ets:insert(pipes_map, {Coords, Distance}) end, PathsList),
    {LoopingPaths, NextPathsList} = move(PathsList),
    case LoopingPaths of
        [] -> get_longest_loop(NextPathsList, Distance + 1, LongestLooping);
        _ -> get_longest_loop(NextPathsList, Distance + 1, LoopingPaths)
    end.

move(PathsList) ->
    move(PathsList, [], []).

move([[{Coords, Dir} | RPath] | RPathsList], Acc, LoopingPaths) ->
    DestCoords = get_next_coords(Coords, Dir),
    case ets:lookup(pipes_map, DestCoords) of
        [] ->
            move(RPathsList, Acc, LoopingPaths);
        [{_, Distance}] when is_integer(Distance) ->
            move(RPathsList, Acc, [[Coords | RPath] | LoopingPaths]);
        [{_, Pipe}] ->
            case can_enter(Pipe, opposite(Dir)) of
                false ->
                    move(RPathsList, Acc, LoopingPaths);
                NextDirection ->
                    move(RPathsList, [[{DestCoords, NextDirection}, Coords | RPath] | Acc], LoopingPaths)
            end
    end;
move([], Acc, LoopingPaths) ->
    {LoopingPaths, Acc}.

get_next_coords({X, Y}, up) -> {X, Y - 1};
get_next_coords({X, Y}, down) -> {X, Y + 1};
get_next_coords({X, Y}, left) -> {X - 1, Y};
get_next_coords({X, Y}, right) -> {X + 1, Y}.

can_enter(updown, up) -> down;
can_enter(updown, down) -> up;
can_enter(leftright, left) -> right;
can_enter(leftright, right) -> left;
can_enter(downright, right) -> down;
can_enter(downright, down) -> right;
can_enter(downleft, left) -> down;
can_enter(downleft, down) -> left;
can_enter(upleft, up) -> left;
can_enter(upleft, left) -> up;
can_enter(upright, up) -> right;
can_enter(upright, right) -> up;
can_enter(_, _) -> false.

opposite(left) -> right;
opposite(right) -> left;
opposite(up) -> down;
opposite(down) -> up.

parse([], _) ->
    ok;
parse([Line | Lines], Y) ->
    parse_line(Line, 1, Y),
    parse(Lines, Y + 1).

parse_line(<<>>, _, _) ->
    ok;
parse_line(<<Symbol:8, R/binary>>, X, Y) ->
    parse_symbol(Symbol, X, Y),
    parse_line(R, X + 1, Y).

parse_symbol($., _X, _Y) ->
    ok;
parse_symbol($S, X, Y) ->
    ets:insert(pipes_map, {start, {X, Y}});
parse_symbol(Pipe, X, Y) ->
    ets:insert(pipes_map, {{X, Y}, symbol_to_pipe(Pipe)}).

symbol_to_pipe($|) -> updown;
symbol_to_pipe($-) -> leftright;
symbol_to_pipe($F) -> downright;
symbol_to_pipe($7) -> downleft;
symbol_to_pipe($J) -> upleft;
symbol_to_pipe($L) -> upright.

