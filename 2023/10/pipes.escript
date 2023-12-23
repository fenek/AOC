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
    Map = ets:new(pipes_map, []),
    parse(Lines, Map, 1),
    solve_pipes(Map).

solve_pipes(Map) ->
    [{_, Coords}] = ets:lookup(Map, start),
    ets:insert(Map, {Coords, 0}),
    {_, InitialCoords} = move([ {Coords, Dir} || Dir <- [up, down, left, right] ], Map),
    solve(InitialCoords, Map, 1, 0).

solve([], _Map, _Distance, MaxLoop) ->
    MaxLoop;
solve(CoordsList, Map, Distance, MaxLoop) ->
    lists:foreach(fun({Coords, _Dir}) -> ets:insert(Map, {Coords, Distance}) end, CoordsList),
    {HasLoop, NextCoordsList} = move(CoordsList, Map),
    case HasLoop of
        true -> solve(NextCoordsList, Map, Distance + 1, max(MaxLoop, Distance));
        false -> solve(NextCoordsList, Map, Distance + 1, MaxLoop)
    end.

move(CoordsList, Map) ->
    move(CoordsList, Map, [], false).

move([{Coords, Dir} | RCoords], Map, Acc, HasLoop) ->
    DestCoords = get_next_coords(Coords, Dir),
    case ets:lookup(Map, DestCoords) of
        [] ->
            move(RCoords, Map, Acc, HasLoop);
        [{_, Distance}] when is_integer(Distance) ->
            move(RCoords, Map, Acc, true);
        [{_, Pipe}] ->
            case can_enter(Pipe, opposite(Dir)) of
                false ->
                    move(RCoords, Map, Acc, HasLoop);
                NextDirection ->
                    move(RCoords, Map, [{DestCoords, NextDirection} | Acc], HasLoop)
            end
    end;
move([], _Map, Acc, HasLoop) ->
    {HasLoop, Acc}.

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

parse([], _, _) ->
    ok;
parse([Line | Lines], Map, Y) ->
    parse_line(Line, Map, 1, Y),
    parse(Lines, Map, Y + 1).

parse_line(<<>>, _, _, _) ->
    ok;
parse_line(<<Symbol:8, R/binary>>, Map, X, Y) ->
    parse_symbol(Symbol, Map, X, Y),
    parse_line(R, Map, X + 1, Y).

parse_symbol($., _Map, _X, _Y) ->
    ok;
parse_symbol($S, Map, X, Y) ->
    ets:insert(Map, {start, {X, Y}});
parse_symbol(Pipe, Map, X, Y) ->
    ets:insert(Map, {{X, Y}, symbol_to_pipe(Pipe)}).

symbol_to_pipe($|) -> updown;
symbol_to_pipe($-) -> leftright;
symbol_to_pipe($F) -> downright;
symbol_to_pipe($7) -> downleft;
symbol_to_pipe($J) -> upleft;
symbol_to_pipe($L) -> upright.

