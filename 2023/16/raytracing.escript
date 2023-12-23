#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).
-export([raytrace/6]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve([Line | _] = Lines) ->
    Energized = ets:new(energized, [public, bag]),
    {_, Contraption0} = parse(Lines),
    Contraption = Contraption0#{ max_x => byte_size(Line), max_y => length(Lines) },
    Ray = send_ray(Contraption, 1, 1, right, Energized),
    collect_ray(Ray),
    Result = sets:size(sets:from_list([ element(1, T) || T <- ets:tab2list(Energized) ])),
    ets:delete(Energized),
    Result.

send_ray(Contraption, X, Y, Dir, Energized) ->
    spawn(?MODULE, raytrace, [Contraption, X, Y, Dir, Energized, self()]).

raytrace(#{ max_x := MaxX, max_y := MaxY } = _Contraption, X, Y, _Dir, _Energized, Parent)
  when X == 0; Y == 0; X > MaxX; Y > MaxY ->
    Parent ! self();
raytrace(Contraption, X, Y, Dir, Energized, Parent) ->
    case energize(X, Y, Dir, Energized) of
        already_energized ->
            Parent ! self();
        ok ->
            case maps:get({X, Y}, Contraption, empty) of
                {splitter, x} when Dir == up; Dir == down ->
                    Ray1 = send_ray(Contraption, X + 1, Y, right, Energized),
                    Ray2 = send_ray(Contraption, X - 1, Y, left, Energized),
                    collect_ray(Ray1),
                    collect_ray(Ray2),
                    Parent ! self();
                {splitter, y} when Dir == left; Dir == right ->
                    Ray1 = send_ray(Contraption, X, Y + 1, down, Energized),
                    Ray2 = send_ray(Contraption, X, Y - 1, up, Energized),
                    collect_ray(Ray1),
                    collect_ray(Ray2),
                    Parent ! self();
                {mirror, slash} when Dir == right ->
                    raytrace(Contraption, X, Y - 1, up, Energized, Parent);
                {mirror, slash} when Dir == down ->
                    raytrace(Contraption, X - 1, Y, left, Energized, Parent);
                {mirror, slash} when Dir == up ->
                    raytrace(Contraption, X + 1, Y, right, Energized, Parent);
                {mirror, slash} when Dir == left ->
                    raytrace(Contraption, X, Y + 1, down, Energized, Parent);
                {mirror, backslash} when Dir == right ->
                    raytrace(Contraption, X, Y + 1, down, Energized, Parent);
                {mirror, backslash} when Dir == down ->
                    raytrace(Contraption, X + 1, Y, right, Energized, Parent);
                {mirror, backslash} when Dir == up ->
                    raytrace(Contraption, X - 1, Y, left, Energized, Parent);
                {mirror, backslash} when Dir == left ->
                    raytrace(Contraption, X, Y - 1, up, Energized, Parent);
                _ ->
                    {NewX, NewY} = apply_dir(X, Y, Dir),
                    raytrace(Contraption, NewX, NewY, Dir, Energized, Parent)
            end
    end.

energize(X, Y, Dir, Energized) ->
    Dirs = ets:lookup(Energized, {X, Y}),
    case lists:keyfind(Dir, 2, Dirs)  of
        false ->
            ets:insert(Energized, {{X, Y}, Dir}),
            ok;
        _ ->
            already_energized
    end.

apply_dir(X, Y, up) -> {X, Y - 1};
apply_dir(X, Y, down) -> {X, Y + 1};
apply_dir(X, Y, left) -> {X - 1, Y};
apply_dir(X, Y, right) -> {X + 1, Y}.

collect_ray(Ray) ->
    receive
        Ray -> ok
    after
        5000 ->
            io:format("timeout~n"),
            error(timeout)
    end.

parse(Lines) ->
    lists:foldl(fun parse_line/2, {1, #{}}, Lines).

parse_line(Line, {Y, Contraption}) ->
    parse_line(Line, 1, Y, Contraption).
parse_line(<<$., RBin/binary>>, X, Y, Contraption) ->
    parse_line(RBin, X + 1, Y, Contraption);
parse_line(<<$|, RBin/binary>>, X, Y, Contraption) ->
    parse_line(RBin, X + 1, Y, Contraption#{ {X, Y} => {splitter, y} });
parse_line(<<$-, RBin/binary>>, X, Y, Contraption) ->
    parse_line(RBin, X + 1, Y, Contraption#{ {X, Y} => {splitter, x} });
parse_line(<<$/, RBin/binary>>, X, Y, Contraption) ->
    parse_line(RBin, X + 1, Y, Contraption#{ {X, Y} => {mirror, slash} });
parse_line(<<$\\, RBin/binary>>, X, Y, Contraption) ->
    parse_line(RBin, X + 1, Y, Contraption#{ {X, Y} => {mirror, backslash} });
parse_line(<<>>, _X, Y, Contraption) ->
    {Y + 1, Contraption}.

