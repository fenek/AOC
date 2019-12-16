#!/usr/bin/env escript

-mode(compile).

-define(DEBUG, 0).

main([Filename, Steps]) ->
    {ok, Input} = file:read_file(Filename),
    Moons = make_moons(binary:split(Input, <<"\n">>, [global])),

    NMoons = simulate(list_to_integer(Steps), Moons),
    TotalEnergy = total_energy(NMoons),

    io:format("Start: ~p~n~nFinal: ~p~n~n~p~n~n", [Moons, NMoons, TotalEnergy]).

total_energy(Moons) ->
    maps:fold(fun(_, Moon, Total) ->
                      Total + potential_energy(Moon) * kinetic_energy(Moon)
              end, 0, Moons).

potential_energy(#{ x := X, y := Y, z := Z }) -> abs(X) + abs(Y) + abs(Z).

kinetic_energy(#{ vx := VX, vy := VY, vz := VZ }) -> abs(VX) + abs(VY) + abs(VZ).

make_moons(Inputs) ->
    make_moons(Inputs, 1, #{}).

make_moons([], _, Moons) ->
    Moons;
make_moons([In | RInputs], N, Moons) ->
    case re:run(In, "=([-0-9]+)", [global, {capture, all_but_first, binary}]) of
        {match, [[XBin], [YBin], [ZBin]]} ->
            NMoons = Moons#{ N => make_moon(b2i(XBin), b2i(YBin), b2i(ZBin)) },
            make_moons(RInputs, N + 1, NMoons);
        _ ->
            make_moons(RInputs, N, Moons)
    end.

make_moon(X, Y, Z) ->
    #{ x => X, y => Y, z => Z, vx => 0, vy => 0, vz => 0 }.

b2i(Bin) -> binary_to_integer(Bin).

simulate(0, Moons) ->
    Moons;
simulate(StepsLeft, Moons) ->
    Moons1 = apply_gravity(Moons),
    Moons2 = apply_velocity(Moons1),
    simulate(StepsLeft - 1, Moons2).

apply_gravity(Moons) ->
    lists:foldl(fun({N1, N2}, Moons0) ->
                        Moon1 = maps:get(N1, Moons0),
                        Moon2 = maps:get(N2, Moons0),

                        dbg("Updating ~p and ~p~n", [N1, N2]),
                        {NMoon1, NMoon2} = update_velocities(Moon1, Moon2),
                        dbg("~n"),

                        Moons0#{ N1 := NMoon1, N2 := NMoon2 }
                end, Moons, [ {X, Y} || X <- [1, 2 ,3], Y <- lists:seq(X + 1, 4) ]).

update_velocities(Moon1, Moon2) ->
    dbg("Before: ~p, ~p~n", [Moon1, Moon2]),
    Res = update_velocities(Moon1, Moon2, [{x, vx}, {y, vy}, {z, vz}]),
    dbg("After: ~p~n", [Res]),
    Res.

update_velocities(Moon1, Moon2, []) ->
    {Moon1, Moon2};
update_velocities(Moon1, Moon2, [{PosKey, VKey} | RKeys]) ->
    Pos1 = maps:get(PosKey, Moon1),
    Pos2 = maps:get(PosKey, Moon2),
    V1 = maps:get(VKey, Moon1),
    V2 = maps:get(VKey, Moon2),

    {NV1, NV2} = update_velocity(Pos1, Pos2, V1, V2),

    update_velocities(Moon1#{ VKey := NV1 }, Moon2#{ VKey := NV2 }, RKeys).

update_velocity(Pos, Pos, V1, V2) -> {V1, V2};
update_velocity(Pos1, Pos2, V1, V2) when Pos1 > Pos2 -> {V1 - 1, V2 + 1};
update_velocity(Pos1, Pos2, V1, V2) when Pos1 < Pos2 -> {V1 + 1, V2 - 1}.

apply_velocity(Moons) ->
    maps:fold(fun(N, #{ x := X, y := Y, z := Z, vx := VX, vy := VY, vz := VZ } = Moon, Moons0) ->
                      Moons0#{ N := Moon#{ x := X + VX, y := Y + VY, z := Z + VZ } }
              end, Moons, Moons).

-if(?DEBUG == 1).

dbg(String) -> dbg(String, []).
dbg(Fmt, Vals) -> io:format(Fmt, Vals).

-else.

dbg(_) -> ok.
dbg(_, _) -> ok.

-endif.

