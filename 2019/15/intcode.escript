#!/usr/bin/env/escript

-mode(compile).

%-define(DBG, 1).

main([Filename]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = re:replace(Input0, <<"\\n">>, <<>>, [{return, binary}, global]),
    Codes = binary:split(Input, <<",">>, [global]),

    DroneZero = spawn_intcode(Codes, self(), self()),

    {OxygenSystemCoords, Depth, KnownTiles} = run_hive_mind(DroneZero),
    
    print_known_tiles(KnownTiles),

    io:format("~p, depth: ~p~n", [OxygenSystemCoords, Depth]),

    Minutes = spread_oxygen(KnownTiles, OxygenSystemCoords),

    io:format("Minutes to fill: ~p~n~n", [Minutes]).

spread_oxygen(Tiles, OxygenSystemCoords) ->
    spread_oxygen(Tiles, [OxygenSystemCoords], -1).

spread_oxygen(_Tiles, [], Minutes) ->
    Minutes;
spread_oxygen(Tiles, SpreadPoints, Minutes) ->
    {NTiles, NSpreadPoints} = spread_oxygen_step(Tiles, SpreadPoints, []),
    spread_oxygen(NTiles, NSpreadPoints, Minutes + 1).

spread_oxygen_step(NTiles, [], SpreadPointsAcc) ->
    {NTiles, SpreadPointsAcc};
spread_oxygen_step(Tiles0, [SpreadPoint | RSpreadPoints], SpreadPointsAcc) ->
    NewSpreadPoints = find_oxygen_spread(SpreadPoint, Tiles0),
    spread_oxygen_step(maps:without(NewSpreadPoints, Tiles0),
                       RSpreadPoints, NewSpreadPoints ++ SpreadPointsAcc).

find_oxygen_spread(Coords, Tiles) ->
     lists:flatmap(fun(Shift) ->
                          MaybeCoords = shift_coords(Coords, Shift),
                          case maps:get(MaybeCoords, Tiles, wall) of
                              wall -> [];
                              _ -> [MaybeCoords]
                          end
                  end, [{0, -1}, {0, 1}, {1, 0}, {-1, 0}]).   

shift_coords({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.

print_known_tiles(KnownTiles) ->
    {MinX, MinY, MaxX, MaxY} =
    maps:fold(fun({X, Y}, _, {MinX0, MinY0, MaxX0, MaxY0}) ->
                      {min(X, MinX0), min(Y, MinY0), max(X, MaxX0), max(Y, MaxY0)}
              end, {0, 0, 0, 0}, KnownTiles),
    lists:foreach(fun(Y) ->
                          lists:foreach(fun(X) ->
                                                print_tile({X, Y}, KnownTiles)
                                        end, lists:seq(MinX, MaxX)),
                          io:format("~n", [])
                  end, lists:seq(MinY, MaxY)),
    io:format("~n", []).

print_tile({0, 0}, _) ->
    io:format("0", []);
print_tile(Coords, KnownTiles) ->
    case maps:get(Coords, KnownTiles, undefined) of
        wall -> io:format("#", []);
        undefined -> io:format(" ", []);
        empty -> io:format(".", []);
        oxygen -> io:format("!", [])
    end.

run_hive_mind(DroneZero) ->
    KnownTiles = #{ {0, 0} => empty },
    hive_mind_replicate(DroneZero, #{ known_tiles => KnownTiles,
                                      drones => #{ DroneZero => {{0, 0}, 0} } }).

hive_mind(#{ known_tiles := KnownTiles, drones := Drones } = State) ->
    case maps:size(Drones) of
        0 ->
            {maps:get(oxygen_coords, State), maps:get(oxygen_depth, State), KnownTiles};
        _ ->
            wait_for_any_drone(State)
    end.

wait_for_any_drone(#{ known_tiles := KnownTiles0, drones := Drones0 } = State) ->
    case read() of
        {input, Drone, 0} ->
            {DroneTarget, _} = maps:get(Drone, Drones0),
            KnownTiles = KnownTiles0#{ DroneTarget => wall },
            Drones = terminate(Drone, Drones0),
            hive_mind(State#{ known_tiles := KnownTiles, drones := Drones});
        {input, Drone, 1} ->
            {DroneTarget, _} = maps:get(Drone, Drones0),
            NState = State#{ known_tiles := KnownTiles0#{ DroneTarget => empty } },
            hive_mind_replicate(Drone, NState);
        {input, Drone, 2} ->
            {Oxygen, Depth} = maps:get(Drone, Drones0),
            KnownTiles = KnownTiles0#{ Oxygen => oxygen },
            NState = State#{ known_tiles := KnownTiles,
                             oxygen_coords => Oxygen, oxygen_depth => Depth },
            hive_mind_replicate(Drone, NState)
    end.

hive_mind_replicate(Drone, #{ known_tiles := KnownTiles, drones := Drones0 } = State) ->
    {_, Depth} = maps:get(Drone, Drones0),
    input_request = read(Drone),
    PossibleDirections = possible_directions(Drone, KnownTiles, Drones0),
    Drones1 = lists:foldl(fun({TargetCoords, Command}, DronesAcc) ->
                                  dbg("NEW DRONE ~p (~p)~n", [TargetCoords, Command]),
                                  NewDrone = replicate(Drone),
                                  input_request = read(Drone),
                                  input_request = read(NewDrone),
                                  write(NewDrone, Command),
                                  DronesAcc#{ NewDrone => {TargetCoords, Depth + 1} }
                          end, Drones0, PossibleDirections),

    Drones = terminate(Drone, Drones1),
    hive_mind(State#{ known_tiles := KnownTiles, drones := Drones }).

terminate(Drone, Drones0) ->
    write(Drone, halt),
    case read(Drone) of
        input_request ->
            halt = read(Drone);
        halt ->
            ok
    end,
    maps:remove(Drone, Drones0).

possible_directions(Drone, KnownTiles, Drones) ->
    {Coords, _} = maps:get(Drone, Drones),
    lists:flatmap(fun(Dir) ->
                          MaybeCoords = apply_direction(Coords, Dir),
                          case maps:is_key(MaybeCoords, KnownTiles) of
                              true -> [];
                              false -> [{MaybeCoords, direction_to_command(Dir)}]
                          end
                  end, [north, south, east, west]).

apply_direction({X, Y}, north) -> {X, Y - 1};
apply_direction({X, Y}, south) -> {X, Y + 1};
apply_direction({X, Y}, east) -> {X + 1, Y};
apply_direction({X, Y}, west) -> {X - 1, Y}.

direction_to_command(north) -> 1;
direction_to_command(south) -> 2;
direction_to_command(east) -> 4;
direction_to_command(west) -> 3.

%% ----------------------------------------------------------------
%% INTCODE IMPLEMENTATION
%% ----------------------------------------------------------------

write(Pid, Int) ->
    Pid ! {input, self(), Int}.

read(Pid) ->
    receive
        {'DOWN', _Ref, process, Pid, normal} ->
            halt;
        {'DOWN', _Ref, process, Pid, Reason} ->
            exit({crash, {Pid, Reason}});
        {input, Pid, Int} ->
            Int;
        {input_request, Pid} ->
            input_request
    end.

read() ->
    receive
        {'DOWN', _Ref, process, Pid, normal} ->
            {halt, Pid};
        {'DOWN', _Ref, process, Pid, Reason} ->
            exit({crash, {Pid, Reason}});
        {input, Pid, Int} ->
            {input, Pid, Int};
        {input_request, Pid} ->
            {input_request, Pid}
    end.

spawn_intcode(Codes, Input, Output) ->
    {Pid, _Ref} = spawn_monitor(fun() -> init_intloop(Codes, Input, Output) end),
    Pid.

replicate(Pid) ->
    write(Pid, get_state),
    State = read(Pid),
    {NewPid, _Ref} = spawn_monitor(fun() -> init_intloop(State) end),
    NewPid.

init_intloop(State) ->
    lists:foreach(fun({K, V}) -> put(K, V) end, State),
    intloop(get(ip)).

init_intloop(Codes, Input, Output) ->
    put(relative_base, 0),
    put(input, Input),
    put(output, Output),

    store_program(0, Codes),
    
    intloop(0).

store_program(N, []) ->
    dbg("LOADED ~p~n", [N-1]),
    ok;
store_program(N, [CodeBin | RProgram]) ->
    store(N, binary_to_integer(CodeBin)),
    store_program(N + 1, RProgram).

intloop(IP) ->
    put(ip, IP),
    Opcode = load(IP),
%    dbg("~p: ~p~n", [IP, Opcode]),
    case Opcode rem 100 of
        1 ->
            addition(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        2 ->
            multiplication(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        3 -> 
            input(dest(Opcode, 1, IP)),
            intloop(IP + 2);
        4 ->
            output(param(Opcode, 1, IP)),
            intloop(IP + 2);
        5 ->
            intloop(jump_if_true(param(Opcode, 1, IP), param(Opcode, 2, IP), IP));
        6 ->
            intloop(jump_if_false(param(Opcode, 1, IP), param(Opcode, 2, IP), IP));
        7 ->
            less_than(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        8 ->
            equals(param(Opcode, 1, IP), param(Opcode, 2, IP), dest(Opcode, 3, IP)),
            intloop(IP + 4);
        9 ->
            update_relative_base(param(Opcode, 1, IP)),
            intloop(IP + 2);
        99 ->
            ok;
        _ ->
            exit({unknown_opcode, IP, Opcode})
    end.

param(Opcode, ParamPos, IP) ->
    Val =
    case param_mode(Opcode, ParamPos) of
        0 -> load(load(IP + ParamPos));
        1 -> load(IP + ParamPos);
        2 -> load(get(relative_base) + load(IP + ParamPos))
    end,
    %dbg("PARAM VAL ~p~n", [Val]),
    Val.

param_mode(Opcode, ParamPos) ->
    Mode = (Opcode div round(math:pow(10, 1 + ParamPos))) rem 10,
    %dbg("MODE ~p, ~p: ~p~n", [Opcode, ParamPos, Mode]),
    Mode.

dest(Opcode, ParamPos, IP) ->
    case param_mode(Opcode, ParamPos) of
        2 -> get(relative_base) + load(IP + ParamPos);
        _ -> load(IP + ParamPos)
    end.

addition(Int1, Int2, Dest) ->
    store(Dest, Int1 + Int2).

multiplication(Int1, Int2, Dest) ->
    store(Dest, Int1 * Int2).

input(Dest) ->
    InputSrc = get(input),
    InputSrc ! {input_request, self()},
    receive
        {input, InputSrc, get_state} ->
            output(get()),
            input(Dest);
        {input, InputSrc, halt} ->
            exit(normal);
        {input, InputSrc, Value} ->
            store(Dest, Value)
    end.

output(Value) ->
    get(output) ! {input, self(), Value}.

jump_if_true(0, _, IP) ->
    IP + 3;
jump_if_true(_, NewIP, _) ->
    NewIP.

jump_if_false(0, NewIP, _) ->
    NewIP;
jump_if_false(_, _, IP) ->
    IP + 3.

less_than(Int1, Int2, Dest) when Int1 < Int2 ->
    store(Dest, 1);
less_than(_, _, Dest) ->
    store(Dest, 0).

equals(Int, Int, Dest) ->
    store(Dest, 1);
equals(_, _, Dest) ->
    store(Dest, 0).

update_relative_base(Value) ->
    put(relative_base, get(relative_base) + Value).

store(Pos, Value) ->
    put(Pos, Value).

load(Pos) ->
    Loaded = case get(Pos) of
                 undefined -> 0;
                 Loaded0 -> Loaded0
             end,
    Loaded.

-ifdef(DBG).
dbg(Fmt, Vals) -> io:format(Fmt, Vals).
-else.
dbg(_, _) -> ok.
-endif.

