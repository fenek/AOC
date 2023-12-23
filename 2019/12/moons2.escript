#!/usr/bin/env escript

-mode(compile).

-define(DEBUG, 0).

main([Filename]) ->
    {ok, Input} = file:read_file(Filename),
    Moons = make_moons(binary:split(Input, <<"\n">>, [global])),
    MoonsList = maps:to_list(Moons),

    XSim = spawn_simulator([ {X, VX, N} || {N, #{ x := X, vx := VX }} <- MoonsList ]),
    YSim = spawn_simulator([ {Y, VY, N} || {N, #{ y := Y, vy := VY }} <- MoonsList ]),
    ZSim = spawn_simulator([ {Z, VZ, N} || {N, #{ z := Z, vz := VZ }} <- MoonsList ]),

    io:format("Simulators X/Y/Z: ~p/~p/~p~n", [XSim, YSim, ZSim]),

    XSteps = collect(XSim),
    YSteps = collect(YSim),
    ZSteps = collect(ZSim),

    io:format("Steps X/Y/Z: ~p / ~p / ~p~n~n", [XSteps, YSteps, ZSteps]),

    LCM = lcm(lcm(XSteps, YSteps), ZSteps),

    io:format("~p~n~n", [LCM]).

%% ---------------------------------
%% Communication with simulators
%% ---------------------------------

spawn_simulator(Moons) ->
    Master = self(),
    spawn_monitor(fun() ->
                          SortedMoons = lists:keysort(1, Moons),
                          Res = simulate(SortedMoons, 1, SortedMoons),
                          Master ! {result, self(), Res}
                  end).

collect({Pid, Ref}) ->
    receive
        {result, Pid, Res} ->
            Res;
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("~p crashed: ~p~n", [Pid, Reason])
    end.

%% ---------------------------------
%% Simulator logic
%% ---------------------------------

simulate(Moons, Step, OrigMoons) ->
    NMoons = step([], Moons),
    SortedNMoons = lists:keysort(1, NMoons),
    case SortedNMoons == OrigMoons of
        true -> Step;
        false -> simulate(SortedNMoons, Step + 1, OrigMoons)
    end.

step(_, []) ->
    [];
step(Before, [{Pos, V, N} = Moon | After]) ->
    NV = V - calc_smaller(Before, Pos) + calc_greater(After, Pos),
    NPos = Pos + NV,
    [ {NPos, NV, N} | step([Moon | Before], After) ].

calc_smaller([], _) ->
    0;
calc_smaller([{Smaller, _, _} | R], Pos) when Smaller < Pos ->
    1 + calc_smaller(R, Pos);
calc_smaller([_ | R], Pos) ->
    calc_smaller(R, Pos).

calc_greater([], _) ->
    0;
calc_greater([{Greater, _, _} | R], Pos) when Greater > Pos ->
    1 + calc_greater(R, Pos);
calc_greater([_ | R], Pos) ->
    calc_greater(R, Pos).

%% ---------------------------------
%% Other logic
%% ---------------------------------

lcm(A, B) ->
    A * B div gcd(A, B).

gcd(A, B) when B > A -> gcd(B, A);
gcd(A, B) when A rem B > 0 -> gcd(B, A rem B);
gcd(A, B) when A rem B =:= 0 -> B.

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

-if(?DEBUG == 1).

dbg(String) -> dbg(String, []).
dbg(Fmt, Vals) -> io:format(Fmt, Vals).

-else.

dbg(_) -> ok.
dbg(_, _) -> ok.

-endif.

