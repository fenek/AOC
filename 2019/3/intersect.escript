#!/usr/bin/env escript

-mode(compile).

main([InputName, DistanceType]) ->
    TS1 = erlang:monotonic_time(millisecond),
    {ok, Input} = file:read_file(InputName),
    [WireInputRaw1, WireInputRaw2 | _] = binary:split(Input, <<"\n">>, [global]),

    ets:new(wire1, [public, named_table]),
    ets:new(wire2, [public, named_table]),

    TS2 = erlang:monotonic_time(millisecond),
    Ref1 = trace_wire(WireInputRaw1, wire1),
    Ref2 = trace_wire(WireInputRaw2, wire2),

    wait_for(Ref1),
    wait_for(Ref2),

    TS3 = erlang:monotonic_time(millisecond),
    Distance = find_closest_intersection(mode_to_atom(DistanceType)),

    TS4 = erlang:monotonic_time(millisecond),

%    io:format("~p~n~n~p~n~n", [ets:tab2list(wire1), ets:tab2list(wire2)]),
    io:format("read / trace / find: ~pms / ~pms / ~pms~n~n", [TS2-TS1, TS3-TS2, TS4-TS3]),
    io:format("~p~n~n", [Distance]).

trace_wire(WireInputRaw, WireTab) ->
    WireInput = binary:split(WireInputRaw, <<",">>, [global]),
    {_Pid, Ref} = spawn_monitor(fun() -> trace_wire(undefined, 0, 0, 0, WireInput, WireTab, 0) end),
    Ref.

trace_wire(_Dir, 0, _X, _Y, [], _WireTab, _WireLen) ->
    ok;
trace_wire(_, 0, X, Y, [Next | RWireInput], WireTab, WireLen) ->
    {Dir, Steps} = parse_wire_vector(Next),
    trace_wire(Dir, Steps, X, Y, RWireInput, WireTab, WireLen);
trace_wire(Dir, Steps, X, Y, WireInput, WireTab, WireLen) ->
    NextX = X + delta_x(Dir),
    NextY = Y + delta_y(Dir),
    NewWireLen = WireLen + 1,
    ets:insert_new(WireTab, {{NextX, NextY}, NewWireLen}),
    trace_wire(Dir, Steps - 1, NextX, NextY, WireInput, WireTab, NewWireLen).

parse_wire_vector(<<Direction:8, Steps/binary>>) ->
    {parse_direction(Direction), binary_to_integer(Steps)}.

parse_direction($U) -> up;
parse_direction($D) -> down;
parse_direction($L) -> left;
parse_direction($R) -> right.

delta_x(left) -> -1;
delta_x(right) -> 1;
delta_x(_) -> 0.

delta_y(up) -> 1;
delta_y(down) -> -1;
delta_y(_) -> 0.

wait_for(Ref) ->
    receive
        {'DOWN', Ref, process, Object, Info} ->
            io:format("Worker ~p finished with ~p~n", [Object, Info])
    after
        30000 ->
            throw(ets:tab2list(board))
    end.

find_closest_intersection(Mode) ->
    ets:foldl(fun({{X, Y} = Coords, Len1}, Best) ->
                      case ets:lookup(wire2, Coords) of
                          [{_, Len2}] ->
                              case distance(Mode, X, Y, Len1, Len2) of
                                  Dist when Dist < Best -> Dist;
                                  _ -> Best
                              end;
                          _ ->
                              Best
                      end
              end, infinity, wire1).

mode_to_atom("manhattan") -> manhattan;
mode_to_atom("length") -> wire_length.

distance(manhattan, X, Y, _, _) ->
    abs(X) + abs(Y);
distance(wire_length, _, _, Len1, Len2) ->
    Len1 + Len2.

