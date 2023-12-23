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
    {Moves, Network} = parse(Lines),
    StartingNodes = get_ending_with_a(Network),
    lcm([solve(Moves, Node, 0, Moves, Network) || Node <- StartingNodes]).

% Least Common Multiple
lcm([A]) ->
    A;
lcm([A | R]) ->
    lcm(A, lcm(R)).

lcm(A, B) ->
    A * B div gcd(A, B).

% Greatest Common Denominator
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

get_ending_with_a(Network) ->
    lists:filtermap(fun({<<_, _, "A">> = Node, l}) -> {true, Node};
                       (_) -> false end, maps:keys(Network)).

solve(_, <<_, _, "Z">>, Steps, _, _) ->
    Steps;
solve([], Current, Steps, Moves, Network) ->
    solve(Moves, Current, Steps, Moves, Network);
solve([Move | RMoves], Current, Steps, Moves, Network) ->
    solve(RMoves, maps:get({Current, Move}, Network), Steps + 1, Moves, Network).

parse([MovementPattern | NetworkInput]) ->
    Moves = parse_pattern(MovementPattern),
    Network = parse_network(NetworkInput),
    {Moves, Network}.

parse_pattern(<<>>) -> [];
parse_pattern(<<$L, R/binary>>) -> [ l | parse_pattern(R) ];
parse_pattern(<<$R, R/binary>>) -> [ r | parse_pattern(R) ].

parse_network(NetworkIn) ->
    lists:foldl(fun(<<From:3/binary, " = (", Left:3/binary, ", ", Right:3/binary, ")">>, Acc) ->
                        Acc#{ {From, l} => Left, {From, r} => Right }
                end, #{}, NetworkIn).

