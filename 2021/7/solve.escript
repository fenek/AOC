#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    PositionsBin = binary:split(string:trim(Data), <<",">>, [global]),
    Positions = [ binary_to_integer(B) || B <- PositionsBin, B /= <<>> ],
    SortedPositions = lists:sort(Positions),
    {OnlyZeros, WithoutZeros} = lists:splitwith(fun(E) -> E == 0 end, SortedPositions),
    WithoutZerosLen = length(WithoutZeros),
    OnlyZerosLen = length(OnlyZeros),
    Costs = compute_costs(WithoutZeros, 0, OnlyZerosLen, 0, 0, WithoutZerosLen,
                          lists:sum(WithoutZeros), []),
    io:format("~p~n~p~n~p~n", [SortedPositions, Costs, lists:min(Costs)]).

compute_costs([], _, _, _, _, _, _, Costs) ->
    Costs;
compute_costs(Positions, CurrPos, AtCount, BehindCount, BehindSum, AheadCount, AheadSum, Costs) ->
%    io:format("~p: ~p~n", [CurrPos, Positions]),
    NCosts = [BehindSum + AheadSum | Costs],
    NCurrPos = CurrPos + 1,
    {NPositions, NAtCount} = take(Positions, NCurrPos),
    NBehindCount = BehindCount + AtCount,
    NBehindSum = BehindSum + NBehindCount,
    NAheadCount = AheadCount - NAtCount,
    NAheadSum = AheadSum - AheadCount,
    compute_costs(NPositions, NCurrPos, NAtCount, NBehindCount, NBehindSum, NAheadCount, NAheadSum, NCosts).

take(Positions, Pos) ->
    {Taken, NPositions} = lists:splitwith(fun(E) -> E == Pos end, Positions),
    {NPositions, length(Taken)}.

