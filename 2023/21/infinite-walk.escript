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
    #{ start := {SX, SY} = Start, max_x := MaxX, max_y := MaxY } = Garden = parse(Lines),
    Patterns = find_patterns([Start], Garden, #{}),
    #{ Start := {CenterP, _},
       {1, 1} := {Diag1P, _}, {1, MaxY} := {Diag2P, _},
       {MaxX, 1} := {Diag3P, _}, {MaxX, MaxY} := {Diag4P, _},
       {SX, 1} := {UpP, _}, {SX, MaxY} := {DownP, _},
       {1, SY} := {LeftP, _}, {MaxX, SY} := {RightP, _} } = Patterns,
    Steps = 26501365,
    Steps2 = Steps - 66,
    Steps3 = Steps - 132,

    Center = sum_pattern(CenterP, Steps),

    Up = sum_straight(UpP, Steps2),
    Down = sum_straight(DownP, Steps2),
    Left = sum_straight(LeftP, Steps2),
    Right = sum_straight(RightP, Steps2),

    Diag1 = sum_diagonal(Diag1P, Steps3),
    Diag2 = sum_diagonal(Diag2P, Steps3),
    Diag3 = sum_diagonal(Diag3P, Steps3),
    Diag4 = sum_diagonal(Diag4P, Steps3),

    Center + Diag1 + Diag2 + Diag3 + Diag4 + Up + Down + Left + Right.

sum_straight(Pattern, Steps) ->
    lists:sum(Pattern) * (Steps div 262)
    + sum_pattern(Pattern, Steps rem 262)
    + sum_pattern(Pattern, (Steps rem 262) - 131).

sum_diagonal(Pattern, Steps) ->
    CompleteRowsPairs = Steps div 262,
    PartialBigger = CompleteRowsPairs * 2 + 1,
    PartialSmaller = PartialBigger + 1,
    FormerCount = lists:sum(lists:seq(1, 2 * CompleteRowsPairs - 1, 2)),
    LatterCount = lists:sum(lists:seq(2, 2 * CompleteRowsPairs, 2)),
    CompleteSectors = FormerCount * sum_pattern(Pattern, Steps)
                    + LatterCount * sum_pattern(Pattern, Steps - 131),
    CompleteSectors
    + PartialBigger * sum_pattern(Pattern, Steps rem 262)
    + PartialSmaller * sum_pattern(Pattern, (Steps rem 262) - 131).

sum_pattern(Pattern, Steps) ->
    sum_pattern(Pattern, Steps, 0, 0).
sum_pattern(_, -2, SumEven, _) ->
    SumEven;
sum_pattern(_, -1, _, SumOdd) ->
    SumOdd;
sum_pattern([], StepsLeft, SumEven, _) when StepsLeft rem 2 == 0 ->
    SumEven;
sum_pattern([], _, _, SumOdd) ->
    SumOdd;
sum_pattern([E], StepsLeft, SumEven, SumOdd) ->
    sum_pattern([], StepsLeft - 2, SumEven + E, SumOdd);
sum_pattern([E, O | R], StepsLeft, SumEven, SumOdd) ->
    sum_pattern(R, StepsLeft - 2, SumEven + E, SumOdd + O).

find_patterns([Start | RStarts], Garden, Patterns) ->
    case maps:is_key(Start, Patterns) of
        false ->
            {Pattern, Exits0} = walk(sets:from_list([Start], [{version, 2}]),
                                    sets:new([{version, 2}]), [], Garden, 0, #{}),
            Exits = maps:filter(fun(_K, {_, Steps}) -> Steps /= 0 end, Exits0),
            NStarts = exits_to_starts(Exits, Garden),
            find_patterns(RStarts ++ NStarts, Garden, Patterns#{ Start => {lists:reverse(Pattern), Exits} });
        true ->
            find_patterns(RStarts, Garden, Patterns)
    end;
find_patterns([], _Garden, Patterns) ->
    Patterns.

exits_to_starts(Exits, #{ max_x := MaxX, max_y := MaxY }) ->
    case maps:get({x, 1}, Exits, undefined) of
        {Y0, _} -> [{MaxX, Y0}];
        undefined -> []
    end
    ++
    case maps:get({y, 1}, Exits, undefined) of
        {X0, _} -> [{X0, MaxY}];
        undefined -> []
    end
    ++
    case maps:get({x, MaxX}, Exits, undefined) of
        {YM, _} -> [{1, YM}];
        undefined -> []
    end
    ++
    case maps:get({y, MaxY}, Exits, undefined) of
        {XM, _} -> [{XM, 1}];
        undefined -> []
    end.

walk(CurrentTiles, PrevTiles, Pattern, #{ max_x := MaxX, max_y := MaxY } = Garden, Step, Exits0) ->
    case sets:size(CurrentTiles) of
        0 ->
            {Pattern, Exits0};
        CurrSize ->
            Exits = update_exits(CurrentTiles, MaxX, MaxY, Step, Exits0),
            NPattern = [CurrSize | Pattern],
            NextTiles = sets:subtract(next_tiles(CurrentTiles, Garden), PrevTiles),
            walk(NextTiles, CurrentTiles, NPattern, Garden, Step + 1, Exits)
    end.

update_exits(Tiles, MaxX, MaxY, StepsLeft, Exits0) ->
    sets:fold(fun({X, Y}, Exits) ->
                      Exits1 = case X == 1 orelse X == MaxX of
                                   true ->
                                       case maps:get({x, X}, Exits, undefined) of
                                           undefined -> Exits#{ {x, X} => {Y, StepsLeft} };
                                           _ -> Exits
                                       end;
                                   false ->
                                       Exits
                               end,
                      case Y == 1 orelse Y == MaxY of
                          true ->
                              case maps:get({y, Y}, Exits1, undefined) of
                                  undefined -> Exits1#{ {y, Y} => {X, StepsLeft} };
                                  _ -> Exits1
                              end;
                          false ->
                              Exits1
                      end
              end, Exits0, Tiles).

next_tiles(CurrentTiles, Garden) ->
    sets:fold(fun(CurrTile, NextTiles0) ->
                      lists:foldl(fun(NextTile, NextTiles1) ->
                                          case maps:is_key(NextTile, Garden) of
                                              true -> sets:add_element(NextTile, NextTiles1);
                                              false -> NextTiles1
                                          end
                                  end, NextTiles0, neighbours(CurrTile))
              end, sets:new([{version, 2}]), CurrentTiles).

neighbours({X, Y}) -> [{X + 1, Y},  {X - 1, Y}, {X, Y + 1}, {X, Y - 1}].

parse([L | _] = Lines) ->
    lists:foldl(fun({Y, Line}, Garden) ->
                        parse_line(Line, Y, Garden)
                end, #{ max_x => byte_size(L), max_y => length(Lines) },
                lists:zip(lists:seq(1, length(Lines)), Lines)).

parse_line(Line, Y, Garden) ->
    parse_line(Line, 1, Y, Garden).

parse_line(<<$., RBin/binary>>, X, Y, Garden) ->
    parse_line(RBin, X + 1, Y, Garden#{ {X, Y} => walkable });
parse_line(<<$#, RBin/binary>>, X, Y, Garden) ->
    parse_line(RBin, X + 1, Y, Garden);
parse_line(<<$S, RBin/binary>>, X, Y, Garden) ->
    parse_line(RBin, X + 1, Y, Garden#{ {X, Y} => walkable, start => {X, Y} });
parse_line(<<>>, _X, _Y, Garden) ->
    Garden.

