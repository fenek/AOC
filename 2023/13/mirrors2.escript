#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve(Lines) ->
    {RowSum, ColSum} = lists:foldl(fun(Puzzle, {RowSum0, ColSum0}) ->
                                           {RowRef, ColRef} = find_reflections_in_puzzle(Puzzle),
                                           {RowSum0 + RowRef, ColSum0 + ColRef}
                                   end, {0, 0}, parse(Lines)),
    ColSum + 100 * RowSum.

find_reflections_in_puzzle({Rows, Cols}) ->
    case find_reflection(Rows) of
        undefined ->
            {0, find_reflection(Cols)};
        Value ->
            {Value, 0}
    end.

find_reflection(#{ max := Max } = Sequence) ->
    find_reflection(1, Max, Sequence).

find_reflection(Max, Max, _Sequence) ->
    undefined;
find_reflection(N, Max, Sequence) ->
    case is_reflection(N, N + 1, Max, Sequence) of
        false ->
            find_reflection(N + 1, Max, Sequence);
        true ->
            N
    end.

is_reflection(N1, N2, Max, Sequence) ->
    is_reflection(N1, N2, Max, Sequence, false).
is_reflection(N1, N2, Max, _Sequence, SmudgeRemoved) when N1 < 1; N2 > Max ->
    % if it's a reflection that didn't require removing a smudge,
    % this is the original one and not what we're looking for
    SmudgeRemoved;
is_reflection(N1, N2, Max, Sequence, SmudgeRemoved) ->
    #{ N1 := Val1, N2 := Val2 } = Sequence,
    case Val1 == Val2 of
        true ->
            is_reflection(N1 - 1, N2 + 1, Max, Sequence, SmudgeRemoved);
        false when SmudgeRemoved ->
            false;
        false ->
            case one_away(Val1, Val2) of
                true ->
                    is_reflection(N1 - 1, N2 + 1, Max, Sequence, true);
                false ->
                    false
            end
    end.

one_away(V1, V2) -> one_set_bit(V1 bxor V2, false).

one_set_bit(0, true) -> true;
one_set_bit(V, true) when V band 1 == 1 -> false;
one_set_bit(V, _) when V band 1 == 1 -> one_set_bit(V bsr 1, true);
one_set_bit(V, OneFound) -> one_set_bit(V bsr 1, OneFound).

parse(Lines) ->
    parse(Lines, []).
parse([], Acc) ->
    Acc;
parse([<<>> | RLines], Acc) ->
    parse(RLines, Acc);
parse(Lines, Acc) ->
    {Pattern, RLines} = parse_pattern(Lines),
    parse(RLines, [Pattern | Acc]).

parse_pattern([ Line | _ ] = Lines) ->
    parse_pattern(Lines, 1, #{}, #{ max => byte_size(Line) }).

parse_pattern(Lines, RowNum, Rows, Cols) when hd(Lines) == <<>>; Lines == [] ->
    {{Rows#{ max => RowNum - 1 }, Cols}, Lines};
parse_pattern([Line | RLines], RowNum, Rows0, Cols0) ->
    {RowValue, Cols} = parse_line(Line, Cols0),
    parse_pattern(RLines, RowNum + 1, Rows0#{ RowNum => RowValue }, Cols).

parse_line(Line, Cols) ->
    parse_line(Line, 1, 0, Cols).

parse_line(<<Object:8, RLine/binary>>, ColNum, RowValue0, Cols) ->
    ObjValue = object_to_value(Object),
    RowValue = RowValue0 bsl 1 + ObjValue,
    ColValue0 = maps:get(ColNum, Cols, 0),
    ColValue = ColValue0 bsl 1 + ObjValue,
    parse_line(RLine, ColNum + 1, RowValue, Cols#{ ColNum => ColValue });
parse_line(<<>>, _, Value, Cols) ->
    {Value, Cols}.

object_to_value($#) -> 1;
object_to_value($.) -> 0.

