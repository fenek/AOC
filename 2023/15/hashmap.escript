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

solve([Line]) ->
    Steps = parse(Line),
    solve(Steps, init_boxes()).

solve([{Label, Op} | RSteps], Boxes0) ->
    BoxNum = hash(Label),
    Box = maps:get(BoxNum, Boxes0),
    solve(RSteps, Boxes0#{ BoxNum := apply_op(Label, Op, Box) });
solve([], Boxes) ->
    focusing_power(Boxes).

focusing_power(Boxes) ->
    maps:fold(fun(BoxNum, Lenses, Sum) ->
                      Sum + focusing_power(BoxNum, Lenses)
              end, 0, Boxes).

focusing_power(BoxNum, Lenses) ->
    focusing_power(BoxNum, Lenses, 1, 0).
focusing_power(BoxNum, [{_Label, Focal} | RLenses], Slot, Sum) ->
    focusing_power(BoxNum, RLenses, Slot + 1, Sum + (BoxNum + 1) * Slot * Focal);
focusing_power(_, [], _, Sum) ->
    Sum.

apply_op(Label, remove, Box) ->
    lists:keydelete(Label, 1, Box);
apply_op(Label, Focal, Box) ->
    lists:keystore(Label, 1, Box, {Label, Focal}).

init_boxes() ->
    maps:from_list([ {N, []} || N <- lists:seq(0, 255) ]).

hash(Bin) ->
    hash(Bin, 0).
hash(<<Ch:8, RBin/binary>>, Acc) ->
    hash(RBin, ((Acc + Ch) * 17) rem 256);
hash(<<>>, Acc) ->
    Acc.

parse(Line) ->
    parse(Line, []).
parse(<<>>, Sequence) ->
    lists:reverse(Sequence);
parse(Bin, Sequence) ->
    {Step, RBin} = parse_step(Bin),
    parse(RBin, [Step | Sequence]).

parse_step(Bin) ->
    {Label, RBin} = parse_label(Bin),
    {Focal, RBin2} = parse_focal(RBin),
    {{Label, Focal}, RBin2}.

parse_label(Bin) ->
    parse_label(Bin, <<>>).
parse_label(<<Ch:8, Rest/binary>>, Acc) when Ch /= $-, Ch /= $= ->
    parse_label(Rest, <<Acc/binary, Ch>>);
parse_label(Rest, Acc) ->
    {Acc, Rest}.

parse_focal(<<"-,", Rest/binary>>) ->
    {remove, Rest};
parse_focal(<<"-">>) ->
    {remove, <<>>};
parse_focal(<<"=", Rest/binary>>) ->
    parse_focal(Rest, 0).

parse_focal(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_focal(Rest, Acc * 10 + Digit - $0);
parse_focal(<<",", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_focal(<<>>, Acc) ->
    {Acc, <<>>}.

