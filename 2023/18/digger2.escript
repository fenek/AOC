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
    N = length(Lines),
    DigPlan = parse(Lines),
    DigSite = dig(DigPlan),
    Area = shoelace(DigSite, N),
    Brim = lists:sum([element(2,  T) || T <- DigPlan]),
    Area + Brim div 2 + 1.

shoelace(Vertices, N) ->
    sum(Vertices, 0, N, 0) div 2.

sum(_Vertices, N, N, Sum) ->
    Sum;
sum(Vertices, I, N, Sum) ->
    {X1, Y1} = maps:get(I, Vertices),
    {X2, Y2} = maps:get((I + 1) rem N, Vertices),
    sum(Vertices, I + 1, N, Sum + (Y1 + Y2)*(X1 - X2)).

dig(Plan) ->
    {DigSite, _ ,_, _} = lists:foldl(fun dig/2, {#{ 0 => {1, 1} }, 0, 1, 1}, Plan),
    DigSite.

dig({{DX, DY}, Steps}, {DigSite, I, X, Y}) ->
    NX = X + Steps * DX,
    NY = Y + Steps * DY,
    NI = I + 1,
    NDigSite = DigSite#{ NI => {NX, NY} },
    {NDigSite, NI, NX, NY}.

parse(Lines) ->
    lists:map(fun parse_line/1, Lines).

parse_line(<<_:8, " ", RBin/binary>>) ->
    {_, <<" (#", StepsBin:5/binary, DirCh:8, _/binary>>} = parse_int(RBin),
    {ch_to_dir(DirCh), binary_to_integer(StepsBin, 16)}.

ch_to_dir($3) -> {0, -1};
ch_to_dir($1) -> {0, 1};
ch_to_dir($2) -> {-1, 0};
ch_to_dir($0) -> {1, 0}.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

