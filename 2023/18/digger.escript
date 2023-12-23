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
    DigPlan = parse(Lines),
    DigSite = dig(DigPlan),
    fill(DigSite).

fill(DigSite) ->
    fill(lists:sort(maps:keys(DigSite)), DigSite, 0).

fill([], _DS, Sum) ->
    Sum;
fill(T0, DigSite, Sum) ->
    case consume_continous(T0, DigSite) of
        {ok, Length, T1} ->
            fill(T1, DigSite, Sum + Length);
        none ->
            {Length, T1} = consume_inner(T0, DigSite),
            fill(T1, DigSite, Sum + Length)
    end.

consume_continous([{X, Y1} = C1, {X, Y2} = C2 | RT], DigSite) when Y2 == Y1 + 1 ->
    consume_continous(C1, C2, RT, DigSite);
consume_continous(_, _) ->
    none.

consume_continous(First, {X, LY}, [{X, NY} = Next | RT], DigSite) when NY == LY + 1 ->
    consume_continous(First, Next, RT, DigSite);
consume_continous({X, FY}, {_, LY} = Last, T, DigSite) ->
    case maps:is_key({X - 1, FY}, DigSite) == maps:is_key({X - 1, LY}, DigSite) of
        true ->
            {ok, LY - FY + 1, T};
        false ->
            {ok, LY - FY, [Last | T]}
    end.

consume_inner([First | RT], DigSite) ->
    consume_inner(First, RT, DigSite).

consume_inner({X, FY} = First, [{X, LY} | RT] = T0, DigSite) ->
    case consume_continous(T0, DigSite) of
        {ok, _, T1} ->
            consume_inner(First, T1, DigSite);
        none ->
            {LY - FY + 1, RT}
    end.

dig(Plan) ->
    {DigSite, _ ,_} = lists:foldl(fun dig/2, {#{}, 1, 1}, Plan),
    DigSite.

dig({{DX, DY}, Steps, Color}, {DigSite, X, Y}) ->
    dig_and_paint(X, Y, DX, DY, Steps, Color, DigSite).

dig_and_paint(X, Y, _DX, _DY, 0, _Color, DigSite) ->
    {DigSite, X, Y};
dig_and_paint(X, Y, DX, DY, Steps, Color, DigSite) ->
    NX = X + DX,
    NY = Y + DY,
    NDigSite = DigSite#{ {NX, NY} => Color },
    dig_and_paint(NX, NY, DX, DY, Steps - 1, Color, NDigSite).

parse(Lines) ->
    lists:map(fun parse_line/1, Lines).

parse_line(<<DirCh:8, " ", RBin/binary>>) ->
    {Steps, <<" ", RBin2/binary>>} = parse_int(RBin),
    {ch_to_dir(DirCh), Steps, parse_color(RBin2)}.

parse_color(<<"(#", ColorStr:6/binary, _/binary>>) ->
    binary_to_integer(ColorStr, 16).

ch_to_dir($U) -> {0, -1};
ch_to_dir($D) -> {0, 1};
ch_to_dir($L) -> {-1, 0};
ch_to_dir($R) -> {1, 0}.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

