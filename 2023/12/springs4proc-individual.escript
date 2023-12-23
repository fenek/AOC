#!/usr/bin/env escript

-mode(compile).

-export([solve/2, processor/1]).

main([Filename, UnfoldMultiplier]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output
        = [ timer:tc(?MODULE, solve, [Lines, list_to_integer(UnfoldMultiplier)])
            || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve(Lines, UnfoldMultiplier) ->
    Processors = [ spawn_link(?MODULE, processor, [self()]) || _ <- lists:seq(1, 4) ],
    send_lines(Processors, parse(Lines, UnfoldMultiplier)),
    Result = collect_and_sum(length(Lines), 0),
    send_lines(Processors, [fin, fin, fin, fin]),
    Result.

% Not very generic but does the job.
send_lines([P1, P2, P3, P4] = Ps, [L1, L2, L3, L4 | RL]) ->
    P1 ! L1, P2 ! L2, P3 ! L3, P4 ! L4,
    send_lines(Ps, RL);
send_lines(_, []) ->
    ok.

collect_and_sum(0, Sum) ->
    Sum;
collect_and_sum(N, Sum) ->
    receive
        {answer, Answer} ->
            collect_and_sum(N - 1, Sum + Answer)
    after
        5000 ->
            error(timeout)
    end.

processor(Parent) ->
    Cache = ets:new(spring_cache, []),
    processor(Parent, Cache).

processor(Parent, Cache) ->
    receive
        fin ->
            ok;
        Line ->
            Parent ! {answer, solve_line(Line, Cache)},
            processor(Parent, Cache)
    end.

solve_line({Springs, Checksums}, Cache) ->
    find_solutions(Springs, Checksums, Cache).

find_solutions([{ok, _} | RS], Checksums, Cache) ->
    find_solutions(RS, Checksums, Cache);
find_solutions(Springs, [], _Cache) ->
    case lists:all(fun({T, _}) -> T == either orelse T == ok end, Springs) of
        true ->
            1;
        false ->
            0
    end;
find_solutions(Springs, [Checksum | RChecksums], Cache) ->
    Candidates = fit(Springs, Checksum),
    lists:foldl(fun(RSprings, Sum) ->
                        Sum + find_solutions_cached(RSprings, RChecksums, Cache)
                end, 0, Candidates);
find_solutions(_, _, _) ->
    0.

find_solutions_cached(Springs, Checksums, Cache) ->
    case ets:lookup(Cache, {Springs, Checksums}) of
        [{_, Total}] ->
            Total;
        _ ->
            Total = find_solutions(Springs, Checksums, Cache),
            ets:insert(Cache, {{Springs, Checksums}, Total}),
            Total
    end.

fit(Springs0, Count) ->
    Springs = initial_skip(Springs0, Count),
    case exact_fit(Springs, Count) of
        none -> preprocess_and_fit_next(Springs, Count);
        RSprings -> [ RSprings | preprocess_and_fit_next(Springs, Count) ]
    end.

preprocess_and_fit_next([{either, 1} | R], Count) -> fit(R, Count);
preprocess_and_fit_next([{either, N} | R], Count) -> fit([{either, N - 1} | R], Count);
preprocess_and_fit_next([{ok, _} | R], Count) -> fit(R, Count);
preprocess_and_fit_next(_, _Count) -> [].

initial_skip([{either, N}, {ok, _} | R], Count) when N < Count ->
    initial_skip(R, Count);
initial_skip(Springs, _Count) ->
    Springs.

exact_fit([{either, 0} | R], 0) ->
    R;
exact_fit([{either, _} | _] = R, 0) ->
    R;
exact_fit([{ok, _} | R], 0) ->
    R;
exact_fit([], 0) ->
    [];

exact_fit([{bad, Count}, {either, N} | R], Count) ->
    exact_fit([{either, N - 1} | R], 0);
exact_fit([{either, N} | R], Count) when N > Count + 1 ->
    exact_fit([{ok, 1}, {either, N - Count - 1} | R], 0);
exact_fit([{NotOK, N} | R], Count) when N =< Count, NotOK /= ok ->
    exact_fit(R, Count - N);
exact_fit([{either, _} | R], _Count) ->
    exact_fit([{ok, 1} | R], 0);

exact_fit(_, _) ->
    none.

parse(Lines, UnfoldMultiplier) ->
    [ parse_line(Line, UnfoldMultiplier) || Line <- Lines ].

parse_line(Line, UnfoldMultiplier) ->
    [SpringsBin0, ChecksumsBin0] = binary:split(Line, <<" ">>),
    SpringsBin = unfold(SpringsBin0, <<>>, $?, UnfoldMultiplier),
    ChecksumsBin = unfold(ChecksumsBin0, <<>>, $,, UnfoldMultiplier),
    Checksums = parse_ints(ChecksumsBin),
    Springs = parse_springs(SpringsBin),
    {Springs, Checksums}.

unfold(Bin, Acc, _Symbol, 1) ->
    <<Acc/binary, Bin/binary>>;
unfold(Bin, Acc, Symbol, N) ->
    unfold(Bin, <<Acc/binary, Bin/binary, Symbol>>, Symbol, N - 1).

parse_springs(SpringsBin) ->
    parse_springs(SpringsBin, []).
parse_springs(<<>>, Springs) ->
    lists:reverse(Springs);
parse_springs(Bin, Springs) ->
    {Group, RBin} = parse_group(Bin),
    parse_springs(RBin, [Group | Springs]).

parse_group(<<Type:8, RBin/binary>>) ->
    parse_group(RBin, Type, 1).
parse_group(<<Type:8, RBin/binary>>, Type, N) ->
    parse_group(RBin, Type, N + 1);
parse_group(RBin, Type, N) ->
    {{char_to_type(Type), N}, RBin}.

char_to_type($.) -> ok;
char_to_type($#) -> bad;
char_to_type($?) -> either.

parse_ints(Bin) ->
    parse_ints(Bin, []).
parse_ints(<<>>, Acc) ->
    lists:reverse(Acc);
parse_ints(<<",", Bin/binary>>, Acc) ->
    parse_ints(Bin, Acc);
parse_ints(Bin, Acc) ->
    {Int, Rest} = parse_int(Bin),
    parse_ints(Rest, [Int | Acc]).

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

