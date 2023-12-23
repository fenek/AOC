#!/usr/bin/env escript

-mode(compile).

-export([solve/2]).

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
    lists:foldl(fun(Line, Sum) ->
                        Sum + solve_line(Line)
                end, 0, parse(Lines, UnfoldMultiplier)).

solve_line({Springs, Checksums}) ->
    find_solutions(Springs, Checksums).

find_solutions([{ok, _} | RS], Checksums) ->
    find_solutions(RS, Checksums);
find_solutions(Springs, []) ->
    case lists:all(fun({T, _}) -> T == either orelse T == ok end, Springs) of
        true ->
            1;
        false ->
            0
    end;
find_solutions(Springs, [Checksum | RChecksums]) ->
    Candidates = fit(Springs, Checksum),
    lists:foldl(fun(RSprings, Sum) ->
                        Sum + find_solutions(RSprings, RChecksums)
                end, 0, Candidates);
find_solutions(_, _) ->
    0.

fit(Springs0, Count) ->
    Springs = initial_skip(Springs0, Count),
    case exact_fit(Springs, Count) of
        none ->
            case Springs of
                [{either, 1} | R] ->
                    fit(R, Count);
                [{either, N} | R] ->
                    fit([{either, N - 1} | R], Count);
                [{ok, _} | R] ->
                    fit(R, Count);
                _ ->
                    []
            end;
        RSprings ->
            case Springs of
                [{either, 1} | R] ->
                    [ RSprings | fit(R, Count) ];
                [{either, N} | R] ->
                    [ RSprings | fit([{either, N - 1} | R], Count) ];
                [{ok, _} | R] ->
                    [ RSprings | fit(R, Count) ];
                _ ->
                    [ RSprings ]
            end
    end.

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

