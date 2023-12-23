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
    lists:foldl(fun(Line, Sum) ->
                        Sum + solve_line(Line)
                end, 0, parse(Lines)).

solve_line({Springs, Checksums}) ->
    check_springs(Springs, [], lists:reverse(Checksums)).

check_springs([either | R], Path, Checksums) ->
    check_springs(R, [ok | Path], Checksums) + check_springs(R, [bad | Path], Checksums);
check_springs([OkOrBad | R], Path, Checksums) ->
    check_springs(R, [OkOrBad | Path], Checksums);
check_springs([], Path, Checksums) ->
    case get_checksum(Path) of
        Checksums -> 1;
        _ -> 0
    end.

get_checksum(Path) ->
    get_checksum(Path, 0, []).
get_checksum([bad | R], C, Acc) ->
    get_checksum(R, C + 1, Acc);
get_checksum([ok | R], 0, Acc) ->
    get_checksum(R, 0, Acc);
get_checksum([ok | R], C, Acc) ->
    get_checksum(R, 0, [C | Acc]);
get_checksum(_, 0, Acc) ->
    lists:reverse(Acc);
get_checksum([], C, Acc) ->
    get_checksum([], 0, [C | Acc]).

parse(Lines) ->
    lists:map(fun parse_line/1, Lines).

parse_line(Line) ->
    parse_line(Line, []).
parse_line(<<" ", ChecksumsBin/binary>>, Springs) ->
    Checksums = parse_ints(ChecksumsBin),
    {lists:reverse(Springs), Checksums};
parse_line(<<$., RBin/binary>>, Springs) ->
    parse_line(RBin, [ok | Springs]);
parse_line(<<$#, RBin/binary>>, Springs) ->
    parse_line(RBin, [bad | Springs]);
parse_line(<<$?, RBin/binary>>, Springs) ->
    parse_line(RBin, [either | Springs]).

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

