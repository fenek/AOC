#!/usr/bin/env escript

% Alternative 3: Use ETS for numbers index and list for gears, instead of maps for both

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
    Numbers0 = ets:new(numbers, []),
    {_, _, Numbers, MaybeGears} = lists:foldl(fun parse/2, {1, 1, Numbers0, []}, Lines),
    sum_gear_ratios(Numbers, MaybeGears).

parse(<<>>, {_, Y, Numbers, Parts}) ->
    {1, Y + 1, Numbers, Parts};
parse(<<Digit:8, _/binary>> = Bin, {X, Y, Numbers, Parts}) when Digit >= $0, Digit =< $9 ->
    {Number, Length, Rest} = parse_int(Bin),
    ets:insert(Numbers, [ { {IX, Y}, Number } || IX <- lists:seq(X, X + Length - 1) ]),
    parse(Rest, {X + Length, Y, Numbers, Parts});
parse(<<$*, Rest/binary>>, {X, Y, Numbers, Parts}) ->
    parse(Rest, {X + 1, Y, Numbers, [{X, Y} | Parts]});
parse(<<_:8, Rest/binary>>, {X, Y, Numbers, Parts}) ->
    parse(Rest, {X + 1, Y, Numbers, Parts}).

parse_int(Bin) ->
    parse_int(Bin, <<>>).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, <<Acc/binary, Digit>>);
parse_int(Bin, Acc) ->
    {binary_to_integer(Acc), byte_size(Acc), Bin}.

sum_gear_ratios(Numbers, MaybeGears0) ->
    lists:foldl(
      fun({X, Y}, Sum) ->
             case find_numbers(X, Y, Numbers) of
                 [N1, N2] -> Sum + N1 * N2;
                 _ -> Sum
             end
      end, 0, MaybeGears0).

find_numbers(X, Y, Numbers) ->
    maybe_number(X - 1, Y, Numbers)
    ++ maybe_number(X + 1, Y, Numbers)
    ++ maybe_numbers(X, Y - 1, Numbers)
    ++ maybe_numbers(X, Y + 1, Numbers).

maybe_number(X, Y, Numbers) ->
    case ets:lookup(Numbers, {X, Y}) of
        [] -> [];
        [{_, Number}] -> [Number]
    end.

maybe_numbers(X, Y, Numbers) ->
    case maybe_number(X, Y, Numbers) of
        [] ->
            maybe_number(X - 1, Y, Numbers) ++ maybe_number(X + 1, Y, Numbers);
        Number ->
            Number
    end.

