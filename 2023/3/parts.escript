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
    {_, _, Numbers, Parts} = lists:foldl(fun parse/2, {1, 1, #{}, #{}}, Lines),
    sum_isolated_numbers(Numbers, Parts).

parse(<<>>, {_, Y, Numbers, Parts}) ->
    {1, Y + 1, Numbers, Parts};
parse(<<Digit:8, _/binary>> = Bin, {X, Y, Numbers, Parts}) when Digit >= $0, Digit =< $9 ->
    {Number, Length, Rest} = parse_int(Bin),
    parse(Rest, {X + Length, Y, Numbers#{ {X, Y} => {Number, Length} }, Parts});
parse(<<$., Rest/binary>>, {X, Y, Numbers, Parts}) ->
    parse(Rest, {X + 1, Y, Numbers, Parts});
parse(<<Symbol:8, Rest/binary>>, {X, Y, Numbers, Parts}) ->
    parse(Rest, {X + 1, Y, Numbers, Parts#{ {X, Y} => Symbol }}).

parse_int(Bin) ->
    parse_int(Bin, <<>>).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, <<Acc/binary, Digit>>);
parse_int(Bin, Acc) ->
    {binary_to_integer(Acc), byte_size(Acc), Bin}.

sum_isolated_numbers(Numbers, Parts) ->
    maps:fold(fun(Coords, {Number, Length}, Sum) ->
                      case is_isolated(Coords, Length, Parts) of
                          false -> Sum + Number;
                          true -> Sum
                      end
              end, 0, Numbers).

is_isolated(NumCoords, Length, Parts) ->
    not lists:any(fun(Coords) -> maps:is_key(Coords, Parts) end,
                  generate_adjacent(NumCoords, Length)).

generate_adjacent({NumX, NumY}, Length) ->
    [ {X, Y} || X <- lists:seq(NumX - 1, NumX + Length), Y <- lists:seq(NumY - 1, NumY + 1) ].

