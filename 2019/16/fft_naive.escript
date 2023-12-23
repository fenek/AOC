#!/usr/bin/env escript

-mode(compile).

main([Filename, Phases]) ->
    {ok, Input} = file:read_file(Filename),
    InList = [ list_to_integer([Digit]) || Digit <- string:trim(binary_to_list(Input), both, "\n") ],

    Workers = [ N || N <- lists:seq(1, length(InList)) ],

    FFT = lists:foldl(fun(Phase, InList0) ->
                              io:format("Phase ~p...~n", [Phase]),
                              fft_phase(InList0)
                      end, InList, lists:seq(1, list_to_integer(Phases))),

    io:format("~p~nfirst eight: ~p~n", [FFT, lists:sublist(FFT, 8)]).

fft_phase(InList) ->
    fft_phase(InList, 1).

fft_phase(InList, Pos) when Pos > length(InList) ->
    [];
fft_phase(InList, Pos) ->
    [_ | Initial] = Pattern = pattern(Pos),
    [ abs(calc_fft(InList, Initial, Pattern) rem 10) | fft_phase(InList, Pos + 1) ].

calc_fft([], _, _) ->
    0;
calc_fft(Digits, [], OrigPattern) ->
    calc_fft(Digits, OrigPattern, OrigPattern);
calc_fft([Digit | RDigits], [F | RPattern], OrigPattern) ->
    Digit * F + calc_fft(RDigits, RPattern, OrigPattern).

pattern(I) ->
    case get(I) of
        undefined ->
            Pattern = lists:flatmap(fun(F) -> lists:duplicate(I, F) end, [0, 1, 0, -1]),
            put(I, Pattern),
            Pattern;
        Cached ->
            Cached
    end.
