#!/usr/bin/env escript

-mode(compile).

main([LowerS, UpperS]) ->
    Count = find(list_to_integer(LowerS), list_to_integer(UpperS), 0),

    io:format("~p~n~n", [Count]).

find(Candidate, Upper, Count) when Candidate > Upper ->
    Count;
find(Candidate, Upper, Count) ->
    case validate(Candidate) of
        true ->
            find(Candidate + 1, Upper, Count + 1);
        false ->
            find(Candidate + 1, Upper, Count)
    end.

validate(Candidate) ->
    validate(Candidate, false).

validate(Candidate, HasDouble) when Candidate < 10 ->
    HasDouble;
validate(Candidate, HasDouble) ->
    DigitLast = Candidate rem 10,
    DigitPreLast = (Candidate div 10) rem 10,
    case DigitLast >= DigitPreLast of
        true -> validate(Candidate div 10, HasDouble orelse DigitLast == DigitPreLast);
        false -> false
    end.

