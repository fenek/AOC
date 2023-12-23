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
    validate(Candidate, infinity, 0, false).

validate(Candidate, _DigitInSeries, SeriesLength, HasDouble) when Candidate == 0 ->
    HasDouble orelse SeriesLength == 2;
validate(Candidate, DigitInSeries, _SeriesLength, _HasDouble)
  when Candidate rem 10 > DigitInSeries ->
    false;
validate(Candidate, DigitInSeries, SeriesLength, HasDouble)
  when Candidate rem 10 == DigitInSeries ->
    validate(Candidate div 10, DigitInSeries, SeriesLength + 1, HasDouble);
validate(Candidate, _DigitInSeries, SeriesLength, HasDouble) ->
    validate(Candidate div 10, Candidate rem 10, 1, HasDouble orelse SeriesLength == 2).

