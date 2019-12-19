#!/usr/bin/env escript

-mode(compile).

-export([phase_worker/3, collector/2]).

%% Works only with inputs where message offset is in the second half of the input

main([Filename, Phases]) ->
    {ok, Input0} = file:read_file(Filename),
    Input = string:trim(binary_to_list(Input0), both, "\n"),
    Period = length(Input),

    InList = [ list_to_integer([Digit]) || Digit <- Input ],

    MessageOffset = list_to_integer(lists:sublist(Input, 7)),
    MinPos = MessageOffset + 1,
    MaxPos = Period * 10000,

    io:format("Slice: ~p~n~n", [{MinPos, MaxPos}]),
    
    TS1 = erlang:monotonic_time(millisecond),
    
    Collector = spawn_monitor(?MODULE, collector, [self(), MinPos]),
    
    {FirstPhase, _} =
    lists:foldl(fun(Phase, {PrevPhase, _}) ->
                        spawn_monitor(?MODULE, phase_worker, [PrevPhase, MinPos, Phase])
                end, Collector, lists:seq(list_to_integer(Phases), 1, -1)),

    ReverseInput = lists:reverse(InList),
    stream_data(FirstPhase, MinPos, MaxPos, ReverseInput, ReverseInput),

    OutTable = receive
                   {'ETS-TRANSFER', Tid, _, _} ->
                       Tid
               end,

    TS2 = erlang:monotonic_time(millisecond),
    io:format("Calc time: ~pms~n", [TS2 - TS1]),
    %io:format("~p~n", ets:tab2list(OutTable)),

    Message = [ element(2, hd(ets:lookup(OutTable, N))) || N <- lists:seq(MinPos, MinPos + 7) ],
    io:format("Message: ~p~n", [Message]).

stream_data(Pid, MinPos, CurrPos, [], InList) ->
    stream_data(Pid, MinPos, CurrPos, InList, InList);
stream_data(_Pid, MinPos, CurrPos, _, _) when CurrPos < MinPos ->
    ok;
stream_data(Pid, MinPos, CurrPos, [Digit | RDigits], InList) ->
    io:format("Data left: ~p                    \r", [CurrPos-MinPos]),
    Pid ! {data, CurrPos, Digit},
    stream_data(Pid, MinPos, CurrPos - 1, RDigits, InList).

collector(Master, MinPos) ->
    io:format("Collector started~n", []),
    Table = ets:new(data, [{heir, Master, transfer}]),
    collector(Master, MinPos, Table).

collector(Master, MinPos, Table) ->
    receive
        {data, MinPos, Value} ->
            ets:insert(Table, {MinPos, Value}),
            io:format("Collector stopped~n", []);
        {data, Pos, Value} ->
            ets:insert(Table, {Pos, Value}),
            collector(Master, MinPos, Table)
    end.

phase_worker(OutputPid, MinPos, Phase) ->
    io:format("Phase ~p started~n", [Phase]),
    phase_worker(OutputPid, MinPos, Phase, 0).

phase_worker(OutputPid, MinPos, Phase, Sum) ->
    receive
        {data, MinPos, Value} ->
            NSum = Sum + Value,
            OutputPid ! {data, MinPos, NSum rem 10},
            io:format("Phase ~p stopped~n", [Phase]);
        {data, Pos, Value} ->
            NSum = Value + Sum,
            OutputPid ! {data, Pos, NSum rem 10},
            phase_worker(OutputPid, MinPos, Phase, NSum)
    end.

