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
    Modules = parse(Lines),
    {_, LCS, HCS} =
    lists:foldl(fun(_, {ModulesState, LCSum, HCSum}) ->
                        {NModulesState, LC, HC} = press_button(ModulesState),
                        {NModulesState, LCSum + LC, HCSum + HC}
                end, {Modules, 0, 0}, lists:seq(1, 1000)),
    LCS * HCS.

press_button(Modules) ->
    simulate(Modules, queue:in({button, low, broadcaster}, queue:new()), 0, 0).

simulate(Modules, SignalQueue, LowCount0, HighCount0) ->
    case queue:out(SignalQueue) of
        {{value, {From, Value, To}}, NSignalQueue} ->
            {LowCount, HighCount} = bump_count(Value, LowCount0, HighCount0),
            %io:format("~w -~w-> ~w~n", [From, Value, To]),
            case maps:get(To, Modules, undefined) of
                {{Type, State}, Outs} ->
                    {NState, NSignals} = sim_module(From, Value, To, Type, State, Outs),
                    simulate(Modules#{ To => {{Type, NState}, Outs} },
                             add_signals(NSignals, NSignalQueue), LowCount, HighCount);
                undefined ->
                    simulate(Modules, NSignalQueue, LowCount, HighCount)
            end;
        {empty, _} ->
            {Modules, LowCount0, HighCount0}
    end.

bump_count(low, LC, HC) -> {LC + 1, HC};
bump_count(high, LC, HC) -> {LC, HC + 1}.

sim_module(_From, high, _Name, flipflop, State, _Outs) ->
    {State, []};
sim_module(_From, low, Name, flipflop, off, Outs) ->
    {on, bcast(Name, high, Outs)};
sim_module(_From, low, Name, flipflop, on, Outs) ->
    {off, bcast(Name, low, Outs)};
sim_module(From, Value, Name, conj, InRecent0, Outs) ->
    InRecent = lists:keyreplace(From, 1, InRecent0, {From, Value}),
    OutVal = case lists:all(fun({_, InVal}) -> InVal == high end, InRecent) of
                 true -> low;
                 false -> high
             end,
    {InRecent, bcast(Name, OutVal, Outs)};
sim_module(button, low, broadcaster, broadcaster, nostate, Outs) ->
    {nostate, bcast(broadcaster, low, Outs)}.

bcast(From, Value, Outs) ->
    [ {From, Value, Out} || Out <- Outs ].

add_signals(Signals, Queue) ->
    lists:foldl(fun queue:in/2, Queue, Signals).

parse(Lines) ->
    Modules = maps:from_list(lists:map(fun parse_line/1, Lines)),
    maps:fold(fun(Name, {_, Outs}, NModules) ->
                      add_conj_ins(Name, Outs, NModules)
              end, Modules, Modules).

add_conj_ins(Name, Outs, Modules0) ->
    lists:foldl(fun(Out, ModulesAcc) ->
                        case maps:get(Out, ModulesAcc, undefined) of
                            {{conj, Ins}, ConjOuts} ->
                                ModulesAcc#{ Out => {{conj, [{Name, low} | Ins]}, ConjOuts} };
                            _ ->
                                ModulesAcc
                        end
                end, Modules0, Outs).

parse_line(<<"broadcaster -> ", RBin/binary>>) ->
    Out = parse_outputs(RBin),
    {broadcaster, {{broadcaster, nostate}, Out}};
parse_line(<<TypeCh:8, RBin1/binary>>) ->
    {Name, <<" -> ", RBin2/binary>>} = parse_name(RBin1),
    Out = parse_outputs(RBin2),
    {Name, {type_ch_to_module(TypeCh), Out}}.

type_ch_to_module($%) -> {flipflop, off};
type_ch_to_module($&) -> {conj, []}.

parse_outputs(Bin) ->
    parse_outputs(Bin, []).
parse_outputs(Bin, Acc) ->
    case parse_name(Bin) of
        {Name, <<", ", RBin/binary>>} -> parse_outputs(RBin, [Name | Acc]);
        {Name, <<>>} -> lists:reverse([Name | Acc])
    end.

parse_name(Bin) ->
    parse_name(Bin, <<>>).
parse_name(<<Letter:8, RBin/binary>>, Acc) when Letter >= $a, Letter =< $z ->
    parse_name(RBin, <<Acc/binary, Letter>>);
parse_name(RBin, Acc) ->
    {binary_to_atom(Acc), RBin}.

