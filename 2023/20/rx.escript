#!/usr/bin/env escript

-mode(compile).

-export([solve/1]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 100) ],
    io:format("Result: ~w~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve(Lines) ->
    Modules = parse(Lines),
    Ends = find_subgraphs_ends([rx], Modules),
    lcm(lists:map(fun(End) -> find_cycle(End, prev_nodes(End, Modules)) end, Ends)).

% Least Common Multiple
lcm([A]) ->
    A;
lcm([A | R]) ->
    lcm(A, lcm(R)).

lcm(A, B) ->
    A * B div gcd(A, B).

% Greatest Common Denominator
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).


%% =====================================================================
%% Decompose into subgraphs
%% =====================================================================

prev_nodes(End, Modules) ->
    prev_nodes(sets:new([{version, 2}]), [End], Modules).

prev_nodes(Seen, [], Modules) ->
    maps:with(sets:to_list(Seen), Modules);
prev_nodes(Seen, Names, Modules) ->
    NewNames = lists:flatmap(
                 fun(Name) ->
                         {_, Ins, _} = maps:get(Name, Modules, {other, [], []}),
                         lists:filter(
                           fun(In) -> not sets:is_element(In, Seen) end, Ins)
                 end, Names),
    NSeen = lists:foldl(fun sets:add_element/2, Seen, NewNames),
    prev_nodes(NSeen, NewNames, Modules).

find_subgraphs_ends(Names, Modules) ->
    NNames = lists:flatmap(fun(Name) ->
                          {_, Ins, _Outs} = maps:get(Name, Modules),
                          case only_output_of_ins(Ins, Modules) of
                              true -> Ins;
                              false -> [Name]
                          end
                  end, Names),
    case NNames == Names of
        true -> Names;
        false -> find_subgraphs_ends(NNames, Modules)
    end.

only_output_of_ins(Ins, Modules) ->
    lists:all(fun(In) ->
                      {_, _, Outs} = maps:get(In, Modules),
                      length(Outs) == 1
              end, Ins).

%% =====================================================================
%% Cycle detection
%% =====================================================================

find_cycle(Terminating, Modules) ->
    find_cycle(Terminating, 0, Modules, Modules).

find_cycle(Terminating, SeqCount, Modules, OrigModules) ->
    {_TSignals, NModules} = simulate(Terminating, [], Modules, press_button()),
    case NModules == OrigModules of
        true ->
            SeqCount + 1;
        false ->
            find_cycle(Terminating, SeqCount + 1, NModules, OrigModules)
    end.

press_button() -> queue:in({button, low, broadcaster}, queue:new()).

%% =====================================================================
%% Simulation, just like in part 1
%% =====================================================================

simulate(Terminating, TerminatingSignals, Modules, SignalQueue) ->
    case queue:out(SignalQueue) of
        {{value, {_From, Value, Terminating}}, NSignalQueue} ->
            simulate(Terminating, [Value | TerminatingSignals], Modules, NSignalQueue);
        {{value, {From, Value, To}}, NSignalQueue} ->
            case maps:get(To, Modules, undefined) of
                {{Type, State}, Ins, Outs} ->
                    {NState, NSignals} = sim_module(From, Value, To, Type, State, Outs),
                    simulate(Terminating, TerminatingSignals,
                             Modules#{ To => {{Type, NState}, Ins, Outs} },
                             add_signals(NSignals, NSignalQueue));
                undefined ->
                    simulate(Terminating, TerminatingSignals, Modules, NSignalQueue)
            end;
        {empty, _} ->
            {TerminatingSignals, Modules}
    end.

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

%% =====================================================================
%% Parsing
%% =====================================================================

parse(Lines) ->
    Modules = maps:from_list(lists:map(fun parse_line/1, Lines)),
    maps:fold(fun(Name, {_, _, Outs}, NModules) ->
                      add_ins(Name, Outs, NModules)
              end, Modules, Modules).

add_ins(Name, Outs, Modules0) ->
    lists:foldl(fun(Out, ModulesAcc) ->
                        case maps:get(Out, ModulesAcc, undefined) of
                            {{conj, ConjIns}, Ins, ConjOuts} ->
                                ModulesAcc#{ Out => {{conj, [{Name, low} | ConjIns]}, [Name | Ins], ConjOuts} };
                            {Module, Ins, ModuleOuts} ->
                                ModulesAcc#{ Out => {Module, [Name | Ins], ModuleOuts} };
                            undefined ->
                                ModulesAcc#{ Out => {{terminate, nostate}, [Name], []} }
                        end
                end, Modules0, Outs).

parse_line(<<"broadcaster -> ", RBin/binary>>) ->
    Out = parse_outputs(RBin),
    {broadcaster, {{broadcaster, nostate}, [button], Out}};
parse_line(<<TypeCh:8, RBin1/binary>>) ->
    {Name, <<" -> ", RBin2/binary>>} = parse_name(RBin1),
    Out = parse_outputs(RBin2),
    {Name, {type_ch_to_module(TypeCh), [], Out}}.

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

