#!/usr/bin/env escript

-mode(compile).

-export([solve/1, solve_single/2]).
-export([forwarder/2, workflow/3]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    [ {_, Result} | _ ] = Output = [ timer:tc(?MODULE, solve, [Lines]) || _ <- lists:seq(1, 100) ],
    io:format("Result: ~p~n", [Result]),
    Timings = [element(1, Item) / 1000000 || Item <- Output ],
    io:format("Min / Avg / Max: ~fs / ~fs / ~fs~n",
              [lists:min(Timings), lists:sum(Timings) / 100, lists:max(Timings)]).

solve(Lines) ->
    {_, Ref} = spawn_monitor(?MODULE, solve_single, [Lines, self()]),
    receive
        {'DOWN', Ref, _, _, normal} -> ok;
        {'DOWN', Ref, _, _, _} = Crash -> error(Crash)
    after
        5000 ->
            error(timeout)
    end,
    receive
        {result, Result} -> Result
    after
        5000 ->
            error(timeout)
    end.

solve_single(Lines, Parent) ->
    {Workflows, _RLines} = parse_workflows(Lines),

    boot_workflows(Workflows),
    ets:new(parts, [public, named_table]),
    ets:insert(parts, [{in_flow, 1}]),

    in ! #{x => {1, 4000}, m => {1, 4000}, a => {1, 4000}, s => {1, 4000}},

    Result = combinations(collect_accepted()),
    Parent ! {result, Result},
    % Necessary as the workflow processes might not terminate before next iteration begins
    unregister_all(Workflows).

combinations(Parts) ->
    lists:foldl(fun(Part, Sum) ->
                        Sum + lists:foldl(fun(V, M) -> V*M end,
                                          1, [ T - F + 1 || {F, T} <- maps:values(Part) ])
                end, 0, Parts).

unregister_all(Ws) ->
    lists:foreach(fun erlang:unregister/1, ['A', 'R' | [ Name || {Name, _} <- Ws]]).

%% ======================================================
%% Workflows as processes
%% ======================================================

boot_forwarder(Name, Parent) ->
    Pid = spawn_link(?MODULE, forwarder, [Name, Parent]),
    wait_ready(Pid).

wait_ready(Pid) ->
    receive
        {ready, Pid} -> ok
    after
        1000 ->
            exit(timeout_ready)
    end.

forwarder(Name, Parent) ->
    register(Name, self()),
    Parent ! {ready, self()},
    forwarder_loop(Name, Parent).

forwarder_loop(Name, Parent) ->
    receive
        Part ->
            Parent ! {Name, Part},
            forwarder_loop(Name, Parent)
    end.

collect_accepted() ->
    collect_accepted([]).
collect_accepted(Acc) ->
    NAcc = receive
               {'A', Part} -> [Part | Acc];
               {'R', _} -> Acc
           after
               1000 ->
                   error(timeout)
           end,
    case ets:lookup(parts, in_flow) of
        [{_, 1}] ->
            NAcc;
        _ ->
            ets:update_counter(parts, in_flow, -1),
            collect_accepted(NAcc)
    end.

boot_workflows(Workflows) ->
    boot_forwarder('A', self()),
    boot_forwarder('R', self()),
    lists:foreach(fun boot_workflow/1, Workflows).

boot_workflow({Name, Conds}) ->
    Pid = spawn_link(?MODULE, workflow, [Name, Conds, self()]),
    wait_ready(Pid).

%% ======================================================
%% Core workflow logic
%% ======================================================

workflow(Name, Conds, Parent) ->
    register(Name, self()),
    Parent ! {ready, self()},
    workflow_loop(Conds).

workflow_loop(Conds) ->
    receive
        Part ->
            process_conds(Part, Conds),
            workflow_loop(Conds)
    end.

process_conds(Part, [{Param, Op, Val, IfTrue} | Else]) ->
    ParamRange = maps:get(Param, Part),
    case process_cond(Part, Param, ParamRange, Op, Val) of
        true ->
            IfTrue ! Part;
        false ->
            process_conds(Part, Else);
        {false, PassingPart, FailingPart} ->
            ets:update_counter(parts, in_flow, 1),
            IfTrue ! PassingPart,
            process_conds(FailingPart, Else)
    end;
process_conds(Part, [Else]) ->
    Else ! Part.

process_cond(_Part, _Param, {From, _To}, lt, Value) when From >= Value ->
    false;
process_cond(_Part, _Param, {_From, To}, lt, Value) when To < Value ->
    true;
process_cond(Part, Param, {From, To}, lt, Value) ->
    {false, Part#{ Param => {From, Value - 1} }, Part#{ Param => {Value, To} }};
process_cond(_Part, _Param, {From, _To}, gt, Value) when From > Value ->
    true;
process_cond(_Part, _Param, {_From, To}, gt, Value) when To =< Value ->
    false;
process_cond(Part, Param, {From, To}, gt, Value) ->
    {false, Part#{ Param => {Value + 1, To} }, Part#{ Param => {From, Value} }}.


%% ======================================================
%% Parsing
%% ======================================================

parse_workflows(Lines) ->
    parse_workflows(Lines, []).
parse_workflows([<<>> | RLines], Workflows) ->
    {Workflows, RLines};
parse_workflows([Line | RLines], Workflows) ->
    parse_workflows(RLines, [parse_workflow(Line) | Workflows]).

parse_workflow(Line) ->
    {Name, <<${, RBin1/binary>>} = parse_name(Line),
    Conditions = parse_conditions(RBin1),
    {Name, Conditions}.

parse_conditions(Bin) ->
    {Param, <<CompareCh:8, RBin1/binary>>} = parse_name(Bin),
    case CompareCh of
        $} -> % end of sequence
            [Param]; % either A or R
        _ ->
            CompareOp = compare_ch_to_op(CompareCh),
            {Value, <<":", RBin2/binary>>} = parse_int(RBin1),
            {Target, <<",", RBin3/binary>>} = parse_name(RBin2),
            [{Param, CompareOp, Value, Target} | parse_conditions(RBin3)]
    end.

compare_ch_to_op($<) -> lt;
compare_ch_to_op($>) -> gt.

parse_name(Bin) ->
    parse_name(Bin, <<>>).
parse_name(<<Letter:8, RBin/binary>>, Acc)
  when Letter >= $a, Letter =< $z; Letter == $A; Letter == $R ->
    parse_name(RBin, <<Acc/binary, Letter>>);
parse_name(RBin, Acc) ->
    {binary_to_atom(Acc), RBin}.

parse_int(Bin) ->
    parse_int(Bin, 0).
parse_int(<<Digit:8, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_int(Rest, Acc * 10 + Digit - $0);
parse_int(Bin, Acc) ->
    {Acc, Bin}.

