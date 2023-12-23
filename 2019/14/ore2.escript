#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
    {ok, Input} = file:read_file(Filename),
    
    load_recipes(binary:split(Input, <<"\n">>, [global])),

    storage_add(<<"ORE">>, 1000000000000),
    Units = produce_fuel(),

    io:format("~p~n", [Units]).

load_recipes([]) ->
    ok;
load_recipes([<<>> | RRecipes]) ->
    load_recipes(RRecipes);
load_recipes([Recipe | RRecipes]) ->
    [CompoundsAllIn, ProductIn] = binary:split(Recipe, <<" => ">>),
    {Product, ProductUnits} = parse_compound(ProductIn),
    CompoundsIn = binary:split(CompoundsAllIn, <<", ">>, [global]),
    Compounds = [ parse_compound(CompoundIn) || CompoundIn <- CompoundsIn ],

    put(Product, {ProductUnits, Compounds}),

    load_recipes(RRecipes).

parse_compound(CompoundIn) ->
    [CountBin, Name] = binary:split(CompoundIn, <<" ">>),
    {Name, binary_to_integer(CountBin)}.

produce_fuel() ->
    produce_fuel(0, 1).

produce_fuel(Total, Step) ->
    Backup = backup_state(),
    try produce(<<"FUEL">>, Step) of
        _ -> produce_fuel(Total + Step, Step * 10)
    catch
        error:out_of_ore ->
            case Step of
                1 ->
                    Total;
                _ ->
                    restore_state(Backup),
                    produce_fuel(Total, 1)
            end
    end.

backup_state() ->
    get().

restore_state(Backup) ->
    lists:foreach(fun({K, V}) -> put(K, V) end, Backup).

produce(Product, ExpectedUnits) ->
    {ProductUnits, Compounds} = get(Product),

    Factor = case ExpectedUnits rem ProductUnits of
                 0 -> ExpectedUnits div ProductUnits;
                 _ -> ExpectedUnits div ProductUnits + 1
             end,

    lists:foreach(fun({Compound, CompoundUnits}) ->
                          ensure(Compound, Factor * CompoundUnits),
                          storage_take(Compound, Factor * CompoundUnits)
                  end, Compounds),

    storage_add(Product, Factor * ProductUnits).

ensure(<<"ORE">>, Units) ->
    case storage_peek(<<"ORE">>) < Units of
        true -> error(out_of_ore);
        _ -> ok
    end;
ensure(Compound, Units) ->
    case Units - storage_peek(Compound) of
        Enough when Enough =< 0 -> ok;
        NotEnough -> produce(Compound, NotEnough)
    end.

storage_add(Name, Units) ->
    %io:format("ADD ~s ~p~n", [Name, Units]),
    Curr = case get({storage, Name}) of
               undefined -> 0;
               Curr0 -> Curr0
           end,
    put({storage, Name}, Curr + Units).

storage_take(Name, Units) ->
    %io:format("TAKE ~s ~p~n", [Name, Units]),
    case get({storage, Name}) - Units of
        Negative when Negative < 0 -> exit({inconsistency, Name});
        OK -> put({storage, Name}, OK)
    end.

storage_peek(Name) ->
    case get({storage, Name}) of
        undefined -> 0;
        Val -> Val
    end.

