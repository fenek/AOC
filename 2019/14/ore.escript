#!/usr/bin/env escript

-mode(compile).

main([Filename, FuelNeeded]) ->
    {ok, Input} = file:read_file(Filename),
    
    load_recipes(binary:split(Input, <<"\n">>, [global])),

    produce(list_to_integer(FuelNeeded)),

    io:format("~p~n", [get(ore)]).

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

produce(FuelUnits) ->
    put(ore, 0),
    produce(<<"FUEL">>, FuelUnits).

produce(_, None) when None < 1 ->
    ok;
produce(<<"ORE">>, ExpectedUnits) ->
    put(ore, get(ore) + ExpectedUnits),
    storage_add(<<"ORE">>, ExpectedUnits);
produce(Product, ExpectedUnits) ->
    {ProductUnits, Compounds} = get(Product),
    io:format("FORMULA ~p => ~p ~s~n", [Compounds, ProductUnits, Product]),

    lists:foreach(fun({Compound, CompoundUnits}) ->
                          ensure(Compound, CompoundUnits),
                          storage_take(Compound, CompoundUnits)
                  end, Compounds),

    storage_add(Product, ProductUnits),
    
    produce(Product, ExpectedUnits - ProductUnits).

ensure(Compound, Units) ->
    produce(Compound, Units - storage_peek(Compound)).

storage_add(Name, Units) ->
    io:format("ADD ~s ~p~n", [Name, Units]),
    Curr = case get({storage, Name}) of
               undefined -> 0;
               Curr0 -> Curr0
           end,
    put({storage, Name}, Curr + Units).

storage_take(Name, Units) ->
    io:format("TAKE ~s ~p~n", [Name, Units]),
    case get({storage, Name}) - Units of
        Negative when Negative < 0 -> exit({inconsistency, Name});
        OK -> put({storage, Name}, OK)
    end.

storage_peek(Name) ->
    case get({storage, Name}) of
        undefined -> 0;
        Val -> Val
    end.

