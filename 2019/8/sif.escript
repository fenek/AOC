#!/usr/bin/env escript

-mode(compile).

main([Filename, Mode]) ->
    {ok, Input} = file:read_file(Filename),

    XDim = 25,
    YDim = 6,

    Image = load_image(string:trim(Input, both, "\n"), XDim, YDim),

    case Mode of
        "validate" ->
            io:format("~p~n", [validate(Image)]);
        "draw" ->
            Rendered = render(Image, XDim, YDim),
            draw(Rendered, XDim)
    end.

load_image(Input, XDim, YDim) ->
    load_image(Input, XDim*YDim, 1, #{}).

load_image(<<>>, _, _, Image) ->
    Image;
load_image(Input, Size, LayerNo, Image) ->
    <<Layer:Size/binary, RImg/binary>> = Input,
    load_image(RImg, Size, LayerNo + 1, Image#{ LayerNo => Layer }).

draw([], _) ->
    ok;
draw(Rendered, XDim) ->
    Rest = draw_line(Rendered, XDim),
    io:format("~n", []),
    draw(Rest, XDim).

draw_line(Rest, 0) ->
    Rest;
draw_line([0 | Rest], Left) ->
    io:format(" ", []),
    draw_line(Rest, Left - 1);
draw_line([1 | Rest], Left) ->
    io:format("X", []),
    draw_line(Rest, Left - 1).

render(Image, XDim, YDim) ->
    render(lists:keysort(1, maps:to_list(Image)), lists:duplicate(XDim * YDim, 2)).

render([], Rendered) ->
    Rendered;
render([{_, Layer} | RLayers], Rendered) ->
    NRendered = apply_layer(Layer, Rendered),
    render(RLayers, NRendered).

apply_layer(<<>>, []) ->
    [];
apply_layer(<<C, RLayer/binary>>, [2 | RRendered]) ->
    [decode(C) | apply_layer(RLayer, RRendered)];
apply_layer(<<_, RLayer/binary>>, [C | RRendered]) ->
    [C | apply_layer(RLayer, RRendered)].

decode($0) -> 0;
decode($1) -> 1;
decode($2) -> 2.

validate(Image) ->
    {_Zeros, Ones, Twos} = validate(maps:values(Image), {infinity, 0, 0}),
    Ones * Twos.

validate([], Best) ->
    Best;
validate([Layer | RLayers], {BestZeros, _, _} = Best) ->
    case count(Layer) of
        {Zeros, _, _} = NewBest when Zeros < BestZeros ->
            validate(RLayers, NewBest);
        _ ->
            validate(RLayers, Best)
    end.

count(Layer) ->
    count(Layer, 0, 0, 0).

count(<<>>, Zeros, Ones, Twos) -> {Zeros, Ones, Twos};
count(<<$0, R/binary>>, Zeros, Ones, Twos) -> count(R, Zeros + 1, Ones, Twos);
count(<<$1, R/binary>>, Zeros, Ones, Twos) -> count(R, Zeros, Ones + 1, Twos);
count(<<$2, R/binary>>, Zeros, Ones, Twos) -> count(R, Zeros, Ones, Twos + 1).
