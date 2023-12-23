#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, Data} = file:read_file("input"),
    [SequenceBin | BoardsInput0] = binary:split(Data, <<"\n">>, [global]),
    Sequence0 = binary:split(SequenceBin, <<",">>, [global]),
    BoardsInput = [ B || B <- BoardsInput0, B /= <<>> ],
    Sequence = [ binary_to_integer(B) || B <- Sequence0 ],
    Boards = parse_boards(BoardsInput, []),
    solve(Boards, Sequence).

solve(Boards, [Next | RSequence]) ->
    case cross_out_boards(Boards, Next) of
        [] ->
            [PreWinBoard] = Boards,
            Board = cross_out(PreWinBoard, Next),
            Sum = maps:fold(fun({r, _}, Row, Acc) -> Acc + lists:sum(Row);
                               (_, _, Acc) -> Acc end, 0, Board),
            io:format("~p, ~p~n~p~n", [Sum, Next, Sum*Next]);
        NBoards ->
            solve(NBoards, RSequence)
    end.

cross_out_boards(Boards, Int) ->
    cross_out_boards(Boards, Int, []).

cross_out_boards([], _, BoardsAcc) ->
    BoardsAcc;
cross_out_boards([Board0 | RBoards], Int, BoardsAcc) ->
    Board1 = cross_out(Board0, Int),
    case lists:any(fun(V) -> V == [] end, maps:values(Board1)) of
        true ->
            cross_out_boards(RBoards, Int, BoardsAcc);
        false ->
            cross_out_boards(RBoards, Int, [Board1 | BoardsAcc])
    end.

cross_out(Board0, Int) ->
    maps:map(fun(_K, Ints0) -> lists:delete(Int, Ints0) end, Board0).

parse_boards([], Boards) ->
    Boards;
parse_boards(Input, Boards) ->
    {Board, RInput} = parse_board(Input),
    parse_boards(RInput, [Board | Boards]).

parse_board(Input) ->
    parse_board(Input, #{}, 1).

parse_board(RInput, Board, 6) ->
    {Board, RInput};
parse_board([ Row | RInput ], Board0, RowNum) ->
    Cells = [ binary_to_integer(B) || B <- binary:split(Row, <<" ">>, [global]), B /= <<>> ],
    Board1 = Board0#{ {r, RowNum} => Cells },
    Board2 = update_cols(Board1, Cells, 1),
    parse_board(RInput, Board2, RowNum + 1).

update_cols(Board, [Int | RInt], ColNum) ->
    Col = maps:get({c, ColNum}, Board, []),
    update_cols(Board#{ {c, ColNum} => [Int | Col] }, RInt, ColNum + 1);
update_cols(Board, [], _) ->
    Board.

