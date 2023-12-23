#!/usr/bin/env escript

-mode(compile).

main([Mode]) ->
    {ok, Data} = file:read_file("input"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    Numbers = [ binary_to_integer(Bin) || Bin <- Lines, Bin /= <<>> ],
    case Mode of
        "brute" -> brute(Numbers)
    end.

brute(Numbers) ->
    lists:foreach(
      fun(A) ->
              lists:foreach(
                fun(B) ->
                        lists:foreach(
                          fun(C) ->
                                  case A + B + C of
                                      2020 ->
                                          io:format("~p ~p ~p~n~p~n", [A, B, C, A*B*C]),
                                          exit(done);
                                      _ ->
                                          ok
                                  end
                          end, Numbers)
                end, Numbers)
      end, Numbers).
