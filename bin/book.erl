#!/usr/bin/env escript

-define(BOOKPATH, '/data/books').

list_files(RegExp) ->
    filelib:fold_files(?BOOKPATH, "((?i)" ++ RegExp ++ ")", true,
                       fun(File, Acc) -> [File|Acc] end, []).

% Returns [{Index, {File, Path}}]
process_paths(Paths) ->
    Sorted = lists:sort(fun({A,_}, {B,_}) -> A < B end,
                        [{filename:basename(P), P} || P <- Paths]),
    lists:zip(lists:seq(1, length(Sorted)), Sorted).

main(Args) ->
    Paths = process_paths(case length(Args) of
                              0 -> list_files("");
                              _ -> list_files(hd(Args))
                          end),
    case length(Paths) of
        0 ->
            io:format("Nothing found!~n");
        _ ->
            [io:format("\e[32m~3w \e[33m~s\n",
                       [Index, File]) || {Index, {File, _}} <- Paths],
            {ok, [Choice|_]} = io:fread("\e[32mOpen>\e[0m ", "~d"),
            {_, {_, Path}} = lists:nth(Choice, Paths),
            open_port({spawn_executable, "/usr/bin/open"}, [{args, [Path]}])
    end.
