%% migration

%% This is an autogenerate file. Please adjust

-module(test2_migration).
-behaviour(migration).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test2.

get_prev_rev() ->
    test1.

-record(test_table, {sample_column_1, extra_info}).

up() ->
    F = fun(X) ->
                {test_table,
                 X#test_table.sample_column_1,
                 X#test_table.extra_info, whoa}
        end,
    {atomic, ok} = mnesia:transform_table(
                     test_table,
                     F,
                     [sample_column_1, extra_info, sample_column_2]),

    {atomic, ValList} = mnesia:transaction(fun() -> mnesia:read(test_table, test_val) end),
    io:format("ValList: ~p~n", [hd(ValList)]),
    hd(ValList).

down() ->
    F = fun({test_table, C1, Extra, _C2} = _X) ->
                {test_table, C1, Extra}
        end,
    {atomic, ok} = mnesia:transform_table(test_table,
                                          F, [sample_column_1, extra_info]),
    {atomic, ValList} = mnesia:transaction(fun() -> mnesia:read(test_table, test_val) end),
    io:format("ValList: ~p~n", [hd(ValList)]),
    hd(ValList).
