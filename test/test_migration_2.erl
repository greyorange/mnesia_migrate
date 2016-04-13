%% migration

%% This is an autogenerate file. Please adjust

-module(test_migration_2).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).
-include("upgrade.hrl").

init([]) -> ok.

get_current_rev() ->
    test_migration_2.

get_prev_rev() ->
    test_migration_1.

up() ->
    F = fun(X) ->
	    {test_table, X#test_table.sample_column_1, X#test_table.extra_info, whoa}
    end,
    {atomic, ok} = mnesia:transform_table(test_table,
					  F, [sample_column_1, extra_info, sample_column_2]),

    {atomic, ValList} = mnesia:transaction(fun() -> mnesia:read(test_table, test_val) end),
    io:format("ValList: ~p~n", [hd(ValList)]),
    hd(ValList).

down() ->
    F = fun(X) ->
            {test_table, X#test_table.sample_column_1, X#test_table.extra_info}
        end,
    {atomic, ok} = mnesia:transform_table(test_table,
			F, [sample_column_1, extra_info]),
    {atomic, ValList} = mnesia:transaction(fun() -> mnesia:read(test_table, test_val) end),
    io:format("ValList: ~p~n", [hd(ValList)]),
    hd(ValList).
