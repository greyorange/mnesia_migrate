%% migration

%% This is an autogenerate file. Please adjust

-module(test_migration_2).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test_migration_2.

get_prev_rev() ->
    test_migration_1.

%% Old test_table record
-record(test_table, {sample_column_1=null, extra_info=null}).

-record(new_test_table, {sample_column_1=null, sample_column_2=null, extra_info=null}).

up() ->
    {atomic, ok} = mnesia:transform_table(test_table,
			fun(X) ->
				{test_table, X#test_table.sample_column_1,
			        X#test_table.extra_info,
			        whoa}
			end,
    		   [sample_column_1, extra_info, sample_column_2]),
    {atomic, ValList} = mnesia:transaction(fun() -> mnesia:all_keys(test_table) end),
    io:format("ValList: ~p~n", [ValList]),
    ValList.

down() ->
    {atomic, ok} = mnesia:transform_table(test_table,
			fun(X) ->
				{X#test_table.sample_column_1,
			        X#new_test_table.extra_info
			        }
			end,
    		   record_info(fields, test_table)),
    {atomic, ValList} = mnesia:transaction(fun() -> mnesia:all_keys(test_table) end),
    io:format("ValList: ~p~n", [ValList]),
    ValList.

