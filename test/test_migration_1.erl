%% migration

%% This is an autogenerate file. Please adjust

-module(test_migration_1).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test_migration_1.

get_prev_rev() ->
    none.

-record(test_table, {sample_column_1=null, extra_info=null}).

up() ->
    Attr = [{ram_copies, [node()]}, {attributes, record_info(fields, test_table)}],
    case mnesia:create_table(test_table, Attr) of
        {atomic, ok}      -> io:format("table created~n", []),
	                     mnesia:transaction(fun() -> mnesia:write(test_table, #test_table{sample_column_1 = test_val}, write) end),
			     {atomic, ValList} = mnesia:transaction(fun() -> mnesia:all_keys(test_table) end),
			     hd(ValList);
        {aborted, Reason} -> io:format("mnesia create table error: ~p~n", [Reason]),
        		     throw({error, Reason})
    end.

down() ->
    mnesia:delete_table(test_table).

