-module(db_migration).
-export([start_mnesia/0, transform/2, fetch_all_changed_tables/0, get_base_revision/0,
	 create_migration_file/1, init_migrations/0, get_old_rev_id_using_file_pid/1,
	 get_next_revision/1, get_current_head/1, get_new_rev_id_using_file_pid/1,
	 read_config/0
	]).

-define(URNAME, user).
-define(TABLE, schema_migrations).
-define(BaseDir, "/home/gaurav/project/erlang_learning/migrations/").
-define(ProjDir, "/home/gaurav/project/mnesia_migrate/").

read_config() ->
    case file:consult(?ProjDir ++ "priv/config") of
        {ok, Conf} ->
		    lists:foreach(fun({conf, Key, Val}) -> io:format("Key: ~p, Val: ~p ~n", [Key, Val]) end, Conf) ;
        {error, Reason} ->
		io:format("Error opening file.. Reason -> ~p~n", [Reason])
    end.


start_mnesia() ->
	mnesia:start().

% New_struct = Function to output data compatible modified schema.
%transform(Old_struct, New_struct) ->
%        RecV2 = [uid, uname, upass, umail],
%	{atomic, ok} = mnesia:transform_table(?URNAME,
%					      fun(Old_struct) ->
%							      New_struct
%					      end, RecV2, ?URNAME).

init_migrations() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            io:format("Table schema_migration not found, creating...~n", []),
            Attr = [{type, ordered_set}, {disc_copies, migresia:list_nodes()}],
            case mnesia:create_table(?TABLE, Attr) of
                {atomic, ok}      -> io:format(" => created~n", []);
                {aborted, Reason} -> throw({error, Reason})
            end
    end.

transform(_Old_struct, _New_struct) ->
	ok.

fetch_all_changed_tables() ->
    Models = models:all(),
    Tabletomigrate = [TableName || {TableName, Options} <- Models , proplists:get_value(attributes, Options) /= mnesia:table_info(TableName, attributes)],
    io:fwrite("Tables needing migration : ~p~n", [Tabletomigrate]),
    Tabletomigrate.

get_old_rev_id_using_file_pid(FilePid) ->
    ListTokens = string:tokens(io:get_line(FilePid, "\n"), " = "),
    if hd(ListTokens) == "OldRevisionId" ->
	OldrevId = tl(ListTokens);
    hd(ListTokens) /= "OldRevisionId" ->
        OldrevId = get_old_rev_id_using_file_pid(FilePid)
    end,
    OldrevId.

get_new_rev_id_using_file_pid(FilePid) ->
    ListTokens = string:tokens(io:get_line(FilePid, "\n"), " = "),
    if hd(ListTokens) == "NewRevisionId" ->
	NewrevId = tl(ListTokens);
    hd(ListTokens) /= "NewRevisionId" ->
        NewrevId = get_new_rev_id_using_file_pid(FilePid)
    end,
    NewrevId.

get_base_revision() ->
    {ok, Filenamelist} = file:list_dir(?BaseDir),
    Res = lists:filter(fun(Filename) ->
        %io:fwrite("file:~p~n", [Filename]),
        {ok, FilePid} = file:open(?BaseDir++Filename, [read]),
        OldrevId = get_old_rev_id_using_file_pid(FilePid),
        %file:close(FilePid),
        %io:fwrite("Rev id is ~p~n", [string:to_upper(OldrevId)]),
        string:equal(list_to_binary(OldrevId),<<"None\n">>)
    end,
    Filenamelist),
    io:fwrite("Base Rev id is ~p~n", [Res]),
    case Res of
        [] -> [];
	[Head | _Tail] -> hd(string:tokens(Head, "_"))
    end.


get_next_revision(RevId) ->
    %BaseDir = "/home/gaurav/project/erlang_learning/migrations/",
    {ok, Filenamelist} = file:list_dir(?BaseDir),
    Res = lists:filter(fun(Filename) ->
        {ok, FilePid} = file:open(?BaseDir++Filename, [read]),
        OldrevId = get_old_rev_id_using_file_pid(FilePid),
        %file:close(FilePid),
        %io:fwrite("Curr Rev: ~p~n", [OldrevId]),
        string:equal(list_to_binary(OldrevId),list_to_binary(RevId++"\n"))
    end,
    Filenamelist),
    %io:fwrite("Base Rev: ~p Next Rev ~p~n", [RevId,Res]),
    Res.

get_current_head(RevId) ->
    case get_next_revision(RevId) of
	[] -> RevId ;
        _ -> get_current_head(hd(string:tokens(hd(get_next_revision(RevId)),"_")))
    end.

create_migration_file(CommitMessage) ->
    erlydtl:compile('schema.template', migration_template),
    NewRevisionId = string:substr(uuid:to_string(uuid:uuid4()),1,8),
    BaseRev = get_base_revision(),
    OldRevisionId = get_current_head(BaseRev),
    Filename = NewRevisionId ++ "_" ++ string:substr(CommitMessage, 1, 20) ,
    {ok, Data} = migration_template:render([{new_rev_id , NewRevisionId}, {old_rev_id, OldRevisionId},
					  {modulename, Filename}, {tabtomig, fetch_all_changed_tables()},
					  {commitmessage, CommitMessage}]),
    file:write_file(?BaseDir ++ Filename ++ ".erl", Data).
