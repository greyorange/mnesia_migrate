-module(db_migration).
-export([start_mnesia/0, transform/2, get_base_revision/0,
	 create_migration_file/1, init_migrations/0,
	 get_next_revision/1, get_current_head/1,
	 read_config/0, create_migration_file/0
	]).

-define(URNAME, user).
-define(TABLE, schema_migrations).
-define(BaseDir, "/home/gaurav/project/mnesia_migrate/migrations/").
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

%fetch_all_changed_tables() ->
    %set_path("/home/gaurav/project/butler_server/src"),
%    Models = models:all(),
%    Tabletomigrate = [TableName || {TableName, Options} <- Models , proplists:get_value(attributes, Options) /= mnesia:table_info(TableName, attributes)],
%    io:fwrite("Tables needing migration : ~p~n", [Tabletomigrate]),
%    Tabletomigrate.

get_base_revision() ->
    Modulelist = filelib:wildcard("migrations/*.beam"),
    Res = lists:filter(fun(Filename) ->
        Modulename = list_to_atom(filename:basename(Filename, ".beam")),
        string:equal(Modulename:get_prev_rev(),none)
    end,
    Modulelist),
    BaseModuleName = list_to_atom(filename:basename(Res, ".beam")),
    io:fwrite("Base Rev file is ~p~n", [BaseModuleName]),
    case Res of
        [] -> none;
	_ -> BaseModuleName:get_current_rev()
    end.


get_next_revision(RevId) ->
    Modulelist = filelib:wildcard("migrations/*.beam"),
    Res = lists:filter(fun(Filename) ->
        Modulename = list_to_atom(filename:basename(Filename, ".beam")),
        OldrevId = Modulename:get_prev_rev(),
        string:equal(OldrevId, RevId)
    end,
    Modulelist),
    %io:fwrite("Base Rev: ~p Next Rev ~p~n", [RevId,Res]),
    case Res of
    [] -> [];
    _ -> ModuleName = list_to_atom(filename:basename(Res, ".beam")), ModuleName:get_current_rev()
    end.

get_current_head(RevId) ->
    case get_next_revision(RevId) of
	[] -> RevId ;
        NextRevId -> get_current_head(NextRevId)
    end.

create_migration_file(CommitMessage) ->
    erlydtl:compile('schema.template', migration_template),
    NewRevisionId = string:substr(uuid:to_string(uuid:uuid4()),1,8),
    BaseRev = get_base_revision(),
    OldRevisionId = get_current_head(BaseRev),
    Filename = NewRevisionId ++ "_" ++ string:substr(CommitMessage, 1, 20) ,
    {ok, Data} = migration_template:render([{new_rev_id , NewRevisionId}, {old_rev_id, OldRevisionId},
					  {modulename, Filename}, {tabtomig, []},
					  {commitmessage, CommitMessage}]),
    file:write_file(?BaseDir ++ Filename ++ ".erl", Data).

create_migration_file() ->
    erlydtl:compile('schema.template', migration_template),
    NewRevisionId = "a" ++ string:substr(uuid:to_string(uuid:uuid4()),1,8),
    BaseRev = get_base_revision(),
    OldRevisionId = get_current_head(BaseRev),
    Filename = NewRevisionId ++ "_migration" ,
    {ok, Data} = migration_template:render([{new_rev_id , NewRevisionId}, {old_rev_id, OldRevisionId},
					  {modulename, Filename}, {tabtomig, []},
					  {commitmessage, "migration"}]),
    file:write_file(?BaseDir ++ Filename ++ ".erl", Data),
    io:format("New file created ~p~n", [Filename ++ ".erl"]).
