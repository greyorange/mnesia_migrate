-module(db_migration_tests).

-include_lib("eunit/include/eunit.hrl").

check_file_creation_test_() ->
	[
	 {"Check Src Dir Exists",
	   ?_assertEqual(true, filelib:is_dir(db_migration:get_migration_source_filepath()))},
	 {"Check Beam Dir Exists",
	   ?_assertEqual(true, filelib:is_dir(db_migration:get_migration_beam_filepath()))}
	].

check_function_output_format_test_() ->
	[
	 {"check_get_base_revision_output",
	  ?_assertEqual(true, is_atom(db_migration:get_base_revision()))},
	 {"check_get_current_head_output",
	  ?_assertEqual(true, is_atom(db_migration:get_current_head()))},
	 %{"check_find_pending_migrations_output",
	 % ?_assertEqual(true, is_list(db_migration:find_pending_migrations()))},
	 {"check_get_revision_tree_output",
	  ?_assertEqual(true, is_list(db_migration:get_revision_tree()))}
	].

check_migration_file_creation_test_() ->
	[%{require, rm, unix},
	 {"check revision file creation",
	  fun () ->
	      {ok, Dir} = file:get_cwd(),
	      code:add_path(Dir),
	      %io:format("path: ~p~n", [Dir]),
	      Filename = db_migration:create_migration_file(),
	      ?_assertEqual(true, filelib:is_file(Filename))
	      %io:format("file: ~p~n", [Filename])
	  end
	 },
	 {"check revision file creation and deletion",
	  fun () ->
	      {ok, Dir} = file:get_cwd(),
	      code:add_path(Dir),
	      Filename = db_migration:create_migration_file(),
	      ?_assertCmd("rm " ++ Filename)
	  end
	 }
	].

check_database_related_function_test_() ->
	[
	 {"check_applied_head",
	  fun() -> db_migration:start_mnesia(),
	  ?_assertEqual(true, is_atom(db_migration:get_applied_head()))
	  end
	 }
	].

check_db_related_test_() ->
	?_assertEqual(false, is_list(db_migration:get_applied_head())).
