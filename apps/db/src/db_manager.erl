-module(db_manager).
-export([get_database/1, create_database/1]).
%% Find database with passed name.
%% Return {ok, Db} or {error, Reason}.
get_database(DbName) ->
	Childs = supervisor:which_children(db_sup),
	find_db(Childs, DbName).
%% Create supervised database with name DbName.
%% Return {ok, Db} or {error, Reason}.
create_database(DbName) ->
	case get_database(DbName) of 
		{error, not_exists} -> 
			case supervisor:start_child(db_sup, [DbName]) of
				{ok, DbPid} -> {ok, db_server:convert_pid(DbPid, DbName)};
				{error, Error} -> {error, Error}
			end;
		{ok, _} -> {error, already_exists}
	end.
find_db([], _DbName) ->
	{error, not_exists};
find_db([{_, restarting, _, _}|_T], _DbName) ->
	{error, not_exists};
find_db([{_, undefined, _, _}|_T], _DbName) ->
	{error, not_exists};
find_db([{_, DbPid, _, _}|T], DbName) ->
	case db_server:get_name(DbPid) of 
		DbName -> {ok, db_server:convert_pid(DbPid, DbName)};
		_ -> find_db(T, DbName)
	end.
