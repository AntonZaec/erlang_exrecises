-module(db_manager).
-export([get_database/1, create_database/1, is_exists/1, remove_database/1, do/2]).

%% Execute Fun(Db) and return result
%% Use this function for reducing calling via network
do(DbName, Fun) ->
	{ok, Db} = get_database(DbName),
	Fun(Db).

%% Check database existing
is_exists(DbName) ->
	case get_database(DbName) of
		{error, not_exists} -> false;
		{ok, _Db} -> true
	end.
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
%% Remove database
remove_database(DbName) ->
	case get_database(DbName) of
		{ok, Db} -> 
			db_server:destroy(Db),
			ok = supervisor:terminate_child(db_sup, db_server:get_pid(Db)),
			ok;
		{error, not_exists} -> 
			ok
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
