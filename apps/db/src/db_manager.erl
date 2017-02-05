-module(db_manager).
-export([init/0, get_database/1, create_database/1, is_exists/1, remove_database/1, do/3]).

%% Must be called from application initialization
init() ->
	ets:new(db_object_cache, [set, public, named_table]),
	ok.
%% Execute Fun(Db) and return result
%% Use this function for reducing calling via network
do(DbName, Fun, Args) ->
	{ok, Db} = get_database(DbName),
	RealArgs = lists:map(fun(db) -> Db; (El) -> El end, Args),
	apply(Fun, RealArgs).
%% Check database existing
is_exists(DbName) ->
	case get_database(DbName) of
		{error, not_exists} -> false;
		{ok, _Db} -> true
	end.
%% Find database with passed name.
%% Return {ok, Db} or {error, Reason}.
get_database(DbName) ->
	%% We can register each db process by name DbName,
	%% but I think it is bad idea. I decided to use cache.
	case get_database_from_cache(DbName) of
		{ok, Db} -> {ok, Db};
		{error, not_cached} ->
			Childs = supervisor:which_children(kvdb),
			Res = find_db(Childs, DbName),
			case Res of
				{ok, Db} -> add_to_cache(DbName, Db);
				_ -> ok
			end,
			Res
	end.
%% Create supervised database with name DbName.
%% Return {ok, Db} or {error, Reason}.
create_database(DbName) ->
	case get_database(DbName) of 
		{error, not_exists} -> 
			case supervisor:start_child(kvdb, [DbName]) of
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
			ok = supervisor:terminate_child(kvdb, db_server:get_pid(Db)),
			ok;
		{error, not_exists} -> 
			ok
	end.
%% Return list of databases with numbers of elements
%% For example, [{db1, 100}, {db2, 150}, ...]
info() ->
	ets:foldl(fun({DbName, Db}, Acc) -> 
		[{DbName, db_server:count_keys(Db)}|Acc] end, [], db_object_cache).


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

get_database_from_cache(DbName) ->
	case ets:lookup(db_object_cache, DbName) of 
		[{DbName, Db}] -> {ok, Db};
		[] -> {error, not_cached}
	end.

add_to_cache(DbName, Db) ->
	ets:insert(db_object_cache, {DbName, Db}),
	spawn(fun() -> protect_cache(db_server:get_pid(Db), DbName) end).

protect_cache(DbPid, DbName) ->
	process_flag(trap_exit, true),
	MonRef = monitor(process, DbPid),
	receive
		{_Tag, MonRef, _Type, _Object, _Info} -> ets:delete(db_object_cache, DbName)
	end.