-module(db_handler).
-export([init/2, allowed_methods/2, 
	content_types_provided/2, db_to_json/2,
	is_conflict/2, content_types_accepted/2, 
	create_database/2, delete_resource/2, where_is_not_db/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> 
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%%====================================================================
%% GET
%%====================================================================
content_types_provided(Req, State) ->
	{[{<<"application/json">>, db_to_json}], Req, State}.

db_to_json(Req, State) ->
	DbName = get_db_name(Req),
	Hosts = get_hosts(),
	DbData = read_db(DbName, Hosts, []),
	{convert_db_data_to_json(DbData), Req, State}.

%%====================================================================
%% PUT
%%====================================================================
is_conflict(Req, State) -> 
	Hosts = get_hosts(),
	DbName = get_db_name(Req),
	HostsWithoutDb = where_is_not_db(DbName, Hosts),
	Result = if 
		length(HostsWithoutDb) == 0 ->
			true;
		true -> 
			false
	end,
	{Result, Req, State}.

content_types_accepted(Req, State) -> 
	ContentType = cowboy_req:header(<<"content-type">>, Req),
	{[{ContentType, create_database}], Req, State}.

create_database(Req, State) ->
	DbName = get_db_name(Req),
	Hosts = get_hosts(),
	create_db_on_hosts(DbName, Hosts),
	{true, Req, State}.

%%====================================================================
%% DELETE
%%====================================================================
delete_resource(Req, State) ->
	DbName = get_db_name(Req),
	Hosts = get_hosts(),
	remove_db_on_hosts(DbName, Hosts),
	{true, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================
get_db_name(Req) ->
	list_to_atom(bitstring_to_list(cowboy_req:binding(db_name, Req))).
get_hosts() ->
	{ok, Hosts} = application:get_env(http_db, cluster),
	Hosts.

read_db(_DbName, [], Acc) ->
	Acc;
read_db(DbName, [Host|T], Acc) ->
	DataFromHost = rpc:call(
		Host, db_manager, do, [DbName, fun db_server:read_all/1]),
	%% It will be dangerous if there is a lot of data in database
	%% TODO: Rewrite it
	read_db(DbName, T, lists:append(Acc, DataFromHost)).

prepare_db_data_for_json([]) ->
	[];
prepare_db_data_for_json([{Key, Value}|T]) ->
	[{[{key, Key}, {value, Value}]}|prepare_db_data_for_json(T)].

convert_db_data_to_json(DbData) ->
	jiffy:encode(prepare_db_data_for_json(DbData)).

remove_db_on_hosts(_DbName, []) ->
	[];
remove_db_on_hosts(DbName, [Host|T]) ->
	rpc:call(Host, db_manager, remove_database, [DbName]),
	remove_db_on_hosts(DbName, T).

create_db_on_hosts(_DbName, []) ->
	ok;
create_db_on_hosts(DbName, [Host|T]) ->
	case rpc:call(Host, db_manager, create_database, [DbName]) of
		{ok, _} -> true;
		{error, Reason} -> throw({create_db_error, Host, Reason})
	end,
	create_db_on_hosts(DbName, T).

where_is_not_db(_DbName, []) ->
	[];
where_is_not_db(DbName, [Host|T]) ->
	IsDbOnHost = rpc:call(Host, db_manager, is_exists, [DbName]),
	if 
		IsDbOnHost -> where_is_not_db(DbName, T);
		true -> [Host|where_is_not_db(DbName, T)]
	end.