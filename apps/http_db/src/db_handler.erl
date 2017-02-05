-module(db_handler).
-export([
	init/2, allowed_methods/2, resource_exists/2, 
	content_types_provided/2, db_to_json/2,
	is_conflict/2, content_types_accepted/2, create_database/2, 
	delete_resource/2]).

%%====================================================================
%% ALL
%%====================================================================
init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> 
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

resource_exists(Req, State) ->
	Hosts = cluster:get_hosts(),
	DbName = req_helper:get_db_name(Req),
	{cluster:is_db_everywhere(DbName, Hosts), Req, State}.
%%====================================================================
%% GET
%%====================================================================
content_types_provided(Req, State) ->
	{[{<<"application/json">>, db_to_json}], Req, State}.

db_to_json(Req, State) ->
	DbName = req_helper:get_db_name(Req),
	Hosts = cluster:get_hosts(),
	DbData = read_db(DbName, Hosts, []),
	{convert_db_data_to_json(DbData), Req, State}.

%%====================================================================
%% PUT
%%====================================================================
is_conflict(Req, State) -> 
	Hosts = cluster:get_hosts(),
	DbName = req_helper:get_db_name(Req),
	{cluster:is_db_everywhere(DbName, Hosts), Req, State}.

content_types_accepted(Req, State) -> 
	ContentType = cowboy_req:header(<<"content-type">>, Req),
	{[{ContentType, create_database}], Req, State}.

create_database(Req, State) ->
	DbName = req_helper:get_db_name(Req),
	Hosts = cluster:get_hosts(),
	create_db_on_hosts(DbName, Hosts),
	{true, Req, State}.

%%====================================================================
%% DELETE
%%====================================================================
delete_resource(Req, State) ->
	DbName = req_helper:get_db_name(Req),
	Hosts = cluster:get_hosts(),
	remove_db_on_hosts(DbName, Hosts),
	{true, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================
read_db(_DbName, [], Acc) ->
	Acc;
read_db(DbName, [Host|T], Acc) ->
	DataFromHost = rpc:call(
		Host, db_manager, do, [DbName, fun db_server:read_all/1, [db]]),
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