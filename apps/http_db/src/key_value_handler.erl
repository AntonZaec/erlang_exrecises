-module(key_value_handler).
-export([
	init/2, allowed_methods/2, malformed_request/2, resource_exists/2,
	content_types_provided/2, read/2,
	valid_content_headers/2, is_conflict/2, content_types_accepted/2, write/2,
	delete_resource/2]).

%%====================================================================
%% ALL
%%====================================================================
init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> 
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

malformed_request(Req, State) ->
	Hosts = cluster:get_hosts(),
	DbName = req_helper:get_db_name(Req),
	{not cluster:is_db_everywhere(DbName, Hosts), Req, State}.

resource_exists(Req=#{method := <<"PUT">>}, State) ->
	%% It is not matter
	{true, Req, State};
resource_exists(Req, State) ->
	DbName = req_helper:get_db_name(Req),
	Hosts = cluster:get_hosts(),
	Key = req_helper:get_record_key(Req),
	TargetHost = select_host_for_key(Key, Hosts),
	case read_key_value(TargetHost, DbName, Key) of
		{ok, Value} -> {true, Req, {value, Value}};
		{error, instance} -> {false, Req, State}
	end.

valid_content_headers(Req=#{method := <<"PUT">>}, State) ->
	IsContentTypeValid = cowboy_req:header(
		<<"content-type">>, Req) == <<"application/json">>,
	{IsContentTypeValid, Req, State};
valid_content_headers(Req, State) ->
	{true, Req, State}.
%%====================================================================
%% GET
%%====================================================================
content_types_provided(Req, State) ->
	{[{<<"application/json">>, read}], Req, State}.

read(Req, State) ->
	{value, Value} = State,
	{record_to_json(Value), Req, State}.
%%====================================================================
%% PUT
%%====================================================================
is_conflict(Req, State) -> 
	{false, Req, State}.

content_types_accepted(Req, State) -> 
	{[{<<"application/json">>, write}], Req, State}.

write(Req, State) ->
	case req_helper:get_record(Req) of
		{error, wrong_json} -> {false, Req, State};
		{ok, Value} -> 
			DbName = req_helper:get_db_name(Req),
			Key = req_helper:get_record_key(Req),
			Hosts = cluster:get_hosts(),
			TargetHost = select_host_for_key(Key, Hosts),
			write_key_value(TargetHost, DbName, Key, Value),
			{true, Req, State}
	end.

%%====================================================================
%% DELETE
%%====================================================================
delete_resource(Req, State) ->
	DbName = req_helper:get_db_name(Req),
	Key = req_helper:get_record_key(Req),
	Hosts = cluster:get_hosts(),
	TargetHost = select_host_for_key(Key, Hosts),
	remove_record(TargetHost, DbName, Key),
	{true, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================
remove_record(TargetHost, DbName, Key) ->
	rpc:call(TargetHost, db_manager, do, [
		DbName, fun db_server:delete/2, [Key, db]]),
	ok.

record_to_json(Value) ->
	jiffy:encode({[{value, Value}]}).

select_host_for_key(Key, Hosts) ->
	<<Val:128>> = crypto:hash(md5, term_to_binary(Key)),
	HostNumber = Val rem length(Hosts) + 1,
	lists:nth(HostNumber, Hosts).

write_key_value(TargetHost, DbName, Key, Value) ->
	ok = rpc:call(TargetHost, db_manager, do, [
		DbName, fun db_server:write/3, [Key, Value, db]]).

read_key_value(TargetHost, DbName, Key) ->
	ReadRes = rpc:call(TargetHost, db_manager, do, [
		DbName, fun db_server:read/2, [Key, db]]),
	case ReadRes of
		{ok, _Value} -> ReadRes;
		{error, instance} -> ReadRes
	end.