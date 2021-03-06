%%%-------------------------------------------------------------------
%% @doc http_db public API
%% @end
%%%-------------------------------------------------------------------

-module(http_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([{
		'_', [
			%TODO: Create API for list of databases
			{"/http_db/:db_name/:key", key_value_handler, []},
			{"/http_db/:db_name/", db_handler, []},
			{"/http_db/", cowboy_static, {priv_file, http_db, "static/index.htm"}}]}]),
	{ok, _} = cowboy:start_clear(
		http_db_listener, 
		10, 
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}),
    http_db_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
