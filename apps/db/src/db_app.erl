-module(db_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_State, _Args) ->
	db_sup:start_link().

stop(_State) ->
	ok.