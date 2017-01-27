-module(db_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_State, DbNum) ->
	db_sup:start_link(DbNum).

stop(_State) ->
	ok.