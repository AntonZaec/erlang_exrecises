-module(db_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, db_sup}, db_sup, []).

init(_Args) ->
	SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
	ChildSpec = [
		#{
			id => db_instance,
			start => {db_server, start_link, []},
			restart => transient,
            shutdown => 100,
			type => worker,
			modules => [db_server]}],
	{ok, {SupFlags, ChildSpec}}.