-module(db_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(DbNum) ->
	{ok, SupPid} = supervisor:start_link(db_sup, []),
	start_every_child(DbNum, SupPid),
	{ok, SupPid}.

init(_Args) ->
	SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
	ChildSpec = [
		#{
			id => db_instance,
			start => {db_server, new, []},
			restart => transient,
            shutdown => 100,
			type => worker,
			modules => [db_server]}],
	{ok, {SupFlags, ChildSpec}}.

start_every_child(0, _SupPid) -> ok;
start_every_child(DbNum, SupPid) ->
	supervisor:start_child(SupPid, []),
	start_every_child(DbNum - 1, SupPid).