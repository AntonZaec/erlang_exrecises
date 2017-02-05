-module(cluster).
-export([get_hosts/0, where_is_not_db/2, is_db_everywhere/2]).

get_hosts() ->
	{ok, Hosts} = application:get_env(http_db, cluster),
	Hosts.

where_is_not_db(_DbName, []) ->
	[];
where_is_not_db(DbName, [Host|T]) ->
	IsDbOnHost = rpc:call(Host, db_manager, is_exists, [DbName]),
	if 
		IsDbOnHost -> where_is_not_db(DbName, T);
		true -> [Host|where_is_not_db(DbName, T)]
	end.

is_db_everywhere(DbName, Hosts) ->
	HostsWithoutDb = where_is_not_db(DbName, Hosts),
	length(HostsWithoutDb) == 0.