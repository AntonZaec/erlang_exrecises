-module(db_server).
-behaviour(gen_server).
-export([new/1, start_link/1, destroy/1, write/3, delete/2, 
		read/2, match/2, count_keys/1, avg/1,
		get_name/1, convert_pid/1, convert_pid/2]).
-export([init/1, handle_call/3, terminate/2]).

%% Create database with name Name
new(Name) ->
	{ok, ServerPid} = gen_server:start_link(db_server, Name, []),
	TableId = gen_server:call(ServerPid, {get_table_id}),
	{ServerPid, TableId}.
%% For calling by supervisor
start_link(Name) ->
	{ServerPid, _} = new(Name),
	{ok, ServerPid}.
%% Clear database
destroy(Db) ->
	{SrvPid, _} = Db, 
	gen_server:call(SrvPid, {destroy_action}).

write(Key, Element, Db) -> 
	{SrvPid, _} = Db, 
	gen_server:call(SrvPid, {write_action, Key, Element}).

delete(Key, Db) -> 
	{SrvPid, _} = Db, 
	gen_server:call(SrvPid, {delete_action, Key}).

read(Key, Db) -> 
	{_, TableId} = Db, 
	%% We don't call gen_server:call for concurrent data reading
	case ets:lookup(TableId, Key) of
		[{Key, Element}] -> {ok, Element};
		[] -> {error, instance}
	end.
%% Return all keys with value equal to Element
match(Element, Db) -> 
	{SrvPid, _} = Db, 
	gen_server:call(SrvPid, {match_action, Element}).
%% Count key in storage
count_keys(Db) ->
	{SrvPid, _} = Db, 
	gen_server:call(SrvPid, {count_keys}).
%% Compute average of all numeric keys
avg(Db) ->
	{SrvPid, _} = Db, 
	gen_server:call(SrvPid, {average}).
%% Return database name
get_name(SrvPid) ->
	gen_server:call(SrvPid, {get_table_id}).
%% Create database object from pid
convert_pid(DbPid) ->
	{DbPid, get_name(DbPid)}.
convert_pid(DbPid, DbName) ->
	{DbPid, DbName}.

%% Callback for gen_server
init(Name) ->
	{ok, create_db_state(Name)}.
%% Callback for gen_server
terminate(_Reason, _State) ->
	ok.
%% Callbacks for gen_server
handle_call({write_action, Key, Element}, _From, TableId) ->
	ets:insert(TableId, {Key, Element}),
	{reply, ok, TableId};
handle_call({delete_action, Key}, _From, TableId) ->
	ets:delete(TableId, Key),
	{reply, ok, TableId};
handle_call({match_action, Element}, _From, TableId) ->
	Matches = ets:foldl(fun({Key, Value}, Acc) -> 
		if 
			Value =:= Element -> [Key|Acc]; 
			true -> Acc
		end
	end, [], TableId),
	{reply, Matches, TableId};
handle_call({destroy_action}, _From, TableId) ->
	TableName = ets:info(TableId, name),
	ets:delete(TableId),
	{reply, ok, create_db_state(TableName)};
handle_call({count_keys}, _From, TableId) ->
	Size = ets:info(TableId, size),
	{reply, Size, TableId};
handle_call({average}, _From, TableId) -> 
	{Sum, Total} = ets:foldl(fun({Key, _}, Acc) -> 
		{CurSum, CurTotal} = Acc,
		if 
			is_number(Key) -> {CurSum + Key, CurTotal + 1}; 
			true -> Acc
		end
	end, {0, 0}, TableId),
	{reply, Sum/Total, TableId};
handle_call({get_table_id}, _From, TableId) ->
	{reply, TableId, TableId}.

create_db_state(Name) ->
	ets:new(Name, [set, protected, {read_concurrency, true}, named_table]).