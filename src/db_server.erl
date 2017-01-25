-module(db_server).
-behaviour(gen_server).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-export([init/1, handle_call/3, terminate/2]).

new() ->
	{ok, ServerPid} = gen_server:start_link(db_server, [], []),
	{db, ServerPid}.

init(_Args) ->
	{ok, create_db_state()}.

terminate(_Reason, _State) ->
	ok.

destroy(Db) -> 
	{db, SrvPid} = Db,
	gen_server:call(SrvPid, {destroy_action}).

write(Key, Element, Db) -> 
	{db, SrvPid} = Db,
	gen_server:call(SrvPid, {write_action, Key, Element}).

delete(Key, Db) -> 
	{db, SrvPid} = Db,
	gen_server:call(SrvPid, {delete_action, Key}).

read(Key, Db) -> 
	{db, SrvPid} = Db,
	gen_server:call(SrvPid, {read_action, Key}).

match(Element, Db) -> 
	{db, SrvPid} = Db,
	gen_server:call(SrvPid, {match_action, Element}).

handle_call(Action, _From, DbState) ->
	{Reply, NewDbState} = case Action of
		{write_action, Key, Element} -> 
			{ok, write_private(Key, Element, DbState)};
		{read_action, Key} -> 
			{read_private(Key, DbState), DbState};
		{delete_action, Key} -> 
			{ok, delete_private(Key, DbState)};
		{match_action, Element} -> 
			{match_private(Element, DbState), DbState};
		{destroy_action} -> 
			{ok, []}
	end,
	{reply, Reply, NewDbState}.

create_db_state() ->
	[].

write_private(Key, Element, []) -> [{Key, Element}];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	[{HeadKey, HeadElement}|write_private(Key, Element, T)];
write_private(Key, Element, [{HeadKey, _HeadElement}|T]) when Key =:= HeadKey ->
	[{Key, Element}|T];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key < HeadKey ->
	[{Key, Element}|[{HeadKey, HeadElement}|T]].

delete_private(_, []) -> [];
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	[{HeadKey, HeadElement}|delete_private(Key, T)];
delete_private(Key, [{HeadKey, _HeadElement}|T]) when Key =:= HeadKey ->
	T;
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key < HeadKey ->
	[{HeadKey, HeadElement}|T].

read_private(Key, [{HeadKey, HeadElement}|_T]) when Key =:= HeadKey ->
	{ok, HeadElement};
read_private(Key, [{HeadKey, _HeadElement}|T]) when Key > HeadKey ->
	read_private(Key, T);
read_private(_Key, _) -> {error, instance}.

match_private(Element, [{HeadKey, HeadElement}|T]) when Element =:= HeadElement ->
	[HeadKey|match_private(Element, T)];
match_private(Element, [{_HeadKey, HeadElement}|T]) when Element =/= HeadElement ->
	match_private(Element, T);
match_private(_Element, []) -> [].