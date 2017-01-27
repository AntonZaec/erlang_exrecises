-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2, loop/1]).

new() -> spawn(db, loop, [[]]).

destroy(DbPid) -> 
	rpc({self(), destroy_action}, DbPid).

write(Key, Element, DbPid) -> 
	rpc({self(), write_action, Key, Element}, DbPid).

delete(Key, DbPid) -> 
	rpc({self(), delete_action, Key}, DbPid).

read(Key, DbPid) -> 
	rpc({self(), read_action, Key}, DbPid).

match(Element, DbPid) -> 
	rpc({self(), match_action, Element}, DbPid).

rpc(Message, DbPid) -> 
	IsDbAlive = erlang:is_process_alive(DbPid),
	case IsDbAlive of
		true ->
			DbPid ! Message,
			receive
				Res -> Res
			after 
				1000 -> {error, timeout}
			end;
		false -> {error, db_process_died}
	end.
	
loop(Db) ->
	NewDb = receive
		{From, write_action, Key, Element} -> 
			DbAfterWriting = write_private(Key, Element, Db),
			From ! ok,
			DbAfterWriting;
		{From, read_action, Key} -> 
			From ! read_private(Key, Db),
			Db;
		{From, delete_action, Key} -> 
			DbAfterDeleting = delete_private(Key, Db),
			From ! ok,
			DbAfterDeleting;
		{From, match_action, Element} -> 
			From ! match_private(Element, Db),
			Db;
		{From, destroy_action} -> 
			From ! ok,
			[]
	end,
	loop(NewDb).

write_private(Key, Element, []) -> [{Key, Element}];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	[{HeadKey, HeadElement}|write_private(Key, Element, T)];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key =:= HeadKey ->
	[{Key, Element}|T];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key < HeadKey ->
	[{Key, Element}|[{HeadKey, HeadElement}|T]].

delete_private(Key, []) -> [];
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	[{HeadKey, HeadElement}|delete_private(Key, T)];
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key =:= HeadKey ->
	T;
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key < HeadKey ->
	[{HeadKey, HeadElement}|T].

read_private(Key, [{HeadKey, HeadElement}|T]) when Key =:= HeadKey ->
	{ok, HeadElement};
read_private(Key, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	read_private(Key, T);
read_private(Key, _) -> {error, instance}.

match_private(Element, [{HeadKey, HeadElement}|T]) when Element =:= HeadElement ->
	[HeadKey|match_private(Element, T)];
match_private(Element, [{HeadKey, HeadElement}|T]) when Element =/= HeadElement ->
	match_private(Element, T);
match_private(Element, []) -> [].