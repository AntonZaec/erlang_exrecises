-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(Db) -> ok.

write_private(Key, Element, []) -> [{Key, Element}];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	[{HeadKey, HeadElement}|write_private(Key, Element, T)];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key =:= HeadKey ->
	[{Key, Element}|T];
write_private(Key, Element, [{HeadKey, HeadElement}|T]) when Key < HeadKey ->
	[{Key, Element}|[{HeadKey, HeadElement}|T]].

write(Key, Element, Db) -> write_private(Key, Element, Db).

delete_private(Key, []) -> [];
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	[{HeadKey, HeadElement}|delete_private(Key, T)];
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key =:= HeadKey ->
	T;
delete_private(Key, [{HeadKey, HeadElement}|T]) when Key < HeadKey ->
	[{HeadKey, HeadElement}|T].

delete(Key, Db) -> delete_private(Key, Db).

read_private(Key, [{HeadKey, HeadElement}|T]) when Key =:= HeadKey ->
	{ok, HeadElement};
read_private(Key, [{HeadKey, HeadElement}|T]) when Key > HeadKey ->
	read_private(Key, T);
read_private(Key, _) -> {error, instance}.

read(Key, Db) -> read_private(Key, Db).

match_private(Element, [{HeadKey, HeadElement}|T]) when Element =:= HeadElement ->
	[HeadKey|match_private(Element, T)];
match_private(Element, [{HeadKey, HeadElement}|T]) when Element =/= HeadElement ->
	match_private(Element, T);
match_private(Element, []) -> [].

match(Element, Db) -> match_private(Element, Db).