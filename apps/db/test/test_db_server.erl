-module(test_db_server).
-include_lib("eunit/include/eunit.hrl").
-import(db_server, [new/1, destroy/1, write/3, delete/2, read/2, match/2]).

write_one_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	?assert(write("A", "record1", Db) =:= ok),
	?assert(read("A", Db) =:= {ok, "record1"}).
write_two_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	?assert(write("A", "record1", Db) =:= ok),
	?assert(read("A", Db) =:= {ok, "record1"}),
	?assert(write("B", "record2", Db) =:= ok),
	?assert(read("A", Db) =:= {ok, "record1"}),
	?assert(read("B", Db) =:= {ok, "record2"}).
write_existing_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	?assert(write("A", "record1", Db) =:= ok),
	?assert(write("B", "record2", Db) =:= ok),
	?assert(write("A", "record3", Db) =:= ok),
	?assert(read("A", Db) =:= {ok, "record3"}),
	?assert(read("B", Db) =:= {ok, "record2"}).
deleting_one_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	write("A", "record1", Db),
	?assert(delete("A", Db) =:= ok),
	?assert(read("A", Db) =:= {error, instance}).
deleting_first_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	write("A", "record1", Db),
	write("B", "record2", Db),
	?assert(delete("A", Db) =:= ok),
	?assert(read("A", Db) =:= {error, instance}),
	?assert(read("B", Db) =:= {ok, "record2"}).
deleting_last_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	write("A", "record1", Db),
	write("B", "record2", Db),
	?assert(delete("B", Db) =:= ok),
	?assert(read("B", Db) =:= {error, instance}),
	?assert(read("A", Db) =:= {ok, "record1"}).
deleting_middle_test() -> 
	Db = new(list_to_atom(integer_to_list(?LINE))),
	write("A", "record1", Db),
	write("B", "record2", Db),
	write("C", "record3", Db),
	?assert(delete("B", Db) =:= ok),
	?assert(read("B", Db) =:= {error, instance}),
	?assert(read("A", Db) =:= {ok, "record1"}),
	?assert(read("C", Db) =:= {ok, "record3"}).
empty_matching_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	write("A", "record1", Db),
	?assert(match("record2", Db) =:= []).
matching_test() ->
	Db = new(list_to_atom(integer_to_list(?LINE))),
	write("A", "record1", Db),
	write("B", "record2", Db),
	write("C", "record1", Db),
	?assert(lists:sort(match("record1", Db)) =:= lists:sort(["A", "C"])).