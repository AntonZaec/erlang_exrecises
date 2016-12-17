-module(test_db).
-include_lib("eunit/include/eunit.hrl").
-import(db, [new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new_test() ->
	?assert(new() =:= []).

write_one_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	?assert(Db2 =:= [{"A", "record1"}]).
write_two_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	?assert(Db2 =:= [{"A", "record1"}]),
	Db3 = write("B", "record2", Db2),
	?assert(Db3 =:= [{"A", "record1"}, {"B", "record2"}]).
write_existing_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	Db3 = write("B", "record2", Db2),
	Db4 = write("A", "record3", Db3),
	?assert(Db4 =:= [{"A", "record3"}, {"B", "record2"}]).

deleting_one_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	?assert(delete("A", Db2) =:= []).

deleting_first_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	Db3 = write("B", "record2", Db2),
	?assert(delete("A", Db3) =:= [{"B", "record2"}]).

deleting_last_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	Db3 = write("B", "record2", Db2),
	?assert(delete("B", Db3) =:= [{"A", "record1"}]).

deleting_middle_test() -> 
	Db = new(),
	Db2 = write("A", "record1", Db),
	Db3 = write("B", "record2", Db2),
	Db4 = write("C", "record3", Db3),
	?assert(delete("B", Db4) =:= [{"A", "record1"}, {"C", "record3"}]).

reading_existing_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	?assert(read("A", Db2) =:= {ok, "record1"}).

reading_not_existing_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	?assert(read("B", Db2) =:= {error, instance}).

empty_matching_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	?assert(match("record2", Db2) =:= []).

matching_test() ->
	Db = new(),
	Db2 = write("A", "record1", Db),
	Db3 = write("B", "record2", Db2),
	Db4 = write("C", "record1", Db3),
	?assert(match("record1", Db4) =:= ["A", "C"]).