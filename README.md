Homeworks which was completed while I was interning in Reksoft.

HOMEWORK I
---

Tasks:

1. Write module bool.erl and define dunctions b_not/1, b_and/2, b or/2 и b_nand/2 on atoms true and false.
2. Write a function, that return list like [ 1, 2,..., N -1, N ]. Example: `[1,2,3] = create(3).`
3. Write a function, that return reversed list like [N, N-1, ..., 1]. Example: `[3,2,1] = reverse_create(3).`
4. Write a function that print numbers in range [1, N].
5. Write a function that print odd numbers in range [1, N].
6. Write a module db.erl for key-value database. Using functions from module lists is not allowed. Interface:
`
db:new() => Db.
db:destroy(Db) => ok.
db:write(Key, Element, Db) => NewDb.
db:delete(Key, Db) => NewDb.
db:read(Key, Db) => {ok, Element} | {error, instance}.
db:match(Element, Db) => [Keyl, ..., KeyN].
`
7. Write a function, that return list's elements which are less or equal then number. Example: `[1,2,3] = filter([1,2,3,4,5], 3).`
8. Write a function that reverse list: `[3,2,1] = reverse([1,2,3]).`
9. Write a function that receive list of lists as argument and return their concatenation. Example: `[1,2,3,4,five] = concatenate([[1,2,3], [], [4,five]]).`
10. Write function flatten. Example: `[1,2,3,4,5,6] = flatten([[1, [2, [3], []]], [[[4]]], [5,6]]).`
11. Write a computer of ariphmetic's expressions. 

  11.1. Write a parser. Parser receive string and return parsed expression. For example, `{minus, {plus, {num, 2}, {num, 3}}, {num, 4}} = parse("((2 + 3)-4)").`
  
  11.2. Write a computer. Computer compute value of parsed expressions.
  
  11.3. Write a printer. Printer print parsed expression to the screen.
  
  11.4. Write a simplyfier. It simplify parsed expressions. For example, `{num, 2} = simplify({plus, {num, 2}, {num, 0}}).`
  
  11.5. Write a compiler. Compiler transform parsed expression to the actions of stack-based machine.
  
  11.6. Write a simulator. Simulator execute result of compiling.

  11.7. Append if expessions.

Result:

|Task|File|
|---|---|
|1|[modules/bool.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/bool.erl)|
|2,3|[modules/create_list.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/create_list.erl)|
|4,5|[modules/print_num.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/print_num.erl)|
|6|[modules/db.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/db.erl)|
|7,8,9,10|[modules/list.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/list.erl)|
|11|[modules/calc.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/calc.erl)|

HOMEWORK II
---
Tasks:

1. Change code of database from previous homework. Database must work in separated process. The function *write* must return tuple `{error, Reason}` if process died.
2. Create ring of processess. Some sent message must move through ring M times.
3. Create graph of processes and implement finding of connected component on this graph.
4. Create a lottery between processes. The processes must select one "best of the best" process.

Result:

|Task|File|
|---|---|
|1|[modules/db.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/db.erl)|
|2|[modules/process_ring.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/process_ring.erl)|
|3|[modules/process_graph.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/process_graph.erl)|
|4|[modules/process_game.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/process_game.erl)|

HOMEWORK III
---

Task:

1. Create your own supervisor.

Result:

1. [modules/mysupervisor.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/mysupervisor.erl)

LAZY LISTS
---

Task:

1. Create any lazy list.

Result:

1. [modules/lazy_fib.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/lazy_fib.erl)


HOMEWORK IV (monday 16.01.2017)
---

Tasks:

1. Implement pattern observer with interface:
`add_handler(Observer, Event, Callback). fire(Observer, Event).`
2. Test it on some example.
3. Do same things with gen_event.

Result:

|Task|File|
|---|---|
|1|[modules/observer.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/observer.erl)|
|2|[modules/mouse_observer.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/mouse_observer.erl),[modules/mouse.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/mouse.erl),[modules/mouse_test.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/mouse_test.erl)|
|3|[modules/mouse_observer2.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/mouse_observer2.erl),[modules/mouse_test.erl](https://github.com/AntonZaec/erlang_exrecises/blob/master/modules/mouse_test.erl)|


HOMEWORK V
---

Task:

1. Rewrite database module with gen_server.
2. Implement supervisor for database using behaviour supervisor.
3. Create application using behaviour application.
 
Result:

1. [apps/db](https://github.com/AntonZaec/erlang_exrecises/tree/master/apps/db).

HOMEWORK VI
---

Task:

1. Rewrite database module with ets tables instead of lists.

Result:

1. [apps/db](https://github.com/AntonZaec/erlang_exrecises/tree/master/apps/db/src/db_server.erl).

FINAL PROJECT
---

Task:

1. Create REST API for database.

Result:

1. [apps/http_db](https://github.com/AntonZaec/erlang_exrecises/tree/master/apps/http_db)
