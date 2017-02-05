http_db
=====

Distributed key-value storage with REST API

Build
-----

    $ rebar3 compile
    
Usage
-----
Change names of hosts in `src/http_db.app.src` if needed (field `cluster`).

Start http_db with commands:

```
rebar3 shell
net_kernel:start(['host3@127.0.0.1', longnames]). 
application:start(http_db).
application:start(http_db).
```

Now you can open `http://127.0.0.1:8080/http_db` and you will see hello page.

Now you must create several nodes for data storage (from current directory):

```
cd _build/default/lib/db/src
erl -name host1@127.0.0.1
application:start(kvdb).
```

You can repeate last step for host2 or other hosts from environment variable `cluster`.

Interact with storage via REST API:

- Create database: `curl -X PUT http://127.0.0.1:8080/http_db/db1`
- Read all data from database: `curl -X GET http://127.0.0.1:8080/http_db/db1`
- Drop database: `curl -X DELETE http://127.0.0.1:8080/http_db/db1`
- Write key-value pair: `curl -H 'Content-Type: application/json' -X PUT -d '{"value":"value1"}' http://127.0.0.1:8080/http_db/db1/key1`. Json object must conatin field `value`
- Read value for key: `curl -X GET http://127.0.0.1:8080/http_db/db1/key1`
- Delete record: `curl -X DELETE http://127.0.0.1:8080/http_db/db1/key1`
