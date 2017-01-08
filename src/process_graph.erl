-module(process_graph).
-export([create/2, search_components/1]).
-include_lib("eunit/include/eunit.hrl").


create(VertexNum, ConnectionProbability) ->
	Procs = create_processes(VertexNum, []),
	create_edges(Procs, Procs, ConnectionProbability),
	Procs.

search_components(Vertexes) ->
	search_components_impl(Vertexes, []).

search_components_impl([], Result) ->
	Result;
search_components_impl([H|T], Result) -> 
	H ! {search_req, self()},
	receive
		{search_result, ConnComponent} -> 
			search_components_impl(
				lists:subtract(T, ConnComponent), [ConnComponent|Result])
	end.

create_processes(0, _) ->
	[];
create_processes(VertexNum, Procs) ->
	NewPid = spawn(fun() -> create_vertex([]) end),
	[NewPid|create_processes(VertexNum - 1, Procs)].

create_edges([], AllProcs, _) ->
	finalyze_creating(AllProcs);
create_edges(Procs, AllProcs, ConnectionProbability) ->
	[CurProc|OtherProcs] = Procs,
	Connections = [X || X <- AllProcs, 
		random:uniform() =< ConnectionProbability, X =/= CurProc],
	add_connections(CurProc, Connections),
	create_edges(OtherProcs, AllProcs, ConnectionProbability).

finalyze_creating([]) ->
	ok;
finalyze_creating([H|T]) ->
	H ! creating_finished,
	finalyze_creating(T).

add_connections(_, []) ->
	ok;
add_connections(Vert1, ListOfVertexes) ->
	[Vert2|T] = ListOfVertexes,
	Vert1 ! {add_connection, Vert2},
	Vert2 ! {add_connection, Vert1},
	add_connections(Vert1, T).

create_vertex(ConnVertexes) ->
	receive
		{add_connection, NewConnVertex} -> 
			create_vertex([NewConnVertex|lists:delete(NewConnVertex, ConnVertexes)]);
		creating_finished -> start_white(ConnVertexes)
	end.

start_white(ConnVertxes) ->
	receive
		{search_req, From} -> 
			ConnVertxesWithoutInit = lists:delete(From, ConnVertxes),
			send_search_request(ConnVertxesWithoutInit),
			start_grey(From, ConnVertxesWithoutInit, [])
	end.

send_search_request([]) ->
	ok;
send_search_request([H|T]) -> 
	H ! {search_req, self()},
	send_search_request(T).

start_grey(InitProc, [], Result) ->
	InitProc ! {search_result, [self()|Result]},
	start_black(InitProc);
start_grey(InitProc, ConnWhiteVertexes, Result) ->
	receive
		{search_req, From} -> 
			From ! {search_result, {self(), is_not_white}},
			start_grey(InitProc, ConnWhiteVertexes, Result);
		{search_result, {From, is_not_white}} ->
			start_grey(InitProc, lists:delete(From, ConnWhiteVertexes), Result);
		{search_result, ConnGreyVertexes} -> 
			NewResult = lists:umerge(Result, ConnGreyVertexes),
			NewConnWhiteVertexes = lists:subtract(ConnWhiteVertexes, NewResult),
			start_grey(InitProc, NewConnWhiteVertexes, NewResult)
	end.

start_black(InitProc) ->
	receive
		{search_req, From} -> 
			From ! {search_result, {self(), is_not_white}},
			start_black(InitProc)
	end.

one_vertex_test() ->
	Vertexes = create_processes(1, []),
	finalyze_creating(Vertexes),
	Components = search_components(Vertexes),
	?assert(Components =:= [Vertexes]).

unlinked_graph_test() ->
	G = create(100, 0),
	Components = search_components(G),
	?assert(lists:sort(lists:append(Components)) =:= lists:sort(G)).

linked_graph_test() ->
	G = create(100, 1),
	[H|T] = search_components(G),
	?assert(length(T) =:= 0),
	?assert(lists:sort(H) =:= lists:sort(G)).
	
base_test() ->
	Procs = create_processes(10, []),
	[V0, V1, V2, V3, V4, V5, V6, V7, V8, V9] = Procs,
	add_connections(V0, [V1, V2]),
	add_connections(V4, [V5, V6, V7]),
	add_connections(V8, [V5]),
	add_connections(V9, [V4, V8]),
	finalyze_creating(Procs),
	Components = search_components(Procs),
	ComponentsSortedByLength = lists:map(
		fun(A) -> lists:sort(A) end,
		lists:sort(
			fun(A, B) -> length(A) < length(B) end, Components)),
	?assert(ComponentsSortedByLength =:= [[V3], [V0, V1, V2], [V4, V5, V6, V7, V8, V9]]).