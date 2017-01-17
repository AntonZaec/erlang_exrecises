-module(mouse_observer).
-export([create/0, click/1, dblclick/1]).

create() ->
	Observer = observer:create(mouse_observer),
	observer:add_handler(Observer, 'MouseClickEvent', click),
	observer:add_handler(Observer, 'MouseDoubleClickEvent', dblclick),
	Observer.

click(Event) ->
	io:format("Click event: ~p.~n", [Event]).

dblclick(Event) ->
	io:format("Double click event: ~p.~n", [Event]).


