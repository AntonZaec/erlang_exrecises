-module(mouse_observer).
-export([create/0, click/1, dblclick/1, fire/2]).
%% Function create observer for mouse's actions.
%% See example in mouse_test.erl
create() ->
	Observer = observer:create(mouse_observer),
	observer:add_handler(Observer, 'MouseClickEvent', click),
	observer:add_handler(Observer, 'MouseDoubleClickEvent', dblclick),
	Observer.
%% Callback for click event.
click(Event) ->
	io:format("Click event ~p was catched.~n", [Event]).
%% Callback for double click event.
dblclick(Event) ->
	io:format("Double click event ~p was catched.~n", [Event]).

fire(Observer, Event) ->
	observer:fire(Observer, Event).