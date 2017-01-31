-module(mouse_observer2).
-behaviour(gen_event).
-export([create/0, fire/2]).
-export([init/1, handle_event/2]).
%% Function create observer for mouse's actions
%% See example in mouse_test.erl
create() ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, ?MODULE, []),
	Pid.
%% Fire event
fire(Observer, Event) ->
	gen_event:notify(Observer, Event),
	ok.
%% Callbacks for gen_event
init(_Args) ->
	{ok, []}.

handle_event('MouseClickEvent', _State) ->
	io:format("Click event was catched.~n"),
	{ok, _State};
handle_event('MouseDoubleClickEvent', _State) ->
	io:format("Double click event was catched.~n"),
	{ok, _State};
handle_event(Event, _State) ->
	io:format("Unknown event ~p.~n", [Event]),
	{ok, _State}.
