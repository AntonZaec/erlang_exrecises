-module(observer).
-export([create/1, fire/2, add_handler/3]).

%% Function create observer with implementation in module Module.
%% See example in mouse_observer.erl
create(Module) ->
	{observer, spawn(fun() -> loop(Module, maps:new()) end)}.
%% Function register Callback for processing Event.
%% See example in mouse_observer.erl
add_handler({observer, ObsPid}, Event, Callback) ->
	ObsPid ! {add_handler_act, Event, Callback},
	ok.
%% Function trigger Event.
%% See example in mouse.erl
fire({observer, ObsPid}, Event) ->
	ObsPid ! {fire_act, Event},
	ok.

loop(Module, EventsListeners) ->
	receive
		{fire_act, Event} -> 
			try maps:get(Event, EventsListeners) of
				Callbacks -> 
					call(Module, Event, Callbacks)
			catch
				error:{badkey, _} -> error
			end,
			loop(Module, EventsListeners);
		{add_handler_act, Event, Callback} ->
			NewListeners = case maps:find(Event, EventsListeners) of
				{ok, Listeners} -> EventsListeners#{Event := [Callback|Listeners]};
				error -> EventsListeners#{Event => [Callback]}
			end, 
			loop(Module, NewListeners)
	end.

call(_Module, _Event, []) -> ok;
call(Module, Event, [Clb|T]) ->
	Module:Clb(Event),
	call(Module, Event, T).