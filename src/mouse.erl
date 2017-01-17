-module(mouse).
-export([create/1, click/1, dblclick/1, move/1]).

create(Observer) ->
	{mouse, spawn(fun()-> loop(Observer) end)}.

loop(Observer) ->
	receive
		{click, Ref, Pid} -> 
			Pid ! {Ref, observer:fire(Observer, 'MouseClickEvent')};
		{dblclick, Ref, Pid} -> 
			Pid ! {Ref, observer:fire(Observer, 'MouseDoubleClickEvent')};
		{move, Ref, Pid} -> 
			Pid ! {Ref, observer:fire(Observer, 'MouseMoveEvent')}
	end,
	loop(Observer).

click({mouse, Pid}) ->
	rpc(click, Pid).

dblclick({mouse, Pid}) ->
	rpc(dblclick, Pid).

move({mouse, Pid}) ->
	rpc(move, Pid).

rpc(Action, Pid) ->
	Ref = make_ref(),
	Pid ! {Action, Ref, self()},
	receive
		{Ref, Result} -> Result
	end.