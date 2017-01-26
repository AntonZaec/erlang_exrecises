-module(mouse).
-export([create/1, click/1, dblclick/1, move/1]).
%% Function create observed mouse
%% See example in mouse_test.erl
create(Observer) ->
	{mouse, spawn(fun()-> loop(Observer) end)}.
%% See example in mouse_test.erl
click({mouse, Pid}) ->
	rpc(click, Pid).
%% See example in mouse_test.erl
dblclick({mouse, Pid}) ->
	rpc(dblclick, Pid).
%% See example in mouse_test.erl
move({mouse, Pid}) ->
	rpc(move, Pid).

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

rpc(Action, Pid) ->
	Ref = make_ref(),
	Pid ! {Action, Ref, self()},
	receive
		{Ref, Result} -> Result
	end.