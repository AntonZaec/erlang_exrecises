-module(mouse).
-export([create/1, click/1, dblclick/1, move/1]).
%% Function create observed mouse
%% See example in mouse_test.erl
create(Module) ->
	{mouse, spawn(fun()-> loop(Module, Module:create()) end)}.
%% See example in mouse_test.erl
click({mouse, Pid}) ->
	rpc(click, Pid).
%% See example in mouse_test.erl
dblclick({mouse, Pid}) ->
	rpc(dblclick, Pid).
%% See example in mouse_test.erl
move({mouse, Pid}) ->
	rpc(move, Pid).

loop(Module, Observer) ->
	receive
		{click, Ref, Pid} -> 
			Pid ! {Ref, Module:fire(Observer, 'MouseClickEvent')};
		{dblclick, Ref, Pid} -> 
			Pid ! {Ref, Module:fire(Observer, 'MouseDoubleClickEvent')};
		{move, Ref, Pid} -> 
			Pid ! {Ref, Module:fire(Observer, 'MouseMoveEvent')}
	end,
	loop(Module, Observer).

rpc(Action, Pid) ->
	Ref = make_ref(),
	Pid ! {Action, Ref, self()},
	receive
		{Ref, Result} -> Result
	end.