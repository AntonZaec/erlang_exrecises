-module(mouse_test).
-export([test/0]).
%% Function test observer using mouse example
test() ->
	Mouse = mouse:create(mouse_observer:create()),
	mouse:click(Mouse),
	mouse:dblclick(Mouse),
	mouse:move(Mouse).