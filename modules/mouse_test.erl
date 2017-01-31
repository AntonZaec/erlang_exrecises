-module(mouse_test).
-export([test/0, test_ge/0]).
%% Function test observer using mouse example
test() ->
	test_impl(mouse:create(mouse_observer)).
%% Function test gen_event-observer 
test_ge()->
	test_impl(mouse:create(mouse_observer2)).
test_impl(Mouse) ->
	mouse:click(Mouse),
	mouse:dblclick(Mouse),
	mouse:move(Mouse).