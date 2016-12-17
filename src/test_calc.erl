-module(test_calc).
-include_lib("eunit/include/eunit.hrl").
-import(calc, [eval/1]).

eval_test() ->
	?assert(eval("1") =:= 1),
	?assert(eval("(1+2)") =:= 3),
	?assert(eval("((1+2)*2)") =:= 6),
	?assert(eval("~((1+2)*2)") =:= -6),
	?assert(eval("((1+3)*(~(4)))+10)") =:= -6).
