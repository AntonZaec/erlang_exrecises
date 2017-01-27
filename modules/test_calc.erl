-module(test_calc).
-include_lib("eunit/include/eunit.hrl").
-import(calc, [eval/1]).

eval_test() ->
	?assert(eval("1") =:= 1),
	?assert(eval("(1+2)") =:= 3),
	?assert(eval("((1+2)*2)") =:= 6),
	?assert(eval("~((1+2)*2)") =:= -6),
	?assert(eval("((1+3)*(~(4)))+10)") =:= -6).
eval_if_test() ->
	?assert(eval("if (1+1) then (1+2) else (4+3)") =:= 3),
	?assert(eval("if (1-1) then (1+2) else (4+3)") =:= 7),
	?assert(eval("(2 + (if (4/4) then (1+2) else (4+3)))") =:= 5),
	?assert(eval("(2 + (if (4*0) then (1+2) else (4+3)))") =:= 9).