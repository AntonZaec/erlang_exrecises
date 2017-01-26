-module(test_list).
-include_lib("eunit/include/eunit.hrl").
-import(list, [filter/2, reverse/1, concatenate/1, flatten/1]).

filter_test() ->
	?assert(list:filter([-2,-1,0,1,2,3,4], 1) =:= [-2,-1,0,1]).

reverse_test() ->
	?assert(list:reverse([1,2,3]) =:= [3,2,1]).

concatenate_test() ->
	?assert(list:concatenate([[1], [2], [3], [[4]]]) =:= [1,2,3,[4]]).

flatten_test() ->
	?assert(list:flatten([[[1]], [2], [3,[4,[5]]]]) =:= [1,2,3,4,5]).