-module(lazy_fib).
-export([fibonacci/0, fibonacci/2]).

%% Function create generator for fibonacci numbers
fibonacci() ->
	fibonacci(1, 0, 0).
%% Function create list of fibinacci numbers via generator
%% For, example: fibonacci(fibonacci(), 10) == [1,1,2,3,5,8,13,21,34,55]
fibonacci(_, 0) ->
	[];
fibonacci(Gen, N) ->
	[Val|NextGen] = Gen(),
	[Val|fibonacci(NextGen, N-1)].

fibonacci(1, _, _) ->
	fun() -> [1|fibonacci(2, 0, 0)] end;
fibonacci(2, _, _) ->
	fun() -> [1|fibonacci(3, 1, 1)] end;
fibonacci(N, Pre, PrePre) ->
	NewVal = Pre + PrePre,
	fun() -> [NewVal|fibonacci(N+1, NewVal, Pre)] end.