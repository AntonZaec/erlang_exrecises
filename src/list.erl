-module(list).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

filter(List, UpperBound) ->
	[X || X <- List, X =< UpperBound].

reverse_private([], Result) -> Result;
reverse_private([H|T], Result) ->
	reverse_private(T, [H|Result]).

reverse(L) -> reverse_private(L, []).

cat_two([], Result) -> Result;
cat_two([H|T], Result) ->
	NewResult = [H|Result],
	cat_two(T, NewResult).

concatenate_private([], Result) -> Result;
concatenate_private([H|T], Result) ->
	NewResult = cat_two(H, Result),
	concatenate_private(T, NewResult).

concatenate(L) -> reverse(concatenate_private(L, [])).

flatten_private([]) -> [];
flatten_private([H|T]) ->
	concatenate([flatten_private(H), flatten_private(T)]);
flatten_private(H) -> [H]. 

flatten(L) -> flatten_private(L).