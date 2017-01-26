-module(list).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

%% Function return list of values from List,
%% which are smaller or equal then UpperBound.
filter(List, UpperBound) ->
	[X || X <- List, X =< UpperBound].

%% Function reverse list L
reverse(L) -> reverse_private(L, []).

%%Function concatenate all lists from list L.
%%List L looks like [[1], [2], [3], [4], [abcd], ["123"], ...].
concatenate(L) -> reverse(concatenate_private(L, [])).

%%Function move all elements from inner list to one list.
%%For example, [[[1]], [2], [3,[4,[5]]]] will be [1,2,3,4,5].
flatten(L) -> flatten_private(L).

reverse_private([], Result) -> Result;
reverse_private([H|T], Result) ->
	reverse_private(T, [H|Result]).

cat_two([], Result) -> Result;
cat_two([H|T], Result) ->
	NewResult = [H|Result],
	cat_two(T, NewResult).

concatenate_private([], Result) -> Result;
concatenate_private([H|T], Result) ->
	NewResult = cat_two(H, Result),
	concatenate_private(T, NewResult).

flatten_private([]) -> [];
flatten_private([H|T]) ->
	concatenate([flatten_private(H), flatten_private(T)]);
flatten_private(H) -> [H]. 