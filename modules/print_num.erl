-module(print_num).
-export([print_all/1, print_odd/1]).

print_range(I, Bound) when I =< Bound ->
	io:format('~B~n', [I]),
	print_range(I + 1, Bound);
print_range(_, _) -> ok.

print_all(N) -> 
	print_range(1, N).

print_odd_range(I, Bound) when I =< Bound -> 
	case (I rem 2) =/= 0 of
		true -> io:format('~B~n', [I]);
		_ -> true
	end,
	print_odd_range(I + 1, Bound);
print_odd_range(_, _) -> ok.
print_odd(N) ->
	print_odd_range(1, N).
