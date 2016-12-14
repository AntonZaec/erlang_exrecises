-module(create_list).
-export([create/1, reverse_create/1]).

reverse_create(0) -> [];
reverse_create(N) -> [N|reverse_create(N-1)].

create_private(Bound, Bound) -> [Bound];
create_private(I, Bound) ->
	[I|create_private(I+1, Bound)].

create(N) -> create_private(1, N).
	

