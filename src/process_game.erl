-module(process_game).
-export([create_gamers/1, start_game/1]).

%% Function create processes for game.
create_gamers(Num) ->
	Res = create_gamers_impl(Num),
	build_connections(Res, hd(Res)),
	Res.
%% Function start game. Every process get random number
%% and process with maximum number will win.
start_game(Procs) ->
	hd(Procs) ! {start_game, self()},
	receive
		{winner, WinPid} ->
			io:format("Winner is ~p.~n", [WinPid])
	end.

create_gamers_impl(0) -> [];
create_gamers_impl(Num) ->
	[spawn(fun() -> start_gamer() end)|create_gamers_impl(Num - 1)].

build_connections(Procs, Head) ->
	if 
		length(Procs) >= 2 ->
			[First, Second|_] = Procs,
			First ! Second,
			[_|T] = Procs,
			build_connections(T, Head);
		true -> 
			[Last|_] = Procs,
			Last ! Head
	end.

start_gamer() ->
	receive
		NextInRing -> wait_start_signal(NextInRing)
	end.

wait_start_signal(NextInRing) -> 
	random:seed(
		erlang:unique_integer(),
		erlang:unique_integer(),
		erlang:unique_integer()),
	receive
		{start_game, InitProc} -> 
			NextInRing ! {start_game, InitProc},
			Number = random:uniform(1000),
			io:format("Process ~p get random number ~p.~n", [self(), Number]),
			select_best(Number, NextInRing)
	end.

select_best(MyNumber, NextInRing) ->
	receive
		{start_game, InitProc} ->
			NextInRing ! {MyNumber, self(), InitProc};
		{CurMaxNumber, CurWinner, InitProc} ->
			if 
				CurMaxNumber >= MyNumber -> 
					NextInRing ! {CurMaxNumber, CurWinner, InitProc},
					io:format("~p is less then ~p. Current winner is ~p.~n", 
						[MyNumber, CurMaxNumber, CurWinner]);
				true -> 
					NextInRing ! {MyNumber, self(), InitProc},
					io:format("~p is greater then ~p. New winner is ~p.~n", 
						[MyNumber, CurMaxNumber, self()])
			end
	end,
	wait_winner(NextInRing).

wait_winner(NextInRing) ->
	receive
		{CurMaxNumber, CurWinner, InitProc} ->
			if 
				self() =:= CurWinner -> 
					InitProc ! {winner, self()},
					NextInRing ! {CurMaxNumber, CurWinner, InitProc};
				true -> ok
			end,
			NextInRing ! {CurMaxNumber, CurWinner, InitProc}
	end.