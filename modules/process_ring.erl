-module(process_ring).
-export([start/1]).

%% Start process ring.
%% Return Pid for sending messages.
%% For example:
%% RingBeginPid = start(10) %Create ring of 10 processes
%% RingBeginPid ! {"message", 5} %Five passing of message through the ring
start(ProcNum) ->
	MainProc = self(),
	spawn(fun() -> create_ring(ProcNum, [], MainProc) end),
	receive
		RingBegin -> RingBegin
	end.

create_ring(1, Procs, MainProc) ->
	MainProc ! self(),
	[NextNode|_] = lists:reverse(Procs),
	receive_new_message(NextNode);
create_ring(ProcNum, Procs, MainProc) ->
	Pid = self(),
	NextNode = spawn(fun() -> create_ring(ProcNum - 1, [Pid|Procs], MainProc) end),
	receive_send(NextNode).

receive_send(NextNode) ->
	receive
		{Msg, Counter} -> 
			io:format("~p send message ~p to ~p~n", 
				[self(), Msg, NextNode]),
			NextNode ! {Msg, Counter},
			receive_send(NextNode)
	end.

receive_send_dec(NextNode) ->
	receive
		{Msg, 1} -> 
			io:format("~p receive message ~p~n", 
				[self(), Msg]),
			io:format("Pass another message.~n");
		{Msg, Counter} -> 
			io:format("~p send message ~p to ~p~n", 
				[self(), Msg, NextNode]),
			io:format("New Ring!~n"),
			NextNode ! {Msg, Counter - 1},
			receive_send_dec(NextNode)
	end.

receive_new_message(NextNode) ->
	receive 
		{Message, Counter} -> 
			NextNode ! {Message, Counter},
			receive_send_dec(NextNode);
		_ -> io:format("Wrong message format. Tuple like {Message, RepeatNumber} expected.")
	end,
	receive_new_message(NextNode).