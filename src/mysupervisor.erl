-module(mysupervisor).
-export([create/0, run/2]).

%% Function create supervisor
create() -> 
	spawn(fun()-> init() end).
%% Supervisor create process for F and restart it if error occures.
run(SV, F) ->
	SV ! {supervised_proc, F},
	ok.

init() ->
	erlang:process_flag(trap_exit, true),
	sv([]).

sv(SupervisedProcs) -> 
	receive
		{supervised_proc, Fun} -> 
			sv([{spawn_link(Fun), Fun}|SupervisedProcs]);
		{'EXIT', Pid, normal} ->
			io:format("Process ~p finished.~n", [Pid]),
			sv(lists:keydelete(Pid, 1, SupervisedProcs));
		{'EXIT', Pid, Reason} -> 
			{_, ProcFun} = lists:keyfind(Pid, 1, SupervisedProcs),
			io:format("Process ~p go down because ~p. Restarting...~n", [Pid, Reason]),
			sv([{spawn_link(ProcFun), ProcFun}|lists:keydelete(Pid, 1, SupervisedProcs)])
	end.