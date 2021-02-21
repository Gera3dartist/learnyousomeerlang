-module(my_server).
-export(export_all).


call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, Msg},
	receive
		{Ref, ok} ->
			erlang:demonitor(Pid),
			ok;
		{'DOWN', Ref, process, Reason} -> 
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.
