-module(linkmon).
-export([myproc/0, chain/1, start_critic/0, judge2/2, judge/3, critic1/0, critic2/0,restarter/0, start_critic2/0]).


myproc() -> 
	timer:sleep(5000),
	exit(reason).


chain(0) ->
	receive 
		_ -> ok
	after 
		2000 -> exit("chain dies here")
	end;
chain(N) -> 
	Pid = spawn(fun() -> chain(N-1) end),
	link(Pid),
	receive
		_ -> ok
	end.

judge(Pid, Album, Band) -> 
	Pid ! {self(), {Album, Band}},
	receive {Pid, Ciricism} -> 
		Ciricism
	after 2000 ->
		timeout
	end.


start_critic() -> 
	spawn(?MODULE, critic1, []).


critic1() -> 
	receive 
		{From, {'Manowar', 'Wariors of the world'}} -> From ! {self(), 'that is awesome'};
		{From, {'Dio', _}} -> From ! {self(), 'Dio is a legent'};
		{From, {'Metallica', 'Master of Puppets'}} -> From ! {self(), 'pretty good'};
		{From, {_Singer, _Album}} -> From ! {self(), 'not sure, they are worth attention'}
	end,
	critic1().



judge2(Album, Band) -> 
	Ref = make_ref(),
	critic2 ! {self(), Ref, {Album, Band}},
	receive {Ref, Ciricism} -> 
		Ciricism
	after 2000 ->
		timeout
	end.


critic2() -> 
	receive 
		{From, Ref, {'Manowar', 'Wariors of the world'}} -> From ! {Ref, 'that is awesome'};
		{From, Ref, {'Dio', _}} 						 -> From ! {Ref, 'Dio is a legent'};
		{From, Ref, {'Metallica', 'Master of Puppets'}}  -> From ! {Ref, 'pretty good'};
		{From, Ref, {_Singer, _Album}} 					 -> From ! {Ref, 'not sure, they are worth attention'}
	end,
	critic2().



restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, critic2, []),
	register(critic2, Pid),
	receive
		{'EXIT', Pid, normal} -> % finished nomally
			ok; 
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
			ok;
		{'EXIT', Pid, _} -> % should restart
			restarter()
	end.



start_critic2() -> 
	spawn(?MODULE, restarter, []).




