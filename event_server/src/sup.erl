-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod, Arg) ->
	spawn(?MODULE, init, [{Mod, Arg}]).


start_link(Mod, Arg) ->
	spawn_link(?MODULE, init, [{Mod, Arg}]).


init({Mod, Arg}) ->
	process_flag(trap_exit, true),
	loop({Mod, start_link, Arg}).


loop({Module, Func, Arg}) ->
	Pid = apply(Module, Func, Arg),
	receive
		{'EXIT', _From, shutdown} -> exit(shutdown);
		{'EXIT', Pid, Reason} -> 
			io:format("Process: ~p exited for reason: ~p", [Pid, Reason]),
			loop({Module, Func, Arg})
	end.







