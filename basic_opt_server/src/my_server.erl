-module(my_server).
-export([call/2, cast/2, reply/2, start/2, start_link/2]).


% file on site: https://learnyousomeerlang.com/static/erlang/my_server.erl

%%% Public API
start(Module, InitialState) -> 
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).


%%% generic part
call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {sync, self(), Ref, Msg},
	io:format("called server ~p~n", [Pid]),
	receive
		{Ref, Reply} ->
			erlang:demonitor(Ref, [flush]),
			Reply;
		{'DOWN', Ref, process, Reason} -> 
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%%% Private stuff
init(Module, InitialState) -> 
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} -> 
            loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} -> 
            loop(Module, Module:handle_call(Msg, {Pid, Ref}, State));
        _ ->
            io:format("Unexpected message~n")
    end.


