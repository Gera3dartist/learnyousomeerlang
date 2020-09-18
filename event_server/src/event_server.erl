-module(event_server).
-compile(export_all).

-record(state, {clients, events}).
-record(event, {name="", description="", pid, timeout={{1970, 1, 1}, {0, 0, 0}}}).


valid_datetime({Date, Time}) ->
	try 
		calendar:valid_date(Date) andalso valid_time(Time)
	catch
		error:function_clause ->
			false
	end.


valid_time({H, M, S}) -> valid_time(H, M, S).


valid_time(H, M, S) when H >= 0, H < 24, 
						 M >= 0, M < 60, 
						 S >= 0, S < 60 ->
	true;
valid_time(_,_,_) ->
	false.


subscribe(Pid) ->
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), Ref, {subscribe, Pid}},
	receive
		{Ref, ok} ->
			ok;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

add_event(Name, Description, TimeOut) ->
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
	receive
		{Ref, Msg} ->
			Msg
	after 5000 ->
		{error, timeout}
	end.

add_event2(Name, Description, TimeOut) ->
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
	receive
		{Ref, {error, Reason}} ->
			erlang:error(Reason);
		{Ref, Msg} ->
			Msg
	after 5000 ->
		{error, timeout}
	end.


cancel(Name) -> 
	Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), Ref, {cancel, Name}},
	receive
		{Ref, ok} ->
			ok
	after 5000 ->
		{error, timeout}
	end.


listen(Delay) ->
	receive
		M = {done, _Name, _Description} ->
		[M|listen(0)]
	after Delay * 1000 ->
		[]
	end.


loop(S=#state{}) ->
	receive
		{Pid, MsgRef, {subscribe, Client}} ->
			Ref = erlang:monitor(process, Client),
			NewClients = orddict:store(Ref, Client, S#state.clients),
			Pid ! {MsgRef, ok},
			loop(S#state{clients=NewClients});

		{Pid, MsgRef, {add, EventName, Description, TimeOut}} ->
			case valid_datetime(TimeOut) of 
				true ->
					%% start event
					EventPid = event:start_link(EventName, TimeOut),
					%% add event to state
					NewEvents = orddict:store(
						EventName, 
						#event{
							name=EventName, 
							description=Description,
							pid=EventPid,
							timeout=TimeOut},
						S#state.events),

					%% say ok to calling process
					Pid ! {MsgRef, ok},
					%% spin loop with new events
					loop(S#state{events=NewEvents});
				false ->
					%% return error
					Pid ! {MsgRef, error, bad_timeout},
					loop(S)
			end;

		{_Pid, _Ref, {cancel, EventName}} ->
			%% find event in a events store
			NewEvents = case orddict:find(EventName, S#state.events) of
				{ok, Event} ->
					%% if exists cancel and remove
					event:cancel(Event#event.pid),
					orddict:erase(EventName, S#state.events);
				error->
					%% otherwise return old state
					S#state.events
				end,
			%% spin loop with new events
			loop(S#state{events=NewEvents});

		{done, EventName} ->
			case orddict:find(EventName, S#state.events) of
				{ok, E} ->
					send_to_clients(
						{done, E#event.name, E#event.description}, S#state.clients),
					NewEvents = orddict:erase(EventName, S#state.events),
					loop(S#state{events=NewEvents});
				error ->
					loop(S)
			end;
		{'DOWN', Ref, process, _Pid, _Reason} -> 
			loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
		shutdown -> 
			exit(shutdown);
		code_change ->
			?MODULE:loop(S);
		Unknown -> 
			io:format("Unknown message: ~p~n", [Unknown]),
			loop(S)
	end.


send_to_clients(Msg, Clients) ->
	orddict:map(fun(_Ref, ClientPid) ->  ClientPid ! Msg end, Clients).


init() -> 
	loop(#state{clients=orddict:new(), events=orddict:new()}).

start() ->
	register(?MODULE, Pid=spawn(?MODULE, init, [])),
	Pid.

start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	Pid.

terminate() ->
	?MODULE ! shutdown.


































