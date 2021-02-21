-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-record(cat, {name, color=green, description}).


%%% Client API

start_link() -> spawn_link(fun init/0).


%% Synchronous call
%% because we start `receive` and blocking 
%% till response
order_cat(Pid, Name, Color, Description) ->
	my_server:call(Pid, {order, Name, Color, Description}).


return_cat(Pid, Cat = #cat{}) -> 
	Pid ! {return, Cat},
	ok.

%% Synchronouous call
close_shop(Pid) ->
	my_server:call(Pid, terminate).


init() -> loop([]).


loop(Cats) ->
	receive
		{Pid, Ref, {order, Name, Color, Description}} ->
			if 
				Cats =:= [] ->
					Pid ! {Ref, make_cat(Name, Color, Description)},
					loop(Cats);
				Cats =/= [] -> 
					Pid ! {Ref, hd(Cats)},
					loop(tl(Cats))
			end;
		{return, Cat=#cat{}} -> 
			loop([Cat|Cats]);
		{Pid, Ref, terminate} ->
			io:format("terminating..."),
			Pid ! {Ref, ok},
			terminate(Cats);
		Unknown ->
			io:format("Unknown message: ~p~n", [Unknown]),
			loop(Cats)
	end.

%%% Private functions
terminate(Cats) ->
	[io:format("~p was set free~n", [C#cat.name]) || C <- Cats],
	ok.


make_cat(Name, Color, Description) ->
	#cat{name=Name, color=Color, description=Description}.























