-module(kitchen).
-export([fridge_2/1, start/1, store/2, take/2, store2/2, take2/2]).

fridge_1() ->
	receive 
		{From, {store, _Food}} ->
			From ! {self(), ok},
			fridge_1();
		{From, {take, _Food}} -> 
			From ! {self(), not_found},
			fridge_1();
		terminate ->
			ok
	end.

fridge_2(FoodList) -> 
	receive
		{From, {store, _Food}} -> 
			From ! {self(), ok},
			fridge_2([_Food|FoodList]);
		{From, {take, _Food}} ->
			case lists:member(_Food, FoodList) of
				true -> 
					From ! {self(), {ok, _Food}},
					fridge_2(lists:delete(_Food, FoodList));
				false -> 
					From ! {self(), not_found},
					fridge_2(FoodList)
			end;
		terminate ->
			ok
	end.

start(FoodList) ->
	spawn(?MODULE, fridge_2, [FoodList]).

store(Pid, Food) -> 
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

take(Pid, Food) -> 
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

store2(Pid, Food) -> 
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.

take2(Pid, Food) -> 
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.








