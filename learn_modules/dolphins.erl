-module(dolphins).
-export([dolphin/0]).

dolphin() ->
	receive
		{From, do_a_flip} -> 
			From ! {self(), "How about no?"};
		{From, fish} ->
			From ! {self(), "cool, I like fish"};
		_ ->
			io:format("we're smorter than you, humans~n"),
			dolphin()
	end.