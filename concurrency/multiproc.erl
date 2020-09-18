-module(multiproc).
-export([sleep/1, flush/0, priority/0, normal/0]).


sleep(Timeout) ->
	receive
	after Timeout -> ok
	end.

flush() -> 
	receive
		_ -> flush()
	after 0 -> ok
	end.

priority() -> 
	receive {Priority, Message} when Priority > 10 ->
		[Message|priority()]
	after 0 -> 
		normal()
	end.

normal() ->
	receive {_, Message} -> [Message|normal()]
	after 0 -> []
	end.

listen() ->
	receive _ -> _
	after 0 -> ok
	end.