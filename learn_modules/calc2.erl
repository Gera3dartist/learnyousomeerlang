-module(calc2).
-export([calculate/1]).


calculate(L) when is_list(L) ->
	[Res] = lists:foldl(fun calculate1/2, [], string:tokens(L, " ")),	
	Res.

read(X) -> 
	case string:to_float(X) of 
		{error,no_float} -> list_to_integer(X);
		{F,_} -> F
	end.


calculate1("+", [N1,N2|S]) -> [N2+N1|S];	
calculate1(X, Stack) -> [read(X)|Stack].
