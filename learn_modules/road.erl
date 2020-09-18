
-module(road).
-compile(export_all).

main([FileName]) ->
	{ok, Bin} = file:read_file(FileName),
	Map = parse_map(Bin),
	io:format("~p~n", [optimal_path(Map)]),
	erlang:halt(0).



parse_map(Bin) when is_binary(Bin) ->
	parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
	Vals = [list_to_integer(X) || X <- string:tokens(Str, "\n\r\t ")],
	group_vals(Vals, []).


group_vals([], Acc) -> 
	lists:reverse(Acc);
group_vals([A,B,C|Rest], Acc) -> 
	group_vals(Rest, [{A,B,C}|Acc]).


shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->

	RouteA1 = {A + DistA, [{a, A}|PathA]},
	RouteA2 = {B + X + DistB, [{x, X}, {b, B}|PathB]},

	RouteB1 = {B + DistB, [{b, B}| PathB]},
	RouteB2 = {A + X + DistA, [{x, X}, {a, A}|PathA]},
	{erlang:min(RouteA1, RouteA2), erlang:min(RouteB1, RouteB2)}.

optimal_path(Map) -> 
	%% { {<int>, [{x, X1}, {a, A1}]}, {<int>, [{x, X1}, {a, A1}]}

	% finds to options for A,B
	{A,B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map),
	{_Dist, Path} = if 
		hd(element(2, A)) =/= {x,0} -> A;
		hd(element(2, B)) =/= {x,0} -> B
	end,
	lists:reverse(Path).
