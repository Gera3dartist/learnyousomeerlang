-module(my_behaviour).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{init, 1}, {fun1, 0}, {fun2, 3}];
behaviour_info(_) -> undefined.