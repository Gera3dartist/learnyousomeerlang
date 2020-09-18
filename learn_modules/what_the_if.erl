-module(what_the_if).
-export([heh_fine/0, help_me/1]).


heh_fine() ->
  if 1 =:= 1 -> 
    works
  end,
  if 1 =:= 2; 1 =:= 1 ->
    works
  end,
  if 1 =:= 2, 1 =:= 1 ->
    fails;
    true -> default
  end.


help_me(Animal) -> 
  Talk = if Animal == cat -> "meow";
           Animal == beef -> "moo";
           true -> "fasdfasfas"
         end,
  {Animal, "says " ++ Talk ++ "!"}.

