-module(recursion).
-export([fac/1, len/1, fac_tail/1, len_tail/1, duplicate/2, tail_duplicate/2, 
tail_reverse/1, reverse/1, sublist/2, tail_sublist/2, zip/2,
lenient_zip/2, tail_zip/2, quicksort/1]).


fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

fac_tail(N) -> fac_tail(N, 1).
fac_tail(0, Acc) -> Acc;
fac_tail(N, Acc) when N > 0 -> fac_tail(N-1, N*Acc).


len([]) -> 0;
len([_|T]) -> 1 + len(T).

len_tail(L) -> len_tail(L, 0).
len_tail([], Acc) -> Acc;
len_tail([_|T], Acc) -> len_tail(T, Acc+1).


duplicate(0,_) -> 
  [];
duplicate(N,Term) ->
  [Term|duplicate(N-1, Term)].


tail_duplicate(N, Term) ->
  tail_duplicate(N, Term, []).

tail_duplicate(0, _, List) -> 
  List;
tail_duplicate(N,Term,List) when N > 0 -> 
  tail_duplicate(N-1, Term, [Term|List]).


reverse([]) ->
   [];
reverse([H|T]) -> 
  reverse(T) ++ [H].


tail_reverse([H|T]) ->
  tail_reverse([H|T], []).

tail_reverse([], Iter) ->
  Iter;
tail_reverse([H|T], Iter) ->
  tail_reverse(T, [H|Iter]).

sublist([],_) -> 
  [];
sublist(_,0) ->
  [];
sublist([H|Tail],N) ->
 [H]++sublist(Tail, N-1).  
 

tail_sublist(List, N) ->
 lists:reverse(tail_sublist(List, N, [])).


tail_sublist([],_,List) ->
  List;
tail_sublist(_,0,List) ->
  List;
tail_sublist([H|T], N, List) when N > 0 ->
  tail_sublist(T, N-1, [H|List]).

zip([], []) ->
  [];
zip([X|Xs], [Y|Ys]) ->
  [{X,Y}|zip(Xs, Ys)].

lenient_zip(_,[]) -> [];
lenient_zip([],_) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X,Y}|lenient_zip(Xs, Ys)].

tail_zip(List1, List2) ->
  reverse(tail_zip(List1, List2, [])).

tail_zip(_, [], List) -> List; 
tail_zip([], _, List) -> List;
tail_zip([X|Xs], [Y|Ys], List) -> 
  tail_zip(Xs, Ys, [{X,Y}|List]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) -> 
  {Smaller, Bigger} = partition(Pivot, Rest, [], []),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Bigger).

partition(_, [], Smaller, Bigger) -> {Smaller, Bigger};
partition(Pivot, [H|T], Smaller, Bigger) -> 
  if H =< Pivot -> partition(Pivot, T, [H|Smaller], Bigger);
     H > Pivot  -> partition(Pivot, T, Smaller, [H|Bigger])
  end.



