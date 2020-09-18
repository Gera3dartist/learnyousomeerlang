-module(exceptions).
-compile(export_all).


rethrow(F) ->
  try F() of 
    _ -> ok
  catch
    Throw -> {throw, cought, Throw}
  end.

errors(F) -> 
  try F() of
    _ -> ok
  catch
    error:Error -> {error, cought, Error}
  end.


exits(F) ->
  try F() of
    _ -> ok
  catch
    exit:Exit -> {exit, cought, Exit}
  end.


sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_hand);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(something_extra).

black_knight(Attack) when is_function(Attack, 0) -> 
  try Attack() of
    _ -> "None shell pass"
  catch
    throw:slice -> "It is but a scratch.";
    error:cut_hand -> "I"ve had worse";
    exit:cut_leg -> "Come on you pansy";
    _:_ -> "just a flesh wound"
  after
    io:fwrite("final clause!~n")
  end.

talk() -> 
  "blah blah".

  
whoa() ->
try
  talk(),
  _Knight = "None shell pass",
  _SomeVar = [N*2 || N <- lists:seq(1,100)],
  _WillReturnThis = tequila
of
  tequila -> "hey this worker"
catch 
  Exception:Reason -> {caught, Exception,Reason}
end.

im_impressed() ->
  try
    talk(),
    exit(up)
  catch
    Exc:Reason -> {cought, Exc, Reason}
  end.

catcher(X,Y) ->
  case catch X/Y of
    {"EXIT", {badarith,_}} -> "this bad";
    N -> io:format("output"), N
  end.

one_or_two(1) -> return;
one_or_two(2) -> throw(return).
