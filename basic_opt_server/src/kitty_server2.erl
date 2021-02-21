-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).


-record(cat, {name, color=green, description}).


%%% Client API

start_link() -> my_server:start_link(?MODULE, []).

%% synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).


%% asynchronous call
return_cat(Pid, Cat=#cat{}) ->
    my_server:cast(Pid, {return, Cat}).

%% synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).



%%% Server functions
init([]) -> [].


handle_call({order, Name, Color, Description}, From, Cats) -> 
    if Cats =:= [] ->
           io:format("ordering cat~n"),
           my_server:reply(From, make_cat(Name, Color, Description)),
           Cats;
       Cats =/= [] ->
           my_server:reply(From, hd(Cats)),
           tl(Cats)
    end;

handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat=#cat{}}, Cats) -> 
    [Cat|Cats].


%% Private functions
make_cat(Name, Color, Description) ->
   #cat{name=Name, color=Color, description=Description}.


terminate(Cats) -> 
  [io:format("~p set free~n", [C#cat.name]) || C <- Cats],
  exit(normal).


{undef,[{kitty_server2,handle_call,
                       [{order,vika,brown,"sleeps a lot"},
                        <0.79.0>,#Ref<0.2518753446.1203503106.94923>,[]],
                       []},
        {my_server,loop,2,[{file,"my_server.erl"},{line,46}]}]}


