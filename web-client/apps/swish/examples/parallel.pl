
start(Pid) :-
    spawn(server(fridge, []), Pid, [
        node('http://localhost:3061'),
    	src_predicates([server/2, fridge/4])
    ]).


server(Pred, State0) :-
    receive({
        rpc(From, Ref, Request) ->
            call(Pred, Request, State0, Response, State),
            From ! Ref-Response,
            server(Pred, State);
        upgrade(Pred1) ->
            server(Pred1, State0)
    }).


parallel_rpc(Pids, Request, Responses) :-
    self(Self),
    maplist(par_rpc_aux(Self, Request), Pids, Refs),
    yield(Refs, Responses).

par_rpc_aux(Self, Request, Pid, Ref) :-
    uuid(Ref),
    spawn((rpc_synch(Pid, Request, Response), Self ! Ref-Response), _).

yield([], []).
yield([Ref|Refs], [Response|Responses]) :-
    receive({Ref-Response -> true}),
    yield(Refs, Responses).
    
    

fridge(store(Food), FoodList, ok, [Food|FoodList]).
fridge(take(Food), FoodList, ok(Food), FoodListRest) :-
    select(Food, FoodList, FoodListRest), !.
fridge(take(_Food), FoodList, not_found, FoodList).



rpc_synch(To, Request, Response) :-
    self(Self),
    uuid(Ref),
    To ! rpc(Self, Ref, Request),
    receive({
        Ref-Response -> true
    }).
    
