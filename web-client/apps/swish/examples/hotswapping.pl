
start(Pid) :-
    spawn(server(fridge, []), Pid, [
        src_predicates([server/2, fridge/4, fridge2/4])
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
    
	
fridge(store(Food), FoodList, ok, [Food|FoodList]).
fridge(take(Food), FoodList, ok(Food), FoodListRest) :-
    select(Food, FoodList, FoodListRest), !.
fridge(take(_Food), FoodList, not_found, FoodList).


fridge2(store(Food), FoodList, okidoki, [Food|FoodList]).
fridge2(take(Food), FoodList, okidoki(Food), FoodListRest) :-
    select(Food, FoodList, FoodListRest), !.
fridge2(take(_Food), FoodList, not_present, FoodList).


rpc_synch(Pid, Request, Response) :-
    self(Self),
    uuid(Ref),
    Pid ! rpc(Self, Ref, Request),
    receive({
        Ref-Response -> true
    }).

upgrade(Pid, Pred) :-
    Pid ! upgrade(Pred).
	
	
/** Examples

start(Pid).

rpc_synch($Pid, store(meat), Response).

rpc_synch($Pid, take(meat), Response).

upgrade($Pid, fridge2).

*/
