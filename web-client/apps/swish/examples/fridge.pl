
start(Pid) :-
    spawn(fridge([]), Pid, [
    	monitor(true),
        src_text("
    
            fridge(FoodList0) :-
                receive({
                    store(From, Food) ->
                        From ! ok,
                        fridge([Food|FoodList0]);
                    take(From, Food) ->
                        (   select(Food, FoodList0, FoodList)
                        ->  From ! ok(Food),
                            fridge(FoodList)
                        ;   From ! not_found,
                            fridge(FoodList0)
                        );
                    terminate ->
                        true
                }).    
        ")
    ]).


/** Examples

start(Pid).
    
self(Self).
    
$Pid ! store($Self, meat).
    
flush.
    
$Pid ! take($Self, meat).
        
$Pid ! terminate.

*/
