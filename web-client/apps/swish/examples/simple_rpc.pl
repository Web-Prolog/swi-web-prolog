  
/* A simple RPC implementation */

my_rpc(URI, Query) :-
    self(Self),
    spawn(query(Query, Pid, Self), Pid, [
        node(URI),
        monitor(true),
        src_predicates([query/3])
    ]),
    wait_answers(Pid, Query).
    
query(Query, Self, Parent) :-
    call_cleanup(Query, Det=true),
    (   var(Det)
    ->  Parent ! success(Self, Query, true),
        receive({
            next -> fail;
            stop ->
                Parent ! stop(Self)
        })
    ;   Parent ! success(Self, Query, false)
    ).

wait_answers(Pid, Query) :-
    receive({
    	success(Pid, Solution, true) ->
            (   Query = Solution
            ;   Pid ! next,
                wait_answers(Pid, Query)
            );
        success(Pid, Solution, false) ->
            Query = Solution,
            receive({
            	down(_, exit) -> 
            		true
            });
        down(Pid, fail) ->
            fail;
        down(Pid, Exception) ->
            throw(Exception)
    }).


/** Examples

my_rpc('http://localhost:3060', member(X,[a,b,c])).

my_rpc('http://localhost:3060', fail).

my_rpc('http://localhost:3060', length(3,[])).

*/
  
              
              
              
                        