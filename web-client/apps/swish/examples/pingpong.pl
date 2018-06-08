
ping(0, Pong_Pid) :-
    Pong_Pid ! finished,
    io:format('Ping finished').
ping(N, Pong_Pid) :-
    self(Self),
    Pong_Pid ! ping(Self),
    receive({
        pong ->
            io:format('Ping received pong')
    }),
    N1 is N - 1,
    ping(N1, Pong_Pid).
    
pong :-
    receive({
        ping(Ping_Pid) ->
            io:format('Pong received ping'),
            Ping_Pid ! pong,
            pong;
        finished ->
            io:format('Pong finished')
    }).
   
start(N) :-
    spawn(pong, Pong_Pid, [
        src_predicates([pong/0])
    ]),
    spawn(ping(N, Pong_Pid), _, [
        src_predicates([ping/2])
    ]).

/** Examples

start(3).

*/