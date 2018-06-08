:- use_module(dispatch).

start :-
    spawn(pong, Pong_Pid),
    spawn(ping(3, Pong_Pid), _, [monitor(true)]).

pong :-
    receive({
        ping(Ping_Pid) ->
            writeln('Pong received ping'),
            Ping_Pid ! pong,
            pong;
        finished ->
            writeln('Pong finished')
    }).
   
ping(0, Pong_Pid) :-
    Pong_Pid ! finished,
    writeln('Ping finished').
ping(N, Pong_Pid) :-
    self(Self),
    Pong_Pid ! ping(Self),
    receive({
        pong ->
            writeln('Ping received pong')
    }),
    N1 is N - 1,
    ping(N1, Pong_Pid).
