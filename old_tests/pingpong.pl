:- use_module(web_prolog).

% :- debug(dispatch).
% :- debug(dispatch(_)).
:- debug(pingpong).

pp(N) :-
    self(Self),
    spawn(ping(Self), Ping, [alias(ping)]),
    spawn(pong(Ping), Pong, [alias(pong)]),
    send(Pong, N),
    receive({done->true}).

ping(Main) :-
    receive({   0-Pong ->
                send(Pong, done),
                send(Main, done)
            ;   N-Pong ->
                debug(pingpong, 'Ping received ~d', [N]),
                N2 is N - 1,
                send(Pong, N2),
                ping(Main)
            }).

pong(Ping) :-
    receive({ done -> true;
              N ->
              debug(pingpong, 'Pong received ~d', [N]),
              self(Pong),
              debug(pingpong, 'Pong: Sending ~p to ~p', [N-Pong, Ping]),
              send(Ping, N-Pong),
              pong(Ping)
            }).
