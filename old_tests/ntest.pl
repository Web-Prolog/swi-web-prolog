:- use_module(distribution).
:- use_module(node).
:- use_module(actors).
:- use_module(library(debug)).

%:- debug(dispatch).
%:- debug(ws).

r(Port, Goal, Id) :-
    format(atom(URL), 'http://localhost:~w/ws', [Port]),
    spawn_remote(URL, Goal, Id, []).

pp(N) :-
    get_time(Start),
    r(3060, ping, Ping),
    r(3061, pong(Start, Ping), Pong),
    send_remote(Pong, N).

ping :-
    receive({   0-Pong ->
                send_remote(Pong, stop)
            ;   N-Pong ->
%               format('Ping received ~d~n', [N]),
                N2 is N - 1,
                send_remote(Pong, N2),
                ping
            }).

pong(Start, Ping) :-
    receive({ stop ->
              get_time(End),
              Time is End - Start,
              writeln(Time)
            ; N ->
%             format('Pong received ~d~n', [N]),
              self_remote(Pong),
%             format('Pong: Sending ~p to ~p~n', [N-Pong, Ping]),
              send_remote(Ping, N-Pong),
              pong(Start, Ping)
            }).
