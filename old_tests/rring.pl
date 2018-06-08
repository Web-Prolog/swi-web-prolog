:- use_module(distribution).
:- use_module(node).
:- use_module(actors).
:- use_module(library(debug)).

%:- debug(ws).
%:- debug(dispatch).

start(NumberProcesses, Message) :-
    get_time(Start),
    self(Self),
    create(NumberProcesses, Self, Message),
    receive({ Msg ->
              get_time(End),
              Wall is End - Start,
              writeln(Msg-Wall) }).

create(1, NextProcess, Message) :- !,
    NextProcess ! Message.
create(NumberProcesses, NextProcess, Message) :-
    Port is (NumberProcesses mod 2) + 3060,
    r(Port, loop(NextProcess), Prev),
    self(Me),
    link(Me, Prev),
    NumberProcesses1 is NumberProcesses - 1,
    create(NumberProcesses1, Prev, Message).

loop(NextProcess) :-
    receive({
        Msg ->
            NextProcess ! Msg
    }).

r(Port, Goal, Id) :-
    format(atom(URL), 'http://localhost:~w/ws', [Port]),
    spawn(Goal, Id, [node(URL)]).
