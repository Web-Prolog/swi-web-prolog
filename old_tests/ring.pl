:- use_module(web_prolog).

s(N, Msg) :-
    spawn(start(N, Msg), _Pid, [
        src_text("
    
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
                spawn(loop(NextProcess), Prev,
                      [ link(true)
                      ]),
                NumberProcesses1 is NumberProcesses - 1,
                create(NumberProcesses1, Prev, Message).

            loop(NextProcess) :-
                receive({
                    Msg ->
                        NextProcess ! Msg
                }).
        ")
    ]).


