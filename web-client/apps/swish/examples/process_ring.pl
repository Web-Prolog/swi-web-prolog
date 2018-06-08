
start(NumberProcesses, Message) :-
    get_time(Start),
    self(Self),
    create(NumberProcesses, Self, Message),
    receive({ 
    	Msg ->
        	get_time(End),
        	Wall is End - Start,
            io:writeln(Msg-Wall)
     }).
              
create(1, NextProcess, Message) :- !,
    NextProcess ! Message.
create(NumberProcesses, NextProcess, Message) :-
    spawn(loop(NextProcess), Prev,
          [ link(true),
            src_predicates([loop/1])
          ]),
    NumberProcesses1 is NumberProcesses - 1,
    create(NumberProcesses1, Prev, Message).

loop(NextProcess) :-
    receive({
        Msg ->
            NextProcess ! Msg
    }).

/** Examples

start(10, hello).
    
start(100, hello).
    
start(1000, hello).
    
start(10000, hello).

start(100000, hello).

*/
