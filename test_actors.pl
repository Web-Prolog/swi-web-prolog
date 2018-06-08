/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_actors,
          [ test_actors/0
          ]).
:- use_module(library(plunit)).
:- use_module(web_prolog).

%:- debug(dispatch).
%:- debug(dispatch(_)).
%:- debug(pingpong).

test_actors :-
    run_tests([ receive,
                local_actors,
                local_pengines,
                ordering
              ]).



:- begin_tests(receive).

test(receive1, X == bar) :-
    self(Self),
    Self ! foo(bar),    
    receive({
        foo(X) -> true
    }).
    
test(receive2, X == baz) :-
    self(Self),
    Self ! not_matching,    
    Self ! foo(bar),    
    receive({
        foo(X) -> true;
        _ -> X = baz
    }),
    receive({
        foo(_) -> true
    }).    

test(receive_after1, X == baz) :-
    receive({
        foo(X) -> true;
        after(1) -> X = baz
    }).
    
test(receive_after2, X == baz) :-
    receive({
        foo(X) -> true;        
        after(0) -> X = baz
    }).
    
test(receive_after3, X == baz) :-
    self(S),
    context_module(Application),
    spawn(p(S), _Pid, [
        application(Application),
        src_text("
    
            p(S) :-
                self(Me),
                Me ! baz,
                receive({
                    X -> 
                        S ! X;
                    after(0) ->     
                        S ! bar
                }).
        ")
    ]),
    receive({
        X -> true
    }).

test(receive_after4, Result == bar) :-
    self(S),
    context_module(Application),
    spawn(p(S), _Pid, [
        application(Application),
        src_text("
    
            p(S) :-
                receive({
                    after(0) ->     
                        S ! bar;
                    Msg -> 
                        S ! Msg
                }).
        ")
    ]),
    receive({
        Result -> true
    }).

test(receive_after5, Result == bar) :-
    self(S),
    context_module(Application),
    spawn(p(S), _Pid, [
        application(Application),
        src_text("
    
            p(S) :-
                receive({
                    Msg -> 
                        S ! Msg;
                    after(0) ->     
                        S ! bar
                }).
        ")
    ]),
    receive({
        Result -> true
    }).
        
    
    
:- end_tests(receive).


:- begin_tests(local_actors).

test(ping_pong) :-
    self(Self),
    context_module(Application),
    spawn(ping(Self), Ping, [alias(ping), application(Application)]),
    spawn(pong(Ping), Pong, [alias(pong), application(Application)]),
    send(Pong, 3),
    receive({ done -> true }).

ping(Main) :-
    receive({   0-Pong ->
                Pong ! done,
                Main ! done
            ;   N-Pong ->
                debug(pingpong, 'Ping received ~d~n', [N]),
                N2 is N - 1,
                Pong ! N2,
                ping(Main)
            }).

pong(Ping) :-
    receive({ done->true;
              N ->
              debug(pingpong, 'Pong received ~d~n', [N]),
              self(Pong),
              debug(pingpong, 'Pong: Sending ~p to ~p~n', [N-Pong, Ping]),
              Ping ! N-Pong,
              pong(Ping)
            }).

test(link, Down == N) :-
    N = 10,
    self(Self),
    context_module(Application),
    spawn(linked(N, Self, Application), Id,
          [ link(true),
            application(Application),
            monitor(true)
          ]),
    receive({ready->true}),
    exit(Id, done),
    receive_all(N, List),
    length(List, Down).

receive_all(0, []) :-
    !.
receive_all(N, List) :-
    receive({ after(1) ->
              List = [];
              H ->
              List = [H|T],
              N2 is N - 1,
              receive_all(N2, T)
            }).

linked(1, Main, _) :-
    !,
    Main ! ready,
    receive({}).
linked(N, Main, Application) :-
    N2 is N - 1,
    spawn(linked(N2, Main, Application), _,
          [ link(true),
            application(Application),
            monitor(Main)
          ]),
    receive({}).

:- end_tests(local_actors).



:- begin_tests(local_pengines).

test(simple, Results = [a,b,c]) :-
    pengine_spawn(Pid, [ 
        src_text("p(a). p(b). p(c)."),
        exit(true),
        monitor(true)
    ]),
    pengine_ask(Pid, p(X), [
        template(X)
    ]),
    collect_answers(Pid, Results),
    receive({
        down(Pid, exit) -> true
    }).
    
test(failure, Results = []) :-
    pengine_spawn(Pid, [ 
        exit(true),
        monitor(true)
    ]),
    pengine_ask(Pid, fail),
    collect_answers(Pid, Results),
    receive({
        down(Pid, exit) -> true
    }).
    
test(error, Results = error(_,_)) :-
    pengine_spawn(Pid, [ 
        exit(true),
        monitor(true)
    ]),
    pengine_ask(Pid, undefined),
    collect_answers(Pid, Results),
    receive({
        down(Pid, exit) -> true
    }).
    
test(exit, Results == foo) :-
    pengine_spawn(Pid, [ 
        monitor(true)
    ]),
    exit(Pid, foo),
    receive({
        down(Pid, Results) -> true
    }).
    
test(output, Results = [a,b,.]) :-
    pengine_spawn(Pid, [ 
        exit(true),
        monitor(true)
    ]),
    pengine_ask(Pid, (pengine_output(a), pengine_output(b)), [
        template(.)
    ]),
    collect_answers(Pid, Results),
    receive({
        down(Pid, exit) -> true
    }).
    
test(input, Results == true) :-
    pengine_spawn(Pid, [ 
        exit(true),
        monitor(true)
    ]),
    pengine_ask(Pid, (pengine_input(prompt, In), pengine_output(In)), [
        template(.)
    ]),
    receive({
        prompt(Pid, prompt) ->
            pengine_respond(Pid, hello)
    }),
    receive({
        output(Pid, hello) -> true
    }),
    receive({
        success(Pid, [.], false) -> true
    }),
    receive({
        down(Pid, exit) -> 
            Results = true
    }).

:- end_tests(local_pengines).


:- begin_tests(ordering).


test(ordering1, Result == [hello, goodbye]) :-
    self(S),
    S ! hello,
    S ! goodbye,
    receive({A -> true}),
    receive({B -> true}),
    Result = [A,B].
    
    
test(ordering2, Result == [hello, goodbye]) :-
    context_module(Application),
    self(Self),
    spawn(( self(S),
            S ! hello,
            S ! goodbye,
            receive({A -> true}),
            receive({B -> true}),
            Result0 = [A,B],
            Self ! Result0
           ), _, [application(Application)]),
    receive({Result -> true}).
                

:- end_tests(ordering).


                 /*******************************
                 *           UTILITIES          *
                 *******************************/


collect_answers(Pid, Results) :-
    receive({
        success(Pid, Heads, true) ->
            pengine_next(Pid),
            collect_answers(Pid, Tail),
            append(Heads, Tail, Results);
        success(Pid, Results, false) ->
            true;
        failure(Pid) ->
            Results = [];
        error(Pid, Results) ->
            true;
        output(Pid, Head) ->
            Results = [Head|Tail],
            collect_answers(Pid, Tail);
        echo(Pid, Head)->
            Results = [Head|Tail],
            collect_answers(Pid, Tail)
    }).
    


