/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
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

:- module(restful_api, [
        check/0
    ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(term_to_json)).
:- use_module(library(apply)).
:- use_module(library(time)).

:- use_module(library(debug)).

:- use_module(pengines2).
:- use_module(format).


    
:- dynamic
    solution_pengine/3,      % Hash, QueryID, Pid
    solution_index/2.        % Pid, Index
    

         /*******************************
         *          HTTP API            *
         *******************************/


:- http_handler(root(ask), http_pengine_ask, [spawn([])]).

http_pengine_ask(Request) :-
    http_parameters(Request,
        [ query(GoalAtom, []),
          template(TemplateAtom, [default(GoalAtom)]),
          offset(Offset, [integer, default(0)]),
          limit(Limit, [integer, default(1)]),
          timeout(Timeout, [integer, default(10)]),
          format(Format, [default(json)])
        ]),
    atomic_list_concat([GoalAtom,+,TemplateAtom], GTAtom),
    read_term_from_atom(GTAtom, Goal+Template, [variable_names(Bindings)]),
    fix_template(Format, Template, Bindings, NewTemplate),
    find_answer(Goal, NewTemplate, Offset, Limit, Timeout, Answer),
    output_result(Format, Answer).
    

%!  find_answer(:Query, +Template, +Offset, +Limit, +Timeout, -Answer) is det.

%   - Caching provided by pengines ensures a fast return of
%     consequtive answers. 
%
%   @tbd: Perhaps the down message would be useful here?

find_answer(Query, Template, Offset, Limit, Timeout, Answer) :-
    query_id(Template-Query, QueryID),
    (   query_pengine(QueryID, Offset, Pid)
    ->  self(Self),
        pengine_next(Pid, [
            reply_to(Self),
            limit(Limit)
        ]),
        wait_answer(Pid, Offset, Limit, Timeout, Answer)
    ;   pengine_spawn(Pid, [
            exit(true)
        ]),
        pengine_ask(Pid, offset(Offset, Query), [ 
            template(Template),
            limit(Limit)
        ]),
        term_hash(QueryID, Hash),
        assertz(solution_pengine(Hash, QueryID, Pid)),
        flag(alive, N, N+1),
        wait_answer(Pid, Offset, Limit, Timeout, Answer)      
    ),
    % M is hardcoded - this must change!
    get_flag(alive, M),
    (   M > 1000
    ->  findnsols(500, Pid, solution_index(Pid, _), Pids),
        maplist(remove, Pids)    
    ;   true
    ).
    

query_id(Term, QueryID) :-
    copy_term(Term, QueryID),
    numbervars(QueryID, 0, _).

query_pengine(QueryID, Index, Pengine) :-
    term_hash(QueryID, Hash),
    solution_pengine(Hash, QueryID, Pengine),
    solution_index(Pengine, Index).


wait_answer(Pid, Offset, Limit, Timeout, Answer) :-
    receive({
        success(Pid, Solutions, true) ->
            Answer = success(anonymous, Solutions, true),
            NewIndex is Offset + Limit,
            retractall(solution_index(Pid, _)),
            assertz(solution_index(Pid, NewIndex));
        success(Pid, Solutions, false) ->
            Answer = success(anonymous, Solutions, false),
            cleanup(Pid);
        failure(Pid) ->
            Answer = failure(anonymous),
            cleanup(Pid);
        error(Pid, Error) ->
            Answer = error(anonymous, Error),
            cleanup(Pid);
        after(Timeout) ->
            exit(Pid, timeout),
            Answer = error(anonymous, timeout)
    }). 
    
    
remove(Pid) :-
    cleanup(Pid),
    exit(Pid, timeout).
    
   
cleanup(Pid) :-
    flag(alive, N, N-1),
    retractall(solution_pengine(_, _, Pid)),
    retractall(solution_index(Pid, _)).


 
check :-
    debug(rest),
    get_flag(alive, M),
    debug(rest, "Alive: ~p", [M]),
    findall(., solution_index(_,_), L1),
    length(L1, N1),
    debug(rest, "Indices: ~p", [N1]),
    findall(., solution_pengine(_,_,_), L2),
    length(L2, N2),
    debug(rest, "Pengines: ~p", [N2]),
    findall(., (thread_property(P, status(suspended)), 
                atom_length(P, N3), N3 > 30), L4),
    length(L4, N4),
    debug(rest, "Engines: ~p (Should be N more than the others if N websockets are running.)", [N4]),
    nodebug.
    
    

:- http_handler(root(send), http_pengine_send, []).

http_pengine_send(Request) :-
    http_parameters(Request,
        [ name(Name, []),
          message(MessageAtom, [])
        ]),
    read_term_from_atom(MessageAtom, Message, []),
    debug(rest, "Sending: ~p to ~p", [Message, Name]),
    send(Name, Message),
    reply_json(_{ok:true}).

    

