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

:- module(pengines2,  
          [ flush/0,                            % From actors
            pengine_spawn/1,                    % -Pid
            pengine_spawn/2,                    % -Pid, +Options
            pengine_ask/2,                      % +Pid, :Query
            pengine_ask/3,                      % +Pid, :Query, +Options
            pengine_next/1,                     % +Pid
            pengine_next/2,                     % +Pid, +Options
            pengine_stop/1,                     % +Pid                   
            pengine_stop/2,                     % +Pid, +Options                   
            pengine_abort/1,                    % +Pid    
            pengine_input/2,                    % +Prompt, ?Answer
            pengine_respond/2,                  % +Pid, +Answer
            pengine_output/1,                   % +Term
            pengine_exit/1,                     % +Reason
            pengine_exit/2,                     % +Pid, +Reason
            
            speak/1,                            % +Term
%            pengine_listing/0,
%            pengine_listing/1,
            op(200, xfx, @)
          ]).

:- use_module(library(debug)).

:- use_module(actors).
:- use_module(dollar_expansion).

    
:- meta_predicate 
    session(:, +, +).
    

%!  pengine_spawn(-Pid) is det.
%!  pengine_spawn(-Pid, +Options) is det.
%
%   Spawn a new pengine.  Options:
%
%     - exit(+Bool)
%       Determines if the pengine session must exit after having run 
%       a query to completion. Defaults to false.
%     - reply_to(+Target)
%       Send messages to Target. Default is the thread or engine that
%       called pengine_spawn/1-2.
    
    
pengine_spawn(Pid) :-
    pengine_spawn(Pid, []).

pengine_spawn(Pid, Options) :-
    self(Self),
    option(reply_to(Target), Options, Self),
    option(exit(Exit), Options, true),
    spawn(session(Pid, Target, Exit), Pid, [
          application(pengines2)
        | Options
    ]).


:- thread_local '$parent'/1.

session(Pid, Parent, Exit) :-
    assertz('$parent'(Parent)),
    session2(Pid, Parent, Exit).

session2(Pid, Parent, Exit) :-
    catch(guarded_session(Pid, Parent, Exit), exit_query, (
            Parent ! abort(Pid),
            session2(Pid, Parent, Exit)
        )
    ).

guarded_session(Module:Pid, Parent, Exit) :-
    add_self(Pid), % TODO: Figure out why this is needed
    receive({
        pengine:ask(Goal, Options) ->
            ask(Module:Goal, Pid, Parent, Options)
    }),
    (   Exit == true
    ->  true
    ;   guarded_session(Module:Pid, Parent, Exit)
    ).


add_self(ID@Pid) :-
    !,
    self(ID@Pid).
add_self(_).


% TODO: Currently only works when sandboxed = false, probably due
% to offset/2 not being made and declared as safe.

ask(Goal0, Self, Parent, Options) :-
    strip_module(Goal0, M, Goal1),
    option(template(Template0), Options, Goal1),
    option(limit(Limit), Options, 1),
    option(reply_to(ReplyTo), Options, Parent),
    maybe_expand(Goal1, Goal, Template0, Template, ReplyTo, Self), % FIXME? Hack - see below!    
    State = count(Limit),
    OutPut = replyto(ReplyTo),
    (   call_cleanup(findn0(State, Template, M:Goal, Solutions, Error), Det=true),
        (   var(Error),
            arg(1, OutPut, Out)    
        ->  (   var(Det)
            ->  Out ! success(Self, Solutions, true),
                receive({
                    pengine:next -> 
                        fail;
                    pengine:next(Count) -> 
                        nb_setarg(1, State, Count),
                        fail;
                    pengine:next(From, Count) -> 
						(	From \= ReplyTo
						->	nb_setarg(1, OutPut, From)
						;	true
						),
                        nb_setarg(1, State, Count),
                        fail;
                    pengine:stop(From) ->
                        From ! stop(Self);
                    pengine:stop ->
						arg(1, OutPut, Out),
                        Out ! stop(Self)  
                })
            ;   Out ! success(Self, Solutions, false)
            )
        ;   ReplyTo ! error(Self, Error)
        )
    ;   ReplyTo ! failure(Self)
    ).


/* It seems that since toplevel variables are stored as thread_local
   we cannot expand variables earlier than here. It would be nice if 
   this could be done earlier.
*/

maybe_expand(Goal0, Goal, Template0, Template, ReplyTo, Self) :-
    is_dict(Template0), !,
    dict_pairs(Template0, Tag, Pairs0),
    catch(wp_expand_query(Goal0, Goal, Pairs0, Pairs), Error, true),
    (   var(Error)
    ->  dict_pairs(Template, Tag, Pairs)
    ;   ReplyTo ! error(Self, Error)
    ).
maybe_expand(Goal, Goal, Template, Template, _ReplyTo, _Self). 


        
findn0(State, Template, Goal, Solutions, Error) :-
    catch(findn(State, Template, Goal, Solutions), Error, true).
    
findn(N, Template, Goal, Solutions) :- 
    findnsols(N, Template, Goal, Solutions), 
    Solutions \== [].


%!  pengine_ask(+Pid, :Query) is det.
%!  pengine_ask(+Pid, :Query, +Options) is det.
%
%   Call pengine Pid with Query. Options:
%
%     - template(+Template)
%       Template is a variable (or a term containing variables) 
%       shared with Query. By default, the template is identical to
%       Query.
%     - limit(+Integer)
%       Integer indicates the maximum number of solutions to retrieve
%       in one batch. A value of 1 means a unary list (default).
%     - reply_to(+Target)
%       Send messages to Target. Default is the thread or engine that
%       called pengine_spawn/1-2.

pengine_ask(Pid, Goal) :-
    pengine_ask(Pid, Goal, []).

pengine_ask(Pid, Goal, Options) :-
    Pid ! pengine:ask(Goal, Options). 
    
        
%!  pengine_next(+Pid) is det.
%!  pengine_next(+Pid, +Options) is det.
%
%   Ask pengine Pid for more solutions to Query. Options:
%
%     - limit(+Integer)
%       Integer indicates the maximum number of solutions to retrieve
%       in one batch. A value of 1 means a unary list (default).
%     - reply_to(+Target)
%       Send messages to Target. Default is the thread or engine that
%       called pengine_spawn/1-2.

pengine_next(Pid) :-
    pengine_next(Pid, []).

pengine_next(Pid, Options) :-
	(	option(limit(Limit), Options)
	->	(	option(reply_to(ReplyTo), Options)
    	->	Pid ! pengine:next(ReplyTo, Limit)
		;	Pid ! pengine:next(Limit)
		)
	;	Pid ! pengine:next
	).


%!  pengine_stop(+Pid) is det.
%
%   Tell pengine Pid to stop. If successful, delivers a message
%   `stop(Pid)` to the mailbox of the process that called 
%   pengine_spawn/2-3. Options:
%
%     - reply_to(+Target)
%       Send `stop' message to Target. Default is the thread or
%       engine that called pengine_spawn/1-2.

pengine_stop(Pid) :-
    pengine_stop(Pid, []).
    
pengine_stop(Pid, Options) :-
    (	option(reply_to(ReplyTo), Options)
    ->	Pid ! pengine:stop(ReplyTo)
	;	Pid ! pengine:stop
	).
    
    
%!  pengine_output(+Term) is det.
%
%   Send Term to the parent process.

pengine_output(Term) :-
    engine_self(Self), 
    '$parent'(Parent),
    Parent ! output(Self, Term).


speak(Term) :-
    engine_self(Self), 
    '$parent'(Parent),
    Parent ! speak(Self, Term).


%!  pengine_input(+Prompt, -Input) is det.
%
%   Send Prompt to the parent process and wait for input. Prompt may
%   be any term, compound or atomic.
%
%   @bug: Why does Parent and _Parent not unify in the remote case?

pengine_input(Prompt, Input) :-
    engine_self(Self),
    '$parent'(Parent),
    Parent ! prompt(Self, Prompt),
    receive({ 
        input(_Parent, Input) ->
            true
    }).


%!  pengine_respond(+Pid, +Input) is det.
%
%   Send a response in the form of a term Input to a pengine Pid that
%   has prompted its parent process for input.

pengine_respond(Pid, Term) :-
    self(Self),
    Pid ! input(Self, Term).
    

%!  pengine_abort(+Pid) is det.
%
%   Tell pengine Pid to abort any query that it currently runs. If
%   successful, delivers a message `abort(Pid)' to the mailbox of the
%   process that called pengine_spawn/2-3.

pengine_abort(Pid) :-
    catch(thread_signal(Pid, throw(exit_query)), _, true).


%!  pengine_exit(+Reason) is det.
%!  pengine_exit(+Pid, +Reason) is det.
%
%   These just copies the functionality of exit/1 and exit/2.

pengine_exit(Reason) :-
    exit(Reason).
    
pengine_exit(Pid, Reason) :-
    exit(Pid, Reason).

              
%!  pengine_listing is det.
%!  pengine_listing(+Spec) is det.
%
%   List the content of the current pengine or a specified predicate
%   in the pengine.

pengine_listing :-
    pengine_listing(_).

pengine_listing(Spec) :-
    isolation:curr_module(Module),
    with_output_to(string(String), listing(Module:Spec)),
    pengine_output(String).
    

