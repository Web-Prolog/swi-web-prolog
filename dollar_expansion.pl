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

:- module(dollar_expansion,
          [ wp_expand_query/4,          % +Query0, -Query, +Bindings0, -Bindings
            wp_expand_answer/2          % +Answer0, -Answer
          ]).
          
         

%%    wp_expand_query(+Query0, -Query, +Bindings0, -Bindings) is det.
%%    wp_expand_answer(+Answer0, -Answer) is det.
%
%    These predicates realise reuse of toplevel variables using the
%    $Var notation. 

:- thread_local '$topvar'/2.

wp_expand_query(Query, Expanded, Bindings, ExpandedBindings) :-
    expand_vars(Bindings, Query, Expanded),
    term_variables(Expanded, Free),
    delete_bound_vars(Bindings, Free, ExpandedBindings).
    
expand_vars(_, Var, Var) :-
    var(Var), !.
expand_vars(_, Atomic, Atomic) :-
    atomic(Atomic), !.
expand_vars(Bindings, $(Var), Value) :-
    name_var(Var, Bindings, Name),
    (   toplevel_var(Name, Value)
    ->  !
    ;   throw(error(existence_error(variable, Name), _))
    ).
expand_vars(Bindings, Term, Expanded) :-
    compound_name_arity(Term, Name, Arity), !,
    compound_name_arity(Expanded, Name, Arity),
    End is Arity + 1,
    expand_args(1, End, Bindings, Term, Expanded).

expand_args(End, End, _, _, _) :- !.
expand_args(Arg0, End, Bindings, T0, T) :-
    arg(Arg0, T0, V0),
    expand_vars(Bindings, V0, V1),
    arg(Arg0, T, V1),
    Arg1 is Arg0 + 1,
    expand_args(Arg1, End, Bindings, T0, T).

name_var(Var, [VarName - TheVar|_], VarName) :-
    Var == TheVar, !.
name_var(Var, [_|T], Name) :-
    name_var(Var, T, Name).


delete_bound_vars([], _, []).
delete_bound_vars([H|T0], Free, [H|T1]) :-
    H = (_Name - Value),
    v_member(Value, Free), !,
    delete_bound_vars(T0, Free, T1).
delete_bound_vars([_|T0], Free, T1) :-
    delete_bound_vars(T0, Free, T1).

v_member(V, [H|T]) :-
    (   V == H
    ;   v_member(V, T)
    ).

wp_expand_answer(Bindings0, Bindings0) :-
    dict_pairs(Bindings0, _Tag, Bindings),
    assert_bindings(Bindings).

assert_bindings([]).
assert_bindings([Var-Value|Tail]) :-
    (   ( nonvar(Value) ; attvar(Value))
    ->  retractall('$topvar'(Var, _)),
        asserta('$topvar'(Var, Value))
    ;   true
    ),
    assert_bindings(Tail).

toplevel_var(Var, Binding) :-
    '$topvar'(Var, Binding).

print_toplevel_variables :-
    (   toplevel_var(Name, Value)
    *-> format('$~w =~t~12|~p~n', [Name, Value]),
        fail
    ;   format('No defined toplevel variables~n')
    ).
