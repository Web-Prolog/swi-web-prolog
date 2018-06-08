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

:- module(isolation,
          [ consult_text/1,             % +SourceCodeText
            with_source/2,              % :Goal, +Options
            source_data/2,              % ?SourceID, ?Data
            translate_local_sources/3
          ]).
:- use_module(library(modules)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(http/http_open)).
:- use_module(library(sandbox)).
:- use_module(library(debug)).

    
:- meta_predicate
    with_source(0, +),
    prepare_source(:, +).

:- multifile
    prepare_module/3,               % +Module, +Application, +Options
    prepare_goal/3.                 % +GoalIn, -GoalOut, +Options

:- dynamic
    source_data/2.                  % +Id, ?Data

%!  with_source(:Goal, +Options)
%
%   Run Goal in an isolated module.  Options:
%
%     - module(?Module)
%       Use Module for the isolation.  If unbound it will be bound
%       to a random name.
%     - application(+Application)
%       Module to inherit from.  Default is `user`.
%     - source_id(+Atom)
%       Specifies the identifier we give to the source.  Default
%       is the module name.
%     - debug(+Boolean)
%       If true, keep the source code around.
%     - sandboxed(+Boolean)
%       If `true`, enforce sandboxing while loading the code
%     - program_space(+Limit)
%       Limit the size of the temporary module (limits the size of the
%       program and dynamic predicates created by it).

with_source(Goal, Options) :-
    strip_module(Goal, _Spec, Plain),
    option(application(Application), Options, user),
    option(module(Module), Options, _),
    call_cleanup(
        in_temporary_module(
            Module,
            prepare_source(Application, Options),
            execute(Module:Plain, Options)),
        cleanup_data(Module, Options)).

cleanup_data(Module, Options) :-
    option(source_id(ID), Options, Module),
    retractall(source_data(ID, _)).

execute(Goal, Options) :-
    option(sandboxed(true), Options),
    !,
    safe_goal(Goal),
    call(Goal).
execute(Goal, _Options) :-
    call(Goal).

    
    

%!  prepare_source(:Application, +Options) is det.
%
%   Load the source into the pengine's module.
%
%   @throws =prepare_source_failed= if it failed to prepare the
%           sources.

:- thread_local curr_module/1.

prepare_source(Module:Application, Options) :-
    assert(curr_module(Module)),
    (   option(program_space(SpaceLimit), Options)
    ->  set_module(Module:program_space(SpaceLimit))
    ;   true
    ),
    delete_import_module(Module, user),
    add_import_module(Module, Application, start),
    prep_module(Module, Application, Options).

prep_module(Module, Application, Options) :-
    maplist(copy_flag(Module, Application), [var_prefix]),
    forall(prepare_module(Module, Application, Options), true),
    partition(source_option, Options, Sources, Options1),
    setup_call_cleanup(
        '$set_source_module'(OldModule, Module),
        maplist(load_source(Module, Options1), Sources),
        '$set_source_module'(OldModule)).

copy_flag(Module, Application, Flag) :-
    current_prolog_flag(Application:Flag, Value),
    !,
    set_prolog_flag(Module:Flag, Value).
copy_flag(_, _, _).

source_option(src_text(_)).
source_option(src_uri(_)).

load_source(Module, Options, src_text(Text)) :-
    !,
    load_src_text(Text, Module, Options).
load_source(Module, Options, src_uri(URI)) :-
    !,
    load_src_uri(URI, Module, Options).



consult_text(Src) :-
    curr_module(Module),
    load_src_text(Src, Module, []).


                 /*******************************
                 *        COMPILE SOURCE        *
                 *******************************/

/** load_src_text(+SrcText, +Module, +Options) is det

Asserts the clauses defined in SrcText in   the  private database of the
current actor. This  predicate  processes   the  `src_text'  option of
spawn/3.
*/

load_src_text(Src, Module, Options) :-
    option(source_id(ID), Options, Module),
    setup_call_cleanup(
        open_chars_stream(Src, Stream),
        load_files(Module:ID,
                   [ stream(Stream),
                     module(Module),
                     silent(true)
                   | Options
                   ]),
        close(Stream)),
    keep_source(ID, Src, Options).

system:'#file'(File, _Line) :-
    prolog_load_context(stream, Stream),
    set_stream(Stream, file_name(File)),
    set_stream(Stream, record_position(false)),
    set_stream(Stream, record_position(true)).

%%   load_src_uri(+URI, +Module, Options) is det
%
%    Asserts the clauses defined in URI in   the private database of the
%    current actor. This predicate processes   the `src_uri' option of
%    spawn/3.
%
%    @tbd: make a sensible guess at the encoding.

load_src_uri(URI, Module, Options) :-
    option(source_id(ID), Options, Module),
    (   option(debug(false), Options, false)
    ->  setup_call_cleanup(
            http_open(URI, Stream, []),
            ( set_stream(Stream, encoding(utf8)),
              load_files(Module:ID,
                         [ stream(Stream),
                           module(Module)
                         | Options
                         ])
            ),
            close(Stream))
    ;   setup_call_cleanup(
            http_open(URI, TempStream, []),
            ( set_stream(TempStream, encoding(utf8)),
              read_string(TempStream, _, Src)
            ),
            close(TempStream)),
        setup_call_cleanup(
            open_chars_stream(Src, Stream),
            load_files(Module:ID,
                       [ stream(Stream),
                         module(Module)
                       | Options
                       ]),
            close(Stream)),
        keep_source(ID, Src, Options)
    ).


keep_source(ID, SrcText, Options) :-
    option(debug(true), Options),
    !,
    to_string(SrcText, SrcString),
    assertz(source_data(ID, source(ID, SrcString))).
keep_source(_, _, _).

to_string(String, String) :-
    string(String),
    !.
to_string(Atom, String) :-
    atom_string(Atom, String),
    !.


%%	translate_local_sources(+OptionsIn, -Options, +Module) is det.
%
%	Translate  the  `src_predicates`  and  `src_list`  options  into
%	`src_text`. We need to do that   anyway for remote actors. For
%	local actors, we could avoid  this   step,  but  there is very
%	little point in transferring source to a local actor anyway as
%	local actors can access any  Prolog   predicate  that you make
%	visible to the application.
%
%	Multiple sources are concatenated  to  end   up  with  a  single
%	src_text option.

translate_local_sources(OptionsIn, Options, Module) :-
    translate_local_sources(OptionsIn, Sources, Options2, Module),
    (	Sources == []
    ->	Options = Options2
    ;	Sources = [Source]
    ->	Options = [src_text(Source)|Options2]
    ;	atomics_to_string(Sources, Source)
    ->	Options = [src_text(Source)|Options2]
    ).

translate_local_sources([], [], [], _).
translate_local_sources([H0|T], [S0|S], Options, M) :-
    nonvar(H0),
    translate_local_source(H0, S0, M), !,
    translate_local_sources(T, S, Options, M).
translate_local_sources([H|T0], S, [H|T], M) :-
    translate_local_sources(T0, S, T, M).

translate_local_source(src_predicates(PIs), Source, M) :-
    must_be(list, PIs),
    with_output_to(string(Source),
		   maplist(listing(M), PIs)).
translate_local_source(src_list(Terms), Source, _) :-
    must_be(list, Terms),
    with_output_to(string(Source),
		   forall(member(Term, Terms),
			  format('~k .~n', [Term]))).
translate_local_source(src_text(Source), Source, _).

listing(M, PI) :-
	listing(M:PI).