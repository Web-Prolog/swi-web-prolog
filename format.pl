:- encoding(utf8).
/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, Torbjörn Lager,
                              VU University Amsterdam
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

:- module(format,
          [ message_to_json_data/3,
            fix_template/4,
            output_result/2
          ]).

/** <module> Pengines: Web Logic Programming Made Easy

The library(pengines) provides an  infrastructure   for  creating Prolog
engines in a (remote) pengine server  and accessing these engines either
from Prolog or JavaScript.

@author Torbjörn Lager and Jan Wielemaker
*/
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(term_to_json)).

:- use_module(dollar_expansion).


:- multifile
    write_result/3,                 % +Format, +Event, +Dict
    event_to_json/3.                % +Event, -JSON, +Format




%!  fix_template(+Format, +Template, +Bindings, -NewTemplate) is det.
%
%   Generate the template for json(-s) Format  from the variables in
%   the asked Goal. Variables starting  with an underscore, followed
%   by an capital letter are ignored from the template. If a json
%   format is specified, the template option is ignored.

fix_template(Format, _Template, Bindings, NewTemplate) :-
    json_lang(Format),
    !,    
    exclude(anon, Bindings, NamedBindings),
    dict_create(NewTemplate, json, NamedBindings).
fix_template(_, Template, _, Template).

%%%%%


%!  send_error(+Error) is det.
%
%   Send an error to my parent.   Remove non-readable blobs from the
%   error term first using replace_blobs/2. If  the error contains a
%   stack-trace, this is resolved to a string before sending.

send_error(error(Formal, context(prolog_stack(Frames), Message))) :-
    is_list(Frames),
    !,
    with_output_to(string(Stack),
                   print_prolog_backtrace(current_output, Frames)),
    pengine_self(Self),
    replace_blobs(Formal, Formal1),
    replace_blobs(Message, Message1),
    pengine_reply(error(Self, error(Formal1,
                                    context(prolog_stack(Stack), Message1)))).
send_error(Error) :-
    pengine_self(Self),
    replace_blobs(Error, Error1),
    pengine_reply(error(Self, Error1)).

%!  replace_blobs(Term0, Term) is det.
%
%   Copy Term0 to Term, replacing non-text   blobs. This is required
%   for error messages that may hold   streams  and other handles to
%   non-readable objects.

replace_blobs(Blob, Atom) :-
    blob(Blob, Type), Type \== text,
    !,
    format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term) :-
    compound(Term0),
    !,
    compound_name_arguments(Term0, Name, Args0),
    maplist(replace_blobs, Args0, Args),
    compound_name_arguments(Term, Name, Args).
replace_blobs(Term, Term).



%!  fix_bindings(+Format, +EventIn, +Bindings, -Event) is det.
%
%   Generate the template for json(-s) Format  from the variables in
%   the asked Goal. Variables starting  with an underscore, followed
%   by an capital letter are ignored from the template.

fix_bindings(Format,
             ask(Goal, Options0), Bindings,
             ask(Goal, NewOptions)) :-
    json_lang(Format),
    !,
    exclude(anon, Bindings, NamedBindings),
    template(NamedBindings, Template, Options0, Options1),
    select_option(chunk(Paging), Options1, Options2, 1),
    NewOptions = [ template(Template),
                   chunk(Paging),
                   bindings(NamedBindings)
                 | Options2
                 ].
fix_bindings(_, Command, _, Command).

template(_, Template, Options0, Options) :-
    select_option(template(Template), Options0, Options),
    !.
template(Bindings, Template, Options, Options) :-
    dict_create(Template, json, Bindings).

anon(Name=_) :-
    sub_atom(Name, 0, _, _, '_'),
    sub_atom(Name, 1, 1, _, Next),
    char_type(Next, prolog_var_start).


%!  json_lang(+Format) is semidet.
%
%   True if Format is a JSON variation.

json_lang(json) :- !.
json_lang(Format) :-
    sub_atom(Format, 0, _, _, 'json-').



%!  output_result(+Format, +EventTerm) is det.
%!  output_result(+Format, +EventTerm, +OptionsDict) is det.
%
%   Formulate an HTTP response from a pengine event term. Format is
%   one of =prolog=, =json= or =json-s=.


output_result(Format, Event) :-
    output_result(Format, Event, _{}).

output_result(prolog, Event, _) :-
    !,
    format('Content-type: text/x-prolog; charset=UTF-8~n~n'),
    write_term(Event,
               [ quoted(true),
                 ignore_ops(true),
                 fullstop(true),
                 blobs(portray),
                 portray_goal(portray_blob),
                 nl(true)
               ]).
output_result(Lang, Event, Dict) :-
    write_result(Lang, Event, Dict),
    !.
output_result(Lang, Event, _) :-
    json_lang(Lang),
    !,
    (   message_to_json_data(Event, JSON, Lang)
    ->  % cors_enable, TODO: fix this!
	    format('Access-Control-Allow-Origin: ~p~n', [*]),
        disable_client_cache,
        reply_json(JSON)
    ;   assertion(message_to_json_data(Event, _, Lang))
    ).
output_result(Lang, _Event, _) :-    % FIXME: allow for non-JSON format
    domain_error(pengine_format, Lang).

%!  portray_blob(+Blob, +Options) is det.
%
%   Portray non-text blobs that may  appear   in  output  terms. Not
%   really sure about that. Basically such  terms need to be avoided
%   as they are meaningless outside the process. The generated error
%   is hard to debug though, so now we send them as `'$BLOB'(Type)`.
%   Future versions may include more info, depending on `Type`.

:- public portray_blob/2.               % called from write-term
portray_blob(Blob, _Options) :-
    blob(Blob, Type),
    writeq('$BLOB'(Type)).


%!  write_result(+Lang, +Event, +Dict) is semidet.
%
%   Hook that allows for different output formats. The core Pengines
%   library supports `prolog` and various   JSON  dialects. The hook
%   event_to_json/3 can be used to refine   the  JSON dialects. This
%   hook must be used if  a   completely  different output format is
%   desired.

%!  disable_client_cache
%
%   Make sure the client will not cache our page.
%
%   @see http://stackoverflow.com/questions/49547/making-sure-a-web-page-is-not-cached-across-all-browsers

disable_client_cache :-
    format('Cache-Control: no-cache, no-store, must-revalidate\r\n\c
            Pragma: no-cache\r\n\c
            Expires: 0\r\n').

message_to_json_data(Message, JSON, Lang) :-
    event_to_json(Message, JSON, Lang),
    !.
/* Experimental
message_to_json_data(DictIn, DictOut, json) :-
	is_dict(DictIn, Tag),
	!,
	DictOut = DictIn.put('$type', Tag).
*/
message_to_json_data(success(Pid, Bindings0, More),
                        json{type:success, pid:PidString,
                             data:Bindings, more:More},
                        json) :-
    !,
    term_string(Pid, PidString),
    term_to_json(Bindings0, Bindings).
message_to_json_data(success(Pid, Bindings0, Projection, Time, More),
                        json{type:success, pid:PidString, time:Time,
                             data:Bindings, more:More, projection:Projection},
                        json) :-
    !,
    term_string(Pid, PidString),
    term_to_json(Bindings0, Bindings).
message_to_json_data(failure(Pid, Time),
                     json{type:failure, pid:PidString, time:Time}, _) :-
    !,
    term_string(Pid, PidString).
message_to_json_data(error(Pid, ErrorTerm), Error, _Style) :-
    !,
    Error0 = json{type:error, pid:PidString, data:Message},
    add_error_details(ErrorTerm, Error0, Error),
    term_string(Pid, PidString),
    message_to_string(ErrorTerm, Message).
message_to_json_data(EventTerm, json{type:F, pid:PidString}, _Style) :-
    functor(EventTerm, F, 1),
    !,
    arg(1, EventTerm, Pid),
    term_string(Pid, PidString).
message_to_json_data(EventTerm, json{type:F, pid:PidString, data:JSON}, _Style) :-
    functor(EventTerm, F, 2),
    arg(1, EventTerm, Pid),
    term_string(Pid, PidString),
    arg(2, EventTerm, Data),
    term_to_json(Data, JSON).

:- public add_error_details/3.

%%  add_error_details(+Error, +JSON0, -JSON)
%
%   Add format error code and  location   information  to an error. Also
%   used by pengines_io.pl.

add_error_details(Error, JSON0, JSON) :-
    add_error_code(Error, JSON0, JSON1),
    add_error_location(Error, JSON1, JSON).

%%  add_error_code(+Error, +JSON0, -JSON) is det.
%
%   Add a =code= field to JSON0 of Error is an ISO error term. The error
%   code is the functor name of  the   formal  part  of the error, e.g.,
%   =syntax_error=,  =type_error=,  etc.   Some    errors   carry   more
%   information:
%
%     - existence_error(Type, Obj)
%     {arg1:Type, arg2:Obj}, where Obj is stringified of it is not
%     atomic.

add_error_code(error(existence_error(Type, Obj), _), Error0, Error) :-
    atom(Type),
    !,
    to_atomic(Obj, Value),
    Error = Error0.put(_{code:existence_error, arg1:Type, arg2:Value}).
add_error_code(error(Formal, _), Error0, Error) :-
    callable(Formal),
    !,
    functor(Formal, Code, _),
    Error = Error0.put(code, Code).
add_error_code(_, Error, Error).

% What to do with large integers?
to_atomic(Obj, Atomic) :- atom(Obj),   !, Atomic = Obj.
to_atomic(Obj, Atomic) :- number(Obj), !, Atomic = Obj.
to_atomic(Obj, Atomic) :- string(Obj), !, Atomic = Obj.
to_atomic(Obj, Atomic) :- term_string(Obj, Atomic).


%%  add_error_location(+Error, +JSON0, -JSON) is det.
%
%   Add a =location= property if the  error   can  be  associated with a
%   source location. The location is an   object  with properties =file=
%   and =line= and, if available, the character location in the line.

add_error_location(error(_, file(Path, Line, -1, _CharNo)), Term0, Term) :-
    atom(Path), integer(Line),
    !,
    Term = Term0.put(_{location:_{file:Path, line:Line}}).
add_error_location(error(_, file(Path, Line, Ch, _CharNo)), Term0, Term) :-
    atom(Path), integer(Line), integer(Ch),
    !,
    Term = Term0.put(_{location:_{file:Path, line:Line, ch:Ch}}).
add_error_location(_, Term, Term).


%!  event_to_json(+Event, -JSONTerm, +Lang) is semidet.
%
%   Hook that translates a Pengine event  structure into a term suitable
%   for reply_json/1, according to the language specification Lang. This
%   can be used to massage general Prolog terms, notably associated with
%   `success(Pid, Bindings, Projection,  Time,   More)`  and  `output(Pid,
%   Term)` into a format suitable for processing at the client side.

:- multifile format:event_to_json/3.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

prolog:error_message(sandbox(time_limit_exceeded, Limit)) -->
    [ 'Could not prove safety of your goal within ~f seconds.'-[Limit], nl,
      'This is normally caused by an insufficiently instantiated'-[], nl,
      'meta-call (e.g., call(Var)) for which it is too expensive to'-[], nl,
      'find all possible instantations of Var.'-[]
    ].
