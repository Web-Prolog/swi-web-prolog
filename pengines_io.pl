/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2017, VU University Amsterdam
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

:- module(pengines_io,
          [ pengine_writeln/1,          % +Term
            pengine_nl/0,
            pengine_flush_output/0,
            pengine_format/1,           % +Format
            pengine_format/2,           % +Format, +Args

            pengine_write_term/2,       % +Term, +Options
            pengine_write/1,            % +Term
            pengine_writeq/1,           % +Term
            pengine_display/1,          % +Term
            pengine_print/1,            % +Term
            pengine_write_canonical/1,  % +Term

            pengine_listing/0,
            pengine_listing/1,          % +Spec
            pengine_portray_clause/1,   % +Term

            pengine_read/1,             % -Term
            pengine_read_line_to_string/2, % +Stream, -LineAsString
            pengine_read_line_to_codes/2, % +Stream, -LineAsCodes

            pengine_io_predicate/1,     % ?Head
            pengine_bind_io_to_html/1,  % +Module
            pengine_io_goal_expansion/2 % +Goal, -Expanded
          ]).
:- use_module(library(lists)).
%:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(listing)).
:- use_module(library(yall)).
:- use_module(library(sandbox), []).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- if(exists_source(library(prolog_stream))).
:- use_module(library(prolog_stream)).
:- endif.
:- html_meta send_html(html).

:- meta_predicate
    pengine_format(+,:).

/** <module> Provide Prolog I/O for HTML clients

This module redefines some of  the   standard  Prolog  I/O predicates to
behave transparently for HTML clients. It  provides two ways to redefine
the standard predicates: using goal_expansion/2   and  by redefining the
system predicates using redefine_system_predicate/1. The   latter is the
preferred route because it gives a more   predictable  trace to the user
and works regardless of the use of other expansion and meta-calling.

*Redefining* works by redefining the system predicates in the context of
the pengine's module. This  is  configured   using  the  following  code
snippet.

  ==
  :- pengine_application(myapp).
  :- use_module(myapp:library(pengines_io)).
  format:prepare_module(Module, myapp, _Options) :-
        pengines_io:pengine_bind_io_to_html(Module).
  ==

*Using goal_expansion/2* works by  rewriting   the  corresponding  goals
using goal_expansion/2 and use the new   definition  to re-route I/O via
pengine_input/2 and pengine_output/1. A pengine  application is prepared
for using this module with the following code:

  ==
  :- pengine_application(myapp).
  :- use_module(myapp:library(pengines_io)).
  myapp:goal_expansion(In,Out) :-
        pengine_io_goal_expansion(In, Out).
  ==
*/

:- setting(write_options, list(any), [max_depth(1000)],
           'Additional options for stringifying Prolog results').


                 /*******************************
                 *            OUTPUT            *
                 *******************************/

%!  pengine_writeln(+Term)
%
%   Emit Term as <span class=writeln>Term<br></span>.

pengine_writeln(Term) :-
    pengine_output,
    !,
    pengine_module(Module),
    send_html(span(class(writeln),
                   [ \term(Term,
                           [ module(Module)
                           ]),
                     br([])
                   ])).
pengine_writeln(Term) :-
    writeln(Term).

%!  pengine_nl
%
%   Emit a <br/> to the pengine.

pengine_nl :-
    pengine_output,
    !,
    send_html(br([])).
pengine_nl :-
    nl.

%!  pengine_flush_output
%
%   No-op.  Pengines do not use output buffering (maybe they should
%   though).

pengine_flush_output :-
    pengine_output,
    !.
pengine_flush_output :-
    flush_output.

%!  pengine_write_term(+Term, +Options)
%
%   Writes term as <span class=Class>Term</span>. In addition to the
%   options of write_term/2, these options are processed:
%
%     - class(+Class)
%       Specifies the class of the element.  Default is =write=.

pengine_write_term(Term, Options) :-
    pengine_output,
    !,
    option(class(Class), Options, write),
    pengine_module(Module),
    send_html(span(class(Class), \term(Term,[module(Module)|Options]))).
pengine_write_term(Term, Options) :-
    write_term(Term, Options).

%!  pengine_write(+Term) is det.
%!  pengine_writeq(+Term) is det.
%!  pengine_display(+Term) is det.
%!  pengine_print(+Term) is det.
%!  pengine_write_canonical(+Term) is det.
%
%   Redirect the corresponding Prolog output predicates.

pengine_write(Term) :-
    pengine_write_term(Term, []).
pengine_writeq(Term) :-
    pengine_write_term(Term, [quoted(true), numbervars(true)]).
pengine_display(Term) :-
    pengine_write_term(Term, [quoted(true), ignore_ops(true)]).
pengine_print(Term) :-
    current_prolog_flag(print_write_options, Options),
    pengine_write_term(Term, Options).
pengine_write_canonical(Term) :-
    pengine_output,
    !,
    with_output_to(string(String), write_canonical(Term)),
    send_html(span(class([write, cononical]), String)).
pengine_write_canonical(Term) :-
    write_canonical(Term).

%!  pengine_format(+Format) is det.
%!  pengine_format(+Format, +Args) is det.
%
%   As format/1,2. Emits a series  of   strings  with <br/> for each
%   newline encountered in the string.
%
%   @tbd: handle ~w, ~q, etc using term//2.  How can we do that??

pengine_format(Format) :-
    pengine_format(Format, []).
pengine_format(Format, Args) :-
    pengine_output,
    !,
    format(string(String), Format, Args),
    split_string(String, "\n", "", Lines),
    send_html(\lines(Lines, format)).
pengine_format(Format, Args) :-
    format(Format, Args).


                 /*******************************
                 *            LISTING           *
                 *******************************/

%!  pengine_listing is det.
%!  pengine_listing(+Spec) is det.
%
%   List the content of the current pengine or a specified predicate
%   in the pengine.

pengine_listing :-
    pengine_listing(_).

pengine_listing(Spec) :-
    pengine_self(Module),
    with_output_to(string(String), listing(Module:Spec)),
    split_string(String, "", "\n", [Pre]),
    send_html(pre(class(listing), Pre)).

pengine_portray_clause(Term) :-
    pengine_output,
    !,
    with_output_to(string(String), portray_clause(Term)),
    split_string(String, "", "\n", [Pre]),
    send_html(pre(class(listing), Pre)).
pengine_portray_clause(Term) :-
    portray_clause(Term).


                 /*******************************
                 *         PRINT MESSAGE        *
                 *******************************/

:- multifile user:message_hook/3.

%!  user:message_hook(+Term, +Kind, +Lines) is semidet.
%
%   Send output from print_message/2 to   the  pengine. Messages are
%   embedded in a <pre class=msg-Kind></pre> environment.

user:message_hook(Term, Kind, Lines) :-
    Kind \== silent,
    engine_self(_),
    phrase(html(Lines), Tokens),
    with_output_to(string(HTMlString), print_html(Tokens)),
    (   source_location(File, Line)
    ->  Src = File:Line
    ;   Src = (-)
    ),
    pengine_output(message(Term, Kind, HTMlString, Src)).

message_lines([]) -->
    !.
message_lines([nl|T]) -->
    !,
    html('\n'),                     % we are in a <pre> environment
    message_lines(T).
message_lines([flush]) -->
    !.
message_lines([ansi(Attributes, Fmt, Args)|T]) -->
    !,
    { foldl(style, Attributes, Fmt-Args, HTML) },
    html(HTML),
    message_lines(T).
message_lines([H|T]) -->
    html(H),
    message_lines(T).

style(bold, Content, b(Content)).
style(fg(Color), Content, span(style('color:'+Color), Content)).
style(_, Content, Content).



                 /*******************************
                 *             INPUT            *
                 *******************************/

pengine_read(Term) :-
    pengine_input,
    !,
    prompt(Prompt, Prompt),
    pengine_input(Prompt, Term).
pengine_read(Term) :-
    read(Term).

pengine_read_line_to_string(From, String) :-
    pengine_input,
    !,
    must_be(oneof([current_input,user_input]), From),
    (   prompt(Prompt, Prompt),
        Prompt \== ''
    ->  true
    ;   Prompt = 'line> '
    ),
    pengine_input(_{type: console, prompt:Prompt}, StringNL),
    string_concat(String, "\n", StringNL).
pengine_read_line_to_string(From, String) :-
    read_line_to_string(From, String).

pengine_read_line_to_codes(From, Codes) :-
    pengine_read_line_to_string(From, String),
    string_codes(String, Codes).


                 /*******************************
                 *             HTML             *
                 *******************************/

lines([], _) --> [].
lines([H|T], Class) -->
    html(span(class(Class), H)),
    (   { T == [] }
    ->  []
    ;   html(br([])),
        lines(T, Class)
    ).

%!  send_html(+HTML) is det.
%
%   Convert html//1 term into a string and send it to the client
%   using pengine_output/1.

send_html(HTML) :-
    phrase(html(HTML), Tokens),
    with_output_to(string(HTMlString), print_html(Tokens)),
    pengine_output(HTMlString).


%!  pengine_module(-Module) is det.
%
%   Module (used for resolving operators).

pengine_module(Module) :-
    pengine_self(Pengine),
    !,
    pengine_property(Pengine, module(Module)).
pengine_module(user).

                 /*******************************
                 *        OUTPUT FORMAT         *
                 *******************************/

%!  format:event_to_json(+Event, -JSON, +Format, +VarNames) is semidet.
%
%   Provide additional translations for  Prolog   terms  to  output.
%   Defines formats are:
%
%     * 'json-s'
%     _Simple_ or _string_ format: Prolog terms are sent using
%     quoted write.
%     * 'json-html'
%     Serialize responses as HTML string.  This is intended for
%     applications that emulate the Prolog toplevel.  This format
%     carries the following data:
%
%       - data
%         List if answers, where each answer is an object with
%         - variables
%           Array of objects, each describing a variable.  These
%           objects contain these fields:
%           - variables: Array of strings holding variable names
%           - value: HTML-ified value of the variables
%           - substitutions: Array of objects for substitutions
%             that break cycles holding:
%             - var: Name of the inserted variable
%             - value: HTML-ified value
%         - residuals
%           Array of strings representing HTML-ified residual goals.

:- multifile
    format:event_to_json/3.

%!  format:event_to_json(+PrologEvent, -JSONEvent, +Format, +VarNames)
%
%   If Format equals `'json-s'` or  `'json-html'`, emit a simplified
%   JSON representation of the  data,   suitable  for notably SWISH.
%   This deals with Prolog answers and output messages. If a message
%   originates from print_message/3,  it   gets  several  additional
%   properties:
%
%     - message:Kind
%       Indicate the _kind_ of the message (=error=, =warning=,
%       etc.)
%     - location:_{file:File, line:Line, ch:CharPos}
%       If the message is related to a source location, indicate the
%       file and line and, if available, the character location.
   
format:event_to_json(success(Pid, Answers0, More), JSON, 'json-s') :-
    !,
    JSON = json{type:success, pid:PidString, data:Answers, more:More},
    maplist(dollar_expansion:wp_expand_answer, Answers0, Answers1),
    term_string(Pid, PidString),
    maplist(answer_to_json_strings(Pid), Answers1, Answers).
format:event_to_json(success(Pid, Answers0, Projection, Time, More), JSON, 'json-s') :-
    !,
    JSON0 = json{type:success, pid:PidString, time:Time, data:Answers, more:More},
    term_string(Pid, PidString),    
    maplist(answer_to_json_strings(Pid), Answers0, Answers),
    add_projection(Projection, JSON0, JSON).
format:event_to_json(output(Pid, Term), JSON, 'json-s') :-
    !,
    map_output(Pid, Term, JSON).
format:event_to_json(echo(Term), JSON, 'json-s') :-
    !,
    map_echo(Term, JSON).
    
add_projection([], JSON, JSON) :- !.
add_projection(VarNames, JSON0, JSON0.put(projection, VarNames)).


%!  answer_to_json_strings(+Pengine, +AnswerDictIn, -AnswerDict).
%
%   Translate answer dict with Prolog term   values into answer dict
%   with string values.

answer_to_json_strings(Pengine, DictIn, DictOut) :-
    dict_pairs(DictIn, Tag, Pairs),
    maplist(term_string_value(Pengine), Pairs, BindingsOut),
    dict_pairs(DictOut, Tag, BindingsOut).

term_string_value(Pengine, N-V, N-A) :-
    with_output_to(string(A),
                   write_term(V,
                              [ module(Pengine),
                                quoted(true)
                              ])).

%!  format:event_to_json(+Event, -JSON, +Format, +VarNames)
%
%   Implement translation of a Pengine  event to =json-html= format.
%   This format represents the answer  as   JSON,  but  the variable
%   bindings are (structured) HTML strings rather than JSON objects.
%
%   CHR residual goals are not bound to the projection variables. We
%   hacked a bypass to fetch these by   returning them in a variable
%   named   `Residuals`,   which   must   be   bound   to   a   term
%   '$residuals'(List).  Such  a  variable  is    removed  from  the
%   projection and added to residual goals.

format:event_to_json(success(Pid, Answers0, More),
                       JSON, 'json-html') :-
    !,
    JSON = json{type:success, pid:PidString, data:Answers, more:More},
    term_string(Pid, PidString),
    maplist(map_answer(Pid), Answers0, _, Answers).
format:event_to_json(success(Pid, Answers0, Projection, Time, More),
                       JSON, 'json-html') :-
    !,
    JSON0 = json{type:success, pid:PidString, time:Time, data:Answers, more:More},
    term_string(Pid, PidString),    
    maplist(map_answer(Pid), Answers0, ResVars, Answers),
    add_projection(Projection, ResVars, JSON0, JSON).
format:event_to_json(output(Pid, Term), JSON, 'json-html') :-
    !,
    map_output(Pid, Term, JSON).

map_answer(Pid, Bindings0, ResVars, Answer) :-
    dict_bindings(Bindings0, Bindings1),
    select_residuals(Bindings1, Bindings2, ResVars, Residuals0),
    append(Residuals0, Residuals1),
    prolog:translate_bindings(Bindings2, Bindings3, [], Residuals1,
                              Pid:Residuals-_HiddenResiduals),
    maplist(binding_to_html(Pid), Bindings3, VarBindings),
    (   Residuals == []
    ->  Answer = json{variables:VarBindings}
    ;   residuals_html(Residuals, Pid, ResHTML),
        Answer = json{variables:VarBindings, residuals:ResHTML}
    ).

residuals_html([], _, []).
residuals_html([H0|T0], Module, [H|T]) :-
    term_html_string(H0, [], Module, H, [priority(999)]),
    residuals_html(T0, Module, T).

dict_bindings(Dict, Bindings) :-
    dict_pairs(Dict, _Tag, Pairs),
    maplist([N-V,N=V]>>true, Pairs, Bindings).

select_residuals([], [], [], []).
select_residuals([H|T], Bindings, Vars, Residuals) :-
    binding_residual(H, Var, Residual),
    !,
    Vars = [Var|TV],
    Residuals = [Residual|TR],
    select_residuals(T, Bindings, TV, TR).
select_residuals([H|T0], [H|T], Vars, Residuals) :-
    select_residuals(T0, T, Vars, Residuals).

binding_residual('_residuals' = '$residuals'(Residuals), '_residuals', Residuals) :-
    is_list(Residuals).
binding_residual('Residuals' = '$residuals'(Residuals), 'Residuals', Residuals) :-
    is_list(Residuals).
binding_residual('Residual'  = '$residual'(Residual),   'Residual', [Residual]) :-
    callable(Residual).

add_projection(-, _, JSON, JSON) :- !.
add_projection(VarNames0, ResVars0, JSON0, JSON) :-
    append(ResVars0, ResVars1),
    sort(ResVars1, ResVars),
    subtract(VarNames0, ResVars, VarNames),
    add_projection(VarNames, JSON0, JSON).


%!  binding_to_html(+Pengine, +Binding, -Dict) is det.
%
%   Convert a variable binding into a JSON Dict. Note that this code
%   assumes that the module associated  with   Pengine  has the same
%   name as the Pengine.  The module is needed to
%
%   @arg Binding is a term binding(Vars,Term,Substitutions)

binding_to_html(Pid, binding(Vars,Term,Substitutions), JSON) :-
    JSON0 = json{variables:Vars, value:HTMLString},
    term_html_string(Term, Vars, Pid, HTMLString, [priority(699)]),
    (   Substitutions == []
    ->  JSON = JSON0
    ;   maplist(subst_to_html(Pid), Substitutions, HTMLSubst),
        JSON = JSON0.put(substitutions, HTMLSubst)
    ).

%!  term_html_string(+Term, +VarNames, +Module, -HTMLString,
%!                   +Options) is det.
%
%   Translate  Term  into  an  HTML    string   using  the  operator
%   declarations from Module. VarNames is a   list of variable names
%   that have this value.

term_html_string(Term, Vars, Module, HTMLString, Options) :-
    setting(write_options, WOptions),
    merge_options(WOptions,
                  [ quoted(true),
                    numbervars(true),
                    module(Module)
                  | Options
                  ], WriteOptions),
    phrase(term_html(Term, Vars, WriteOptions), Tokens),
    with_output_to(string(HTMLString), print_html(Tokens)).

%!  binding_term(+Term, +Vars, +WriteOptions)// is semidet.
%
%   Hook to render a Prolog result term as HTML. This hook is called
%   for each non-variable binding,  passing   the  binding  value as
%   Term, the names of the variables as   Vars and a list of options
%   for write_term/3.  If the hook fails, term//2 is called.
%
%   @arg    Vars is a list of variable names or `[]` if Term is a
%           _residual goal_.

:- multifile binding_term//3.

term_html(Term, Vars, WriteOptions) -->
    { nonvar(Term) },
    binding_term(Term, Vars, WriteOptions),
    !.
term_html(Term, _Vars, WriteOptions) -->
    term(Term, WriteOptions).

%!  subst_to_html(+Module, +Binding, -JSON) is det.
%
%   Render   a   variable   substitution     resulting   from   term
%   factorization, in this case breaking a cycle.

subst_to_html(Pid, '$VAR'(Name)=Value, json{var:Name, value:HTMLString}) :-
    !,
    term_html_string(Value, [Name], Pid, HTMLString, [priority(699)]).
subst_to_html(_, Term, _) :-
    assertion(Term = '$VAR'(_)).


%!  map_output(+Pid, +Term, -JSON) is det.
%
%   Map an output term. This is the same for json-s and json-html.

map_output(Pid, message(Term, Kind, HTMLString, Src), JSON) :-
    atomic(HTMLString),
    !,
    JSON0 = json{type:output, pid:PidString, message:Kind, data:HTMLString},
    term_string(Pid, PidString), 
    format:add_error_details(Term, JSON0, JSON1),
    (   Src = File:Line,
        \+ JSON1.get(location) = _
    ->  JSON = JSON1.put(_{location:_{file:File, line:Line}})
    ;   JSON = JSON1
    ).
map_output(Pid, Term, json{type:output, pid:PidString, data:Data}) :-
    term_string(Pid, PidString),  
    (   atomic(Term)
    ->  Data = Term
    ;   is_dict(Term, json),
        ground(json)                % TBD: Check proper JSON object?
    ->  Data = Term
    ;   term_string(Term, Data)
    ).

%!  map_echo(+Term, -JSON) is det.
%
%   Map an echo term. This is the same for json-s and json-html.

map_echo(Term, json{type:echo, data:Data}) :-
    (   atomic(Term)
    ->  Data = Term
    ;   is_dict(Term, json),
        ground(json)                % TBD: Check proper JSON object?
    ->  Data = Term
    ;   term_string(Term, Data)
    ).



                 /*******************************
                 *          SANDBOXING          *
                 *******************************/

:- multifile
    sandbox:safe_primitive/1,       % Goal
    sandbox:safe_meta/2.            % Goal, Called

sandbox:safe_primitive(pengines_io:pengine_listing(_)).
sandbox:safe_primitive(pengines_io:pengine_nl).
sandbox:safe_primitive(pengines_io:pengine_print(_)).
sandbox:safe_primitive(pengines_io:pengine_write(_)).
sandbox:safe_primitive(pengines_io:pengine_read(_)).
sandbox:safe_primitive(pengines_io:pengine_write_canonical(_)).
sandbox:safe_primitive(pengines_io:pengine_write_term(_,_)).
sandbox:safe_primitive(pengines_io:pengine_writeln(_)).
sandbox:safe_primitive(pengines_io:pengine_writeq(_)).
sandbox:safe_primitive(pengines_io:pengine_portray_clause(_)).
sandbox:safe_primitive(system:write_term(_,_)).
sandbox:safe_primitive(system:prompt(_,_)).
sandbox:safe_primitive(system:statistics(_,_)).

sandbox:safe_meta(pengines_io:pengine_format(Format, Args), Calls) :-
    sandbox:format_calls(Format, Args, Calls).


                 /*******************************
                 *         REDEFINITION         *
                 *******************************/

%!  pengine_io_predicate(?Head)
%
%   True when Head describes the  head   of  a (system) IO predicate
%   that is redefined by the HTML binding.

pengine_io_predicate(writeln(_)).
pengine_io_predicate(nl).
pengine_io_predicate(flush_output).
pengine_io_predicate(format(_)).
pengine_io_predicate(format(_,_)).
pengine_io_predicate(read(_)).
pengine_io_predicate(read_line_to_string(_,_)).
pengine_io_predicate(read_line_to_codes(_,_)).
pengine_io_predicate(write_term(_,_)).
pengine_io_predicate(write(_)).
pengine_io_predicate(writeq(_)).
pengine_io_predicate(display(_)).
pengine_io_predicate(print(_)).
pengine_io_predicate(write_canonical(_)).
pengine_io_predicate(listing).
pengine_io_predicate(listing(_)).
pengine_io_predicate(portray_clause(_)).

term_expansion(pengine_io_goal_expansion(_,_),
               Clauses) :-
    findall(Clause, io_mapping(Clause), Clauses).

io_mapping(pengine_io_goal_expansion(Head, Mapped)) :-
    pengine_io_predicate(Head),
    Head =.. [Name|Args],
    atom_concat(pengine_, Name, BodyName),
    Mapped =.. [BodyName|Args].

pengine_io_goal_expansion(_, _).


                 /*******************************
                 *      REBIND PENGINE I/O      *
                 *******************************/

:- public
    stream_write/2,
    stream_read/2,
    stream_close/1.

:- thread_local
    pengine_io/2.

stream_write(_Stream, Out) :-
    send_html(pre(class(console), Out)).
stream_read(_Stream, Data) :-
    prompt(Prompt, Prompt),
    pengine_input(_{type:console, prompt:Prompt}, Data).
stream_close(_Stream).

%!  pengine_bind_user_streams
%
%   Bind the pengine user  I/O  streams   to  a  Prolog  stream that
%   redirects  the  input  and   output    to   pengine_input/2  and
%   pengine_output/1. This results in  less   pretty  behaviour then
%   redefining the I/O predicates to  produce   nice  HTML, but does
%   provide functioning I/O from included libraries.

pengine_bind_user_streams :-
    Err = Out,
    open_prolog_stream(pengines_io, write, Out, []),
    set_stream(Out, buffer(line)),
    open_prolog_stream(pengines_io, read,  In, []),
    set_stream(In,  alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In,  alias(current_input)),
    set_stream(Out, alias(current_output)),
    assertz(pengine_io(In, Out)),
    thread_at_exit(close_io).

close_io :-
    retract(pengine_io(In, Out)),
    !,
    close(In, [force(true)]),
    close(Out, [force(true)]).
close_io.

%!  pengine_output is semidet.
%!  pengine_input is semidet.
%
%   True when output (input) is redirected to a pengine.

pengine_output :-
    current_output(Out),
    pengine_io(_, Out).

pengine_input :-
    current_input(In),
    pengine_io(In, _).


%!  pengine_bind_io_to_html(+Module)
%
%   Redefine the built-in predicates for IO   to  send HTML messages
%   using pengine_output/1.

pengine_bind_io_to_html(Module) :-
    forall(pengine_io_predicate(Head),
           bind_io(Head, Module)),
    pengine_bind_user_streams.

bind_io(Head, Module) :-
    prompt(_, ''),
    redefine_system_predicate(Module:Head),
    functor(Head, Name, Arity),
    Head =.. [Name|Args],
    atom_concat(pengine_, Name, BodyName),
    Body =.. [BodyName|Args],
    assertz(Module:(Head :- Body)),
    compile_predicates([Module:Name/Arity]).
