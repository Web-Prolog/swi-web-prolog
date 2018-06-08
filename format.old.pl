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

:- module(format, 
        [ respond/2,
          fix_template/4,
          answer_format/3
        ]).

:- use_module(library(http/http_json)).
:- use_module(library(term_to_json)).
:- use_module(library(apply)).

:- use_module(dollar_expansion).

:- use_module(library(debug)).


         /*******************************
         *     RESPONSE GENERATION      *
         *******************************/
         

respond(prolog, Answer) :- !,
    format('Content-type: text/plain;~n~n'),
    format("~q.", [Answer]).
respond(Format, Answer) :-
    json_lang(Format), !,    
    answer_format(Answer, JSON, Format),
    reply_json(JSON).
    
    

%!  fix_bindings(+Format, +Template, +Bindings, -NewTemplate) is det.
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


%!  json_lang(+Format) is semidet.
%
%   True if Format is a JSON variation.

json_lang(json) :- !.
json_lang(Format) :-
    sub_atom(Format, 0, _, _, 'json-').
    
    
anon(Name=_) :-
    sub_atom(Name, 0, _, _, '_'),
    sub_atom(Name, 1, 1, _, Next),
    char_type(Next, prolog_var_start).

    
    

%!  answer_format(+PrologMessage, -JsonMessage, +Format) is det.
%

answer_format(spawned(Pid),
              json{type:spawned, pid:Pid},
              'json-s') :- !.           
answer_format(success(_Pid, Answers0, More), JSON,
              'json-s') :- !,
    JSON = json{type:success, pid:anonymous, data:Answers, more:More},
    maplist(wp_expand_answer, Answers0, Answers1),
    maplist(answer_to_json_strings, Answers1, Answers).             
answer_format(failure(_Pid),
              json{type:failure, pid:anonymous},
              'json-s') :- !.
answer_format(stop(_Pid),
              json{type:stop, pid:anonymous},
              'json-s') :- !.
answer_format(error(_Pid, ErrorTerm),
              json{type:error, pid:anonymous, data:Message},
              'json-s') :- !,
    message_to_string(ErrorTerm, Message).
answer_format(prompt(_Pid, Term),
              json{type:prompt, pid:anonymous, data:Term},
              'json-s') :- !.
answer_format(output(_Pid, Term),
              json{type:output, pid:anonymous, data:JSON},
              'json-s') :- !,
    map_output(Term, JSON).
answer_format(echo(Term),
              json{type:echo, data:JSON},
              'json-s') :- !,
    map_output(Term, JSON).
answer_format(down(_Pid, ErrorTerm),
              json{type:down, pid:anonymous, data:Message},
              'json-s') :- !,
    message_to_string(ErrorTerm, Message).
              
answer_format(Answer, Answer, prolog).
    

map_output(Term, Data) :-
    (   atomic(Term)
    ->  Data = Term
    ;   is_dict(Term, json),
        ground(json)        % TBD: Check proper JSON object?
    ->  Data = Term
    ;   term_string(Term, Data)
    ). 
    
%%    answer_to_json_strings(+AnswerDictIn, -AnswerDict).
%
%    Translate answer dict with Prolog term   values into answer dict
%    with string values.

answer_to_json_strings(DictIn, DictOut) :-
    dict_pairs(DictIn, Tag, Pairs),
    maplist(term_string_value, Pairs, BindingsOut),
    dict_pairs(DictOut, Tag, BindingsOut).

term_string_value(N-V, N-A) :-
    with_output_to(string(A),write_term(V,
                  [ quoted(true)
                  ])).

