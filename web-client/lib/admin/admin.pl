:- module(admin, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pairs)).
:- use_module(library(settings)).
:- use_module(library(pengines)).


:- http_handler(root(admin/show_settings), show_settings, []).
:- http_handler(root(admin/set_settings), set_settings, []).


%%	show_settings(+Request) is det
%

show_settings(Request) :-
    http_parameters(Request,
            [   type(Type, [])
            ]),
    (	Type == server
    ->	findall(M-N, (current_setting(M:N), server_properties(M)), List)
    ;	Type == applications
    ->	findall(M-N, (current_setting(M:N), application_properties(M)), List)
    ),
    keysort(List, Sorted0),
    delete(Sorted0, pengine-_, Sorted),
    group_pairs_by_key(Sorted, ByModule),
    show_modules(ByModule, JsonByModule),
    reply_json(json([settings=JsonByModule])).


server_properties(http).
server_properties(storage).

application_properties(Module) :-
	current_pengine_application(Module).

show_modules([], []).
show_modules([M-List|T], [Json|JsonList]) :-
	show_module(M, List, Json),
	show_modules(T, JsonList).

show_module(Module, Settings, json([module=Header, settings=JsonSettings])) :-
	show_module_header(Module, Header),
	show_settings(Settings, Module, JsonSettings).

show_module_header(Module, Module).

show_settings([], _, []).
show_settings([H|T], Module, [Json|JsonList]) :-
	show_setting(H, Module, Json),
	show_settings(T, Module, JsonList).

show_setting(H, Module, json([comment=Comment, type=Type, name=Name, value=Value, default=Default])) :-
	setting_property(Module:H, comment(Comment)),
	setting_property(Module:H, type(Type0)),
	setting_property(Module:H, default(Default0)),
	term_to_atom(Default0, Default),
	type_to_json(Type0, Type),
	setting_name(Module:H, Name),
	setting(Module:H, Value0),
	term_to_atom(Value0, Value).


setting_name(Module:Setting, Name) :-
	format(atom(Name), '~q-~q', [Module,Setting]).


type_to_json(oneof(List), json([kind=oneof, options=List])) :- !.
type_to_json(list(atom), json([kind=atomlist])) :- !.
type_to_json(list(compound), json([kind=compoundlist])) :- !.
type_to_json(Type, Type).



%%	set_settings(+Request) is det
%

set_settings(Request) :-
    http_parameters(Request,
            [   name(NameAtom, []),
                value(ValueAtom, [])
            ]),
    atom_to_term(NameAtom, Module:Setting, _),
    format(atom(Name), '~q-~q', [Module,Setting]),
    catch(atom_to_term(ValueAtom, Value, _), Error, true),
    (   var(Error)
    ->  catch(set_setting(Module:Setting, Value), Error, true),
        (   var(Error)
        ->  setting_property(Module:Setting, default(Default)),
            reply_json(json([name=Name, value=ValueAtom, default=Default]))
        ;   message_to_string(Error, Msg),
            reply_json(json([error= @true, msg=Msg, name=Name]))
        )
    ;   message_to_string(Error, Msg),
        reply_json(json([error= @true, msg=Msg, name=Name]))
    ).







