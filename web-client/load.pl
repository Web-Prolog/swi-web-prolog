% Main file to load the pengines demo.  This file is included from
%
%   - debug.pl for local debugging
%   - daemon.pl to run pengines as a Unix service

%:- use_module(library(pengines)).
:- use_module(library(http/http_error)).
:- use_module(server).
:- use_module(storage).

:- use_module('../web_prolog.pl').
%:- use_module(lib/admin/admin).
%:- use_module(lib/admin/server_statistics).
:- use_module(lib/admin/change_passwd).

/*
:- multifile
	pengines:prepare_module/3.

:- pengine_application(swish).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).
	

%swish:(goal_expansion(In,Out) :-
%	pengine_io_goal_expansion(In, Out)).
% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.
:- use_module(library(clpfd), []).
*/