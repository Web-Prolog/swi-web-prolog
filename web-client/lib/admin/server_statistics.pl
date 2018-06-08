/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
			 VU University Amsterdam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(server_statistics, []).
:- use_module(library(pairs)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_stream)).
:- use_module(library(pengines)).

:- http_handler(root(statistics), statistics, []).

%%	statistics(+Request)
%
%	HTTP handler that returns a JSON object describing the server
%	state.

statistics(_Request) :-
    server_statistics(Stats),
    reply_json(Stats).

%%	server_statistics
%
%

server_statistics(_{ servers:Servers,
		     requests:Count,
		     bytes_sent:Sent,
		     pengines:Pengines
		   }) :-
	findall(Port-ID, http_current_worker(Port, ID), Workers),
	group_pairs_by_key(Workers, Servers0),
	servers_stats(Servers0, Servers),
	cgi_statistics(requests(Count)),
	cgi_statistics(bytes_sent(Sent)),
	pengine_statistics(Pengines).


servers_stats([], []).
servers_stats([H|T], [Json|List]) :-
	server_stats2(H, Json),
	servers_stats(T, List).


server_stats2(Port-Workers,
	      _{port:Port, started:ST, cputime:CPU, workers:N}) :-
	length(Workers, N),
	http_server_property(Port, start_time(StartTime)),
	format_time(string(ST), '%+', StartTime),
	statistics(process_cputime, CPU).

%%	pengine_statistics(-Stats) is det.
%
%	Stats is a list of dicts holding statistics per running pengine.

pengine_statistics(Stats) :-
	findall(Stat, pengine_stats(_Pengine, Stat), Stats).

pengine_stats(Pengine, Stats) :-
	pengine_property(Pengine, self(_)),
	catch(pengine_stats2(Pengine, Stats), _, fail).

pengine_stats2(Pengine, pengine{type:remote, application:App}) :-
	pengine_property(Pengine, remote(_Server)), !,
	pengine_property(Pengine, application(App)).
pengine_stats2(Pengine,
	      pengine{type:local,
		      application:App,
		      cputime:CPU,
		      stacks:_{ global:Global,
				local:Local,
				trail:Trail
			      }
		     }) :- !,
	pengine_property(Pengine, application(App)),
	pengine_property(Pengine, thread(Thread)),
	thread_statistics(Thread, cputime,    CPU),
	thread_statistics(Thread, globalused, Global),
	thread_statistics(Thread, localused,  Local),
	thread_statistics(Thread, trailused,  Trail).
