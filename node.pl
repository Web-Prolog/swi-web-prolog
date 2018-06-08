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

:- module(node,
          [ node/0,
            node/1
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_host), []).
:- use_module(library(settings)).

:- use_module(distribution).

%!  node is det.
%!  node(+Address) is det.
%
%   Start the HTTP server for   accepting websocket connections. Address
%   is either of the form `localhost:Port` or a plain `Port`. Default is
%   `localhost:3060`.
%
%   This predicate sets the notion of   the node's _self_ address, which
%   is needed for self/1.  This is determined as follows:
%
%     - If the address is `localhost:Port`, use
%       http://localhost:Port
%     - If the setting http:public_host is provided, use that
%     - Else use the host as known by gethostname/1.

node :-
    node(localhost:3060).

node(Address) :-
    http_server(http_dispatch,
                [ port(Address)
                ]),
    server_url(Address, URL),
    register_node_self(URL).

server_url(localhost:Port, URL) :-
    !,
    format(atom(URL), 'http://localhost:~w', [Port]).
server_url(_Port, URL) :-
    setting(http:public_host, Host),
    Host \== '',
    !,
    setting(http:public_port, Port),
    setting(http:public_scheme, Scheme),
    make_url(Scheme, Host, Port, URL).
server_url(Port, URL) :-
    gethostname(Host),
    http_server_property(Port, scheme(Scheme)),
    make_url(Scheme, Host, Port, URL).

make_url(Scheme, Host, Port, URL) :-
    (   default_port(Scheme, Port)
    ->  format(atom(URL), '~w://~w', [Scheme, Host])
    ;   format(atom(URL), '~w://~w:~w', [Scheme, Host, Port])
    ).

default_port(http, 80).
default_port(https, 443).
