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

:- module(distribution,
          [ spawn_remote/4,                     % +Node, :Goal, -Id, +Options
            send_remote/2,                      % +Id, +Message
            self_remote/1,                      % -Id
            register_node_self/1,               % +URL
            echo/1,
            op(200, xfx, @)
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(option)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).

:- use_module(actors).
:- use_module(pengines2).
:- use_module(isolation).
:- use_module(format).
:- use_module(pengines_io).


:- use_module(library(random)).
actor_uuid(Id) :-
    Max is 1<<128,
    random_between(0, Max, Num),
    atom_number(Id, Num).


:- dynamic
    websocket/3,                        % Node, Thread, Socket
    self_node/1.                        % Node

:- dynamic
    pid_stdout_socket_format/4.           % Pid, Stdout, Socket, Format
    

:- thread_local
    stdout/1.                             % Target
    
    

:- http_handler(root(ws), node_manager, [spawn([]), id(ws)]).

node_manager(Request) :-
    http_upgrade_to_websocket(node_loop, [subprotocols(['pcp-0.2'])], Request).

node_loop(Socket) :-
    ws_receive(Socket, Message, [format(json)]),
    (   Message.opcode == close
    ->  retractall(websocket(_, _, Socket)),
        retract(pid_stdout_socket_format(Pid, _, Socket, _)),
		engine_destroy(Pid),
        thread_self(Me),
        thread_detach(Me)
    ;   Data = Message.data,
        debug(ws, 'Got ~p', [Data]),
        atom_string(Action, Data.command),
        node_action(Action, Data, Socket),
        node_loop(Socket)
    ).



node_action(self, Data, Socket) :-
%    _{options:OptionString} :< Data,
    !,
%    term_string(Options, OptionString),
	actor_uuid(Pid),
%    select_option(format(Format), Options, _RestOptions, 'json-s'),
    assertz(pid_stdout_socket_format(Pid, Pid, Socket, json)),
    send(Pid, self(Pid)).

node_action(!, Data, _Socket) :-
    (	_{name:TargetString, message:MessageString} :< Data
	;	_{pid:TargetString, message:MessageString} :< Data
	),
    !,
    term_string(Target, TargetString),
    term_string(Message, MessageString),
    send(Target, Message).


% clauses for pengines running from a shell    

node_action(pengine_spawn, Data, Socket) :-
    _{options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
	actor_uuid(UUID),
    option(reply_to(Stdout), Options, UUID),
    assertz(actors:stdout(Stdout)),
    select_option(format(Format), Options, RestOptions, 'json-s'),
    pengine_spawn(Pid, [sandboxed(false),reply_to(Stdout)|RestOptions]),
    assertz(pid_stdout_socket_format(Pid, Stdout, Socket, Format)),
    send(Stdout, spawned(Pid)).
node_action(pengine_spawn, _Data, Socket) :-
    !,
	actor_uuid(UUID),
    assertz(actors:stdout(UUID)),
    pengine_spawn(Pid, [sandboxed(false),reply_to(UUID)]),
    assertz(pid_stdout_socket_format(Pid, UUID, Socket, json)),
    send(UUID, spawned(Pid)).
node_action(pengine_ask, Data, Socket) :-
    _{pid:PidString, query:GoalString, options:OptionString} :< Data,
    !,
    read_term_from_atom(GoalString, Goal, [variable_names(Bindings)]), 
    term_string(Options, OptionString),
    term_string(Pid, PidString),
    pid_stdout_socket_format(Pid, _Target, Socket, Format),
    fix_template(Format, Goal, Bindings, NewTemplate),
    pengine_ask(Pid, Goal, [template(NewTemplate)|Options]).
node_action(pengine_ask, Data, Socket) :-
    _{pid:PidString, query:GoalString} :< Data,
    !,
    read_term_from_atom(GoalString, Goal, [variable_names(Bindings)]),    
    term_string(Pid, PidString),
    pid_stdout_socket_format(Pid, _Target, Socket, Format),
    fix_template(Format, Goal, Bindings, NewTemplate),
    pengine_ask(Pid, Goal, [template(NewTemplate)]).   
node_action(pengine_next, Data, _Socket) :-
    _{pid:PidString, options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    term_string(Pid, PidString),
    pengine_next(Pid, Options).    
node_action(pengine_next, Data, _Socket) :-
    _{pid:PidString} :< Data,
    !,
    term_string(Pid, PidString),
    pengine_next(Pid). 
node_action(pengine_stop, Data, _Socket) :-
    _{pid:PidString, options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    term_string(Pid, PidString),
    pengine_stop(Pid, Options).
node_action(pengine_stop, Data, _Socket) :-
    _{pid:PidString} :< Data,
    !,
    term_string(Pid, PidString),
    pengine_stop(Pid).
node_action(pengine_respond, Data, _Socket) :-
    _{pid:PidString, term:String} :< Data,
    !,
    term_string(Term, String),
    term_string(Pid, PidString),
    pengine_respond(Pid, Term).
node_action(pengine_abort, Data, _Socket) :-
    _{pid:PidString} :< Data,
    !,
    term_string(Pid, PidString),
    pengine_abort(Pid).

% clauses for bare actors    

node_action(spawn, Data, Socket) :-
    _{thread:Creator, prolog:String, options:OptionString} :< Data,
    !,
    term_string(Goal, String),
    term_string(Options, OptionString),
    spawn(Goal, Pid, [sandboxed(false)|Options]),
    ws_send(Socket, json(_{command:spawned, thread:Creator, pid:Pid})).
node_action(spawned, Data, _Socket) :-
    _{thread:CreatorId, pid:Pid} :< Data,
    !,
    thread_property(Creator, id(CreatorId)),
    canonical_pid(Pid, CanPid),
    thread_send_message(Creator, spawned(CanPid)).
node_action(send, Data, _Socket) :-
    _{receiver:PidString, prolog:String} :< Data,
    !,
    term_string(Message, String),
    atom_string(Pid, PidString),
    send(Pid, Message).
node_action(send, Data, _Socket) :-
    _{thread:Id, prolog:String} :< Data,
    !,
    term_string(Message, String),
    thread_property(Engine, id(Id)),
    send(thread(Engine), Message).
node_action(_Action, Data, _Socket) :-
    debug(ws, 'Got unknown data: ~p', [Data]).



:- listen(actor(down, Pid),
          debug(ws, 'Actor is down: ~p', [Pid])).
          

canonical_pid(Raw, Pid) :-
    (   string(Raw)
    ->  atom_string(Pid, Raw)
    ;   Pid = Raw
    ).

%!  connection(+Node, -Socket)
%
%   Return an existing connection or create a connection to Node.

connection(Node, Socket) :-
    websocket(Node, _Thread, Socket),
    !.
connection(Node, Socket) :-
    atom_concat(Node, '/ws', WsURI),
    http_open_websocket(WsURI, Socket, [subprotocol('pcp-0.2')]),
    thread_create(node_loop(Socket), Thread, [alias(Node)]),
    assertz(websocket(Node, Thread, Socket)).

%!  spawn_remote(+Node, :Goal, -Id, +Options)
%
%   Spawn a process on a remote node.

spawn_remote(Node, Goal, Id@Node, Options0) :-
    connection(Node, Socket),
    term_string(Goal, String),
    strip_module(Goal, SelfModule, _),
    translate_local_sources(Options0, Options, SelfModule),
    term_string(Options, OptionString),
    thread_self(Me),
    thread_property(Me, id(MyId)),
    ws_send(Socket, json(_{command:spawn, thread:MyId, prolog:String, options:OptionString})),
    thread_get_message(Me, spawned(Id)).


%!  send_remote(Id, Message) :-
%
%   Send a message to a remote process.
%
%   @tbd: 

send_remote(Target, Message) :-
    pid_stdout_socket_format(_, Target, Socket, Format),
    !,
    message_to_json_data(Message, Json, Format),
    ws_send(Socket, json(Json)).
send_remote(thread(Id)@Node, Message) :-
    !,
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{command:send, thread:Id, prolog:String})).
send_remote(Id@Node, Message) :-
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{command:send, receiver:Id, prolog:String})).

%!  self_remote(-Self)
%
%   Get a global identifier for self.

self_remote(Id@Node) :-
    engine_self(Id),
    !,
    self_node(Node).
self_remote(thread(Id)@Node) :-
    thread_self(Thread),
    !,
    self_node(Node),
    thread_property(Thread, id(Id)).

%!  register_node_self(+URL)
%
%   Register the name by which this node is known

register_node_self(URL) :-
    asserta(self_node(URL)).


%!  echo(+Term) is det.
%
%   Send Term to the shell. If the actor executing echo/1 has no 
%   ancestor connected to a shell, nothing happens.

echo(_Term) :-
    actors:stdout(null),
    !.
echo(Term) :-
    actors:stdout(Shell), 
    !,
    send(Shell, echo(Term)).
echo(_Term).  


		 /*******************************
		 *    EXTEND LOCAL PROCESSES	*
		 *******************************/

:- multifile
    actors:hook_self/1.

actors:hook_self(Me) :-
    self_remote(Me).

actors:hook_spawn(Goal, Engine, Options) :-
    select_option(node(Node), Options, RestOptions, localnode),
    Node \== localnode,
    !,
    spawn_remote(Node, Goal, Engine, RestOptions).

actors:hook_send(Id@Node, Message) :-
    !,
    (   self_node(Node)
    ->  (   Id = thread(Tid)
        ->  thread_property(Thread, id(Tid)),
            send(thread(Thread), Message)
        ;   send(Id, Message)
        )
    ;   send_remote(Id@Node, Message)
    ).
actors:hook_send(Socket, Message) :-
    send_remote(Socket, Message).

