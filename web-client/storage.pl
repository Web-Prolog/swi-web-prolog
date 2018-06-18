% This system may only distributed using the GNU General Public License
% because the following components contain GPL-ed code:
%
%     /opt/local/lib/swipl-6.3.15/library/mime.pl
%     GNU Readline library
%
% See http://www.swi-prolog.org/license.html for details on
% SWI-Prolog licensing policies supporting both free and non-free
% Software.

:- module(storage, []).

% http library modules
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_host)).
:- use_module(library(settings)).
:- use_module(library(url)).
:- use_module(library(filesex)).

:- setting(storage_dir, atom, storage, 'The directory for storing files.').

user:file_search_path(storage, Dir) :-
	setting(storage_dir, Dir).

:- http_handler(root(storage), serve_files_in_directory(storage), [prefix]).
:- http_handler(root(storage/store), store, []).
:- http_handler(root(storage/update), update, []).


:- if(current_predicate(uuid/2)).
file_uuid(Id) :-
    uuid(Id, [version(4)]).        % Version 4 is random.
:- else.
:- use_module(library(random)).
file_uuid(Id) :-
    Max is 1<<128,
    random_between(0, Max, Num),
    atom_number(Id, Num).
:- endif.


store(Request) :-
    http_parameters(Request,
        [   program(Program, []),
            type(Type, [default(pl)])
        ]),
	setting(storage_dir, Dir),
    file_uuid(Base),
    make_directory_path(Dir),
    file_name_extension(Base, Type, File),
    directory_file_path(Dir, File, RelPath),
    http_current_host(Request, Hostname, Port, []),
    parse_url(URL, [
        protocol(http),
        host(Hostname),
        port(Port)
    ]),
    setup_call_cleanup(open(RelPath, write, S), write(S, Program), close(S)),
    reply_json(json([url=URL, file=File]), [width(0)]).



update(Request) :-
    http_parameters(Request,
        [   file(File, []),
	    program(Program, [])
        ]),
	setting(storage_dir, Dir),
    directory_file_path(Dir, File, RelPath),
    setup_call_cleanup(open(RelPath, write, S), write(S, Program), close(S)),
    reply_json(json([ok= @true]), [width(0)]).
