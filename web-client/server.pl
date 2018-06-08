:- module(server,
      [ ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/http_server_files)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(http/http_authenticate)).



/** <module> Prolog Web Server

*/


:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(app, Dir)).

user:file_search_path(www, app(www)).
user:file_search_path(apps, app(apps)).


:- http_handler(root(apps), serve_files_in_directory(apps), [prefix]).
:- http_handler(root(.), serve_files_in_directory(www), [prefix]).

:- http_handler(root(tutorial), http_reply_file('www/tutorial.html', []), [prefix]).
:- http_handler(root(admin/server), http_reply_file('www/admin/server.html', []), [prefix, authentication(basic(passwd, admin))]).
:- http_handler(root(admin/applications), http_reply_file('www/admin/applications.html', []), [prefix, authentication(basic(passwd, admin))]).
:- http_handler(root(admin/statistics), http_reply_file('www/admin/statistics.html', []), [prefix]).
:- http_handler(root(admin/account), http_reply_file('www/admin/account.html', []), [prefix, authentication(basic(passwd, admin))]).


:- http_handler(root(admin), http_redirect(moved_temporary, root(admin/server)), []).
:- http_handler(/, http_redirect(moved_temporary, root('docs/index.html')), []).



