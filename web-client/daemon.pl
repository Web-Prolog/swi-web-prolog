#!/home/pengines/bin/swipl

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pengines  is  prepared  to  be  combined  with  the  SWI-Prolog  library
library(http/http_unix_daemon). Pengines is started as a deamon using

  % ./daemon.pl --port=Port [option ...]

See library(http/http_unix_daemon) for details.  The directory =upstart=
contains an Ubuntu Linux upstart script.  Perform the following steps to
make this work:

 - Make sure this file (=daemon.pl=) is executable and that the #!
   points to SWI-Prolog version 7.1.14 or later.  Test this by running

     % ./daemon.pl --help

 - copy upstart/pengines.conf to /etc/init
 - edit /etc/init/pengines.conf, notably the `chdir` line to point to
   this directory.
 - run `initctl reload-configuration`
 - create directories `log` and `storage` with permissions 775 and
   group set to the `www-data` group (see --user and --group of
   daemon.pl).
 - Start the server using

       service pengines start
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- set_prolog_flag(verbose, silent).

:- use_module(library(settings)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_log)).

:- set_setting_default(http:logfile, 'log/httpd.log').

%%	memlimit
%
%	Limit the memory usage of the pengine server (to 8Gb).

memlimit :-
   catch(rlimit(as, _, 8 000 000 000), E,
	 print_message(warning, E)).

:- initialization memlimit.
:- initialization load_settings('pengines.conf').
:- initialization http_daemon.

:- [load].
