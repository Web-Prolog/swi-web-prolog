:- module(io, [
	]).

:- redefine_system_predicate(write(_)).
:- redefine_system_predicate(writeln(_)).
:- redefine_system_predicate(format(_)).
:- redefine_system_predicate(format(_,_)).

write(Term) :-
	distribution:echo(Term).
	
writeln(Term) :-
	distribution:echo(Term).

format(Format) :-
	distribution:echo(Format).
	
format(Format, Args) :-
    system:format(atom(String), Format, Args),
	distribution:echo(String).
	
