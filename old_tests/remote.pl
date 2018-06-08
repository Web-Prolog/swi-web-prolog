:- use_module(web_prolog).
:- use_module(library(debug)).

:- debug(ws).
:- debug(dispatch(_)).

/** <module> Test handling source

```
$ swipl -g 'node(localhost:3061)' remote.pl
$ swipl -g 'node(localhost:3060)' remote.pl

?- test(Id, [node('http://localhost:3061/ws')]).
```

*/

test(Id, Options) :-
    spawn(hello, Id,
          [ src_text("hello :- writeln(hello), sleep(1), hello.")
          | Options
          ]).

sandbox(Options) :-
    spawn(hello, _Id,
          [ src_text("hello :-
			  open('/dev/null', read, In),
                          writeln(In).
		     ")
          | Options
          ]).
