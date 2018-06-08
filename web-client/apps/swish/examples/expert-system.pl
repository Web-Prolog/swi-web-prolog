% A meta-interpreter implementing
% a tiny expert-system
% --------------------------------


prove(true) :- !.
prove((B, Bs)) :- !,
    prove(B),
    prove(Bs).
prove(H) :-
    clause(H, B),
    prove(B).
prove(H) :-
    askable(H),
    format(atom(Q), 'Is it true that: ~q?', [H]),
    pengine_input(Q, Answer),
    Answer == yes.


good_pet(X) :- bird(X), small(X).
good_pet(X) :- cuddly(X), yellow(X).

bird(X) :- has_feathers(X), tweets(X).

yellow(tweety).

askable(tweets(_)).
askable(small(_)).
askable(cuddly(_)).
askable(has_feathers(_)).


/** Examples

prove(good_pet(tweety)).

*/