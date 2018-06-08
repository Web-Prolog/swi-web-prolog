
%%	clpfd_queens(?N, ?Cols) is nondet.
%
%	@param The n-th element of Cols tells the row in which the
%	queen in the n-th column is placed.
%	@author Markus Triska

:- use_module(library(clpfd)).

clpfd_queens(N, Cols) :-
	queens(N, Cols),
	labeling([ff], Cols).

queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).
	
	
/** Examples

clpfd_queens(24, Queens).

*/