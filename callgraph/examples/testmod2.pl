:- module(testmod2, [test1/0, test2/0]).
:- use_module(testmod1).

test1 :-
	pred3,
	forall( dyn1(A), writeln(dyn1(A))).

test2 :-
	assert(testmod1:dyn1(b)).
