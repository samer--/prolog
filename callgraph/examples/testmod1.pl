:- module(testmod1, [pred1/0, pred2/0, pred3/0, dyn1/1]).

:- dynamic dyn1/1.

pred1 :-
	recorda(key1,head(1)),
	writeln(record(key1,head(1))).

pred2 :-
	recorded(key1,head(V)),
	writeln(recorded(key1,head(V))).

pred3 :- 
	pred1,
	pred2,
	pred4.

pred4 :- assert(dyn1(a)).

