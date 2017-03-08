:- module(tabled, [fib/2, pathl//0, pathr//0]).

/** <module> Test predicates for tabling
   NB. this module expects cctabled/1 to be imported into user.
*/

:- use_module(library(ccmacros)).
:- persistent_history.
:- cctable fib/2.

fib(0,1).
fib(1,1).
fib(N,X) :-
   succ(M,N), fib(M,Y),
   succ(L,M), fib(L,Z),
   plus(Y,Z,X).

edge(a,b).
edge(a,c).
edge(b,d).
edge(c,d).
edge(d,e).
edge(d,f).
edge(f,g).
edge(N,M) :- number(N), (M is N+1; M is N-1).

% four tabled transitive closures of edge/2
:- cctable pathl//0, pathl1//0, pathr//0, pathr1//0.
pathl  --> edge; pathl, edge.
pathl1 --> pathl1, edge; edge.
pathr  --> edge; edge, pathr.
pathr1 --> edge, pathr1; edge.

:- cctable pp//0, qq//0.
pp --> qq, edge; edge.
qq --> pp.

% for testing handling of input and output variables in continuations.
path_a(Y) :- pathl(a,X), Y=a(X).
path1_a(Y) :- pathl1(a,X), Y=a(X).

:- initialization debug(cctab), module(tabled).

:- cctable sent//0, sent1//1.
sent --> {member(A, [b,l,r])}, sent1(A).
sent1(b) --> {member(W,[cool,wicked])}, [W].
sent1(l) --> sent, [not].
sent1(r) --> [really], sent.

