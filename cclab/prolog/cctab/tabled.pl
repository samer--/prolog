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

:- cctable ppp//0, qqq//0.
ppp --> qqq, edge; edge.
qqq --> ppp.

% for testing handling of input and output variables in continuations.
path_a(Y) :- pathl(a,X), Y=a(X).
path1_a(Y) :- pathl1(a,X), Y=a(X).

:- initialization debug(cctab), module(tabled).

:- cctable sent//0, sent1//1.
sent --> {member(A, [b,l,r])}, sent1(A).
sent1(b) --> {member(W,[cool,wicked])}, [W].
sent1(l) --> sent, [not].
sent1(r) --> [really], sent.

:- cctable last/2.
last([X],X).
last([_|Xs],X) :- last(Xs,X).


% specify preterminals as DCG that spits out alternatives
user:term_expansion(Lab | Body, Clause) :-
   dcg_translate_rule(Lab --> Body, Clause).

:- meta_predicate +(2,?,?).
+Lab --> [T], {call(Lab,Vals,[]), member(T,Vals)}.

:- cctable s//0, np//0, vp//0, pp//0, nom//0.

s --> np, vp.

np --> +d, nom
     ; +pn
     ; np, pp.

vp --> +iv
     ; +tv, np
     ; +dv, np, np
     ; vp, pp
     ; +mv, s.

nom --> +n
      ; +adj, nom.

pp --> +p, np.

% preterminal switch declarations
adj | [hot,cold,thin,fat,disgusting,lovely].
pn  | [alice, bob, cuthbert, delia, edna].
d   | [the,a,some,my]. % ,every,no].
mv  | [knew,thought,believed,said].
dv  | [gave,made,baked].
tv  | [saw, ate, hated, baked, liked, walked, ran, loved, caught].
iv  | [lived, worked].
n   | [dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park].
p   | [with,on,under,in,without,by].

