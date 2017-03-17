:- module(test_memo, []).

:- use_module(library(tabling)).
:- use_module(library(math)).
:- use_module(library(ccstate),  [run_ref/1, run_state/3, upd/2]).
:- use_module(library(ccdetmem), [memo/3]).
:- use_module(library(lambda1)).
:- use_module(ccmemo, [guard/1, choose/2, run_ltree/2, memo_nondet/2, memo_nondet/3]).
:- use_module(treeutils).

% -------------- calling stateful nondeterminism ------

run_ltree_ref(Generate, Explore) :-
   run_ref(generate_explore(Generate, Explore)).

generate_explore(Generate, Explore) :-
   run_ltree(Generate, Tree),
   call(Explore, Tree).

depth_first(Items, Tree) :-
   df(Tree, Items, []).

df(leaf(X), [X|L], L).
df(lnode(GenSubtrees), L1, L2) :-
   freeze(L1, df_trees(GenSubtrees, L1, L2)).
df_trees(Gen, L1, L2) :-
   call(Gen, Subtrees),
   foldl(df, Subtrees, L1, L2).


:- module(test_memo).

% ---- test programs -----

test0(r(X)) :- choose([1,2,3],X).

test(Z) :-
   choose([1,2,3],X),
   choose([a(X),b(X),c(X)],Y),
   choose([Y-x, Y-y, Y-z],Z1),
   choose([Z1-u, Z1-v, Z1-w],Z).

hmm(0,_,[]).
hmm(N,Y,[X|Xs]) :-
   succ(M,N),
   trans(Y,X),
   hmm(M,X,Xs).

trans(_,X) :- choose([a,b,c],X).

link(a,X) :- choose([b,c],X).
link(b,d).
link(c,X) :- choose([d,m],X).
link(d,X) :- choose([e,f],X).
link(e,g).
link(f,X) :- choose([g,k],X).
link(g,h).
link(h,X) :- choose([i,j],X).
link(i,X) :- choose([],X).
link(j,X) :- choose([],X).
link(k,l).
link(l,X) :- choose([],X).
link(m,X) :- choose([],X).


link(w,X) :- choose([x,y],X).
link(x,z).
link(y,z).
link(z,X) :- choose([],X).
% link(z,z).

id --> [].

% left and right recursive grammar
sent(S,L1,L2) :- choose([b,l,r],A), sentx(A,S,L1,L2).
sentx(b,_) --> word.
sentx(r,S) --> call(S), out(not).
sentx(l,S) --> out(really), call(S).

word --> {choose([cool,wicked],W)}, out(W).

out(_,[],_) :- !, guard(false).
out(W,[X|T],T) :- guard(W=X).

% incomplete recursive form
pathr(P) --> link, {choose(['B','R'],Q)}, maybe_rec(Q,P).
pathl(P) --> {choose(['B','R'],Q)}, maybe_rec(Q,P), link.



maybe_rec('B',_) --> [].
maybe_rec('R',P) --> call(P).


fib_inc(_,0,1) :- !.
fib_inc(_,1,1) :- !.
fib_inc(Fib,N,X) :-
   succ(M,N), call(Fib,M,Y),
   succ(L,M), call(Fib,L,Z),
   X is Y+Z.

test_fib(Fib,Dump,Goal) :-
   memo(fib_inc(Fib), Fib, Dump),
   call(Goal).

:- table fib/2.
fib(0,1) :- !.
fib(1,1) :- !.
fib(N,X) :-
   succ(M,N), fib(M,Y),
   succ(L,M), fib(L,Z),
   X is Y+Z.

% for testing table copying overhead 
last(Last,[H|T],L) :- T=[] -> L=H; call(Last,T,L).


test_path(l,D,Start, End) :- test_path(pathl(P), P, D, Start, End).
test_path(r,D,Start, End) :- test_path(pathr(P), P, D, Start, End).
test_path(Inc,Complete,Dump, Start, End) :- 
   memo_nondet(Inc,Complete,Dump), 
   call(Complete, Start, End).

test_left_grammar(In,Dump,Tail) :-
   memo_nondet(sent(S), S, Dump),
   call(S,In,Tail).

