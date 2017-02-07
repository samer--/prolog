:- module(test_vmemo, []).

:- use_module(library(tabling)).
:- use_module(library(math)).
:- use_module(library(ccstate),  [run_state/3, upd/2]).
:- use_module(library(lambda1)).
:- use_module(ccvmemo, [guard/1, choose/2, run_ltree/2, cctabled/1]).
:- use_module(library(ccmacros)).
:- use_module(treeutils).

% -------------- calling stateful nondeterminism ------

explore_ltree(Goal, Explore) :-
   rb_empty(Empty),
   run_state(generate_explore(Goal, Explore), Empty, _).

generate_explore(Goal, Explore) :-
   run_ltree(Goal, Tree),
   call(Explore, Tree).


:- module(test_vmemo).

% ---- test programs -----

test0(r(X)) :- choose([1,2,3],X).

test(Z) :-
   choose([1,2,3],X),
   choose([a(X),b(X),c(X)],Y),
   choose([Y-x, Y-y, Y-z],Z1),
   choose([Z1-u, Z1-v, Z1-w],Z).

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

sent(S1,S2) :- cctabled(sent_(S1,S2)).

% left and right recursive grammar
sent_(L1,L2) :- choose([b,l,r],A), sentx(A,L1,L2).
sentx(b) --> word.
sentx(r) --> sent, out(not).
sentx(l) --> out(really), sent.

word --> {choose([cool,wicked],W)}, out(W).

out(_,[],_) :- !, guard(false).
out(W,[X|T],T) :- guard(W=X).

% incomplete recursive form

:- cctable pathr//0, pathl//0.

pathr --> link, {choose(['B','R'],Q)}, maybe_rec(Q,pathr).
pathl --> {choose(['B','R'],Q)}, maybe_rec(Q,pathl), link.

maybe_rec('B',_) --> [].
maybe_rec('R',P) --> call(P).


make_fib(Fib) :- Fib = cctabled(fib_inc(Fib)). % ha

fib_inc(_,0,1) :- !.
fib_inc(_,1,1) :- !.
fib_inc(Fib,N,X) :-
   succ(M,N), call(Fib,M,Y),
   succ(L,M), call(Fib,L,Z),
   X is Y+Z.

test_fib(Fib,Goal) :-
   make_fib(Fib),
   call(Goal).

% test_path(l,D,Start, End) :- test_path(pathl(P), P, D, Start, End).
% test_path(r,D,Start, End) :- test_path(pathr(P), P, D, Start, End).
% test_path(Inc,Complete,Dump, Start, End) :- 
%    memo_nondet(Inc,Complete,Dump), 
%    call(Complete, Start, End).

% test_left_grammar(In,Dump,Tail) :-
%    memo_nondet(sent(S), S, Dump),
%    call(S,In,Tail).

