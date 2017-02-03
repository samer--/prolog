:- module(test_prob, []).

:- use_module(library(tabling)).
:- use_module(library(data/pair), [pair/3]).
:- use_module(library(ccstate),  [run_nb_state/3, upd/1, run_ref/1, run_env/1, env_new/2, env_app/2, env_upd/3, env_get/2]).
:- use_module(library(ccdetmem), [memo/3]).
:- use_module(library(lambda2)).
:- use_module(ccprob,   [memo_prob/3, fail_/0, dist/2, run_prob/2]).
:- use_module(treeutils).

% -------------- calling stateful nondeterminism ------

run_lwtree_ref(Generate, Explore) :-
   run_ref(generate_explore(Generate, Explore)).

generate_explore(Generate, Explore) :-
   run_prob(Generate, Tree),
   call(Explore, Tree).


% depth_first(Items, Tree) :-
%    df(Tree, Items, []).

% df(leaf(X), [X|L], L).
% df(lnode(GenSubtrees), L1, L2) :-
%    freeze(L1, df_trees(GenSubtrees, L1, L2)).
% df_trees(Gen, L1, L2) :-
%    call(Gen, Subtrees),
%    foldl(df, Subtrees, L1, L2).

:- module(test_prob).

% ---- test programs -----
choose(Xs,X) :-
   length(Xs,N),
   P is 1/N,
   maplist(pair(P),Xs,Dist),
   dist(Dist,X).

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
link(i,_) :- fail_.
link(j,_) :- fail_.
link(k,l).
link(l,_) :- fail_.
link(m,_) :- fail_.


link(w,X) :- dist([0.75-x,0.25-y],X).
link(x,X) :- dist([0.5-v,0.5-z],X).
link(y,z).
link(z,X) :- dist([0.5-t, 0.5-u],X).
link(t,_) :- fail_.
link(u,_) :- fail_.
link(v,_) :- fail_.
% link(z,z).

bernoulli(P,X) :- NP is 1 - P, dist([NP-0,P-1],X).
id --> [].

% left and right recursive grammar
sent(S,L1,L2) :- choose([b,l,r],A), sentx(A,S,L1,L2).
sentx(b,_) --> word.
sentx(r,S) --> call(S), out(not).
sentx(l,S) --> out(really), call(S).

word --> {choose([cool,wicked],W)}, out(W).

out(_,[],_) :- !, fail_.
out(W,[X|T],T) :- W=X -> true; fail_.

% incomplete recursive form
pathr(P) --> link, {choose(['B','R'],Q)}, maybe_rec(Q,P).
pathl(P) --> {dist([0.6-'B',0.4-'R'],Q)}, maybe_rec(Q,P), link.
% pathl(P) --> link.

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

test_path(l,D,Start, End) :- test_path(pathl(P), P, D, Start, End).
test_path(r,D,Start, End) :- test_path(pathr(P), P, D, Start, End).
test_path(Inc,Complete,Dump, Start, End) :- 
   memo_prob(Inc,Complete,Dump), 
   call(Complete, Start, End).

test_left_grammar(In,Dump,Tail) :-
   memo_prob(sent(S), S, Dump),
   call(S,In,Tail).

findall1(X,G,Xs) :- run_nb_state(call_and_store_all(X,G), T-T, Xs-[]).
call_and_store_all(X,G) :- call(G), upd(c(X)), fail; true.
c(X, H-[X|T], H-T).
