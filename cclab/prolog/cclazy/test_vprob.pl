:- module(test_vprob, []).

/** <module> Test variant memoisation with explanation graphs
   Eg try this:
   ==
   ?- explore_ltree(pathl(a,K), print_and_dump).
   ==
*/
:- use_module(library(clpr)).
:- use_module(library(data/pair), [pair/3, snd/2]).
:- use_module(library(ccstate),  [run_state/3]).
:- use_module(ccvprob, [ leaf_prob/4, guard/1, dist/2, fail_/0, run_ltree/2, cctabled/1, get_tables/2]).
:- use_module(library(ccmacros)).
:- use_module(library(lambda1)).
:- use_module(library(tabling)).
:- use_module(treeutils).


% -------------- calling stateful nondeterminism ------

explore_ltree(Goal, Explore) :-
   rb_empty(Empty),
   run_state(generate_explore(Goal, Explore), Empty, _).

generate_explore(Goal, Explore) :-
   run_ltree(Goal, Tree),
   call(Explore, Tree).

print_probs_and_dump(Ts1,T1,T) :-
   convert_tree(T,T1),
   get_tables(Ts1,Map),
   maptree(apply_probs(Map), T1, T2),
   maplist(print_variant,Ts1),
   once(print_tree(T2)).

solns_and_probs(T) :-
   tree_yield(T, Ls, []),
   get_tables(Ts1,Map),
   maplist(leaf_prob(Map), Ls, Ls1, Ps1),
   maplist(pair, Ls1, Ps1, LPs),
   sumlist(Ps1, Tot),

   maplist(print_variant,Ts1), nl,
   maplist(pprintln(' '), LPs), nl,
   pprintln('Total probability:', Tot).


apply_probs(M,l(L1),l(L2)) :- !, leaf_prob(M,L1,L2,_).
apply_probs(_,L,L).

user:portray(X) :- float(X), !, format('~3g',[X]).

print_variant(Var-Solns) :- pprintln('',Var), maplist(print_soln,Solns).
print_soln(Y-e(Prob,Expls)) :- pprintln('  ',Y:Prob), maplist(pprintln('    '),Expls).
pprintln(Pref,X) :- write(Pref), print(X), nl.

:- module(test_vprob).

% ---- test programs -----
choose(Xs,X) :-
   length(Xs,N),
   P is 1/N,
   maplist(pair(P),Xs,Dist),
   dist(Dist,X).

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
link(i,_) :- fail_.
link(j,_) :- fail_.
link(k,l).
link(l,_) :- fail_.
link(m,_) :- fail_.

link(w,X) :- dist([0.75-x,0.25-y],X).
link(x,X) :- dist([0.75-v,0.25-z],X).
% link(y,z).
link(y,X) :- dist([0.9-z, 0.1-v],X).
link(z,_) :- fail_.
% link(z,X) :- dist([0.5-t, 0.5-u],X).
% link(z,X) :- dist([1-u],X).
link(t,_) :- fail_.
link(u,_) :- fail_.
link(v,_) :- fail_.

id --> [].

:- cctable sent//0.

% left and right recursive grammar
sent --> {choose([b,l,r],A)}, sentx(A).
sentx(b) --> word.
sentx(l) --> sent, out(not).
sentx(r) --> out(really), sent.

word --> {choose([cool,wicked],W)}, out(W).

out(_,[],_) :- !, guard(false).
out(W,[X|T1],T2) :- guard(T1=T2), guard(W=X).

:- table sent1//0.
sent1 --> sentx1(_).
sentx1(b) --> [cool]; [wicked].
sentx1(l) --> sent1, [not].
sentx1(r) --> [really], sent1.

% incomplete recursive form

:- cctable pathr//0, pathl//0.

pathr --> link, {choose(['B','R'],Q)}, maybe_rec(Q,pathr).
pathl --> {dist([0.6-'B', 0.4-'R'],Q)}, maybe_rec(Q,pathl), link.

bernoulli(P,X) :- NP is 1 - P, dist([NP-0,P-1],X).

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
