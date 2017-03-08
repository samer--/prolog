:- module(ccbeam, [memo_prob/2, lazy_beam/2, dist/2, fail_/0]).

/** <module> Probabilistic choice with beam search to find most probable paths

   Simple beam search using a priority queue to manage threads.
   Without memoisation - nonterminating for left-recursive predicates.
*/

:- use_module(library(typedef)).
:- use_module(library(lazy), [lazy_unfold_finite//2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambda2)).
:- use_module(pack(pha/prolog/library/priorityq)).

:- set_prolog_flag(back_quotes, symbol_char).

:- type weighted(A) ---> number - A.
:- type dist(A) == list(weighted(A)).

:- meta_predicate memo_prob(2,-).
memo_prob(P,P).

dist(Xs,X) :- p_shift(beam, dist(Xs,X)).
fail_ :- p_shift(beam, dist([],_)).

:- meta_predicate lazy_beam(1,-).
lazy_beam(Pred,Stream) :- 
   proc_init(1,Pred,PQ1),
   lazy_unfold_finite(run_beam,Stream,PQ1,_).

run_beam(Ans) --> 
   proc_remove(P, Thread),
   {p_reset(beam, call(Thread,Y), Status)},
   cont_beam(Status, P-Y, Ans).

cont_beam(done, PrX, PrX) --> [].
cont_beam(susp(dist(Xs,X), Cont), Pr-Y, Ans) -->
   foldl(insert(0.9-10, \\X`Y`Cont, Pr), Xs),
   run_beam(Ans).

insert(Thresh-Max, K, P0, P-X) --> 
   proc_stats(T,N),
   (  {T>Thresh, N>=Max} -> {debug(ccbeam, 'Agenda is full (N=~w, T=~w)', [N,T])}
   ;  {P1 is P0*P}, proc_insert(P1,call(K,X))
   ).

proc_init(P,T,P-PQ1)          :- pq_empty(PQ0), pq_insert(P,T,PQ0,PQ1).
proc_insert(P,Th,T1-Q1,T2-Q2) :- pq_insert(P,Th,Q1,Q2), T2 is T1+P.
proc_remove(P,Th,T1-Q1,T2-Q2) :- pq_remove(P,Th,Q1,Q2), T2 is T1-P.
proc_empty(_-Q)               :- pq_empty(Q).
proc_stats(T,S,T-Q,T-Q)       :- pq_size(Q,S).

