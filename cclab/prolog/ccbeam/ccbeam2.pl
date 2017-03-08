:- module(ccbeam, [memo_prob/2, lazy_beam/2, dist/2, fail_/0]).

/** <module> Probabilistic choice with beam search to find most probable paths

   Simple search using a priority queue to manage threads 
   With memoisation. No limit on number of threads.

*/

:- use_module(library(clpr)).
:- use_module(library(rbutils)).
:- use_module(library(typedef)).
:- use_module(library(dcg_pair)).
:- use_module(library(data/store)).
:- use_module(library(math),    [mul/3]).
:- use_module(library(lazy),    [lazy_unfold_finite//2]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [get/1, set/1, run_state/3, upd/2, ref_new/2, ref_get/2, ref_app/2, ref_upd/3]).
:- use_module(library(lambda2)).
:- use_module(pack(pha/prolog/library/priorityq)).

:- set_prolog_flag(back_quotes, symbol_char).

:- type weighted(A) ---> number - A.
:- type dist(A) == list(weighted(A)).

:- meta_predicate memo_prob(2,-).
memo_prob(P, ccbeam:memf(P,R)) :- p_shift(beam, new(R)).

memf(P,R,X,Y) :- p_shift(beam, mem(P,R,X,Y)).
dist(Xs,X) :- p_shift(beam, dist(Xs,X)).
fail_ :- p_shift(beam, dist([],_)).

:- meta_predicate lazy_beam(1,-).
lazy_beam(Pred,Stream) :- 
   store_new(S1), proc_init(1,Pred,PQ1),
   lazy_unfold_finite(run_beam,Stream,S1-PQ1,_).

run_beam(Ans) --> 
   \> proc_remove(P, Thread),
   {p_reset(beam, call(Thread,Y), Status)},
   cont_beam(Status, P-Y, Ans).

cont_beam(done, PrX, PrX) --> [].
cont_beam(susp(Req, Cont), PY, Ans) --> 
   handle(Req, Cont, PY),
   run_beam(Ans).

handle(dist(Xs,X), Cont, Pr-Y) -->
   \> foldl(insert(\\X`Y`Cont, Pr), Xs).
handle(new(R), Cont, Pr-Y) -->
   {rb_empty(T)},
   store_add(T,R) <\> proc_insert(Pr,\\Y`Cont).
handle(mem(Pred,R,X,Z), Cont, Pr-Y) -->
   {K = (\\Z`Y`Cont)},
   \< store_upd(R,Tab,Tab1),
   (  {rb_trans(X, entry(Ys,Conts), entry(Ys,[Pr-K|Conts]), Tab, Tab1)} 
   -> \> rb_fold(insert_(K,Pr),Ys)
   ;  {rb_empty(EmptyDist),
       rb_add(X, entry(EmptyDist,[Pr-K]), Tab, Tab1) },
      \> proc_insert(1,producer(Pred,R,X))
   ).
handle(ans(R,X,Z), _, Pr-_) -->
   \< store_apply(R, rb_trans(X, entry(Zs,Conts), entry(Zs2,Conts))),
   (  {rb_add(Z,Pr,Zs,Zs2)}
   -> \> foldl(resume(Pr,Z), Conts)
   ;  {NewP = OldP+Pr, rb_trans(Z,OldP,NewP,Zs,Zs2)}
   ).

producer(P,R,X,_) :- call(P,X,Y), p_shift(beam, ans(R,X,Y)).
insert(K, P0, P-X) --> resume(P0,P,K,X).
insert_(K, P0, X-P) --> resume(P0,P,K,X).
resume(P, X, P0-K) --> resume(P0,P,K,X).

resume(P0,P,K,X) --> {P1 is P0*P}, proc_insert(P1,call(K,X)).

proc_init(P,T,P-PQ1)          :- pq_empty(PQ0), pq_insert(P,T,PQ0,PQ1).
proc_insert(P,Th,T1-Q1,T2-Q2) :- pq_insert(P,Th,Q1,Q2), T2 is T1+P.
proc_remove(P,Th,T1-Q1,T2-Q2) :- pq_remove(P,Th,Q1,Q2), T2 is T1-P.
proc_empty(_-Q)               :- pq_empty(Q).
proc_stats(T,S,T-Q,T-Q)       :- pq_size(Q,S).
