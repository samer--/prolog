:- module(ccbeam, [lazy_beam/2, lazy_beam/4, dist/2, cctabled/2]).

/** <module> Probabilistic choice with beam search to find most probable paths

   Simple beam search using a priority queue to manage threads.
   With memoisation. No limit on number of threads.
   Working with head goals and variants instead of binary predicates.
*/

:- use_module(library(rbutils)).
:- use_module(library(dcg_pair)).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambda2)).

:- set_prolog_flag(back_quotes, symbol_char).

:- meta_predicate cctabled(0,0), lazy_beam(0,-).
cctabled(TableAs,Head) :- p_shift(beam, tab(TableAs,Head)).
dist(Xs,X)             :- member(P-X,Xs), p_shift(beam, factor(P)).

:- meta_predicate lazy_beam(0,-), lazy_beam(0,-,-,-).
lazy_beam(Goal, Prob) :- lazy_beam(Goal, Prob, _, _).
lazy_beam(Goal, Prob, S2, QMass) :- 
   term_variables(Goal, Y),
   rb_empty(S1), pq_empty(PQ0), 
   pq_insert(1,\\Y`Goal,PQ0,PQ1),
   run_beam(Prob-Y, S1-PQ1, S2-PQ2),
   pq_total(PQ2,QMass).

run_beam(WeightedAns) --> 
   \> pq_remove(P, Thread),
   { findall(Stat-Ans, p_reset(beam, call(Thread,Ans), Stat), Branches),
     partition(\\done-_`true, Branches, Answers, Suspensions) },
   (  {WeightedAns=P-Ans, member(done-Ans, Answers)}
   ;  foldl(\\P|susp(Req,Cont)-Ans`handle(Req, Cont, P-Ans), Suspensions),
      run_beam(WeightedAns)
   ).

handle(factor(P), Cont, P0-Ans) -->
   {P1 is P0*P}, \> pq_insert(P1,\\Ans`Cont).
handle(tab(TableAs,Head), Cont, P-Ans) -->
   { term_variables(Head,Y), 
     term_to_ground(Head,Var),
     K = (\\Y`Ans`Cont) },
   (  \< rb_trans(Var, entry(V,PYs,Conts), entry(V,PYs,[P-K|Conts])) 
   -> \> rb_fold(\\P+K|X-PX`resume(P,PX,K,X), PYs)
   ;  {rb_empty(Empty)},
      \< rb_add(Var, entry(TableAs,Empty,[P-K])),
      \> pq_insert(1,producer(Var,\\Y`Head))
   ).
handle(ans(Var,Y), _, P-_) -->
   \< rb_trans(Var, entry(V,Ys,Conts), entry(V,Ys2,Conts)),
   (  {rb_add(Y,P,Ys,Ys2)}
   -> \> foldl(\\P+Y|P0-K`resume(P0,P,K,Y), Conts)
   ;  {rb_trans(Y,OldP,NewP,Ys,Ys2), NewP is max(OldP,P)},
      {debug(ccbeam, 'New solution for ~p: ~p @ ~p (prev was ~p)',[Var,Y,P,OldP])}
   ).

producer(Var,Generate,_) :- call(Generate,Y), p_shift(beam, ans(Var,Y)).
resume(P0,P,K,X) --> {P1 is P0*P}, pq_insert(P1,call(K,X)).

pq_empty(0-H) :- empty_heap(H).
pq_insert(P,X,T1-H1,T2-H2) :- T2 is T1+P, P1 is -P, add_to_heap(H1,P1,X,H2).
pq_remove(P,X,T1-H1,T2-H2) :- get_from_heap(H1,P1,X,H2), P is -P1, T2 is T1-P.
pq_total(T-_,T).

term_to_ground(T, G) :- copy_term_nat(T,G), numbervars(G,0,_).
