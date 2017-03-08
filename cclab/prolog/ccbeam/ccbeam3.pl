:- module(ccbeam, [lazy_beam/2, dist/2, fail_/0, cctabled/2]).

/** <module> Probabilistic choice with beam search to find most probable paths

   Simple beam search using a priority queue to manage threads.
   With memoisation. No limit on number of threads.
   Working with head goals and variants instead of binary predicates.

*/

:- use_module(library(rbutils)).
:- use_module(library(dcg_pair)).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(lambda2)).
:- use_module(pack(pha/prolog/library/priorityq)).

:- set_prolog_flag(back_quotes, symbol_char).

:- meta_predicate cctabled(0,0).
cctabled(TableAs,Head) :- p_shift(beam, tab(TableAs,Head)).
dist(Xs,X)             :- p_shift(beam, dist(Xs,X)).
fail_                  :- dist([],_).

:- meta_predicate lazy_beam(0,-).
lazy_beam(Goal, Prob) :- 
   term_variables(Goal, Y),
   rb_empty(S1),
   pq_empty(PQ0), 
   pq_insert(1,\\Y`Goal,PQ0,PQ1),
   unfolds_to(run_beam,Prob-Y,S1-PQ1,_).

unfolds_to(Pred,X) -->
   call(Pred,Y), ({Y=X}; unfolds_to(Pred,X)).

run_beam(WeightedAns) --> 
   \> pq_remove(P, Thread),
   {p_reset(beam, call(Thread,Ans), Status)},
   cont_beam(Status, P-Ans, WeightedAns).

cont_beam(done, PAns, PAns) --> [].
cont_beam(susp(Req, Cont), PAns1, PAns2) --> 
   handle(Req, Cont, PAns1),
   run_beam(PAns2).

handle(dist(Ys,Y), Cont, P-Ans) -->
   \> foldl(insert(\\Y`Ans`Cont, P), Ys).
handle(tab(TableAs,Head), Cont, P-Ans) -->
   { term_variables(Head,Y), 
     term_to_ground(Head,Variant),
     K = (\\Y`Ans`Cont) },
   (  \< rb_trans(Variant, entry(V,PYs,Conts), entry(V,PYs,[P-K|Conts])) 
   -> \> rb_fold(insert_(K,P),PYs)
   ;  {rb_empty(EmptyDist)},
      \< rb_add(Variant, entry(TableAs,EmptyDist,[P-K])),
      \> pq_insert(1,producer(Variant,\\Y`Head))
   ).
handle(ans(Variant,Y), _, P-_) -->
   \< rb_trans(Variant, entry(V,Ys,Conts), entry(V,Ys2,Conts)),
   (  {rb_add(Y,P,Ys,Ys2)}
   -> \> foldl(resume(P,Y), Conts)
   ;  {rb_trans(Y,OldP,NewP,Ys,Ys2), NewP is max(OldP,P)},
      {debug(ccbeam, 'New solutions for ~p: ~p @ ~p (prev was ~p)',[Variant,Y,P,OldP])}
   ).

producer(Variant,Generate,_) :- call(Generate,Y), p_shift(beam, ans(Variant,Y)).
insert(K, P0, P-X) --> resume(P0,P,K,X).
insert_(K, P0, X-P) --> resume(P0,P,K,X).
resume(P, X, P0-K) --> resume(P0,P,K,X).
resume(P0,P,K,X) --> {P1 is P0*P}, pq_insert(P1,call(K,X)).

term_to_ground(T, G) :- copy_term_nat(T,G), numbervars(G,0,_).
