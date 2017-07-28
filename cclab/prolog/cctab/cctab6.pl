:- module(cctab, [run_tabled/1, run_tabled/2, cctabled/1, sanitise/2]).

/* <module> Tabling, answers not delivered to producer continuation until completion
   NB - this doesn't work for co-recursive predicates! See cctab5.pl for a full
   description of the problem.
*/

:- use_module(library(rbutils)).
:- use_module(library(dcg_core), [out//1]).
:- use_module(library(data/pair), [fst/2, fsnd/3]).
:- use_module(library(callutils), [mr/5]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, app/1, set/1, get/1]).
:- use_module(library(lambda1)).
:- use_module(tabled, []).


head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

:- meta_predicate cctabled(0).
cctabled(Head) :- p_shift(tab, Head).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K= \Y^Ans^Cont,
   head_to_variant(Head, Variant),
   get(Tabs1),
   (  rb_upd(Variant, tab(Solns,Status1), tab(Solns,Status2), Tabs1, Tabs2)
   -> (  Status1=active(Ks)
      -> Status2=active([K|Ks]), set(Tabs2)
      ;  Status2=complete
      ),
      rb_in(Y, _, Solns),
      run_tab(Cont, Ans)
   ;  rb_empty(Solns),
      rb_insert_new(Tabs1, Variant, tab(Solns,active([])), Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \Y^Head, K, Ans), Ans)
   ).

producer(Variant, Generate, _, Ans) :-
   call(Generate, Y1),
   app(add_soln(Variant, Y1, active(Ks))),
   member(K,Ks),
   call(K,Y1,Ans).
producer(Variant, _, KP, Ans) :-
   app(rb_upd(Variant, tab(Solns,_), tab(Solns,complete))),
   rb_in(Y, _, Solns),
   call(KP,Y,Ans).

add_soln(Variant, Y1, Status, Tabs1, Tabs2) :-
   rb_upd(Variant, tab(Solns1, Status), tab(Solns2, Status), Tabs1, Tabs2),
   rb_insert_new(Solns1, Y1, t, Solns2).

:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :-
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(Goal, Ans), Tables, FinalTables).

sanitise(Tabs1, Tabs2) :- rb_fold(mr(fsnd(clean_tab), out), Tabs1, Tabs2, []).
clean_tab(tab(Solns,Status), Stat-Solns1) :- functor(Status,Stat,_), rb_fold(mr(fst,out), Solns, Solns1, []).


