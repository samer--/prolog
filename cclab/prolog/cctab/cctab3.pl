:- module(cctab, [run_tabled/1, call_tabled/1]).

:- use_module(library(delimcc), [pr_reset/3, pr_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1]).
:- use_module(library(lambda1)).
:- use_module(tabled, []).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

:- meta_predicate call_tabled(0).
call_tabled(Head) :- pr_shift(tab, handler(Head)).

handler(Head, K, Ans) :-
   term_variables(Head,Y), KY= \Y^K,
   get(Tabs1),
   head_to_variant(Head, Variant),
   (  rb_update(Tabs1, Variant, tab(Solns,Ks), tab(Solns,[KY|Ks]), Tabs2) 
   -> set(Tabs2), 
      member(Y, Solns),
      call(K, Ans) 
   ;  rb_insert_new(Tabs1, Variant, tab([],[]), Tabs2),
      set(Tabs2),
      producer(Variant, \Y^Head, KY, Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   call(Generate, Y1),
   get(Tabs1),
   rb_update(Tabs1, Variant, tab(Solns1, Ks), tab(Solns2, Ks), Tabs2),
   \+member(Y1, Solns1), Solns2 = [Y1|Solns1],
   set(Tabs2),
   member(K,[KP|Ks]), 
   call(K,Y1,Ans).

:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :- 
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(pr_reset(tab, \Ans^Goal, Ans), Tables, FinalTables).

