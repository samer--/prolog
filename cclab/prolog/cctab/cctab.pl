:- module(cctab, [run_tabled/1, call_tabled/1]).

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1]).
:- use_module(library(lambda1)).
:- use_module(tabled, []).


head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

:- meta_predicate call_tabled(0).
call_tabled(Head) :- p_shift(tab, Head).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K= \Y^Ans^Cont,
   get(Tabs1),
   head_to_variant(Head, Variant),
   (  rb_update(Tabs1, Variant, tab(Solns,Ks), tab(Solns,[K|Ks]), Tabs2) 
   -> set(Tabs2), 
      member_set(Head, Solns),  % !!! what about variabls in Solns?
      run_tab(Cont, Ans) 
   ;  empty_set(Solns), 
      rb_insert_new(Tabs1, Variant, tab(Solns,[]), Tabs2),
      set(Tabs2),
      run_tab(producer(Head, Variant, Y, Cont, Ans), Ans)
   ).


producer(Head, Variant, Y, Cont, Ans) :-
   copy_term(Head-Y, Head1-Y1),
   call(Head1),
   get(Tabs1),
   rb_update(Tabs1, Variant, tab(Solns1, Ks), tab(Solns2, Ks), Tabs2),
   \+member_set(Head1, Solns1),  
   add_to_set(Head1, Solns1, Solns2), 
   set(Tabs2),
   (  Head=Head1, call(Cont)
   ;  member(K,Ks), call(K,Y1,Ans)
   ).


:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :- 
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(Goal, Ans), Tables, FinalTables).

empty_set([]).
member_set(Y,Ys) :- member(Y,Ys).
add_to_set(Y,Ys1,[Y|Ys1]).
fold_set(_,[]) --> [].
fold_set(P,[X|Xs]) :- call(P,X), fold_set(P,Xs).

