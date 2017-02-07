:- module(cctab, [run_tabled/1, call_tabled/1]).

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, upd/1, set/1, get/1]).
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
   head_to_variant(Head, Variant),
   get(Tabs1),
   (  rb_update(Tabs1, Variant, tab(Solns,Status1), tab(Solns,Status2), Tabs2) 
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

producer(Variant, Generate, KP, Ans) :-
   call(Generate, Y1),
   upd(add_soln(Variant, Y1, active(Ks))),
   member(K,[KP|Ks]), 
   call(K,Y1,Ans).
producer(Variant, _, _, _) :- 
   upd(complete_table(Variant, _)).

complete_table(Variant, Solns, Tabs1, Tabs2) :-
   rb_update(Tabs1, Variant, tab(Solns, _), tab(Solns, complete), Tabs2).

add_soln(Variant, Y1, Status, Tabs1, Tabs2) :-
   rb_update(Tabs1, Variant, tab(Solns1, Status), tab(Solns2, Status), Tabs2),
   rb_insert_new(Solns1, Y1, t, Solns2). 

:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :- 
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(Goal, Ans), Tables, FinalTables).

