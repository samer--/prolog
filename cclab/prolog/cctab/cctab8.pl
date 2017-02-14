:- module(cctab, [run_tabled/1, run_nb_state//1, cctabled/1]).

/** <module> Auto-prompt tabling with table completion

   cctab2 modified to switch into tabling mode (context delimited by 'tab' prompt)
   on calling outermost tabled predicate (the 'leader'). Tables are marked 'complete' 
   on leaving tabling mode and subsequent lookups on complete tables need no delimited 
   context. This more or less implements the functionality of Desouter et al's library.
*/
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1]).
:- use_module(library(lambda1)).
:- use_module(tabled, []).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

:- meta_predicate cctabled(0).
cctabled(Head) :- 
   term_variables(Head,Y), 
   head_to_variant(Head, Variant),
   get(Tabs1),
   (  rb_lookup(Variant, tab(Solns,Status), Tabs1) 
   -> (  Status=complete -> rb_in(Y, _, Solns)
      ;  p_shift(tab, cons(Variant, Y, Tabs1)) % active consumer
      ) 
   ;  (  rb_lookup('$tabling?', true,Tabs1)
      -> p_shift(tab, prod(Variant, Y, Tabs1, Head)) % active producer
      ;  (  rb_insert(Tabs1, '$tabling?', true, Tabs2),
            cont_tab(susp(prod(Variant, Y, Tabs2, Head), fail), Y) 
         ;  get(Tabs3),
            rb_map(Tabs3, cctab:completion_map, Tabs4),
            set(Tabs4),
            rb_lookup(Variant, tab(Solns,_), Tabs4),
            rb_in(Y, _, Solns)
         )
      )
   ).

completion_map(true,false). % Applies to '$$$' key only
completion_map(tab(Solns,_), tab(Solns,complete)).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(cons(Var,Y,Tabs1), Cont), Ans) :-
   rb_update(Tabs1, Var, tab(Solns,Ks), tab(Solns,[\Y^Ans^Cont|Ks]), Tabs2),
   set(Tabs2), 
   rb_in(Y, _, Solns),
   run_tab(Cont, Ans).
cont_tab(susp(prod(Var,Y,Tabs1,Head), Cont), Ans) :-
   rb_empty(Solns), 
   rb_insert_new(Tabs1, Var, tab(Solns,[]), Tabs2),
   set(Tabs2),
   run_tab(producer(Var, \Y^Head, \Y^Ans^Cont, Ans), Ans).

producer(Variant, Generate, KP, Ans) :-
   call(Generate, Y1),
   get(Tabs1),
   rb_update(Tabs1, Variant, tab(Solns1, Ks), tab(Solns2, Ks), Tabs2),
   rb_insert_new(Solns1, Y1, t, Solns2), 
   set(Tabs2),
   member(K,[KP|Ks]), 
   call(K,Y1,Ans).

:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :- 
   rb_empty(Tables),
   run_nb_state(Goal, Tables, FinalTables).

