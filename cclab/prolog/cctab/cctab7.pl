:- module(cctab, [run_tabled/3, cctabled/1, sanitise/3, dist/2]).

:- use_module(library(clpr)).
:- use_module(library(rbtrees)).
:- use_module(library(dcg_core), [out//1]).
:- use_module(library(callutils), [mr/5]).
:- use_module(library(data/pair), [snd/2, fst/2, fsnd/3]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1, app/2, run_state/4]).
:- use_module(library(lambda2)).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

dist(Xs,X) :- member(P-X, Xs), app(expl, out(@X-l(P))).

:- meta_predicate cctabled(0).
cctabled(Head) :- p_shift(tab, Head-P), app(expl, cctab:out(Head-v(P))).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head-P, Cont), Ans) :-
   term_variables(Head,Y), K= (\\P-Y`Ans`Cont),
   get(Tabs1),
   head_to_variant(Head, Variant),
   (  rb_update(Tabs1, Variant, tab(Solns,Ks), tab(Solns,[K|Ks]), Tabs2) 
   -> set(Tabs2), 
      rb_in(Y, e(P,_), Solns),
      run_tab(Cont, Ans) 
   ;  rb_empty(Solns), 
      rb_insert_new(Tabs1, Variant, tab(Solns,[]), Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \\Y`Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   run_state(expl, call(Generate, Y1), E, []),
   get(Tabs1),
   rb_update(Tabs1, Variant, tab(Solns1, Ks), tab(Solns2, Ks), Tabs2),
   (  rb_insert_new(Solns1, Y1, e(PY,[E]), Solns2)
   -> gensym(p,PY), set(Tabs2),
      member(K,[KP|Ks]), call(K,PY-Y1,Ans)
   ;  rb_update(Solns1, Y1, e(PY,Es), e(PY,[E|Es]), Solns2),
      set(Tabs2), fail
   ).

:- meta_predicate run_tabled(0,-,-).
run_tabled(Goal, Expl1-P, Tabs1) :- 
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(run_state(expl,Goal,Expl,[]), Ans-Expl), Tables, FinalTables),
   sanitise(Map, FinalTables, Tabs1),
   maplist(pp(Map), Expl, Expl1),
   expl_prob(Expl1, P).

sanitise(Map, Tabs1, Tabs2) :- rb_fold(mr(fsnd(clean_tab(Map)),out), Tabs1, Tabs2, []).
clean_tab(Map, tab(Solns,_), Solns1) :- rb_fold(mr(fsnd(clean_solns(Map)),out), Solns, Solns1, []).
clean_solns(Map, e(P, Expls), e(P1, Expls1)) :-
   memberchk(P-P1, Map),
   maplist(maplist(pp(Map)), Expls, Expls1),
   foldl(mr(expl_prob,add), Expls1, 0, P1).

pp(_, X-l(P), X-P) :- !.
pp(Map, X-v(P), X-P1) :- memberchk(P-P1, Map).
expl_prob(Expl, P) :- foldl(mr(snd,mul),Expl,1,P). 
add(X,Y,Z) :- {Z=X+Y}.
mul(X,Y,Z) :- {Z=X*Y}.
