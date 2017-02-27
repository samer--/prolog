:- module(cctab, [run_tabled/3, cctabled/2, dist/2, expl/2, expl_tree/4, tree_to_tree/2]).
/** <module> Continuation based probabilistic inference.

   ==
   tables == rbtree(variant, table).
   cont  == pred(pair(symbol, solution), answer).
   table ---> tab(goal, rbtree(solution, expls), list(cont)).
   expls ---> sol(symbol, list(expl)).
   expl == list(pair(label, prob)).
   label ---> @ground ; module:goal.
   prob  ---> l(number) ; v(symbol).
   answer == list(ground).
   ==
*/
:- use_module(library(clpr)).
:- use_module(library(rbutils)).
:- use_module(library(dcg_core), [out//1]).
:- use_module(library(callutils), [mr/5]).
:- use_module(library(data/pair), [pair/3, snd/2, fst/2, fsnd/3]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1, app/2, run_state/4]).
:- use_module(library(lambda2)).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

dist(Xs,X) :- member(P-X, Xs), app(expl, out(@X-l(P))).
expl(Desc, P) :- app(expl, out(Desc-l(P))).

:- meta_predicate cctabled(0,0).
cctabled(TableAs,Head) :- p_shift(tab, t(TableAs,Head,P)), app(expl, cctab:out(TableAs-v(P))).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(t(TableAs,Head,P), Cont), Ans) :-
   term_variables(Head,Y), K= (\\P-Y`Ans`Cont),
   get(Tabs1),
   head_to_variant(TableAs, Variant),
   (  rb_update(Tabs1, Variant, tab(V,Solns,Ks), tab(V,Solns,[K|Ks]), Tabs2) 
   -> set(Tabs2), 
      rb_gen(Y, sol(P,_), Solns),
      run_tab(Cont, Ans) 
   ;  rb_empty(Solns), 
      rb_insert_new(Tabs1, Variant, tab(TableAs,Solns,[]), Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \\Y`Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   run_state(expl, call(Generate, Y1), E, []),
   get(Tabs1),
   rb_update(Tabs1, Variant, tab(V,Solns1, Ks), tab(V,Solns2, Ks), Tabs2),
   (  rb_insert_new(Solns1, Y1, sol(PY,[E]), Solns2)
   -> gensym(p,PY), set(Tabs2), % NB multiple symbols for duplicate solutions via different variants
      member(K,[KP|Ks]), call(K,PY-Y1,Ans)
   ;  rb_update(Solns1, Y1, sol(PY,Es), sol(PY,[E|Es]), Solns2),
      set(Tabs2), fail
   ).

:- meta_predicate run_tabled(0,-,-).
run_tabled(Goal, Final, Tabs1) :- 
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state((run_tab(run_state(expl,Goal,Expl,[]), Ans-Expl),Final=false; Final=true), Tables, FinalTables),
   export_tables(FinalTables, Tabs1).

% strips out continuations, converts goal identifiers to inside probabilities.
export_tables(Tabs1, Tabs2) :- rb_fold(mr(ex_tab(_),out), Tabs1, Tabs2, []).

ex_tab(Map, _ - tab(Var,Solns,_), (Var :- Y/Solns1)) :- 
   term_variables(Var,Y),
   rb_fold(mr(fsnd(ex_soln(Map)),out), Solns, Solns1, []).

ex_soln(Map, sol(Id, Expls), sol(Id, Pin, Expls1)) :-
   memberchk(Id-Pin, Map),
   foldl(ex_expl(Map), Expls, Expls1, 0, Pin). % sum for inside probability

ex_expl(Map, Expl, Expl1) --> {foldl(ex_factor(Map), Expl, Expl1, 1, Pe)}, add(Pe). % product prob
ex_factor(Map, X-PL, X-P1)   --> {pp(Map,PL,P1)}, mul(P1).
pp(_, l(P), P) :- !.
pp(Map, v(Id), Pin) :- memberchk(Id-Pin, Map).

:- meta_predicate expl_tree(+,0,-,-).
expl_tree(Table, Head, Head :- Subtrees, LogProb) :-
   copy_term(Table,Table1), % sorry
   once(member(Head :- Y/Solns, Table1)), % find first matching variant with unification
   member(Y - sol(_,_,Expls), Solns),
   member(Expl, Expls),
   maplist(subexpl_tree(Table), Expl, SubtreesLogProbs),
   maplist(pair,Subtrees,LogProbs,SubtreesLogProbs),
   sumlist(LogProbs, LogProb).

subexpl_tree(Table, (M:Subgoal)-_, Subtree-LogProb) :- !, expl_tree(Table, M:Subgoal, Subtree, LogProb).
subexpl_tree(_, L-Pin, L-LP) :- LP is log(Pin).

tree_to_tree(x, node(sel, [])).
tree_to_tree(Lab:=Val, node(nt(Lab), [node(t(Val),[])])).
tree_to_tree(_:Head :- Expls, node(nt(Label), Subnodes)) :-
   functor(Head,Label,_),
   exclude(=(x), Expls, Expls1),
   maplist(tree_to_tree, Expls1, Subnodes).

% CLPR arithmetic predicates
add(X,Y,Z) :- {Z=X+Y}.
mul(X,Y,Z) :- {Z=X*Y}.

user:portray(node(nt(Label))) :- print(Label).
user:portray(node(t(Data))) :- write('<'), print(Data).
