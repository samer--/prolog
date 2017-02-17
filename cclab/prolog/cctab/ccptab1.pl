:- module(cctab, [ run_tabled/3, cctabled/2, dist/2, event/2
                 , tables_graph/2
                 , graph_viterbi/2
                 , graph_inside/2
                 , graph_outside/4
                 , expected_counts/2
                 , pgraph_tree/4, tree_to_tree/2
                 ]).
/** <module> Continuation based probabilistic inference.

*/
:- use_module(library(clpr)).
:- use_module(library(rbtrees)).
:- use_module(library(typedef)).
:- use_module(library(dcg_core), [out//1]).
:- use_module(library(callutils), [mr/5]).
:- use_module(library(listutils), [map_filter/3]).
:- use_module(library(data/pair), [pair/3, snd/2, fst/2, fsnd/3]).
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, set/1, get/1, app/2, run_state/4]).
:- use_module(library(lambda2)).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

:- type tables == map(variant, table).
:- type table  ---> tab(goal, map(values, list(list(factor))), list(cont)).
:- type factor ---> module:head ; ground-number.
:- type cont   == pred(+values, -values).
:- type values == list(ground).
:- type graph  == list(pair(goal, list(list(factor)))). % could be map(goal,list(_))

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

dist(Xs,X) :- member(P-X, Xs), event(@X,P).
event(Ev, P) :- app(expl, out(Ev-P)).

:- meta_predicate cctabled(0,0).
cctabled(TableAs,Head) :- p_shift(tab, t(TableAs,Head)), app(expl, cctab:out(TableAs)).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(t(TableAs,Head), Cont), Ans) :-
   term_variables(Head,Y), K= (\\Y`Ans`Cont),
   get(Tabs1),
   head_to_variant(TableAs, Variant),
   (  rb_update(Tabs1, Variant, tab(V,Solns,Ks), tab(V,Solns,[K|Ks]), Tabs2) 
   -> set(Tabs2), 
      rb_in(Y, _, Solns),
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
   (  rb_insert_new(Solns1, Y1, [E], Solns2)
   -> set(Tabs2),
      member(K,[KP|Ks]), call(K,Y1,Ans)
   ;  rb_update(Solns1, Y1, Es, [E|Es], Solns2),
      set(Tabs2), fail
   ).

%% run_tabled(+G:pred, F:boolean, -T:tables) is multi.
:- meta_predicate run_tabled(0,-,-).
run_tabled(Goal, Final, Tables) :- 
   rb_empty(EmptyTables),
   term_variables(Goal, Ans),
   run_nb_state((  run_tab(run_state(expl,Goal,Expl,[]), Ans-Expl), Final=false
                ;  Final=true
                ), 
                EmptyTables, Tables).

tables_graph(Tables, Graph) :-
   bagof(G-Es, 
         setof(Es, Tables^tabled_solution(Tables, G, Es), [Es]), 
         Graph).

tabled_solution(Tabs, Goal, Expls1) :-
   rb_in(_, tab(Goal0,Solns,_), Tabs),
   term_variables(Goal0,Y), 
   rb_in(Y,Expls,Solns),
   Goal=Goal0,
   sort(Expls,Expls1).

% --------- graphs with probabilities -----------
:- type p_graph == list(p_soln).
:- type p_soln ---> soln(goal, number, list(pair(list(pair(p_factor, number)), number))).
:- type p_factor ---> module:head ; @ground.

% inside and viterbi probs
graph_inside(Graph, PGraph) :- maplist(p_soln(add,_), Graph, PGraph).
graph_viterbi(Graph, PGraph) :- maplist(p_soln(max,_), Graph, PGraph).

p_soln(Agg, Map, Goal-Expls, soln(Goal, Pin, Expls1)) :-
   memberchk(Goal-Pin, Map),
   foldl(p_expl(Agg, Map), Expls, Expls1, 0, Pin).

p_expl(Agg, Map, Expl, Expl1-Pe) --> {foldl(p_factor(Map), Expl, Expl1, 1, Pe)}, call(Agg,Pe).
p_factor(Map, M:Atom, (M:Atom)-P) --> {memberchk((M:Atom)-P,Map)}, mul(P).
p_factor(_, X-P, @X-P) --> mul(P).

% --------- outside probabilities ----------------
:- meta_predicate graph_outside(+,0,-,-).
graph_outside(Graph,Goal,LogProb,Out) :-
   graph_inside(Graph, IGraph),
   bagof(Q-QCs, setof(QC, q_context(IGraph,Q,QC), QCs), InvGraph),
   maplist(q_alpha(Out), InvGraph),
   memberchk(soln(Goal,Pin,_),IGraph),
   memberchk(Goal-(1/Pin),Out),
   LogProb is log(Pin).

q_context(IGraph, QQ, qc(Pe/BetaQ,P)) :-
   member(soln(P,_,Expls),IGraph),
   member(Expl-Pe,Expls), 
   member(Q-BetaQ,Expl),
   map_factor(Q,BetaQ,QQ).

map_factor(M:G, _, M:G).
map_factor(@Ev,B, (Ev|B)).

q_alpha(Map, Q-QCs) :-
   memberchk(Q-AlphaQ, Map),
   foldl(mr(qc_alpha(Map),add), QCs, 0, AlphaQ).

qc_alpha(Map, qc(Pc,P), AlphaQC) :- 
   memberchk(P-AlphaP, Map),
   freeze(AlphaP, AlphaQC is AlphaP*Pc).

expected_counts(Out,Stats) :- map_filter(eta, Out, Stats).
eta((Ev|Beta)-Alpha,Ev-Eta) :- Eta is Alpha*Beta.

% ---------- explanation tree with log probability ------
:- meta_predicate pgraph_tree(+,0,-,-).
pgraph_tree(Graph, Head, Head :- Subtrees, LogProb) :-
   member(soln(Head,_,Expls), Graph),
   order_by([desc(Pe)], member(Expl-Pe, Expls)),
   maplist(subexpl_tree(Graph), Expl, SubtreesLogProbs),
   maplist(pair, Subtrees, LogProbs, SubtreesLogProbs),
   sumlist(LogProbs, LogProb).

subexpl_tree(G, (M:Subgoal)-_, Subtree-LogProb) :- !, pgraph_tree(G, M:Subgoal, Subtree, LogProb).
subexpl_tree(_, L-Pin, L-LP) :- LP is log(Pin).

tree_to_tree(@Ev, node(t(Ev),[])).
tree_to_tree(_:Head :- Expls, node(nt(Label), Subnodes)) :-
   functor(Head,Label,_),
   exclude(=(x), Expls, Expls1),
   maplist(tree_to_tree, Expls1, Subnodes).

% arithmetic predicates
max(X,Y,Z) :- when(ground(X-Y),Z is max(X,Y)).
add(X,Y,Z) :- when(ground(X-Y),Z is X+Y). %{Z=X+Y}.
mul(X,Y,Z) :- when(ground(X-Y),Z is X*Y). %{Z=X*Y}.
divby(K,X,Y) :- Y is X/K.

user:portray(node(nt(Label))) :- print(Label).
user:portray(node(t(Data))) :- write('|'), print(Data).
