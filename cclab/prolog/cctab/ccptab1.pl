:- module(cctab, [ run_tabled/3, cctabled/2, dist/2, event/1, (:=)/2, vals//1
                 , tables_graph/2
                 , graph_params/2, graph_pgraph/4
                 , graph_viterbi/3, graph_inside/3
                 , graph_viterbi_fn/3, graph_inside_fn/3
                 , graph_stats/5 
                 , pgraph_tree/4, tree_to_tree/2
                 ]).
/** <module> Continuation based probabilistic inference.
*/
:- use_module(library(clpr)).
:- use_module(library(rbtrees)).
:- use_module(library(typedef)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_core), [out//1]).
:- use_module(library(callutils), [mr/5]).
:- use_module(library(listutils), [rep/3, map_filter/3]).
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

dist(Xs,X) :- member(P-X, Xs), event(@P).
event(Ev) :- app(expl, out(Ev)).

:- meta_predicate :=(-,3), vals(3,?,?).
X := Lab :- call(Lab,ID,Xs,[]), member(X,Xs), event(ID->X).
vals(Lab) --> call(Lab,_).

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
   bagof(G-Es, setof(Es, Tables^tabled_solution(Tables, G, Es), [Es]), Graph).

tabled_solution(Tabs, Goal, Expls1) :-
   rb_in(_, tab(Goal0,Solns,_), Tabs),
   term_variables(Goal0,Y), 
   rb_in(Y,Expls,Solns),
   Goal=Goal0,
   sort(Expls,Expls1).

graph_params(G,Params)   :- setof(L, graph_lab(G,L), Labs), maplist(lab_spec,Labs,Params).
graph_lab(G,Lab)         :- member(_-Es, G), member(E, Es), member(Lab->_, E).
lab_spec(Lab,Lab-Params) :- call(Lab,_,Values,[]), length(Values,N), P is 1/N, rep(N,P,Params).

% --------- graphs with probabilities -----------
:- type p_graph == list(p_soln).
:- type p_soln ---> soln(goal, number, list(pair(list(pair(p_factor, number)), number))).
:- type p_factor ---> module:head ; @number.

% inside and viterbi probs
graph_inside(Graph, P, PGraph)  :- graph_pgraph_p(add,Graph,PGraph,P).
graph_viterbi(Graph, P, PGraph) :- graph_pgraph_p(max,Graph,PGraph,P).

graph_inside_fn(Graph, PGraph, cctab:pmap_params(M))  :- graph_pgraph(add,Graph,PGraph,M).
graph_viterbi_fn(Graph, PGraph, cctab:pmap_params(M)) :- graph_pgraph(max,Graph,PGraph,M).

graph_pgraph_p(Agg, G, PG, Params) :- 
   graph_pgraph(Agg, G, PG, Map),
   pmap_params(Map, Params).

graph_pgraph(Agg, G, PG, Map) :- 
   rb_empty(E), 
   foldl(p_soln(Agg), G, PG, E, Map).

pmap_params(Map, Params) :-
   setof(Lab, pmap_lab(Map,Lab), Labs), 
   maplist(lab_info(pmap_lab_val_prob(Map)),Labs,Params).

p_soln(Agg, Goal-Expls, soln(Goal, Pin, Expls1)) -->
   pmap(Goal,Pin),
   run_right(foldl(p_expl(Agg), Expls, Expls1), 0, Pin).

p_expl(Agg, Expl, Expl1-Pe) --> run_right(foldl(p_factor, Expl, Expl1), 1, Pe) <\> call(Agg,Pe). 
p_factor(M:Atom, (M:Atom)-P) --> pmap(M:Atom,P) <\> mul(P).
p_factor(Lab->Val, (Lab->Val)-P) --> pmap(Lab->Val, P) <\> mul(P).
p_factor(@P, e-P) --> \> mul(P).

pmap(X,Y,M1,M2) :- 
   ( rb_insert_new(M1,X,Y,M2) -> true
   ; rb_lookup(X,Y,M1), M1=M2
   ).

pmap_lab(Map,Lab) :- rb_in(Lab->_,_,Map).
pmap_lab_val_prob(Map,Lab,Val,Prob) :- rb_lookup(Lab->Val,Prob,Map) -> true; true. % unify if present
pmap_lab_val_count(Map,Lab,Val,Count) :- rb_lookup(Lab->Val,Alpha-Beta,Map) -> Count is Alpha*Beta; Count=0.
pmap_lab_val_grad(Map,Lab,Val,Alpha) :- rb_lookup(Lab->Val,Alpha,Map) -> true; Alpha=0.
lab_info(Lookup,Lab,Lab-Info) :- call(Lab,_,Vals,[]), maplist(call(Lookup,Lab),Vals,Info).

% For VB: instead of P (=Pr(RV=X_i)), use exp(psi(Alpha_i) - psi(sum_i Alpha_i))

% --------- outside probabilities ----------------
:- meta_predicate graph_stats(+,0,?,-,-).
graph_stats(Graph,Goal,Params,LogProb,Stats) :-
   graph_inside(Graph, Params, IGraph),
   bagof(Q-QCs, setof(QC, q_context(IGraph,Q,QC), QCs), InvGraph),
   memberchk(soln(Goal,Pin,_),IGraph),
   % rb_empty(Empty), pmap(Goal,1/Pin,Empty, Out1),
   rb_empty(Empty), pmap(Goal,1/Pin-Pin,Empty, Out1),
   foldl(q_alpha, InvGraph, Out1, Out2),
   setof(Lab, pmap_lab(Out2,Lab), Labs), 
   maplist(lab_info(pmap_lab_val_count(Out2)),Labs,Stats),
   % maplist(lab_info(pmap_lab_val_grad(Out2)),Labs,Grads),
   % maplist(mul_lab_info,Params,Grads,Stats),
   LogProb is log(Pin).

q_context(IGraph, QBetaQ, qc(Pe,P)) :-
   member(soln(P,_,Expls),IGraph),
   member(Expl-Pe,Expls), 
   member(QBetaQ,Expl).

% q_alpha(Q-QCs) --> pmap(Q,AlphaQ), run_right(foldl(qc_alpha, QCs), 0, AlphaQ).
q_alpha((Q-BetaQ)-QCs) --> pmap(Q,AlphaQ-BetaQ), run_right(foldl(qc_alpha(BetaQ), QCs), 0, AlphaQ).
% qc_alpha(qc(Pc,P)) --> {mul(AlphaP, Pc, AlphaQC)}, pmap(P,AlphaP) <\> add(AlphaQC).
qc_alpha(BetaQ,qc(Pe,P)) --> {mul(AlphaP, Pe/BetaQ, AlphaQC)}, pmap(P,AlphaP-_) <\> add(AlphaQC).

op_param(Op, Lab-Xs, Lab-Ys, Lab-Zs) :- maplist(Op,Xs,Ys,Zs).

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

tree_to_tree(@P, node(p(P),[])).
tree_to_tree(Lab->Val, node(t(Lab->Val),[])).
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
user:portray(node(p(Prob))) :- write('@'), print(Prob).
