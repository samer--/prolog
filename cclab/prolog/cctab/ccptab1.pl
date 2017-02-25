:- module(cctab, [ run_tabled/3, goal_graph/2
                 , run_with_tables/2, run_tab_expl/2, run_expl/2
                 , cctabled/2, dist/2, (:=)/2
                 , tables_graph/2, graph_params/3 
                 , graph_viterbi/3, graph_inside/3, graph_stats/4 
                 , pgraph_tree/4, print_tree/1
                 , em_ml/3, em_map/4, em_vb/4, iterate/4
                 ]).
/** <module> Continuation based probabilistic inference.
 TODO: handle more complex top goal by finding all explanations for it.
 Sampling execution.
 Approximate inference.
*/
:- use_module(library(rbtrees)).
:- use_module(library(apply_macros)).
:- use_module(library(typedef)).
:- use_module(library(insist)).
:- use_module(library(dcg_core), [out//1]).
:- use_module(library(dcg_progress)).
:- use_module(library(dcg_pair)).
:- use_module(library(callutils), [mr/5, (*)/4, const/3]).
:- use_module(library(math),      [stoch/3]).
:- use_module(library(plrand),    [mean_log_dirichlet/2]).
:- use_module(library(data/pair), [pair/3, fst/2, fsnd/3]).
:- use_module(library(data/tree), [print_tree/2]).
:- use_module(library(delimcc),   [p_reset/3, p_shift/2]).
:- use_module(library(ccstate),   [run_nb_state/3, set/1, get/1, app/2, run_state/4]).
:- use_module(library(lambda2)).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

:- type tables == map(variant, table).
:- type table  ---> tab(goal, map(values, list(list(factor))), list(cont)).
:- type factor ---> module:head ; @number ; prim(A)->A.
:- type cont   == pred(+values, -values).
:- type values == list(ground).
:- type graph  == list(pair(goal, list(list(factor)))).

true(_).
head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

dist(Xs,X) :- member(P-X, Xs), event(@P).
event(Ev) :- app(expl, out(Ev)).

:- meta_predicate :=(3,-).
SW := X :- call(SW,ID,Xs,[]), member(X,Xs), event(ID->X).

:- meta_predicate cctabled(0,0).
cctabled(TableAs,Head) :- 
   p_shift(tab, t(TableAs,Head)), 
   copy_term(TableAs, Factor), % protect from further instantiation
   numbervars(Factor, 0, _),
   event(Factor).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(t(TableAs,Head), Cont), Ans) :-
   term_variables(Head,Y), K= (\\Y`Ans`Cont),
   get(Tabs1),
   head_to_variant(TableAs, Variant),
   (  rb_trans(Variant, tab(V,Solns,Ks), tab(V,Solns,[K|Ks]), Tabs1, Tabs2) 
   -> set(Tabs2),         % NB. this saves a COPY of Tabs2, ...
      rb_in(Y, _, Solns), % ... so it's ok if any variables remaining in Y ...
      run_tab(Cont, Ans)  % ... are instantiated when this continuation is run.
   ;  rb_empty(Solns), 
      rb_add(Variant, tab(TableAs,Solns,[]), Tabs1, Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \\Y`Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   run_state(expl, call(Generate, Y1), E, []),
   get(Tabs1),
   rb_trans(Variant, tab(V,Solns1, Ks), tab(V,Solns2, Ks), Tabs1, Tabs2),
   (  rb_add(Y1, [E], Solns1, Solns2)
   -> set(Tabs2), % see above comment about instantiation of ...
      member(K,[KP|Ks]), call(K,Y1,Ans) % ... answer variables in Y1 by K
   ;  rb_trans(Y1, Es, [E|Es], Solns1, Solns2),
      set(Tabs2), fail
   ).

:- meta_predicate run_with_tables(0,-), run_tab_expl(0,-), run_expl(0,-).
run_with_tables(G, T) :- rb_empty(E), run_nb_state(G, E, T).
run_tab_expl(G, Expl) :- term_variables(G,Ans), run_tab(run_expl(G,Expl), Ans-Expl).
run_expl(G, Expl)     :- run_state(expl,G,Expl,[]).

%% run_tabled(+G:pred, F:maybe(list(factor)), -T:tables) is multi.
:- meta_predicate run_tabled(0,-,-), goal_graph(0,-).
run_tabled(Goal, St, Tables) :- 
   run_with_tables((run_tab_expl(Goal,Expl), St=just(Expl);  St=nothing),
                   Tables).

goal_graph(M:Goal, Graph) :- 
   run_with_tables(run_tab(findall(E,run_expl(M:Goal,E),Es), Es), Tables),
   tables_graph(Tables, Graph0),
   prune_graph(M:'$top$', [(M:'$top$')-Es|Graph0], Graph).

tables_graph(Tables, Graph) :-
   % this does a consistency check that each goal has only one distinct set of explanations.
   bagof(G-Es, setof(Es, Tables^tabled_solution(Tables, G, Es), [Es]), Graph).

tabled_solution(Tabs, Goal, Expls1) :-
   rb_in(_, tab(Goal,Solns,_), Tabs),
   term_variables(Goal,Y), 
   rb_in(Y,Expls,Solns),
   numbervars(Goal-Expls, 0, _),
   sort(Expls,Expls1).

prune_graph(Top, GL1, GL2) :-
   list_to_rbtree(GL1,G1), 
   rb_empty(E), children(G1,Top,E,G2),
   rb_visit(G2,GL2).

children(_,_->_) --> !.
children(G,Top) --> 
   {rb_lookup(Top,Expls,G)}, rb_add(Top,Expls),
   foldl(foldl(new_children(G)),Expls).
new_children(G, F) -->
   rb_get(F,_) -> []; children(G,F).

% --- parameters ---
graph_params(Spec,G,Params) :- setof(L, graph_sw(G,L), SWs), maplist(sw_init(Spec),SWs,Params).
graph_sw(G,SW) :- member(_-Es,G), member(E,Es), member(SW->_,E).

sw_init(uniform,SW,SW-Params) :- call(SW,_,Vals,[]), uniform(Vals,Params).
sw_init(K*Spec,SW,SW-Params) :- sw_init(Spec,SW,SW-P0), maplist(mul(K), P0, Params).

uniform(Vals,Probs) :- length(Vals,N), P is 1/N, maplist(const(P),Vals,Probs).

% --------- graphs with probabilities -----------
:- type p_graph == list(p_soln).
:- type p_soln ---> soln(goal, number, list(pair(list(pair(p_factor, number)), number))).
:- type p_factor ---> const; module:head ; prim(A)->A.

pmap(X,Y) --> rb_add(X,Y) -> []; rb_get(X,Y).
pmap_sw(Map,SW) :- rb_in(SW->_,_,Map).
pmap_sw_collate(Def,Map,SW,SW-Info) :- call(SW,_,Vals,[]), maplist(pmap_sw_lookup(Def,Map,SW),Vals,Info).
pmap_sw_lookup(Def,Map,SW,Val,P) :- rb_lookup(SW->Val, P, Map) -> true; call(Def,P).

% inside and viterbi probs
graph_inside(Graph, Params, PGraph)  :- graph_pgraph(add,Graph,Params,PGraph).
graph_viterbi(Graph, Params, PGraph) :- graph_pgraph(max,Graph,Params,PGraph).
graph_pgraph(Op, Graph, Params, PGraph) :- 
   rb_empty(E), 
   foldl(p_soln(Op), Graph, PGraph, E, Map), 
   setof(SW, pmap_sw(Map,SW), SWs),
   maplist(pmap_sw_collate(true,Map),SWs,Params).

p_soln(Op, Goal-Expls, soln(Goal, Pin, Expls1)) -->
   pmap(Goal,Pin),
   run_right(foldl(p_expl(Op), Expls, Expls1), 0, Pin).

p_expl(Op, Expl, Expl1-Pe) --> run_right(foldl(p_factor, Expl, Expl1), 1, Pe) <\> call(Op,Pe). 
p_factor(M:Atom, (M:Atom)-P) --> pmap(M:Atom,P) <\> mul(P).
p_factor(SW->Val, (SW->Val)-P) --> pmap(SW->Val, P) <\> mul(P).
p_factor(@P, const-P) --> \> mul(P).

% --------- outside probabilities, ESS ----------------
:- meta_predicate graph_stats(+,:,?,-).
graph_stats(Graph,Goal,Params,Opts) :-
   maplist(opt(Opts),[grad(Eta), log_prob(LP), inside(InsideG), inverse(InvGraph), outside(Map2)]),
   graph_inside(Graph, Params, InsideG),
   memberchk(soln(Goal,Pin,_),InsideG), log(Pin,LP), % !!! lookup in inside map instead?
   foldl(soln_edges,InsideG,QCs,[]), 
   call(group_pairs_by_key*keysort, QCs, InvGraph),
   rb_empty(Empty), 
   pmap(Goal,1/Pin,Empty, Map1),
   foldl(q_alpha, InvGraph, Map1, Map2),
   maplist(pmap_sw_collate(=(0),Map2)*fst, Params, Eta).

opt(Opts, Opt) :- option(Opt, Opts, _).
soln_edges(soln(P,_,Expls)) --> foldl(expl_edges(P),Expls).
expl_edges(P,Expl-Pe)       --> foldl(factor_edge(Pe,P),Expl).
factor_edge(Pe,P,Q-BetaQ)   --> [Q-qc(BetaQ,Pe,P)].

q_alpha(Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha, QCs), 0, AlphaQ).
qc_alpha(qc(BetaQ,Pe,P)) --> 
   pmap(P, AlphaP) <\> add(AlphaQC),
   % this sort of wrong, but ok, because BetaQ=0 implies that any non-zero Alpha will
   % eventually be multiplied by a zero switch probability to get a zero expected count.
   { when(ground(BetaQ), ( BetaQ =:= 0 -> AlphaQC=0
                         ; when(ground(AlphaP-Pe), AlphaQC is AlphaP*Pe/BetaQ)
                         )) }.

eta(SW-Alphas,SW-Probs1,SW-Eta) :- maplist(mul,Alphas,Probs1,Eta). 
estep(Graph, Goal, P1, Eta, LP) :-
   graph_stats(Graph, Goal, P1, [log_prob(LP), grad(Grad)]), 
   maplist(eta, Grad, P1, Eta).

:- meta_predicate em_ml(+,0,-), em_map(+,+,0,-), em_vb(+,+,0,-).

em_ml(Graph, Goal, t(P1,P2,LP)) :-
   estep(Graph, Goal, P1, Eta, LP),
   maplist(fsnd(stoch), Eta, P2).

em_map(Prior, Graph, Goal, t(P1,P2,LP)) :-
   estep(Graph, Goal, P1, Eta, LP),
   maplist(posterior_mode, Prior, Eta, P2).

posterior_mode(SW-Prior,SW-Eta,SW-Probs2) :- 
   maplist(add,Prior,Eta,Posterior),
   mode_dirichlet(Posterior,Probs2).

mode_dirichlet(A,P) :- maplist(max(0)*add(-1),A,W), stoch(W,P).

em_vb(Prior, Graph, Goal, t(A1,A2,LP)) :-
   maplist(psi,A1,P1),
   estep(Graph, Goal, P1, Eta, LP),
   maplist(posterior, Prior, Eta, A2).

psi(SW-A, SW-P) :- when(ground(A), (mean_log_dirichlet(A,H), maplist(exp,H,P))).
posterior(SW-Prior,SW-Eta,SW-Posterior) :- maplist(add,Eta,Prior,Posterior).

:- meta_predicate iterate(1,?,+,-).
iterate(Setup, LPs) --> {call(Setup, Triple)}, seqmap_with_progress(1,unify3(Triple), LPs).
unify3(PStats,LP,P1,P2) :- copy_term(PStats, t(P1,P2,LP)).

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

% ---- tree conversion and printing ----
print_tree(T) :- tree_to_tree(T,T1), write('  '), print_tree('  ', T1), nl.

tree_to_tree(@P, node(p(P),[])).
tree_to_tree((_:SW)->Val, node(t(SW->Val),[])).
tree_to_tree(_:Head :- Expls, node(nt(Label), Subnodes)) :-
   functor(Head,Label,_),
   exclude(=(x), Expls, Expls1),
   maplist(tree_to_tree, Expls1, Subnodes).

user:portray(node(nt(Label))) :- print(Label).
user:portray(node(t(Data))) :- write('|'), print(Data).
user:portray(node(p(Prob))) :- write('@'), print(Prob).

% lazy arithmetic predicates
max(X,Y,Z) :- when(ground(X-Y),Z is max(X,Y)).
add(X,Y,Z) :- when(ground(X-Y),Z is X+Y). %{Z=X+Y}.
mul(X,Y,Z) :- when(ground(X-Y),Z is X*Y). %{Z=X*Y}.
stoch(X,Y) :- when(ground(X), insist(stoch(X,Y,_))).
log(X,Y) :- when(ground(X), Y is log(X)).
exp(X,Y) :- Y is exp(X). % not lazy

rb_trans(K,V1,V2,T1,T2) :- rb_update(T1,K,V1,V2,T2).
rb_add(K,V,T1,T2) :- rb_insert_new(T1,K,V,T2).
rb_get(K,V,T,T) :- rb_lookup(K,V,T).
