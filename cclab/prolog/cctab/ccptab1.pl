:- module(cctab, [ run_with_tables/2, run_tab_expl/2
                 , run_sampling//2, uniform_sampler//2, lookup_sampler//3
                 , cctabled/2, dist/2, dist/3, (:=)/2
                 , goal_graph/2, tables_graph/2, graph_params/3
                 , graph_viterbi/3, graph_nviterbi/3, graph_inside/3, graph_stats/4
                 , pgraph_tree/4, pgraph_sample_tree/4, print_tree/1
                 , em_ml/2, em_map/3, em_vb/3, iterate/4
                 , with_rnd_state/1
                 ]).
/** <module> Continuation based probabilistic inference.
*/
:- use_module(library(apply_macros)).
:- use_module(library(typedef)).
:- use_module(library(lazy)).
:- use_module(library(dcg_core), [get//1, out//1]).
:- use_module(library(dcg_progress)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(math), [stoch/3]).
:- use_module(library(listutils), [takec/3]).
:- use_module(library(callutils), [mr/5, (*)/4, const/3]).
:- use_module(library(plrand),    [with_rnd_state/1, mean_log_dirichlet/2]).
:- use_module(library(prob/strand), [pure//2]).
:- use_module(library(prob/tagged), [discrete//3, uniform//2]).
:- use_module(library(data/pair), [pair/3, fst/2, fsnd/3]).
:- use_module(library(data/tree), [print_tree/2]).
:- use_module(library(delimcc),   [p_reset/3, p_shift/2]).
:- use_module(library(ccstate),   [run_nb_state/3, set/1, get/1]).
:- use_module(library(rbutils)).
:- use_module(library(lambda2)).
:- use_module(lazymath,  [max/3, add/3, mul/3, log_e/2, stoch/2]).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

:- type tables == map(variant, table).
:- type table  ---> tab(goal, map(values, list(list(factor))), list(cont)).
:- type factor ---> module:head ; @number ; prim(A)->A.
:- type cont   == pred(+values, -values).
:- type values == list(ground).
:- type graph  == list(pair(goal, list(list(factor)))).

% --- effects -----
:- meta_predicate :=(3,-), cctabled(0,0).

SW := X    :- p_shift(prob,sw(SW,X)).
dist(Ps,Xs,X) :- p_shift(prob,dist(Ps,Xs,X)).
dist(Norm,X) :- maplist(pair,Ps,Xs,Norm), p_shift(prob,dist(Ps,Xs,X)).

cctabled(TableAs,Head) :-
   p_shift(tab, tab(TableAs,Head,Inject)),
   call(Inject).

:- meta_predicate run_prob(3,0,?,?).
run_prob(Handler,Goal) --> {p_reset(prob, Goal, Status)}, cont_prob(Status,Handler).
cont_prob(susp(Req,Cont),H) --> call(H,Req), run_prob(H,Cont).
cont_prob(done,_) --> [].

% ------------- handlers for sampling without tabling ------------------
sample(_,tab(_))      --> !, [].
sample(P,sw(SW,X))    --> !, call(P,SW,X).
sample(_,dist(Ps,Xs,X)) --> pure(discrete(Xs,Ps),X).

run_notab(Goal) :- p_reset(tab, Goal, Status), cont_notab(Status).
cont_notab(susp(tab(_,Head,Head), Cont)) :- run_notab(Cont).
cont_notab(done).

:- meta_predicate run_sampling(4,0,+,-).
run_sampling(Sampler,Goal,S1,S2) :-
   run_notab(run_prob(sample(Sampler),Goal,S1,S2)).

uniform_sampler(SW,X) --> {call(SW,_,Xs,[])}, pure(uniform(Xs),X).
lookup_sampler(Map,SW,X) --> {call(SW,ID,Xs,[]), rb_lookup(ID,Ps,Map)}, pure(discrete(Xs,Ps),X).
make_lookup_sampler(Params,cctab:lookup_sampler(Map)) :-
   list_to_rbtree(Params, Map).

% -------- handlers for tabled explanation graph building -----------
:- meta_predicate run_with_tables(0,-), run_tab(0,?), run_tab_expl(0,-).

run_with_tables(G, T) :- rb_empty(E), run_nb_state(G, E, T).
run_tab_expl(G, Expl) :- term_variables(G,Ans), run_tab(run_prob(expl,G,Expl,[]), Ans-Expl).

expl(tab(G))     --> {term_to_ground(G,F)}, [F].
expl(sw(SW,X))   --> {call(SW,ID,Xs,[]), member(X,Xs)}, [ID->X].
expl(dist(Ps,Xs,X)) --> {member2(P,X,Ps,Xs)}, [@P].

run_tab(Goal, Ans)    :- p_reset(tab, Goal, Status), cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(tab(TableAs,Head,cctab:p_shift(prob,tab(TableAs))), Cont), Ans) :-
   term_variables(Head,Y), K= (\\Y`Ans`Cont),
   get(Tabs1),
   term_to_ground(TableAs, Variant),
   (  rb_upd(Variant, tab(V,Solns,Ks), tab(V,Solns,[K|Ks]), Tabs1, Tabs2)
   -> set(Tabs2),         % NB. this saves a COPY of Tabs2, ...
      rb_in(Y, _, Solns), % ... so it's ok if any variables remaining in Y ...
      run_tab(Cont, Ans)   % ... are instantiated when this continuation is run.
   ;  rb_empty(Solns),
      rb_add(Variant, tab(TableAs,Solns,[]), Tabs1, Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \\Y`Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   run_prob(expl, call(Generate, Y1), E, []),
   get(Tabs1),
   rb_upd(Variant, tab(V,Solns1, Ks), tab(V,Solns2, Ks), Tabs1, Tabs2),
   (  rb_add(Y1, [E], Solns1, Solns2)
   -> set(Tabs2), % see above comment about instantiation of ...
      member(K,[KP|Ks]), call(K,Y1,Ans) % ... answer variables in Y1 by K
   ;  rb_upd(Y1, Es, [E|Es], Solns1, Solns2),
      set(Tabs2), fail
   ).

% ----------- mapping tables to graphs --------------
:- meta_predicate goal_graph(0,-).
goal_graph(Goal, Graph) :-
   time(run_with_tables(run_tab(findall(E,run_prob(expl,Goal,E,[]),Es), Es), Tables)),
   time(tables_graph(Tables, Graph0)),
   time(prune_graph(top:'$top$', [(top:'$top$')-Es|Graph0], Graph)).

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

% --- extracting and initialising parameters ---
graph_params(Spec,G,Params) :- setof(L, graph_sw(G,L), SWs), maplist(sw_init(Spec),SWs,Params).
graph_sw(G,SW) :- member(_-Es,G), member(E,Es), member(SW->_,E).

sw_init(uniform,SW,SW-Params) :- call(SW,_,Vals,[]), uniform(Vals,Params).
sw_init(random,SW,SW-Params) :- call(SW,_,Vals,[]), random_dist(Vals,Params).
sw_init(K*Spec,SW,SW-Params)  :- sw_init(Spec,SW,SW-P0), maplist(mul(K), P0, Params).

uniform(Vals,Probs) :- length(Vals,N), P is 1/N, maplist(const(P),Vals,Probs).
random_dist(Vals,Probs) :- length(Vals,N), length(Weights,N), maplist(random,Weights), stoch(Weights,Probs, _).

% --------- graphs with probabilities -----------
:- type p_graph == list(p_soln).
:- type p_soln ---> soln(goal, number, list(pair(list(weighted(p_factor)), number))).
:- type p_factor ---> const; module:head ; prim(A)->A.

pmap(X,Y) --> rb_add(X,Y) -> []; rb_get(X,Y).
pmap_sw(Map,SW) :- rb_in(SW->_,_,Map).
pmap_collate(Def,Map,SW,SW-Info) :- call(SW,_,Vals,[]), maplist(pmap_get(Def,Map,SW),Vals,Info).
pmap_get(Def,Map,SW,Val,P) :- rb_lookup(SW->Val, P, Map) -> true; call(Def,P).

% inside and viterbi probs
graph_inside(Graph, Params, PGraph)  :- graph_pgraph(add,Graph,Params,PGraph).
graph_viterbi(Graph, Params, PGraph) :- graph_pgraph(max,Graph,Params,PGraph).
graph_pgraph(Op, Graph, Params, PGraph) :-
   rb_empty(E),
   foldl(p_soln(Op), Graph, PGraph, E, Map),
   setof(SW, pmap_sw(Map,SW), SWs),
   maplist(pmap_collate(true1,Map),SWs,Params).

true1(_).
p_soln(Op, Goal-Expls, soln(Goal, Pin, Expls1)) -->
   pmap(Goal,Pin),
   run_right(foldl(p_expl(Op), Expls, Expls1), 0, Pin).

p_expl(Op, Expl, Pe-Expl1) --> run_right(foldl(p_factor, Expl, Expl1), 1, Pe) <\> call(Op,Pe).
p_factor(M:Head, (M:Head)-P) --> pmap(M:Head,P) <\> mul(P).
p_factor(SW->Val, (SW->Val)-P) --> pmap(SW->Val, P) <\> mul(P).
p_factor(@P, const-P) --> \> mul(P).

% ---- lazy N-Viterbi algorithm ----
graph_nviterbi(Graph, Params, Tree) :-
   rb_empty(E),
   foldl(np_soln, Graph, PGraph, E, Map),
   setof(SW, pmap_sw(Map,SW), SWs),
   maplist(pmap_collate(true1,Map),SWs,Params),
   member(soln(top:'$top$', Expls), PGraph),
   member(Tree,Expls).

np_soln(Goal-Expls, soln(Goal, NExpls)) -->
   pmap(Goal,NExpls),
   run_right(foldl(np_expl, Expls), [], NExpls).

np_expl(Expl) --> run_right(foldl(np_factor, Expl), [0-[]], Pe) <\> lazy(k_min,Pe).
np_factor(M:Head) --> pmap(M:Head,NExpls) <\> lazy(k_prod,NExpls).
np_factor(SW->Val) --> pmap(SW->Val, P) <\> lazy(k_prod,[LP-(SW->Val)]), {surp(P,LP)}.
np_factor(@P) --> \> lazy(k_prod,[LP-const]), {surp(P,LP)}.

lazy(P,X,S1,S2) :- freeze(S2,call(P,X,S1,S2)).
surp(P,LP) :- when(ground(P), LP is -log(P)).

k_min([],Y,Y) :- !.
k_min(X,[],X) :- !.
k_min([X|Xs],[Y|Ys],[Z|Zs]) :-
   (  better(X,Y)
   -> Z=X, freeze(Zs, k_min(Xs,[Y|Ys],Zs))
   ;  Z=Y, freeze(Zs, k_min([X|Xs],Ys,Zs))
   ).

better(LX-_, LY-_) :- LX =< LY.

k_prod(X,Y,Z) :-
   rb_empty(EmptyT), empty_heap(EmptyQ),
   k_queue(X-Y, EmptyT-EmptyQ, TQ1),
   lazy_unfold_finite(k_next,Z,TQ1,_).

k_next(L-ZZ) -->
   \> pq_get(L,P),
   {P=[X0|X]-[Y0|Y], cons_snd(X0,Y0,ZZ)},
   k_queue(X-[Y0|Y]),
   k_queue([X0|X]-Y).

cons_snd(_-H,_-T,[H|T]).

k_queue(P) --> \< rb_add(P,t), {k_cost(P,L)} -> \> pq_add(L, P); [].
k_cost([X0-_|_]-[Y0-_|_], L) :- L is X0+Y0.

pq_add(L,P,H1,H2) :- add_to_heap(H1,L,P,H2).
pq_get(L,P,H1,H2) :- get_from_heap(H1,L,P,H2).


% --------- outside probabilities, ESS ----------------
:- meta_predicate graph_stats(+,:,?,-).
graph_stats(Graph,Goal,Params,Opts) :-
   maplist(opt(Opts),[grad(Eta), log_prob(LP), inside(InsideG), outside(Map2)]),
   graph_inside(Graph, Params, InsideG),
   memberchk(soln(Goal,Pin,_), InsideG), log_e(Pin,LP),
   foldl(soln_edges, InsideG, QCs, []),
   call(group_pairs_by_key*keysort, QCs, InvGraph),
   rb_empty(Empty),
   pmap(Goal, 1/Pin, Empty, Map1),
   foldl(q_alpha, InvGraph, Map1, Map2),
   maplist(pmap_collate(=(0),Map2)*fst, Params, Eta).

opt(Opts, Opt) :- option(Opt, Opts, _).
soln_edges(soln(P,_,Expls)) --> foldl(expl_edges(P),Expls).
expl_edges(P,Pe-Expl)       --> foldl(factor_edge(Pe,P),Expl).
factor_edge(Pe,P,Q-BetaQ)   --> [Q-qc(BetaQ,Pe,P)].

q_alpha(Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha, QCs), 0, AlphaQ).
qc_alpha(qc(BetaQ,Pe,P)) -->
   pmap(P, AlphaP) <\> add(AlphaQC),
   % this sort of wrong, but ok, because BetaQ=0 implies that any non-zero Alpha will
   % eventually be multiplied by a zero switch probability to get a zero expected count.
   { when(ground(BetaQ), (BetaQ =:= 0 -> AlphaQC=0; mul(AlphaP,Pe/BetaQ,AlphaQC))) }.

eta(SW-Alphas,SW-Probs1,SW-Eta) :- maplist(mul,Alphas,Probs1,Eta).
estep(Graph, P1, Eta, LP) :-
   graph_stats(Graph, _:'$top$', P1, [log_prob(LP), grad(Grad)]),
   maplist(eta, Grad, P1, Eta).

em_ml(Graph, t(P1,P2,LP)) :-
   estep(Graph, P1, Eta, LP),
   maplist(fsnd(stoch), Eta, P2).

em_map(Prior, Graph, t(P1,P2,LP)) :-
   estep(Graph, P1, Eta, LP),
   maplist(posterior_mode, Prior, Eta, P2).

posterior_mode(SW-Prior,SW-Eta,SW-Probs2) :-
   maplist(add,Prior,Eta,Posterior),
   mode_dirichlet(Posterior,Probs2).

mode_dirichlet(A,P) :- maplist(max(0)*add(-1),A,W), stoch(W,P).

em_vb(Prior, Graph, t(A1,A2,LP)) :-
   maplist(psi,A1,P1),
   estep(Graph, P1, Eta, LP),
   maplist(posterior, Prior, Eta, A2).

psi(SW-A, SW-P) :- when(ground(A), (mean_log_dirichlet(A,H), maplist(exp,H,P))).
posterior(SW-Prior,SW-Eta,SW-Posterior) :- maplist(add,Eta,Prior,Posterior).
exp(X,Y) :- Y is exp(X).

:- meta_predicate iterate(1,?,+,-).
iterate(Setup, LPs) --> {call(Setup, Triple)}, seqmap_with_progress(1,unify3(Triple), LPs).
unify3(PStats,LP,P1,P2) :- copy_term(PStats, t(P1,P2,LP)).

% ---------- explanation tree with log probability ------
:- meta_predicate pgraph_tree(+,:,-,-).
pgraph_tree(Graph, Head, Head :- Subtrees, LogProb) :-
   member(soln(Head,_,Expls), Graph),
   order_by([desc(Pe)], member(Pe-Expl, Expls)),
   maplist(subexpl_tree(Graph), Expl, SubtreesLogProbs),
   maplist(pair, Subtrees, LogProbs, SubtreesLogProbs),
   sumlist(LogProbs, LogProb).

subexpl_tree(G, (M:Goal)-_, Tree-LP) :- !, pgraph_tree(G, M:Goal, Tree, LP).
subexpl_tree(_, L-Pin, L-LP) :- LP is log(Pin).

% -------- sample explanation tree from posterior graph ---
:- meta_predicate pgraph_sample_tree(+,0,-,-).
pgraph_sample_tree(Graph, Head, Head :- Subtrees, LogProb) :-
   member(soln(Head,_,Expls), Graph),
   maplist(pair,Ps,Es,Expls), stoch(Ps,Ps1,_), dist(Ps1,Es,Expl),
   maplist(sample_subexpl_tree(Graph), Expl, SubtreesLogProbs),
   maplist(pair, Subtrees, LogProbs, SubtreesLogProbs),
   sumlist(LogProbs, LogProb).

sample_subexpl_tree(G, (M:Goal)-_, Tree-LP) :- !, pgraph_sample_tree(G, M:Goal, Tree, LP).
sample_subexpl_tree(_, L-Pin, L-LP) :- LP is log(Pin).

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

term_to_ground(T1, T2) :- copy_term_nat(T1,T2), numbervars(T2,0,_).
member2(X,Y,[X|_],[Y|_]).
member2(X,Y,[_|XX],[_|YY]) :- member2(X,Y,XX,YY).
