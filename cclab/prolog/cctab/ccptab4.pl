:- module(cctab, [ run_with_tables/2, run_tab_expl/2
                 , run_sampling//2, uniform_sampler//2, make_lookup_sampler/2, fallback_sampler//4
                 , cctabled/2, dist/2, dist/3, (:=)/2, sample/1, fallback_sampler//4
                 , goal_graph/2, tables_graph/2, graph_params/3, semiring_graph_fold/4
                 , graph_viterbi/4, graph_nviterbi/4, graph_inside/3, graph_stats/3 
                 , igraph_sample_tree/3, top_value/2, tree_stats/2 , print_tree/1
                 , learn/3, iterate/4, scanner/3, mh_stream/3, mh_perplexity/3
                 , strand/0, strand/1 % reexport for clients to use
                 ]).

/** <module> Continuation based probabilistic inference.

   This module provides several services for working with probabilistic models and
   is based on the functionality of PRISM. Models are written as Prolog programs
   enriched with extra computational effects: probabilistic choice and tabling.
   Programs can be run in sampling mode or explanation mode. Explanation mode 
   results in a hypergraph representing the computation, which can then be processed
   to get:

      * inside probabilities (generalised sum-product algorithm)
      * the single best explanation (generalised Viterbi algorithm)
      * any number of explanations in order of probability (lazy k-best algorithm)
      * outside probabilities for computing parameter sufficient statistics
 
   Based on these several EM parameter learning methods are provided: maximum likelihood,
   maximum a posterior, variational Bayes, and (to come, probably) Viterbi learning.

   @tbd
      - Goal subsumption in tabling lookup
      - Grammar with integer states instead of difference lists
      - Deterministic annealing
      - Short cut uniform sampler
      - Metropolis-Hastings sampling for evidence
      - Automatic differentiation for counts?
      - Fix free energy computation (to include Dirichlet priors)
      - Alternative mhs_ and mht_ implementations
      - Add lazy list versions of learning, with convergence test
      - Modularise!
*/

:- use_module(library(apply_macros)).
:- use_module(library(typedef)).
:- use_module(library(dcg_progress)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(dcg_core),    [rep//2]).
:- use_module(library(lazy),        [lazy_maplist/3, lazy_unfold/4, lazy_unfold_finite/4]).
:- use_module(library(math),        [stoch/3, divby/3, gammaln/2, exp/2]).
:- use_module(library(listutils),   [cons//1, enumerate/2, foldr/4, split_at/4]).
:- use_module(library(callutils),   [mr/5, (*)/4, const/3]).
:- use_module(library(data/pair),   [pair/3, fst/2, fsnd/3, snd/2]).
:- use_module(library(data/tree),   [print_tree/2]).
:- use_module(library(plrand),      [mean_log_dirichlet/2, log_partition_dirichlet/2]).
:- use_module(library(prob/strand), [pure//2, strand/1, strand/0]).
:- use_module(library(prob/tagged), [discrete//3, uniform//2, dirichlet//2]).
:- use_module(library(delimcc),     [p_reset/3, p_shift/2]).
:- use_module(library(ccstate),     [run_nb_state/3, set/1, get/1]).
:- use_module(library(rbutils),     [rb_gen/3, rb_add//2, rb_trans//3, rb_app//2, rb_get//2]).
:- use_module(library(lambda2)).
:- use_module(lazymath, [max/3, min/3, add/3, sub/3, mul/3, log_e/2, lse/3, stoch/2, lazy/4, surp/2]).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

:- type tables == map(variant, table).
:- type table  ---> tab(goal, map(values, list(list(factor))), list(cont)).
:- type factor ---> module:head ; @number ; sw(A):=A.
:- type cont   == pred(+values, -values).
:- type values == list(ground).
:- type graph  == list(pair(goal, list(list(factor)))).

% ------------- effects -----------
:- meta_predicate :=(3,-), cctabled(0,0), sample(2).

SW := X    :- p_shift(prob,sw(SW,X)).
dist(Ps,Xs,X) :- p_shift(prob,dist(Ps,Xs,X)).
dist(Dist,X) :- maplist(pair,Ps,Xs,Dist), p_shift(prob,dist(Ps,Xs,X)).
sample(Goal) :- p_shift(prob,sample(Goal)). % currently only for sampling execution!

cctabled(TableAs,Head) :- 
   p_shift(tab, tab(TableAs,Head,Inject)), 
   call(Inject).

:- meta_predicate run_prob(3,0,?,?).
run_prob(Handler,Goal) --> {p_reset(prob, Goal, Status)}, cont_prob(Status,Handler).
cont_prob(susp(Req,Cont),H) --> call(H,Req), run_prob(H,Cont).
cont_prob(done,_) --> [].


% ------------- handlers for sampling without tabling ------------------
sample(P,sw(SW,X))    --> !, call(P,SW,X).
sample(_,dist(Ps,Xs,X)) --> !, pure(discrete(Xs,Ps),X).
sample(_,sample(Goal)) --> call(Goal).

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
fallback_sampler(S1, S2, SW,X) --> call(S1,SW,X) -> []; call(S2,SW,X).


% -------- handlers for tabled explanation graph building -----------
:- meta_predicate run_with_tables(0,-), run_tab(0,?), run_tab_expl(0,-).

run_with_tables(G, T) :- rb_empty(E), run_nb_state(G, E, T).
run_tab_expl(G, Expl) :- term_variables(G,Ans), run_tab(run_prob(expl,G,Expl,[]), Ans-Expl).

expl(tab(G))     --> {term_to_ground(G,F)}, [F].
expl(sw(SW,X))   --> {call(SW,ID,Xs,[]), member(X,Xs)}, [ID:=X].
expl(dist(Ps,Xs,X)) --> {member2(P,X,Ps,Xs)}, [@P].

run_tab(Goal, Ans)    :- p_reset(tab, Goal, Status), cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(tab(TableAs,Head,cctab:p_shift(prob,tab(TableAs))), Cont), Ans) :-
   term_variables(Head,Y), K= (\\Y`Ans`Cont),
   get(Tabs1),
   term_to_ground(TableAs, Variant),
   (  rb_trans(Variant, tab(V,Solns,Ks), tab(V,Solns,[K|Ks]), Tabs1, Tabs2) 
   -> set(Tabs2),          % NB. this saves a COPY of Tabs2, ...
      rb_gen(Y, _, Solns), % ... so it's ok if any variables remaining in Y ...
      run_tab(Cont, Ans)   % ... are instantiated when this continuation is run.
   ;  rb_empty(Solns), 
      rb_add(Variant, tab(TableAs,Solns,[]), Tabs1, Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \\Y`Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   run_prob(expl, call(Generate, Y1), E, []),
   get(Tabs1),
   rb_trans(Variant, tab(V,Solns1, Ks), tab(V,Solns2, Ks), Tabs1, Tabs2),
   (  rb_add(Y1, [E], Solns1, Solns2)
   -> set(Tabs2), % see above comment about instantiation of ...
      member(K,[KP|Ks]), call(K,Y1,Ans) % ... answer variables in Y1 by K
   ;  rb_trans(Y1, Es, [E|Es], Solns1, Solns2),
      set(Tabs2), fail
   ).

% ----------- mapping tables to graphs --------------

:- meta_predicate goal_graph(0,-).
goal_graph(Goal, Graph) :- 
   time(run_with_tables(run_tab(findall(E,run_prob(expl,Goal,E,[]),Es), Es), Tables)),
   time(tables_graph(Tables, Graph0)),
   time(prune_graph(=, top:'$top$', [(top:'$top$')-Es|Graph0], Graph)).

top_value(Pairs, Top) :- memberchk((top:'$top$')-Top, Pairs).

tables_graph(Tables, Graph) :-
   % does a consistency check that each goal has only one distinct set of explanations.
   bagof(G-Es, setof(Es, Tables^tabled_solution(Tables, G, Es), [Es]), Graph).

tabled_solution(Tabs, Goal, Expls1) :-
   rb_gen(_, tab(Goal,Solns,_), Tabs),
   term_variables(Goal,Y), 
   rb_gen(Y,Expls,Solns),
   numbervars(Goal-Expls, 0, _),
   sort(Expls,Expls1).

prune_graph(Mapper, Top, GL1, GL2) :-
   list_to_rbtree(GL1,G1), 
   rb_empty(E), children(Top,Mapper,G1,E,G2),
   rb_visit(G2,GL2).

children(_:=_, _, _) --> !.
children(@_,   _, _) --> !.
children(Top,  M, G) --> 
   {rb_lookup(Top,Entry,G)}, rb_add(Top,Entry),
   {call(M, Entry, Expls)},
   foldl(mr(M,foldl(mr(M,new_children(M,G)))),Expls).
new_children(M, G, F) -->
   rb_get(F,_) -> []; children(F,M,G).

% --- extracting and initialising parameters ---
graph_params(Spec,G,Params) :- 
   (setof(L, graph_sw(G,L), SWs) -> true; SWs=[]), 
   maplist(sw_init(Spec),SWs,Params).
graph_sw(G,SW) :- member(_-Es,G), member(E,Es), member(SW:=_,E).

sw_init(uniform,SW,SW-Params) :- call(SW,_,Vals,[]), uniform(Vals,Params).
sw_init(unit,SW,SW-Params)    :- call(SW,_,Vals,[]), maplist(const(1),Vals,Params).
sw_init(random,SW,SW-Params)  :- call(SW,_,Vals,[]), random_dist(Vals,Params).
sw_init(K*Spec,SW,SW-Params)  :- sw_init(Spec,SW,SW-P0), maplist(mul(K), P0, Params).

uniform(Vals,Probs) :- length(Vals,N), P is 1/N, maplist(const(P),Vals,Probs).
random_dist(Vals,Probs) :- same_length(Vals,Ws), maplist(random,Ws), stoch(Ws,Probs, _).


% --------- graphs with probabilities -----------

pmap(X,Y) --> rb_add(X,Y) -> []; rb_get(X,Y).
pmap_sw(Map,SW) :- rb_gen(SW:=_,_,Map).

:- meta_predicate pmap_collate(3,1,+,+,?).
pmap_collate(Conv,Def,Map,SW,SW-XX) :- 
   call(SW,_,Vals,[]), maplist(pmap_get(Conv,Def,Map,SW),Vals,XX).

pmap_get(Conv,Def,Map,SW,Val,X) :- 
   rb_lookup(SW:=Val, P, Map) -> call(Conv,SW:=Val,P,X); call(Def,X).

%% semiring_graph_fold(+SR:sr(A,B,C,T), +G:graph, ?:params(T), -R:list(pair(goal,C))) is det.
%
%  Folds the semiring SR over the explanation graph G, resulting in R, a list of pairs
%  of goals in the original graph with the result of the fold for that goal. Different
%  semirings can produce many kinds of parsing analysis.
semiring_graph_fold(SR, Graph, Params, GoalSums) :- 
   rb_empty(E), 
   foldl(sr_sum(SR), Graph, GoalSums, E, Map), 
   (setof(SW, pmap_sw(Map,SW), SWs) -> true; SWs=[]),
   maplist(pmap_collate(sr_param(SR),true1,Map),SWs,Params).

sr_sum(SR, Goal-Expls, Goal-Sum) -->
   pmap(Goal,Proj),
   {sr_zero(SR,Zero), sr_proj(SR,Goal,Sum,Proj)}, !, 
   run_right(foldr(sr_add_prod(SR),Expls), Zero, Sum).

sr_add_prod(SR, Expl) --> 
   {sr_unit(SR,Unit)}, !, 
   run_right(foldr(sr_factor(SR), Expl), Unit, Prod) <\> sr_plus(SR,Prod), !. 

sr_factor(SR, M:Head)  --> pmap(M:Head,X) <\> sr_times(SR,X), !. 
sr_factor(SR, SW:=Val) --> pmap(SW:=Val,X) <\> sr_times(SR,X), !. 
sr_factor(SR, @P)      --> {sr_inj(SR,const,P,X)}, \> sr_times(SR,X), !.
sr_param(SR,F,X,P) :- sr_inj(SR,F,P,X).
true1(_).

% --------- semirings ---------
sr_inj(r(I,_,_),  _, P, X)     :- call(I,P,X).
sr_inj(best,      F, P, Q-F)   :- log_e(P,Q).
sr_inj(kbest,     F, P, [Q-F]) :- surp(P,Q).
sr_inj(ann(SR),   F, P, Q-F)   :- sr_inj(SR,F,P,Q).
sr_inj(R1-R2,     F, P, Q1-Q2) :- sr_inj(R1,F,P,Q1), sr_inj(R2,F,P,Q2).

sr_proj(r(_,_,_), _, X, X).
sr_proj(best,     G, X-E, X-(G-E)).
sr_proj(kbest,    G, X, Y)         :- freeze(Y,lazy_maplist(k_tag(G),X,Y)).
sr_proj(ann(SR),  G, X-_, Y-G)     :- sr_proj(SR,G,X,Y).
sr_proj(R1-R2,    G, X1-X2, Y1-Y2) :- sr_proj(R1,G,X1,Y1), sr_proj(R2,G,X2,Y2).

sr_plus(r(_,_,O), X) --> call(O,X).
sr_plus(best,     X) --> v_max(X).
sr_plus(kbest,    X) --> lazy(k_min,X).
sr_plus(ann(SR),  X-Expl) --> sr_plus(SR,X) <\> cons(X-Expl).
sr_plus(R1-R2,    X1-X2) --> sr_plus(R1,X1) <\> sr_plus(R2,X2).

sr_times(r(_,O,_), X) --> call(O,X).
sr_times(best,     X-F) --> add(X) <\> cons(F).
sr_times(kbest,    X) --> lazy(k_mul,X).
sr_times(ann(SR),  X-F) --> sr_times(SR,X) <\> cons(X-F).
sr_times(R1-R2,    X1-X2) --> sr_times(R1,X1) <\> sr_times(R2,X2).

sr_zero(r(_,_,O), I) :- m_zero(O,I).
sr_zero(best,     Z-_)   :- m_zero(max,Z).
sr_zero(kbest,    []).
sr_zero(ann(SR),  Z-[])  :- sr_zero(SR,Z).
sr_zero(R1-R2,    Z1-Z2) :- sr_zero(R1,Z1), sr_zero(R2,Z2).

sr_unit(r(_,O,_), I) :- m_zero(O,I).
sr_unit(best,     0-[]).
sr_unit(kbest,    [0-[]]).
sr_unit(ann(SR),  U-[])  :- sr_unit(SR,U).
sr_unit(R1-R2,    U1-U2) :- sr_unit(R1,U1), sr_unit(R2,U2).

m_zero(lse,-inf).
m_zero(add,0).
m_zero(mul,1).
m_zero(max,-inf).
m_zero(min,inf).

v_max(LX-X,LY-Y,Z) :- when(ground(LX-LY),(LX>=LY -> Z=LX-X; Z=LY-Y)).

% ---- lazy k-best algebra ----
k_tag(G,L-X,L-(G-X)). % tag explanaiton with head goal
k_min([],Y,Y) :- !.
k_min(X,[],X) :- !.
k_min([X|Xs],[Y|Ys],[Z|Zs]) :-
   (  LX-_=X, LY-_=Y, LX =< LY
   -> Z=X, freeze(Zs, k_min(Xs,[Y|Ys],Zs))
   ;  Z=Y, freeze(Zs, k_min([X|Xs],Ys,Zs))
   ).

k_mul(X,Y,Z) :-
   empty_set(EmptyS), empty_heap(EmptyQ),
   k_queue(0^X-0^Y, EmptyS-EmptyQ, TQ1),
   lazy_unfold_finite(k_next, Z, TQ1, _).

k_next(L-[XF|YFs]) -->
   \> pq_get(L,P),
   {P=I^[X0|X]-J^[Y0|Y], _-XF=X0, _-YFs=Y0}, 
   {succ(J,J1)}, k_queue(I^X-J1^[Y0|Y]),
   {succ(I,I1)}, k_queue(I1^[X0|X]-J^Y).

k_queue(P) --> {P=I^X-J^Y}, \< add_to_set(I-J), {k_cost(X,Y,L)} -> \> pq_add(L,P); [].
k_cost([X0-_|_],[Y0-_|_], L) :- L is X0+Y0.

pq_add(L,P,H1,H2) :- add_to_heap(H1,L,P,H2).
pq_get(L,P,H1,H2) :- get_from_heap(H1,L,P,H2).
add_to_set(X,S1,[X|S1]) :- \+memberchk(X,S1).
empty_set([]).

% ---------- inside and viterbi probs, explanation trees -----------
graph_inside(Graph, Params, IGraph)  :- 
   semiring_graph_fold(ann(r(=,mul,add)), Graph, Params, IGraph).
graph_viterbi(Graph, Params, Tree, LP) :- 
   semiring_graph_fold(best, Graph, Params, VGraph), top_value(VGraph, LP-Tree).
graph_nviterbi(Graph, Params, Tree, LP) :-
   semiring_graph_fold(kbest, Graph, Params, VGraph), top_value(VGraph, Expls),
   member(LP-Tree,Expls).

igraph_sample_tree(Graph, Tree, LogProb) :-
   igraph_sample_tree(Graph, top:'$top$', Tree, LogProb).
igraph_sample_tree(Graph, Head, Head - Subtrees, LogProb) :-
   memberchk(Head-(_-Expls), Graph), % Head should be unique in graph
   maplist(pair,Ps,Es,Expls), stoch(Ps,Ps1,_), dist(Ps1,Es,Expl),
   maplist(sample_subexpl_tree(Graph), Expl, LogProbs, Subtrees), 
   sumlist(LogProbs, LogProb).

sample_subexpl_tree(G, _-(M:Goal), LP, Tree) :- !, igraph_sample_tree(G, M:Goal, Tree, LP).
sample_subexpl_tree(_, P-(SW:=Val), LP, SW:=Val) :- !, LP is log(P).
sample_subexpl_tree(_, P-const, LP, const) :- LP is log(P).

% --------- outside probabilities, ESS ----------------
graph_stats(Graph,Params,Opts) :-
   maplist(opt(Opts),[grad(Eta), log_prob(LP), inside(InsideG), outside(Map2)]),
   graph_inside(Graph, Params, InsideG),
   top_value(InsideG, Pin-_), log_e(Pin,LP),
   foldl(soln_edges, InsideG, QCs, []), 
   call(group_pairs_by_key*keysort, QCs, InvGraph),
   rb_empty(Empty), 
   pmap(top:'$top$', 1/Pin, Empty, Map1),
   foldl(q_alpha, InvGraph, Map1, Map2),
   maplist(pmap_collate(right,=(0),Map2)*fst, Params, Eta).
right(_,X,X).

opt(Opts, Opt) :- option(Opt, Opts, _).
soln_edges(P-(_-Expls)) --> foldl(expl_edges(P),Expls).
expl_edges(P,Pe-Expl)       --> foldl(factor_edge(Pe,P),Expl).
factor_edge(Pe,P,BetaQ-Q)   --> [Q-qc(BetaQ,Pe,P)].

q_alpha(Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha, QCs), 0, AlphaQ).
qc_alpha(qc(BetaQ,Pe,P)) --> 
   pmap(P, AlphaP) <\> add(AlphaQC),
   { when(ground(BetaQ), ( BetaQ =:= 0 -> AlphaQC=0
                         ; mul(AlphaP,Pe/BetaQ,AlphaQC))) }.

:- meta_predicate accum_stats(//,-), accum_stats(//,+,-).
accum_stats(Pred,Stats) :- 
   rb_empty(C0), 
   call_dcg(Pred,C0,C1),
   (setof(SW, cctab:pmap_sw(C1,SW), SWs) -> true; SWs=[]),
   maplist(cctab:pmap_collate(right,=(0),C1),SWs,Stats).
accum_stats(Pred,SWs,Stats) :- 
   rb_empty(C0), 
   call_dcg(Pred,C0,C1),
   maplist(cctab:pmap_collate(right,=(0),C1),SWs,Stats).

tree_stats(Tree,Counts) :- accum_stats(tree_stats(Tree),Counts).

tree_stats(_-Subtrees) --> foldl(subtree_stats,Subtrees).
subtree_stats(_-Trees) --> foldl(subtree_stats,Trees).
subtree_stats(SW:=Val) --> rb_app(SW:=Val,succ) -> []; rb_add(SW:=Val,1).
subtree_stats(const) --> [].

add_stats(Stats) --> foldl(add_sw,Stats).
add_sw(SW-Counts) --> {call(SW,_,Vals)}, foldl(add_sw_val(SW), Vals, Counts).
add_sw_val(SW,Val,N) --> rb_app(SW:=Val,add(N)) -> []; rb_add(SW:=Val,N).


% ----------------- Learning algorithms -----------------

graph_counts(io, Graph, P1, Eta, LP) :-
   graph_stats(Graph, P1, [log_prob(LP), grad(Grad)]), 
   maplist(eta, Grad, P1, Eta).

graph_counts(vit, Graph, P1, Eta, LP) :-
   graph_viterbi(Graph, P1, Tree, LP),
   when(ground(LP), tree_stats(_-Tree, Eta)).

learn(Stats-ml, Graph, cctab:unify3(t(P1,P2,LP))) :-
   graph_counts(Stats, Graph, P1, Eta, LP),
   maplist(expectation, Eta, P2).

learn(Stats-map(Prior), Graph, cctab:unify3(t(P1,P2,LP))) :-
   graph_counts(Stats, Graph, P1, Eta, LL),
   maplist(posterior_mode, Prior, Eta, P2),
   maplist(log_prob_dirichlet, Prior, P2, LPs),
   foldl(add,LPs,LL,LP).

learn(Stats-vb(Prior), Graph, cctab:unify3(t(A1,A2,LP))) :-
   same_length(Prior,A1),
   maplist(psi,A1,P1),
   graph_counts(Stats, Graph, P1, Eta, LP),
   maplist(posterior, Prior, Eta, A2).
   % !! add KL div from A2 to Prior and entropy of inside distribution

learn(gibbs(Prior), Graph, cctab:gstep(Prior,P0,IG)) :- 
   graph_inside(Graph, P0, IG).

gstep(Prior,P0,IG,LP,P1,P2) :-
   copy_term(P0-IG,P1-IG1),
   igraph_sample_tree(IG1,Tree,LP),
   tree_stats(Tree, Counts),
   maplist(posterior, Prior, Counts, Post),
   maplist(sample_dirichlet, Post, P2).

unify3(PStats,LP,P1,P2) :- copy_term(PStats, t(P1,P2,LP)).

fsnd(P,SW-X,SW-Y,SW-Z) :- call(P,X,Y,Z).
user:goal_expansion(fsnd(P,SX,SY,SZ),(SX=S-X, SY=S-Y, SZ=S-Z, call(P,X,Y,Z))).

eta(SW-Alphas,SW-Probs1,SW-Eta)          :- maplist(mul,Alphas,Probs1,Eta). 
unobserve(SW-Eta,SW-Post,SW-Prior)       :- maplist(sub,Eta,Post,Prior).
posterior(SW-Prior,SW-Eta,SW-Post)       :- maplist(add,Eta,Prior,Post).
posterior_mode(SW-Prior,SW-Eta,SW-Probs) :- maplist(add,Eta,Prior,Post), mode_dirichlet(Post,Probs).
expectation(SW-Alphas, SW-Probs)         :- stoch(Alphas,Probs).
mode_dirichlet(A,P) :- maplist(max(0)*add(-1),A,W), stoch(W,P).
sample_dirichlet(SW-Alpha,SW-Probs) :- sample(pure(dirichlet(Alpha),Probs)).
psi(SW-A, SW-P) :- when(ground(A), (mean_log_dirichlet(A,H), maplist(exp,H,P))).

log_prob_dirichlet(SW-Prior, SW-Probs, LP) :-
   length(Prior,N),
   when(ground(Probs), plrand:log_prob_Dirichlet(N,Prior,Probs,LP)).

:- meta_predicate iterate(1,?,+,-), scanner(2,1,-).
iterate(Setup, LPs) --> {call(Setup, Step)}, seqmap_with_progress(1,Step,LPs).
scanner(Sel, Setup, cctab:scan(Sel, Step)) :- call(Setup,Step).
scan(Sel,Trans,X,P1,P2) :- call(Trans,LP,P1,P2), call(Sel,t(LP,P1,P2),X).
scan0(Trans,S1,S1,S2)   :- call(Trans,S1,S2).

mean(U,M) :- moore(cctab:mm_step, cctab:mm_out, 0-0,U,M).
mm_step(X) --> succ <\> add(X).
mm_out(N-S,M) :- divby(N,S,M).

mh_perplexity(Graph, Prior, Stream) :-
   length(LPs, 10),
   iterate(learn(io-vb(Prior), Graph), LPs, Prior, VBPost),
   maplist(expectation, VBPost, VBProbs), 
   call(log*fst*top_value*graph_inside(Graph), VBProbs, LogPDataGivenVBProbs),
   maplist(log_prob_dirichlet, Prior, VBProbs, LogsPVBProbs),
   sumlist(LogsPVBProbs, LogPVBProbs),
   LogPDataVBProbs is LogPDataGivenVBProbs + LogPVBProbs,
   unfold_machine(mh_posterior(Graph, Prior, VBProbs) 
                  >> mapper(p_params_given_mhs(Prior,VBProbs)) >> mean 
                  >> mapper(sub(LogPDataVBProbs)*log), Stream).

p_params_given_mhs(Prior,Probs,Counts-_,P) :-
   maplist(posterior,Prior,Counts,Post),
   maplist(log_prob_dirichlet,Post,Probs,LPs),
   sumlist(LPs,LP), P is exp(LP).

>>(U,T,M) :- call(U, Unfolder), call(T, Unfolder, M).

mapper(F, unfolder(TA,SA), unfolder(cctab:map_step(TA,F), SA)).
map_step(T,F,Y) --> call(T,X), {call(F,X,Y)}.

moore(TB,OB,SB, unfolder(TA,SA), unfolder(cctab:moore_step(TA,TB,OB), SA-SB)).
moore_step(TA,TB,OB, Out, SA1-SB1, SA2-SB2) :- call(TA,OA,SA1,SA2), call(TB,OA,SB1,SB2), call(OB,SB2,Out).

:- meta_predicate unfold_machine(1,-).
unfold_machine(MakeMachine, Stream) :- call(MakeMachine,unfolder(T,S)), lazy_unfold(T,Stream,S,_).

% ---- Metropolis Hastings sampler ----

mh_stream(Graph, Prior, States) :-
   length(LPs, 10),
   iterate(learn(io-vb(Prior), Graph), LPs, Prior, VBPost),
   maplist(expectation, VBPost, VBProbs), 
   unfold_machine(mh_posterior(Graph,Prior,VBProbs), States).

mh_posterior(Graph, Prior, Probs0, unfolder(scan0(Stepper), State)) :-
   insist(top_value(Graph, [_])),
   graph_viterbi(Graph, Probs0, VTree, _), 
   length(VTree,N),
   maplist(fst,Prior,SWs),
   mhs_init(SWs, VTree, State),
   make_tree_sampler(Graph, SampleGoal),
   make_nat_sampler(N, SampleK), 
   Stepper=cctab:mh_step(SampleK, SampleGoal, SWs, Prior).

% one step, takes state to next state, uses sampling effect
mh_step(SampleK, SampleGoal, SWs, Prior, State1, State2) :-
   call(SampleK, K),
   mhs_extract(K, TK_O, State1, StateExK),
   mhs_dcounts(StateExK, SWs, CountsExK),
   maplist(posterior, Prior, CountsExK, PostExK),
   maplist(expectation, PostExK, ProbsExK),
   mht_goal(TK_O, GoalK), call(SampleGoal, ProbsExK, GoalK, TreeK),
   mht_make(SWs, GoalK, TreeK, TK_P),
   maplist(tree_acceptance_weight(SWs, PostExK, ProbsExK), [TK_O, TK_P], [W_O, W_P]),
   (W_P>=W_O -> Accept=1; PA is exp(W_P-W_O), bernoulli(PA, Accept)),
   (Accept=0 -> State2=State1; mhs_rebuild(TK_P, StateExK, State2)).

make_nat_sampler(N, cctab:dist(Ps,Ks)) :- numlist(1,N,Ks), uniform(Ks,Ps).
make_tree_sampler(G, cctab:sample_goal(P,IG)) :- graph_inside(G, P, IG).
sample_goal(P0, IGraph0, P1, Goal, Tree) :-
   prune_graph(snd, Goal, IGraph0, ISubGraph0),
   copy_term(P0-ISubGraph0, P1-ISubGraph), 
   igraph_sample_tree(ISubGraph, Goal, _-Tree, _).

tree_acceptance_weight(SWs, Prior, Params, Tree, W) :- 
   mht_counts(SWs, Tree, Counts), 
   map_sum(params_log_prob,     Params, Counts, LP),
   map_sum(prior_log_post_part, Prior,  Counts, LZ),
   W is LZ - LP.

params_log_prob(SW-Probs, SW-Counts, LP) :- map_sum(log_mul,Probs,Counts,LP).
log_mul(Prob, N, X) :- X is N*log(Prob).

prior_log_post_part(SW-Prior, SW-Counts, LP) :-
   maplist(add,Prior,Counts,Posterior),
   log_partition_dirichlet(Posterior,LP).

bernoulli(P1,X) :- P0 is 1-P1, dist([P0-0,P1-1],X).
map_sum(P,X,Y,Sum) :- maplist(P,X,Y,Z), sumlist(Z,Sum).

sws_tree_stats(SWs,Tree,Stats) :- accum_stats(tree_stats(Tree),SWs,Stats).
sws_trees_stats(SWs,Trees,Stats) :- accum_stats(tree_stats(_-Trees),SWs,Stats).

% ------------ MH state, multiple implementations -----------------

% % 1. List of trees, recompute counts a lot
% mhs_init(_, VTree, VTree).
% mhs_extract(K, T, Trees, Pre-Suff) :- succ(K0,K), split_at(K0,Pre,[T|Suff],Trees).
% mhs_rebuild(T, Pre-Suff, Trees) :- append(Pre,[T|Suff],Trees). 
% mhs_counts(Trees, SWs, Counts)  :- accum_stats(tree_stats(_-Trees), SWs, Counts).
% mhs_dcounts(Pre-Suff, SWs, Counts) :- accum_stats((tree_stats(_-Pre), tree_stats(_-Suff)), SWs, Counts).

% mht_goal(Goal-_, Goal).
% mht_make(_, Goal, Tree, Goal-Tree).
% mht_counts(SWs,T,C) :- sws_tree_stats(SWs,T,C).

% % 2. List of counts with total counts
% mhs_init(SWs, VTree, Totals-GoalsCounts) :-
%    accum_stats(tree_stats(_-VTree), SWs, Totals),
%    maplist(fsnd(sws_trees_stats(SWs)), VTree, GoalsCounts). 
%    % foldl(mr(snd,maplist(posterior)), GoalsCounts, Zeros, Totals).
   
% mhs_extract(K, G-C, Totals-GoalsCounts, dmhs(CountsExK,Pre,Suff)) :- 
%    succ(K0,K), split_at(K0,Pre,[G-C|Suff],GoalsCounts),
%    maplist(unobserve, C, Totals, CountsExK).

% mhs_rebuild(G-C, dmhs(CountsExK,Pre,Suff), Totals-GoalsCounts) :- 
%    maplist(posterior, C, CountsExK, Totals),
%    append(Pre,[G-C|Suff],GoalsCounts). 

% mhs_dcounts(dmhs(CountsExK,_,_), _, CountsExK).
% mhs_counts(Counts-_, _, Counts).

% mht_goal(Goal-_, Goal).
% mht_make(SWs, Goal, T, Goal-C) :- sws_trees_stats(SWs,T,C).
% mht_counts(_,_-C,C).

% 3. rbtree to map K to tree, stash counts
mhs_init(SWs, VTree, Totals-Map) :-
   accum_stats(tree_stats(_-VTree), SWs, Totals),
   maplist(fsnd(sws_trees_stats(SWs)), VTree, GoalsCounts),
   call(list_to_rbtree*enumerate, GoalsCounts, Map).
   
mhs_extract(K, G-C, Totals-Map, dmhs(K,CountsExK,MapExK)) :-
   rb_delete(Map, K, G-C, MapExK),
   maplist(unobserve, C, Totals, CountsExK).

mhs_rebuild(G-C, dmhs(K,CountsExK,MapExK), Totals-Map) :- 
   maplist(posterior, C, CountsExK, Totals),
   rb_insert_new(MapExK, K, G-C, Map).

mhs_dcounts(dmhs(_,CountsExK,_), _, CountsExK).
mhs_counts(Counts-_, _, Counts).

mht_goal(Goal-_, Goal).
mht_make(SWs, Goal, T, Goal-C) :- sws_trees_stats(SWs,T,C).
mht_counts(_,_-C,C).

% ---- tree conversion and printing ----
print_tree(T) :- tree_to_tree(T,T1), write('  '), print_tree('  ', T1), nl.

tree_to_tree(@P, node(p(P),[])).
tree_to_tree((_:SW):=Val, node(t(SW:=Val),[])).
tree_to_tree((_:Head) - Expls, node(nt(Label), Subnodes)) :-
   functor(Head,Label,_),
   exclude(=(const), Expls, Expls1),
   maplist(tree_to_tree, Expls1, Subnodes).

user:portray(node(nt(Label))) :- print(Label).
user:portray(node(t(Data))) :- write('|'), print(Data).
user:portray(node(p(Prob))) :- write('@'), print(Prob).

% ----- misc -----
term_to_ground(T1, T2) :- copy_term_nat(T1,T2), numbervars(T2,0,_).
member2(X,Y,[X|_],[Y|_]).
member2(X,Y,[_|XX],[_|YY]) :- member2(X,Y,XX,YY).
