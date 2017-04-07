:- module(cctab, [ run_with_tables/2, run_tab_expl/2, run_sampling//2
                 , uniform_sampler//2, make_lookup_sampler/2, fallback_sampler//4
                 , cctabled/2, uniform/2, dist/2, dist/3, (:=)/2, sample/2
                 , goal_graph/2, tables_graph/2, graph_params/3, semiring_graph_fold/4
                 , graph_viterbi/4, graph_nviterbi/4, graph_inside/3, graph_counts/5 
                 , igraph_sample_tree/3, top_value/2, tree_stats/2 , print_tree/1
                 , converge/5, learn/4, learn/5, mc_perplexity/4, mc_machine/5, gibbs_posterior_machine/5
                 , mean/2, mean/5
                 , strand/0, strand/1, unfold/2  % reexport for clients to use
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
      - Modularise!
      - CLP R/Q
      - Automatic differentiation for counts?
      - Grammar with integer states instead of difference lists
      - Goal subsumption in tabling lookup
      - lazy explanation search, ccbeam etc
*/

:- use_module(library(apply_macros)).
:- use_module(library(insist)).
:- use_module(library(typedef)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(lazy),        [lazy_maplist/3, lazy_unfold_finite/4]).
:- use_module(library(math),        [stoch/3, divby/3, gammaln/2]).
:- use_module(library(listutils),   [cons//1, enumerate/2, foldr/4, split_at/4]).
:- use_module(library(callutils),   [mr/5, (*)/4, const/3]).
:- use_module(library(data/pair),   [pair/3, fst/2, fsnd/3, snd/2]).
:- use_module(library(data/tree),   [print_tree/2]).
:- use_module(library(plrand),      [ log_prob_dirichlet/3, mean_log_dirichlet/2
                                    , log_partition_dirichlet/2, kldiv_dirichlet/3]).
:- use_module(library(prob/strand), [pure//2, strand/1, strand/0]).
:- use_module(library(prob/tagged), [discrete//3, uniform//2, dirichlet//2]).
:- use_module(library(delimcc),     [p_reset/3, p_shift/2]).
:- use_module(library(ccstate),     [run_nb_ref/1, nbr_app/2, nbr_app_or_new/3, nbr_dump/1]).
:- use_module(library(rbutils),     [rb_app_or_new/5, rb_fold/4, rb_gen/3, rb_add//2, rb_app//2, rb_get//2]).
:- use_module(library(machines),    [unfold/2, unfolder/3, moore/5, mapper/3, scan0/4, (>>)/3]).
:- use_module(library(lambda2)).
:- use_module(lazymath, [ max/3, min/3, add/3, sub/3, mul/3, pow/3, exp/2, log_e/2, surp/2
                        , lse/2, stoch/2, log_stoch/2, map_sum/3, map_sum/4, patient/4, patient/3, lazy/4]).
:- use_module(ptabled, []).

:- set_prolog_flag(back_quotes, symbol_char).

:- type tables == map(variant, table).
:- type table  ---> tab(goal, map(values, list(list(factor))), list(cont)).
:- type factor ---> module:head ; @number ; sw(A):=A.
:- type cont   == pred(+values, -values).
:- type values == list(ground).
:- type graph  == list(pair(goal, list(list(factor)))).

fsnd3(P,A-X,A-Y,A-Z) :- call(P,X,Y,Z).
user:goal_expansion(fsnd3(P,SX,SY,SZ),(SX=S-X, SY=S-Y, SZ=S-Z, call(P,X,Y,Z))).

% ------------- effects -----------
:- meta_predicate :=(3,-), cctabled(0,0), sample(3,-).

bernoulli(P1,X) :- P0 is 1-P1, dist([P0-0,P1-1],X).
dirichlet(As,Ps) :- sample(pure(dirichlet(As)),Ps).

dist(Dist,X)  :- maplist(pair,Ps,Xs,Dist), p_shift(prob,dist(Ps,Xs,X)).
dist(Ps,Xs,X) :- p_shift(prob,dist(Ps,Xs,X)).
uniform(Xs,X) :- p_shift(prob,uniform(Xs,X)).
sample(P,X)   :- p_shift(prob,sample(P,X)). % currently only for sampling execution!
SW := X       :- p_shift(prob,sw(SW,X)).

sw_values(SW,Values) :- call(SW,_,Values,[]).

cctabled(TableAs,Head) :- 
   p_shift(tab, tab(TableAs,Head,Inject)), 
   call(Inject).

:- meta_predicate run_prob(3,0,?,?).
run_prob(Handler,Goal) --> {p_reset(prob, Goal, Status)}, cont_prob(Status,Handler).
cont_prob(susp(Req,Cont),H) --> call(H,Req), run_prob(H,Cont).
cont_prob(done,_) --> [].

% ------------- handlers for sampling without tabling ------------------
sample(P,sw(SW,X))      --> !, call(P,SW,X).
sample(_,dist(Ps,Xs,X)) --> !, pure(discrete(Xs,Ps),X).
sample(_,uniform(Xs,X)) --> !, pure(uniform(Xs),X).
sample(_,sample(P,X))   --> call(P,X).

run_notab(Goal) :- p_reset(tab, Goal, Status), cont_notab(Status).
cont_notab(susp(tab(_,Head,Head), Cont)) :- run_notab(Cont).
cont_notab(done).

:- meta_predicate run_sampling(4,0,+,-).
run_sampling(Sampler,Goal,S1,S2) :-
   run_notab(run_prob(sample(Sampler),Goal,S1,S2)).

uniform_sampler(SW,X) --> {sw_values(SW,Xs)}, pure(uniform(Xs),X).
lookup_sampler(Map,SW,X) --> {call(SW,ID,Xs,[]), rb_lookup(ID,Ps,Map)}, pure(discrete(Xs,Ps),X).
make_lookup_sampler(Params,cctab:lookup_sampler(Map)) :- list_to_rbtree(Params, Map).
fallback_sampler(S1, S2, SW,X) --> call(S1,SW,X) -> []; call(S2,SW,X).

% -------- handlers for tabled explanation graph building -----------
:- meta_predicate run_with_tables(0,-), run_tab(0,?), run_tab_expl(0,-).

run_with_tables(G, T) :- run_nb_ref((G, nbr_dump(T))).
run_tab_expl(G, Expl) :- term_variables(G,Ans), run_tab(run_prob(expl,G,Expl,[]), Ans-Expl).

expl(tab(G))     --> {term_to_ground(G,F)}, [F].
expl(sw(SW,X))   --> {call(SW,ID,Xs,[]), member(X,Xs)}, [ID:=X].
expl(dist(Ps,Xs,X)) --> {member2(P,X,Ps,Xs)}, [@P].
expl(uniform(Xs,X)) --> {length(Xs,N), P is 1/N, member(X,Xs)}, [@P].

run_tab(Goal, Ans)    :- p_reset(tab, Goal, Status), cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(tab(TableAs,Head,cctab:p_shift(prob,tab(TableAs))), Cont), Ans) :-
   term_variables(Head,Y), K = (\\Y`Ans`Cont),
   term_to_ground(TableAs, Variant),
   nbr_app_or_new(Variant, new_consumer(Res,K), new_producer(Res,TableAs)),
   (  Res=solns(Solns) -> rb_gen(Y, _, Solns), run_tab(Cont, Ans)
   ;  Res=new_producer -> run_tab(producer(Variant, \\Y`Head, K, Ans), Ans)
   ).

new_consumer(solns(Solns), K, tab(V,Solns,Ks), tab(V,Solns,[K|Ks])).
new_producer(new_producer, V, tab(V,Solns,[])) :- rb_empty(Solns).

producer(Variant, Generate, KP, Ans) :-
   run_prob(expl, call(Generate, Y1), E, []),
   nbr_app(Variant, new_soln(Y1,E,Res)),
   Res=new(Ks), member(K,[KP|Ks]), call(K,Y1,Ans).

new_soln(Y1, E, Res, tab(V,Solns1,Ks), tab(V,Solns2,Ks)) :-
   rb_app_or_new(Y1, old_soln(Res,E), new_soln(Res,Ks,E), Solns1, Solns2).
new_soln(new(Ks),Ks,E,[E]).
old_soln(old,E,Es,[E|Es]).

% ----------- mapping tables to graphs --------------

:- meta_predicate goal_graph(0,-).
goal_graph(Goal, Graph) :- 
   time(run_with_tables(run_tab(findall(E,run_prob(expl,Goal,E,[]),Es), Es), Tables)),
   tables_graph(Tables, Graph0),
   prune_graph(=, top:'$top$', [(top:'$top$')-Es|Graph0], Graph).

top_value(Pairs, Top) :- memberchk((top:'$top$')-Top, Pairs).

tables_graph(Tables, Graph) :-
   rb_empty(Empty),
   rb_fold(goal_expls, Tables, Empty, GMap),
   rb_visit(GMap, Graph).

goal_expls(_-tab(Goal,Solns,_)) -->
   {term_variables(Goal,Vars)},
   rb_fold(soln_expls(Goal,Vars), Solns).
soln_expls(G,Y,Y1-Es) -->
   {copy_term(G-Y,G1-Y1), numbervars(G1-Y1, 0, _)}, % NB Es is already ground
   (rb_add(G1,Es) -> []; []). % NB duplicate goals should have the same explanations!

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
   maplist((=) & sw_init(Spec)*sw_values, SWs, Params).
graph_sw(G,SW) :- member(_-Es,G), member(E,Es), member(SW:=_,E).

sw_init(uniform,Vs, Params) :- uniform_probs(Vs,Params).
sw_init(unit,   Vs, Params) :- maplist(const(1),Vs,Params).
sw_init(random, Vs, Params) :- random_probs(Vs,Params).
sw_init(F*Spec, Vs, Params) :- sw_init(Spec,Vs,P0), maplist(F, P0, Params).
sw_init(S1+S2,  Vs, Params) :- sw_init(S1,Vs,P1), sw_init(S2,Vs,P2), maplist(add,P1,P2,Params).

uniform_probs(Vals,Probs) :- length(Vals,N), P is 1/N, maplist(const(P),Vals,Probs).
random_probs(Vals,Probs)  :- maplist(const(1),Vals,Ones), dirichlet(Ones,Probs).

% manipulating parameter lists
map_sw(P,X,Y) :- maplist(fsnd(P),X,Y).
map_swc(P,X,Y) :- map_sw(maplist(P),X,Y).
map_swc(P,X,Y,Z) :- maplist(fsnd3(maplist(P)),X,Y,Z).
map_sum_sw(P,X,Sum) :- map_sum(P*snd,X,Sum).
map_sum_sw(P,X,Y,Sum) :- map_sum(f2sw1(P),X,Y,Sum).
f2sw1(P,SW-X,SW-Y,Z) :- call(P,X,Y,Z).

% --------- switch-value map -----------
pmap(X,Y) --> rb_add(X,Y) -> []; rb_get(X,Y).
pmap_sws(Map,SWs) :- setof(SW, V^X^rb_gen(SW:=V,X,Map), SWs) -> true; SWs=[].

:- meta_predicate pmap_collate(3,1,+,+,?).
pmap_collate(Conv,Def,Map,SW,SW-XX) :- 
   sw_values(SW,Vals), maplist(pmap_get(Conv,Def,Map,SW),Vals,XX).

pmap_get(Conv,Def,Map,SW,Val,X) :- 
   rb_lookup(SW:=Val, P, Map) -> call(Conv,SW:=Val,P,X); call(Def,X).

%% semiring_graph_fold(+SR:sr(A,B,C,T), +G:graph, ?:params(T), -R:list(pair(goal,C))) is det.
%
%  Folds the semiring SR over the explanation graph G, resulting in R, a list of pairs
%  of goals in the original graph with the result of the fold for that goal. Different
%  semirings can produce many kinds of parsing analysis.
semiring_graph_fold(SR, Graph, Params, GoalSums) :- 
   rb_empty(E), 
   foldl(sr_sum(SR), Graph, GoalSums, E, Map), pmap_sws(Map, SWs),
   maplist(pmap_collate(sr_param(SR),true1,Map),SWs,Params).

sr_sum(SR, Goal-Expls, Goal-Sum1) -->
   pmap(Goal,Proj),
   {sr_zero(SR,Zero), sr_proj(SR,Goal,Sum,Sum1,Proj)}, !, 
   run_right(foldr(sr_add_prod(SR),Expls), Zero, Sum).

sr_add_prod(SR, Expl) --> 
   {sr_unit(SR,Unit)}, !, 
   run_right(foldr(sr_factor(SR), Expl), Unit, Prod) <\> sr_plus(SR,Prod), !. 

sr_factor(SR, M:Head)  --> pmap(M:Head,X) <\> sr_times(SR,X), !. 
sr_factor(SR, SW:=Val) --> pmap(SW:=Val,X) <\> sr_times(SR,X), !. 
sr_factor(SR, @P)      --> {sr_inj(SR,const,P,X)}, \> sr_times(SR,X), !.
sr_param(SR,F,X,P) :- sr_inj(SR,F,P,X).

% --------- semirings ---------
sr_inj(r(I,_,_,_),  _, P, X)     :- call(I,P,X).
sr_inj(best(log), F, P, P-F).
sr_inj(best(lin), F, P, Q-F)   :- log_e(P,Q).
sr_inj(kbest,     F, P, [Q-F]) :- surp(P,Q).
sr_inj(ann(SR),   F, P, Q-F)   :- sr_inj(SR,F,P,Q).
sr_inj(R1-R2,     F, P, Q1-Q2) :- sr_inj(R1,F,P,Q1), sr_inj(R2,F,P,Q2).

sr_proj(r(_,P,_,_), _, X, Y, Y) :- call(P,X,Y).
sr_proj(best(_),  G, X-E, X-E, X-(G-E)).
sr_proj(kbest,    G, X, X, Y)         :- freeze(Y,lazy_maplist(k_tag(G),X,Y)).
sr_proj(ann(SR),  G, X-Z, W-Z, Y-G)     :- sr_proj(SR,G,X,W,Y).
sr_proj(R1-R2,    G, X1-X2, Z1-Z2, Y1-Y2) :- sr_proj(R1,G,X1,Z1,Y1), sr_proj(R2,G,X2,Z2,Y2).

sr_plus(r(_,_,_,O), X) --> call(O,X).
sr_plus(best(_),  X) --> v_max(X).
sr_plus(kbest,    X) --> lazy(k_min,X).
sr_plus(ann(SR),  X-Expl) --> sr_plus(SR,X) <\> cons(X-Expl).
sr_plus(R1-R2,    X1-X2) --> sr_plus(R1,X1) <\> sr_plus(R2,X2).

sr_times(r(_,_,O,_), X) --> call(O,X).
sr_times(best(_),  X-F) --> add(X) <\> cons(F).
sr_times(kbest,    X) --> lazy(k_mul,X).
sr_times(ann(SR),  X-F) --> sr_times(SR,X) <\> cons(X-F).
sr_times(R1-R2,    X1-X2) --> sr_times(R1,X1) <\> sr_times(R2,X2).

sr_zero(r(_,_,_,O), I) :- m_zero(O,I).
sr_zero(best(_),  Z-_)   :- m_zero(max,Z).
sr_zero(kbest,    []).
sr_zero(ann(SR),  Z-[])  :- sr_zero(SR,Z).
sr_zero(R1-R2,    Z1-Z2) :- sr_zero(R1,Z1), sr_zero(R2,Z2).

sr_unit(r(_,_,O,_), I) :- m_zero(O,I).
sr_unit(best(_),  0-[]).
sr_unit(kbest,    [0-[]]).
sr_unit(ann(SR),  U-[])  :- sr_unit(SR,U).
sr_unit(R1-R2,    U1-U2) :- sr_unit(R1,U1), sr_unit(R2,U2).

m_zero(add,0).
m_zero(mul,1).
m_zero(max,-inf).
m_zero(min,inf).
m_zero(cons,[]).

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
   semiring_graph_fold(ann(r(=,=,mul,add)), Graph, Params, IGraph).
graph_viterbi(Graph, Params, Tree, LP) :- 
   semiring_graph_fold(best(lin), Graph, Params, VGraph), top_value(VGraph, LP-Tree).
graph_nviterbi(Graph, Params, Tree, LP) :-
   semiring_graph_fold(kbest, Graph, Params, VGraph), top_value(VGraph, Expls),
   member(LP-Tree,Expls).

igraph_sample_tree(Graph, Tree, LogProb) :-
   igraph_sample_tree(Graph, top:'$top$', Tree, LogProb).
igraph_sample_tree(Graph, Head, Head - Subtrees, LogProb) :-
   memberchk(Head-(_-Expls), Graph), % Head should be unique in graph
   maplist(pair,Ps,Es,Expls), stoch(Ps,Ps1,_), dist(Ps1,Es,Expl),
   map_sum(sample_subexpl_tree(Graph), Expl, Subtrees, LogProb). 

sample_subexpl_tree(G, _-(M:Goal),  Tree,    LP) :- !, igraph_sample_tree(G, M:Goal, Tree, LP).
sample_subexpl_tree(_, P-(SW:=Val), SW:=Val, LP) :- !, LP is log(P).
sample_subexpl_tree(_, P-const,     const,   LP) :- LP is log(P).

% ---- explanation entropy ----
inside_graph_entropy(Scaling, IGraph, GoalEntropies) :- 
   rb_empty(E), 
   foldl(goal_entropy(Scaling), IGraph, GoalEntropies, E, Map), 
   rb_visit(Map, GoalEntropies).

goal_entropy(Scaling, Goal-(_ - WeightedExpls), Goal-Entropy) -->
   pmap(Goal,Entropy),
   {maplist(pair, Ws, Es, WeightedExpls), scaling_stoch(Scaling, Ws, Ps)},
   run_right(foldl(expl_entropy(Scaling),Ps,Es), 0, Entropy).

scaling_stoch(lin,X,Y) :- stoch(X,Y).
scaling_stoch(log,X,Y) :- log_stoch(X,Y).

expl_entropy(Scaling, Pe, Expl) --> 
   {when(ground(FactorsEntropy-Pe), expl_entropy(Scaling, Pe, FactorsEntropy, ExplEntropy))},
   run_right(foldl(mr(snd,factor_entropy),Expl), 0, FactorsEntropy) <\> add(ExplEntropy). 

expl_entropy(lin, Pe, HFactors, HE) :- HE is Pe*(HFactors - log(Pe)).
expl_entropy(log, Pe, HFactors, HE) :- HE is exp(Pe)*(HFactors - Pe).

factor_entropy(M:Head) --> !, pmap(M:Head,H) <\> add(H).
factor_entropy(_) --> []. 

% --------- outside probabilities, ESS ----------------
graph_counts(vit, PScaling, Graph, P1, LP-Eta) :-
   call(top_value * semiring_graph_fold(best(PScaling),Graph), P1, LP-Tree),
   when(ground(LP), tree_stats(_-Tree, Eta)).

graph_counts(io(IScaling), PScaling, Graph, P1, LP-Eta) :-
   i_scaling_info(IScaling, Min, TopBeta, TopAlpha, LP),
   scaling_info(IScaling/PScaling, SR, MakeCounts),
   semiring_graph_fold(ann(SR), Graph, P1, InsideG),
   top_value(InsideG, TopBeta-_), 
   foldl(soln_edges, InsideG, QCs, []), 
   call(group_pairs_by_key * keysort, QCs, InvGraph),
   call(rb_empty >> pmap(top:'$top$',TopAlpha), Map1),
   foldl(q_alpha(IScaling), InvGraph, Map1, Map2),
   maplist(pmap_collate(right,=(Min),Map2)*fst, P1, Grad),
   map_swc(MakeCounts, P1, Grad, Eta).
right(_,X,X).

i_scaling_info(lin, 0,    Pin, 1/Pin, LP) :- log_e(Pin,LP).
i_scaling_info(log, -inf, LP, -LP, LP).

scaling_info(lin/lin, r(=,=,mul,add),        mul).
scaling_info(lin/log, r(exp,=,mul,add),      mul_exp1).
scaling_info(log/lin, r(log_e,lse,add,cons), mul_exp2).
scaling_info(log/log, r(=,lse,add,cons),     exp_add).

exp_add(X,Y,Z) :- when(ground(X-Y), Z is exp(X+Y)).
mul_exp1(X,Y,Z) :- when(ground(X-Y), Z is exp(X)*Y).
mul_exp2(X,Y,Z) :- when(ground(X-Y), Z is X*exp(Y)).

opt(Opts, Opt) :- option(Opt, Opts, _).
soln_edges(P-(_-Expls)) --> foldl(expl_edges(P),Expls).
expl_edges(P,Pe-Expl)       --> foldl(factor_edge(Pe,P),Expl).
factor_edge(Pe,P,BetaQ-Q)   --> [Q-qc(BetaQ,Pe,P)].

q_alpha(lin,Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha, QCs), 0, AlphaQ).
q_alpha(log,Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha_log, QCs), [], Alphas), 
                       {lse(Alphas,AlphaQ)}.

qc_alpha(qc(BetaQ,Pe,P)) --> 
   pmap(P, AlphaP) <\> add(AlphaQC),
   { when(ground(BetaQ), ( BetaQ =:= 0 -> AlphaQC=0
                         ; mul(AlphaP,Pe/BetaQ,AlphaQC))) }.

qc_alpha_log(qc(BetaQ,Pe,P)) --> 
   pmap(P, AlphaP) <\> cons(AlphaQC),
   { when(ground(BetaQ), ( BetaQ =:= -inf -> AlphaQC= -inf
                         ; add(AlphaP,Pe-BetaQ,AlphaQC))) }.

:- meta_predicate accum_stats(//,-), accum_stats(//,+,-).
accum_stats(Pred,Stats) :- 
   rb_empty(C0), 
   call_dcg(Pred,C0,C1), pmap_sws(C1, SWs),
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

sw_posteriors(Prior,Eta,Post) :- map_swc(add,Eta,Prior,Post).
sw_expectations(Alphas,Probs) :- map_sw(stoch,Alphas,Probs).
sw_samples(Alphas,Probs)      :- map_sw(dirichlet,Alphas,Probs).
sw_log_prob(Alphas,Probs,LP)  :- map_sum_sw(log_prob_dirichlet,Alphas,Probs,LP).
sw_marg_log_prob(Prior,Eta,LP):- map_sum_sw(marg_log_prob,Prior,Eta,LP).

marg_log_prob(Prior,Eta,LP) :-
   maplist(add,Prior,Eta,Post),
   maplist(log_partition_dirichlet,[Prior,Post],[Bot,Top]),
   LP is Top - Bot.

learn(Method,Stats,Graph,Step) :- learn(Method,Stats,1,Graph,Step).

learn(ml, Stats, ITemp, Graph, cctab:unify3(t(P1,P2,LL))) :-
   graph_counts(Stats, lin, Graph, PP, LL-Eta),
   map_swc(pow(ITemp), P1, PP),
   map_sw(stoch, Eta, P2).

learn(map(Prior), Stats, ITemp, Graph, cctab:unify3(t(P1,P2,LL+LP))) :-
   graph_counts(Stats, lin, Graph, PP, LL-Eta),
   patient(mul(ITemp)*sw_log_prob(Prior), P1, LP),
   sw_posteriors(Prior, Eta, Post),
   map_swc(pow(ITemp), P1, PP),
   map_sw(stoch*maplist(max(0)*add(-1)), Post, P2).

learn(vb(Prior), Stats, ITemp, Graph, cctab:unify3(t(A1,A2,LL-Div))) :-
   maplist(map_swc(true2,Prior), [A1,Pi]), % establish same shape as prior
   map_swc(mul_add(ITemp,1-ITemp), Prior, EffPrior),
   map_sum_sw(log_partition_dirichlet, Prior, LogZPrior),
   patient(vb_helper(ITemp, LogZPrior, EffPrior), A1, Pi - Div),
   graph_counts(Stats, log, Graph, Pi, LL-Eta),
   map_swc(mul_add(ITemp), EffPrior, Eta, A2).

vb_helper(ITemp, LogZPrior, EffPrior, A, Pi - Div) :- 
   map_sw(mean_log_dirichlet, A, PsiA),
   map_swc(math:sub, EffPrior, A, Delta),
   map_swc(mul(ITemp), PsiA, Pi),
   map_sum_sw(log_partition_dirichlet, A, LogZA),
   map_sum_sw(map_sum(math:mul), PsiA, Delta, Diff),
   Div is Diff - LogZA + ITemp*LogZPrior.

mul_add(1,X,Y,Z) :- !, when(ground(Y), Z is X+Y).
mul_add(K,X,Y,Z) :- when(ground(Y), Z is X+K*Y).
unify3(PStats,LP,P1,P2) :- copy_term(PStats, t(P1,P2,LP)).

% --- convergence ---
:- meta_predicate converge(+,1,-,+,-).
converge(Test, Setup, [X0|History], S0, SFinal) :-
   call(Setup, Step),
   call(Step, X0, S0, S1),
   converge_x(Test, Step, X0, History, S1, SFinal).
converge_x(Test, Step, X0, [X1|History], S1, SFinal) :-
   call(Step, X1, S1, S2),
   (  converged(Test, X0, X1) -> History=[], SFinal=S2
   ;  converge_x(Test, Step, X1, History, S2, SFinal)
   ).

converged(abs(Eps), X1, X2) :- abs(X1-X2) =< Eps.
converged(rel(Del), X1, X2) :- abs((X1-X2)/(X1+X2)) =< Del.

% -------------- MCMC methods -----------------

mc_perplexity(Method, Graph, Prior, Stream) :-
   converge(rel(1e-6), learn(vb(Prior), io(lin,lin), Graph), _, Prior, VBPost),
   sw_expectations(VBPost, VBProbs), 
   call(log*fst*top_value*graph_inside(Graph), VBProbs, LogPDataGivenVBProbs),
   call(add(LogPDataGivenVBProbs)*sw_log_prob(Prior), VBProbs, LogPDataVBProbs),
   method_machine_mapper(Method, Prior, Machine, Mapper),
   unfold(call(Machine, Graph, Prior, VBProbs) 
          >> mapper(p_params_given_post(VBProbs)*Mapper) >> mean
          >> mapper(sub(LogPDataVBProbs)*log), Stream).

p_params_given_post(Probs,Post,P) :- sw_log_prob(Post,Probs,LP), P is exp(LP).

method_machine_mapper(gibbs, _,     cctab:gibbs_posterior_machine(posterior), =).
method_machine_mapper(mh,    Prior, mc_machine(mh), cctab:sw_posteriors(Prior)*mcs_counts).

gibbs_posterior_machine(Rot, Graph, Prior, P1, M) :-
   graph_inside(Graph, P0, IG),
   rotation(Rot, sw_posteriors(Prior), gstep(P0,IG), sw_samples, Step),
   unfolder(scan0(Step), P1, M).

rotation(posterior,Post, Step, Sample, Post*Step*Sample).
rotation(counts,   Post, Step, Sample, Step*Sample*Post).
rotation(params,   Post, Step, Sample, Sample*Post*Step).

gstep(P0,IG,P1,Counts) :-
   copy_term(P0-IG,P1-IG1),
   igraph_sample_tree(IG1,Tree,_),
   tree_stats(Tree, Counts).

mc_machine(Method, Graph, Prior, Probs0, M) :-
   insist(top_value(Graph, [_])), % must be a single conjunction
   graph_viterbi(Graph, Probs0, VTree, _), 
   maplist(fst,Prior,SWs),
   mcs_init(SWs, VTree, Info, State),
   make_tree_sampler(Graph, SampleGoal),
   unfolder(scan0(mc_step(Method, Info, SampleGoal, SWs, Prior)), State, M).

mc_sample(SampleGoal, SWs, Probs, T1, T2) :- 
   mct_goal(T1, Goal), call(SampleGoal, Probs, Goal, Tree),
   mct_make(SWs, Goal, Tree, T2).

make_tree_sampler(G, cctab:sample_goal(P,IG)) :- graph_inside(G, P, IG).
sample_goal(P0, IGraph0, P1, Goal, Tree) :-
   prune_graph(snd, Goal, IGraph0, ISubGraph0),
   copy_term(P0-ISubGraph0, P1-ISubGraph), 
   igraph_sample_tree(ISubGraph, Goal, _-Tree, _).

mc_step(mh, Info, SampleGoal, SWs, Prior, State1, State2) :-
   mcs_random_select(Info, TK_O, State1, StateExK),
   mcs_dcounts(StateExK, CountsExK),
   sw_posteriors(Prior, CountsExK, PostExK),
   sw_expectations(PostExK, ProbsExK),
   mc_sample(SampleGoal, SWs, ProbsExK, TK_O, TK_P),
   maplist(tree_acceptance_weight(PostExK, ProbsExK), [TK_O, TK_P], [W_O, W_P]),
   (W_P>=W_O -> Accept=1; call(bernoulli*(math:exp), W_P-W_O, Accept)),
   (Accept=0 -> State2=State1; mcs_rebuild(TK_P, StateExK, State2)).

tree_acceptance_weight(Prior, Params, Tree, W) :- 
   mct_counts(Tree, Counts), 
   sw_posteriors(Prior, Counts, Post),
   map_sum_sw(log_partition_dirichlet, Post, LZ),
   map_sum_sw(map_sum(log_mul), Params, Counts, LP),
   W is LZ - LP.
log_mul(Prob, N, X) :- X is N*log(Prob).

sw_tree_stats(SWs,Tree,Stats) :- accum_stats(tree_stats(Tree),SWs,Stats).
sw_trees_stats(SWs,Trees,Stats) :- accum_stats(tree_stats(_-Trees),SWs,Stats).

% MCS: Monte Carlo state: rbtree to map K to tree, stash counts
mcs_init(SWs, VTree, Ks, Totals-Map) :-
   length(VTree,N), numlist(1,N,Ks),
   accum_stats(tree_stats(_-VTree), SWs, Totals),
   maplist(fsnd(sw_trees_stats(SWs)), VTree, GoalsCounts),
   call(list_to_rbtree * maplist(pair,Ks), GoalsCounts, Map).
   
mcs_random_select(Ks, G-C, Totals-Map, dmhs(K,CountsExK,MapExK)) :-
   uniform(Ks,K),
   rb_delete(Map, K, G-C, MapExK),
   map_swc(sub, C, Totals, CountsExK).

mcs_rebuild(G-C, dmhs(K,CountsExK,MapExK), Totals-Map) :- 
   sw_posteriors(C, CountsExK, Totals),
   rb_insert_new(MapExK, K, G-C, Map).

mcs_dcounts(dmhs(_,CountsExK,_), CountsExK).
mcs_counts(Counts-_, Counts).
mct_goal(Goal-_, Goal).
mct_make(SWs, Goal, T, Goal-C) :- sw_trees_stats(SWs,T,C).
mct_counts(_-C,C).

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

% mean machine
mean(In,Out) :- moore(mm_step, mm_out, 0-0, In, Out).
mm_step(X) --> succ <\> add(X).
mm_out(N-S,M) :- divby(N,S,M).

mean(Zero,Add,DivBy,In,Out) :- call(Zero,Z), moore(mm_step(Add), mm_out(DivBy), 0-Z, In, Out).
mm_step(Add,X) --> succ <\> call(Add,X).
mm_out(DivBy,N-S,M) :- call(DivBy,N,S,M).

term_to_ground(T1, T2) :- copy_term_nat(T1,T2), numbervars(T2,0,_).
member2(X,Y,[X|_],[Y|_]).
member2(X,Y,[_|XX],[_|YY]) :- member2(X,Y,XX,YY).
true2(_,_).
true1(_).
