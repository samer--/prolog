:- module(ccvprob, [ run_ltree/2, dist/2, guard/1, fail_/0, cctabled/1
                   , leaf_prob/4, get_tables/2]).

/** <module> Probabilistic choice as a lazy search tree with recursive memoisation of multiple variants

This is like ccprob, but instead of memoising binary predicates (Input -> Output), it
handles predicates of any arity with separate tables for each calling pattern, like ccvmemo.
*/

:- use_module(library(clpr)).
:- use_module(library(rbutils)).
:- use_module(library(typedef)).
:- use_module(library(callutils), [mr/5]).
:- use_module(library(data/pair), [snd/2]).
:- use_module(library(delimcc), [pr_reset/3, pr_shift/2, p_shift/2]).
:- use_module(library(ccstate), [app/1, upd/2, app/2, run_state/4, get/1]).
:- use_module(library(lambda2)).
:- use_module(library(apply_macros)).

:- set_prolog_flag(back_quotes, symbol_char).

:- type tree(A) ---> leaf(A); node(list(tree(A))).
:- type ltree(A) ---> leaf(A); lnode(pred(list(ltree(A)))).
:- type ltree(L,A) ---> leaf(A); lnode(L,pred(list(ltree(A)))). % with labelled nodes

:- meta_predicate cctabled(0).
cctabled(Head) :- pr_shift(prob, mem(Head,P)), app(expl, ccvprob:c(Head-v(P))). % weird context bug
dist(Xs,X) :- pr_shift(prob, dist(Xs,P-X)), app(expl, c(@X-l(P))).
c(X,[X|T],T).

:- meta_predicate guard(0).
guard(P) :- call(P) -> true; pr_shift(prob, fail).
fail_ :- pr_shift(prob, fail).

get_tables(Tabs1, Map) :- get(Tabs), rb_fold(mr(sanitize(Map),c), Tabs, Tabs1,[]).
sanitize(Map, V-entry(Solns,_), V-Expls) :- rb_fold(mr(map_soln(Map),c), Solns, Expls,[]).

map_soln(Map, Y-e(Prob,Expls), Y-e(Prob1,Expls1)) :-
   memberchk(Prob-Prob1, Map),
   maplist(maplist(pp(Map)), Expls, Expls1),
   foldl(mr(expl_prob,add), Expls1, 0, Prob1).

leaf_prob(M, S:-Ex, S:-Ex2, P) :- 
   maplist(pp(M), Ex, Ex2),
   expl_prob(Ex2, P).

pp(_, X-l(P), X-P) :- !.
pp(Map, X-v(P), X-P1) :- memberchk(P-P1, Map).
expl_prob(Expl, P) :- foldl(mr(snd,mul),Expl,1,P). 
add(X,Y,Z) :- {Z=X+Y}.
mul(X,Y,Z) :- {Z=X*Y}.


%% run_ltree(+P:pred, -T:ltree(list(any))) is det.
:- meta_predicate run_ltree(0,-).
run_ltree(Goal,Result) :- 
   term_variables(Goal, Ans),
   pr_reset(prob, \\leaf(Ans:-Expl)`run_state(expl, Goal, Expl, []), Result).

dist(Xs,PX,K,lnode(dist(Xs), ccvprob:maplist(callprob(\\PX`K),Xs))).
fail(_,lnode(fail,=([]))).

mem(Head,P,K,Tree) :-
   term_variables(Head,Y), 
   head_to_variant(Head, Variant),
   KPY = (\\P-Y`K),
   upd(Tabs1, Tabs2),
   (  rb_update(Tabs1, Variant, entry(Ys,Ks), entry(Ys,[KPY|Ks]), Tabs2)
   -> Tree = lnode(cons(Variant,Ys), ccvprob:rb_fold(cons_expand(KPY),Ys,[]))
   ;  rb_empty(EmptySet),
      rb_insert_new(Tabs1, Variant, entry(EmptySet,[]), Tabs2),
      run_state(expl, call(\\Y`Head, YNew), Expl, []),
      app(rb_trans(Variant, entry(Ys,Ks), entry(Ys2,Ks))),
      (  rb_insert_new(Ys, YNew, e(PY,[Expl]), Ys2) % PY will be the sum of the probabilities of explanations
      -> Tree=lnode(prod(Variant,YNew), ccvprob:maplist(send_to_cont(PY-YNew),[KPY|Ks])), gensym(p,PY)
      ;  rb_update(Ys, YNew, e(PY,Expls), e(PY,[Expl|Expls]), Ys2),
         Tree=lnode(dup(Variant,YNew),=([]))
      )
   ).

cons_expand(KPY,Y-e(P,_),Ts,[T|Ts]) :- callprob(KPY,P-Y,T).
send_to_cont(PY,KPY,T) :- callprob(KPY,PY,T).
callprob(KPY,PY,T)     :- pr_reset(prob, call(KPY,PY), T).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

% for printing annotated search trees
user:portray(dist(Xs))   :- write('?'), maplist(\\F-X`X:S`format(string(S),'~2g',[F]), Xs, Ss), write(Ss).
user:portray(cons(_:X,Ys)) :- rb_keys(Ys,Vals), write('C':X>Vals).
user:portray(dup(_:X,Y)) :- write('D':X>Y).
user:portray(prod(_:X,Y)) :- write('P':X>Y).

