:- module(ccprob, [run_prob/2, dist/2, fail_/0, memo_prob/3]).

/** <module> Probabilistic choice as a lazy search tree with recursive memoisation

This module uses delimited continuations to provided probabilistic choice as a
control effect along with memoisation of recursive, probabilistic generators.
The effect is reified as a lazy search tree with weighted branches.

Use run_lwtree/2 to run a unary predicate in a context that supports
dist/2 for probabilistic choice and memo_prob/{2,3} for creating memoised versions
of a probabilistic generators. The whole of this must be run inside ccstate:run_ref/1,
a context that provides mutable references as a control effect. You can use
treeutils.pl to visualise the search tree.
*/

:- use_module(library(clpr)).
:- use_module(library(rbutils)).
:- use_module(library(typedef)).
:- use_module(library(math), [mul/3]).
:- use_module(library(delimcc), [pr_reset/3, pr_shift/2]).
:- use_module(library(ccstate), [get/1, set/1, run_state/3, upd/2, ref_new/2, ref_get/2, ref_app/2, ref_upd/3]).
:- use_module(library(lambda2)).

:- set_prolog_flag(back_quotes, symbol_char).

:- type weighted(A) ---> number - A.
:- type dist(A) == list(weighted(A)).
:- type wtree(A) ---> leaf(A); wnode(dist(tree(A))).
:- type lwtree(L,A) ---> leaf(A); lwnode(L,pred(dist(ltree(A)))). % with labelled nodes

:- meta_predicate memo_prob(2,-), memo_prob(2,-,-).
memo_prob(P,Q) :- memo_prob(P,Q,_).
memo_prob(P, ccprob:memf(P,R), ccprob:memdump(R)) :-
   rb_empty(T),
   ref_new(T,R).

memdump(R,Memo) :-
   ref_get(R,T),
   rb_visit(T,Pairs),
   maplist(\\K-entry(_,Vals,_)`K-Vals`true, Pairs,Memo).

factor(K) :- upd(P1,P2), {P2 = K*P1}.

memf(P,R,X,Y) :- get(P0), pr_shift(prob, mem(P,R,P0,X,P1,Y)), factor(P1).
dist(Xs,X) :- pr_shift(prob, dist(Xs,X,W)), factor(W).
fail_ :- pr_shift(prob, fail).

%% run_prob(+P:pred(A), -T:lwtree(A)) is det.
:- meta_predicate run_prob(1,-).
run_prob(P,Result) :- pr_reset(prob, to_lwtree(P), Result).

to_lwtree(P,leaf(X:Prob)) :- run_state(call(P,X),1,Prob).

% dist(Xs:dist(B),X:B): handler(lwtree(A)).
dist(Xs,X,W,K,lwnode(dist(Xs),ccprob:maplist(expand1(\\W-X`K),Xs))).
fail(_,lwnode(fail,=([]))).

mem(P,R,P0,X,P1,Y,K,Tree) :-
   PYK = (\\P1-Y`K),
   ref_upd(R,Tab,Tab1),
   (  rb_update(Tab, X, entry(PP,Ys,Conts), entry(PP,Ys,[PC-PYK|Conts]), Tab1)
   -> {PC = P0/PP},
      Tree = lwnode(cons(X,Ys),ccprob:rb_fold(cons_expand1(PYK),Ys,[]))
   ;  rb_empty(EmptyDist),
      rb_insert_new(Tab, X, entry(P0,EmptyDist,[]), Tab1),
      run_state(call(P,X,YNew), P0, Prob),
      ref_app(R, rb_upd(X, entry(_,Ys,Conts), entry(P0,Ys2,Conts))),
      {PY = Prob/P0},
      (  rb_insert_new(Ys,YNew,PY,Ys2)
      -> Tree=lwnode(prod(X,YNew),ccprob:maplist(send_to_cont(PY-YNew),[1-PYK|Conts]))
      ;  {NewP = OldP+PY},
         rb_update(Ys,YNew,OldP,NewP,Ys2),
         Tree=lnode(dup(X,YNew),=([]))
      )
   ).

cons_expand1(Kx,X-W,S,[WY|S]) :- expand1(Kx,W-X,WY).
expand1(Kpx,W-X,W-Y)          :- pr_reset(prob, call(Kpx,W-X), Y).
send_to_cont(PY,P0-Ky,P0-T)   :- pr_reset(prob, call(Ky,PY), T).

% for printing annotated search trees
user:portray(dist(Xs))   :- write('?'), maplist(\\F-X`X:S`format(string(S),'~2g',[F]), Xs, Ss), write(Ss).
user:portray(cons(X,Ys)) :- rb_visit(Ys,Dist), print('C':X>Dist).
user:portray(dup(X,Y))   :- write('D':X>Y).
user:portray(prod(X,Y))  :- write('P':X>Y).
