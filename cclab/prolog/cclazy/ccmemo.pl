:- module(ccmemo, [run_ltree/2, choose/2, guard/1, memo_nondet/3, memo_nondet/2]).

/** <module> Nondeterminism as a lazy search tree with recursive memoisation

This module uses delimited continuations to provided nondeterminism as a
control effect along with memoisation of recursive, nondeterministic
binary relations. The effect is reified as a lazy search tree.

Use run_ltree/2 to run a unary predicate in a context that supports
choose/2 for nondetermism and memo_nondet/{2,3} for creating memoised versions
of a binary predicate. The whole of this must be run inside ccstate:run_ref/1,
a context that provides mutable references as a control effect. Use treeutils.pl
to visualise the tree.
*/

:- use_module(library(rbutils)).
:- use_module(library(typedef)).
:- use_module(library(delimcc), [pr_reset/3, pr_shift/2]).
:- use_module(library(ccstate), [ref_new/2, ref_get/2, ref_app/2, ref_upd/3]).
:- use_module(library(lambda1)).

% nondeterminism as lazy search tree with recursive memoisation

:- type tree(A) ---> leaf(A); node(list(tree(A))).
:- type ltree(A) ---> leaf(A); lnode(pred(list(ltree(A)))).
:- type ltree(L,A) ---> leaf(A); lnode(L,pred(list(ltree(A)))). % with labelled nodes

:- meta_predicate memo_nondet(2,-), memo_nondet(2,-,-).
memo_nondet(P,Q) :- memo_nondet(P,Q,_).
memo_nondet(P, ccmemo:memf(P,R), ccmemo:memdump(R)) :-
   rb_empty(T),
   ref_new(T,R).

memdump(R,Memo) :-
   ref_get(R,T),
   rb_visit(T,Pairs),
   maplist(\ (K-entry(Vals,_))^(K-Vals)^true, Pairs,Memo).

memf(P,R,X,Y) :- pr_shift(nondet, mem(P,R,X,Y)).
choose(Xs,X) :- pr_shift(nondet, choose(Xs,X)).

:- meta_predicate guard(0).
guard(P) :- call(P) -> true; pr_shift(nondet, fail).

%% run_ltree(+P:pred(A), -T:ltree(A)) is det.
%  NB means nondet:prompt(ltree(A)).
:- meta_predicate run_ltree(1,-).
run_ltree(P,Result) :- pr_reset(nondet, to_ltree(P), Result).

to_ltree(P,leaf(X)) :- call(P,X).

% choose(Xs:list(B),X:B): handler(ltree(A)).
choose(Xs,X,K,lnode(choice(Xs),ccmemo:maplist(expand1(\X^K),Xs))).
fail(_,lnode(fail,=([]))).

%% mem(+P:pred(+B,-C), +R:ref(memo(B,C)), +X:B, @Y:C, +K:pred(-ltree(A)), -T:ltree(A)) is det.
mem(P,R,X,Y,K,Tree) :-
   YK = \Y^K,
   ref_upd(R,Tab,Tab1),
   (  rb_upd(X, entry(Ys,Conts), entry(Ys,[YK|Conts]), Tab, Tab1)
   -> Tree = lnode(cons(X,Ys), ccmemo:rb_fold(cons_expand1(YK),Ys,[]))
   ;  rb_empty(EmptySet),
      rb_insert_new(Tab, X, entry(EmptySet,[]), Tab1),
      call(P,X,YNew),
      ref_app(R, rb_upd(X, entry(Ys,Conts), entry(Ys2,Conts))),
      (  rb_insert_new(Ys,YNew,t,Ys2)
      -> Tree=lnode(prod(X,YNew),ccmemo:maplist(send_to_cont(YNew),[YK|Conts]))
      ;  Tree=lnode(dup(X,YNew),=([])), Ys2=Ys
      )
   ).

cons_expand1(Kx,X-_,S,[Y|S]) :- expand1(Kx,X,Y).
expand1(Kx,X,Y) :- pr_reset(nondet, call(Kx,X), Y).
send_to_cont(Y,Ky,T) :- pr_reset(nondet, call(Ky,Y), T).

% for printing annotated search trees
user:portray(choice(Xs)) :- write('?'), write(Xs).
user:portray(cons(X,Ys)) :- rb_keys(Ys,Vals), write('C':X>Vals).
user:portray(dup(X,Y)) :- write('D':X>Y).
user:portray(prod(X,Y)) :- write('P':X>Y).

