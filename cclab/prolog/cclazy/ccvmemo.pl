:- module(ccvmemo, [run_ltree/2, choose/2, guard/1, cctabled/1]).

/** <module> Nondeterminism as a lazy search tree with recursive memoisation of multiple variants

This is like ccmemo, but instead of memoising binary predicates (Input -> Output), it
handles predicates of any arity with separate tables for each calling pattern.
*/

:- use_module(library(rbtrees)).
:- use_module(library(typedef)).
:- use_module(library(delimcc), [pr_reset/3, pr_shift/2]).
:- use_module(library(ccstate), [upd/1, upd/2]).
:- use_module(library(lambda1)).

% nondeterminism as lazy search tree with recursive memoisation 

:- type tree(A) ---> leaf(A); node(list(tree(A))).
:- type ltree(A) ---> leaf(A); lnode(pred(list(ltree(A)))).
:- type ltree(L,A) ---> leaf(A); lnode(L,pred(list(ltree(A)))). % with labelled nodes

:- meta_predicate cctabled(0).
cctabled(Head) :- pr_shift(nondet, mem(Head)).
choose(Xs,X) :- pr_shift(nondet, choose(Xs,X)).

:- meta_predicate guard(0).
guard(P) :- call(P) -> true; pr_shift(nondet, fail).

%% run_ltree(+P:pred, -T:ltree(list(any))) is det.
%  NB means nondet:prompt(ltree(A)).
:- meta_predicate run_ltree(1,-).
run_ltree(Goal,Result) :- 
   term_variables(Goal, Ans),
   pr_reset(nondet, \leaf(Ans)^Goal, Result).
   % pr_reset(nondet, \leaf(Goal)^Goal, Result).


% choose(Xs:list(B),X:B): handler(ltree(A)).
choose(Xs,X,K,lnode(choice(Xs),ccvmemo:maplist(expand1(\X^K),Xs))).
fail(_,lnode(fail,=([]))).

mem(Head,K,Tree) :-
   term_variables(Head,Y), 
   head_to_variant(Head, Variant),
   YK = \Y^K,
   upd(Tabs1, Tabs2),
   (  rb_update(Tabs1, Variant, entry(Ys,Ks), entry(Ys,[YK|Ks]), Tabs2)
   -> Tree = lnode(cons(Variant,Ys), ccvmemo:rb_fold(cons_expand1(YK),Ys,[]))
   ;  rb_empty(EmptySet),
      rb_insert_new(Tabs1, Variant, entry(EmptySet,[]), Tabs2),
      call(\Y^Head, YNew),
      upd(tab_upd(Variant, entry(Ys,Ks), entry(Ys2,Ks))),
      (  rb_insert_new(Ys,YNew,t,Ys2) 
      -> Tree=lnode(prod(Variant,YNew),ccvmemo:maplist(send_to_cont(YNew),[YK|Ks]))
      ;  Tree=lnode(dup(Variant,YNew),=([])), Ys2=Ys
      )
   ).

cons_expand1(Kx,X-_,S,[Y|S]) :- expand1(Kx,X,Y).
expand1(Kx,X,Y) :- pr_reset(nondet, call(Kx,X), Y).
send_to_cont(Y,Ky,T) :- pr_reset(nondet, call(Ky,Y), T).
tab_upd(K,V1,V2,T1,T2) :- rb_update(T1,K,V1,V2,T2).

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

% for printing annotated search trees
user:portray(choice(Xs)) :- write('?'), write(Xs).
user:portray(cons(_:X,Ys)) :- rb_keys(Ys,Vals), write('C':X>Vals).
user:portray(dup(_:X,Y)) :- write('D':X>Y).
user:portray(prod(_:X,Y)) :- write('P':X>Y).

