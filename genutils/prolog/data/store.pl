:- module( store,
	[	store_new/1
	,	store_add//2
	,	store_get//2
	,	store_set//2
	,	store_upd//3
	,	store_apply//2
   ,  store_contains//2
	]).

:- meta_predicate store_apply(+,2,+,-).

/** <module> stores

This module provides a sort of store data structure - terms
can be added to the store and then accessed using a reference
term which is returned by store_add//2.

The type =|ref(A)|= denotes the type of store references
that point to terms of type A.

All predicates except store_new/1 take store input and output 
arguments at the end so they can easily be used in a DCG with
the store as threaded state variable.

Implementation uses library(assoc).
*/

% Implementation using library assoc.pl
:- use_module(library(assoc)).

%% store_new(-H:store) is det.
%  Unifies H with an empty store.
store_new(store(0,A)) :- empty_assoc(A).

%% store_add(+Term:A, -R:ref(A), +H1:store, -H2:store) is det.
%  Add Term to store and return R as a reference to it.
store_add(Term, N1, store(N1,T1), store(N2,T2)) :- put_assoc(N1,T1,Term,T2), succ(N1,N2).

%% store_get(+R:ref(A), -Term:A, +H1:store, -H2:store) is det.
%  Get term refered to by reference R.
store_get(Ref,  Term, store(N,T), store(N,T)) :- get_assoc(Ref,T,Term).

%% store_contains(-R:ref(A), -Term:A, +H1:store, -H2:store) is nondet.
%  True if store contains A under reference R.
store_contains(Ref,  Term, store(N,T), store(N,T)) :- gen_assoc(Ref,T,Term).

%% store_set(+R:ref(A), -Term:A, +H1:store, -H2:store) is det.
%  Update the term referred to by R to take the new values Term.
store_set(Ref, Term, store(N,T1), store(N,T2)) :- get_assoc(Ref,T1,_,T2,Term).

%% store_upd(+R:ref(A), X1:A, X2:A, +H1:store, -H2:store) is det.
%  Unify X1 with the value of R in H1 and replace the value
%  with X2 to get H2. The same effect can be achieved with store_apply//2
%  but this saves a metacall.
store_upd(Ref, N1, N2, store(N,T1), store(N,T2))  :- 
	get_assoc(Ref,T1,N1,T2,N2).

%% store_apply(+R:ref(A), Op:pred(A,A), +H1:store, -H2:store) is det.
%  Apply binary predicate Op to the term in the store reference by R.
%  The old value is replaced by the new value return by Op in its
%  second argument.
store_apply(Ref, Op, store(N,T1), store(N,T2))  :- 
	get_assoc(Ref,T1,N1,T2,N2), call(Op,N1,N2).

/*
% Implementation using library rbtrees.pl
:- use_module(library(rbtrees)).
store_new(store(0,A)) :- rb_empty(A).
store_add(Term, N1, store(N1,T1), store(N2,T2)) :- rb_insert_new(T1,N1,Term,T2), succ(N1,N2).
store_get(Ref,  Term, store(N,T), store(N,T))   :- rb_lookup(Ref,Term,T).
store_set(Ref, Term, store(N,T1), store(N,T2))  :- rb_update(T1,Ref,Term,T2).
store_upd(Ref, Term, Term2, store(N,T1), store(N,T2))  :- rb_update(T1,Ref,Term,Term2,T2).
store_apply(Ref, Op, store(N,T1), store(N,T2))  :- rb_apply(T1,Ref,Op,T2).
*/

user:portray(store(N,_)) :- format('<store|~w items>',[N]).
