:- module(rbutils, [rb_get/4, rb_gen/3, rb_trans/5, rb_add/4]).
:- use_module(library(rbtrees)).
:- reexport(library(rbtrees)).

rb_trans(K,V1,V2,T1,T2) :- rb_update(T1,K,V1,V2,T2).
rb_add(K,V,T1,T2) :- rb_insert_new(T1,K,V,T2).
rb_get(K,V,T,T) :- rb_lookup(K,V,T).
rb_gen(K,V,T) :- rb_in(K0,V,T), K=K0.
