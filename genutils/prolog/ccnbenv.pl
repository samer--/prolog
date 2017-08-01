:- module(ccnbenv, [ run_nb_env/1, nb_get/2, nb_app/2, nb_new/2, nb_get_or_new/3, nb_app_or_new/3, nb_dump/1 ]).

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(rbutils)).

:- meta_predicate run_nb_env(0).
run_nb_env(Goal) :-
   gensym(nbenv,R_), atom_concat(R_,'.',R0), rb_empty(Empty),
   setup_call_cleanup(nb_setval(R0,Empty-0), run(Goal,R0), cleanup(R0)).

cleanup(R0) :-
   nb_getval(R0, KeyMap-_),
   rb_map(KeyMap, nb_delete),
   nb_delete(R0).

run(Goal,E) :- p_reset(nbenv,Goal,Status), cont(Status,E).
cont(done, _).
cont(susp(P,Cont),R0) :- nb_getval(R0,M-I), call(P,R0,M,I), run(Cont,R0).

% DCG state is integer index of next reference to create
dump(Map,_,M,_)  :- rb_map(M,nb_getval,Map).
get(K,X,_,M,_)   :- rb_lookup(K,R,M), nb_getval(R,X).
app(K,P,_,M,_)   :- rb_lookup(K,R,M), aux_app(P,R).
new(K,Q,R0,M1,I) :- rb_add(K,R,M1,M2), aux_new(Q,R0,R,M2,I).
get_or_new(K,Tgt,New,R0,M1,I) :-
   rb_upd_or_ins(K,What,M1,M2),
   (  What=update(R,R) -> nb_getval(R,X),  call(Tgt,X)
   ;  What=insert(R)   -> aux_new(New,R0,R,M2,I)
   ).
app_or_new(K,Upd,New,R0,M1,I) :-
   rb_upd_or_ins(K,What,M1,M2),
   (  What=update(R,R) -> aux_app(Upd,R)
   ;  What=insert(R)   -> aux_new(New,R0,R,M2,I)
   ).

aux_app(P,R) :- nb_getval(R,X), call(P,X,Y), nb_setval(R,Y).
aux_new(New,R0,R,M2,I1) :-
   atom_concat(R0,I1,R), I2 is I1+1,
   call(New,X), nb_setval(R0,M2-I2), nb_setval(R,X).

:- meta_predicate nb_app(+,2), nb_app_or_new(+,2,1), nb_new(+,1), nb_get_or_new(+,1,1).
nb_get(K,X) :- p_shift(nbenv, get(K,X)).
nb_app(K,P) :- p_shift(nbenv, app(K,P)).
nb_new(K,P) :- p_shift(nbenv, new(K,P)).
nb_app_or_new(K,P,Q) :- p_shift(nbenv, app_or_new(K,P,Q)).
nb_get_or_new(K,P,Q) :- p_shift(nbenv, get_or_new(K,P,Q)).
nb_dump(M) :- p_shift(nbenv, dump(M)).
