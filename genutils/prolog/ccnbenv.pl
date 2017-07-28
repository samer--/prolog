:- module(ccnbenv, [ run_nb_env/1, env_get/2, env_app/2, env_app_or_new/3, env_dump/1 ]).

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(rbutils)).

:- meta_predicate run_nb_env(0).
run_nb_env(Goal) :-
   gensym(nbrkm,R0), rb_empty(Empty),
   setup_call_cleanup(nb_setval(R0,Empty), run(Goal,R0), cleanup(R0)).

cleanup(R0) :-
   nb_getval(R0, KeyMap),
   rb_map(KeyMap, nb_delete),
   nb_delete(R0).

run(Goal,R0) :- p_reset(nbenv,Goal,Status), cont(Status,R0).
cont(done, _).
cont(susp(P,Cont),R0) :- nb_getval(R0,M), call(P,R0,M), run(Cont,R0).

dump(Map,_,M) :- rb_map(M,nb_getval,Map).
get(K,X,_,M) :- rb_lookup(K,R,M), nb_getval(R,X).
app(K,P,_,M) :- rb_lookup(K,R,M), nb_getval(R,X), call(P,X,Y), nb_setval(R,Y).
app_or_new(K,Upd,New,R0,M1) :-
   rb_upd_or_ins(K,What,M1,M2),
   (  What=update(R,R) -> nb_getval(R,X),  call(Upd,X,Y), nb_setval(R,Y)
   ;  What=insert(R)   -> gensym(nbenv,R), call(New,X), nb_setval(R0,M2), nb_setval(R,X)
   ).

:- meta_predicate env_app(+,2), env_app_or_new(+,2,1).
env_get(K,P) :- p_shift(nbenv, app(K,V)).
env_app(K,P) :- p_shift(nbenv, app(K,P)).
env_app_or_new(K,P,Q) :- p_shift(nbenv, app_or_new(K,P,Q)).
env_dump(M) :- p_shift(nbenv, dump(M)).
