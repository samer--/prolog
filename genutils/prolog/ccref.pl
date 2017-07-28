:- module(ccref, [ run_ref/1, ref_new/2, ref_get/2, ref_set/2, ref_app/2, ref_upd/3 ]).
/** <module> Delimited context providing mutable references */

:- use_module(library(ccstate), [run_state/4, app/2]).
:- use_module(library(data/store)).

:- meta_predicate run_ref(0), ref_app(+,2).

%% run_ref(+P:pred) is det.
%  Run P inside a run_state/4 with the prompt set to =|ref|=, providing
%  a supply of mutable references using ref_new/2, ref_get/2, ref_set/3 etc.
run_ref(Goal) :-
   store_new(S),
   run_state(ref, Goal, S, _).

ref_new(X,R) :- app(ref, store_add(X,R)).
ref_get(R,X) :- app(ref, store_get(R,X)).
ref_set(R,X) :- app(ref, store_set(R,X)).
ref_app(R,P) :- app(ref, store_apply(R,P)).
ref_upd(R,X,Y) :- app(ref, store_upd(R,X,Y)).
