:- module(ccstate, [ run_state//1, run_state//2
                   , run_nb_state//1
                   , set/1, get/1, app/1, upd/2
                   , app/2
                   , run_ref/1
                   , ref_new/2
                   , ref_get/2
                   , ref_set/2
                   , ref_app/2
                   , ref_upd/3

                   , run_env/1
                   , env_new/2
                   , env_get/2
                   , env_set/2
                   , env_app/2
                   , env_upd/3
                   ]).

/** <module> Stateful computation as an effect using delimited control

   This module provides two kinds of stateful computation, one which undoes
   state changes on backtracking (run_state//{1,2}) and another which preserves
   state changes on backtracking (run_nb_state//1).

   On top this are built two execution contexts which provide mutable
   references (run_ref/1) and a mutable environment (run_env/1).
*/
:- use_module(library(data/store)).
:- use_module(library(data/env)).
:- use_module(library(delimcc)).

:- set_prolog_flag(generate_debug_info, false).

% ------- stateful computation reified as DCG ----------
:- meta_predicate run_state(0,+,-), run_state(+,0,+,-), 
                  run_nb_state(0,+,-), 
                  app(2), app(+,2).

%% run_state(+Pr:prompt(pred(S,S)), +P:pred, +S1:S, -S2:S) is det.
%% run_state(+P:pred, +S1:S, -S2:S) is det.
%
%  Run P in an context that allows set/1 and get/1 to be used to
%  to handle a mutable state, initially S1. The final state is unified
%  with S2. run_state/3 uses the %  prompt =|state|=.
%  State changes are undone on backtracking.
run_state(Goal) --> run_state(state, Goal).
run_state(Prompt, Goal) -->
   {p_reset(Prompt, Goal, Status)},
   cont_state(Prompt, Status).

cont_state(_,done) --> !, [].
cont_state(Prompt,susp(P,Cont)) --> call(P), run_state(Prompt, Cont).


%% run_nb_state(+P:pred, +S1:S, -S2:S) is det.
%
%  Run P in a context where get/1 and set/1 manipulate a mutable state,
%  similar to run_state/3, but state changes are not undone on backtracking.
%  Note that, to ensure preservation of state on backtracking, set/1 saves a 
%  copy of the given term, not the term itself. Implementation uses nb_getval/2
%  and nb_setval/2 with a dynamically generated key.
run_nb_state(Goal, S1, S2) :- 
   gensym(nbs,Key),
   setup_call_cleanup( nb_setval(Key, S1),
                       (run_nb_state_x(state, Goal, Key), nb_getval(Key, S2)),
                       nb_delete(Key)).

run_nb_state_x(Prompt, Goal, Key) :-
   p_reset(Prompt, Goal, Status),
   cont_nb_state(Status, Prompt, Key).

cont_nb_state(done, _, _).
cont_nb_state(susp(P,Cont), Prompt, Key) :-
   nb_getval(Key, S1), call(P, S1, S2),
   nb_setval(Key, S2),
   run_nb_state_x(Prompt, Cont, Key).


% stateful operators
get(S) :- p_shift(state,get(S)).
set(S) :- p_shift(state,set(S)).
app(P) :- p_shift(state,P).
upd(S1,S2) :- p_shift(state,trans(S1,S2)).

app(Pr,P) :- p_shift(Pr,P).

get(S,S,S).
set(S,_,S).
trans(S1,S2,S1,S2).

% --------- stateful references ----------------------
:- meta_predicate run_ref(0), ref_app(+,2).

%% run_ref(+P:pred) is det.
%  Run P inside a run_state/4 with the prompt set to =|ref|=, providing
%  a supply of mutable references using ref_new/2, ref_get/2, ref_set/3 etc.
run_ref(Goal) :-
   store_new(S),
   run_state(ref, Goal, S, _).

ref_new(X,R) :- p_shift(ref, store_add(X,R)).
ref_get(R,X) :- p_shift(ref, store_get(R,X)).
ref_set(R,X) :- p_shift(ref, store_set(R,X)).
ref_app(R,P) :- p_shift(ref, store_apply(R,P)).
ref_upd(R,X,Y) :- p_shift(ref, store_upd(R,X,Y)).

% --------- stateful environment ---------------------
:- meta_predicate run_env(0), env_app(+,2).

%% run_env(+P:pred) is det.
%  Run P inside a run_state/4 with the prompt set to =|env|=, providing
%  an environment containing mutable key-value mappings.
run_env(Goal) :-
   init_env(_,S),
   run_state(env, Goal, S, _).

env_new(R,X) :- p_shift(env, ins_key(R,X)).
env_get(R,X) :- p_shift(env, get_key(R,X)).
env_set(R,X) :- p_shift(env, set_key(R,X)).
env_app(R,P) :- p_shift(env, upd_key(R,X,Y)), call(P,X,Y).
env_upd(R,X,Y) :- p_shift(env, upd_key(R,X,Y)).
