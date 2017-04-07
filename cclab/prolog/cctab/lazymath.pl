:- module(lazymath, [ add/3, sub/3, mul/3, max/3, min/3, stoch/2, exp/2, log_e/2, surp/2, lse/2, pow/3, log_stoch/2
                    , patient/3, patient/4, lazy/4, map_sum/3, map_sum/4]).

:- use_module(library(math), [stoch/3]).
:- use_module(library(callutils)).
:- use_module(library(insist)).

% lazy arithmetic predicates
max(X,Y,Z) :- when(ground(X-Y), Z is max(X,Y)).
min(X,Y,Z) :- when(ground(X-Y), Z is min(X,Y)).
add(X,Y,Z) :- when(ground(X-Y), Z is X+Y). %{Z=X+Y}.
sub(X,Y,Z) :- when(ground(X-Y), Z is Y-X). %{Z=X-Y}.
mul(X,Y,Z) :- when(ground(X-Y), Z is X*Y). %{Z=X*Y}.
stoch(X,Y) :- when(ground(X),   insist(stoch(X,Y,_))).
log_e(X,Y) :- when(ground(X),   Y is log(X)).
exp(X,Y)   :- when(ground(X),   Y is exp(X)).
surp(P,LP) :- when(ground(P),   LP is -log(P)).
pow(1,X,X) :- !.
pow(B,X,Y) :- when(ground(X), Y is X^B).
log_stoch(X,Y) :- when(ground(X), log_stoch_strict(X,Y)).

log_stoch_strict([_],[0]) :- !.
log_stoch_strict(LogWeights,LogProbs) :-
   log_sum_exp(LogWeights, LogTotal),
   maplist((math:sub(LogTotal)), LogWeights, LogProbs).

lse(Xs,Z) :- when(ground(Xs), log_sum_exp(Xs,Z)).

log_sum_exp([X],X) :- !.
log_sum_exp(Xs,Y) :-
   max_list(Xs,M),
   call(add_log(M)*sum_list*maplist(exp_sub(M)),Xs,Y).
exp_sub(M,X,Y) :- Y is exp(X-M).
add_log(M,X,Y) :- Y is M+log(X).

add_exp_sub(M,X,S1,S2) :- S2 is S1 + exp(X-M).

:- meta_predicate lazy(3,?,?,-), patient(2,?,-), patient(3,?,?,-).
lazy(P,X,Y,Z) :- freeze(Z,call(P,X,Y,Z)).
patient(P,X,Y) :- when(ground(X),call(P,X,Y)).
patient(P,X,Y,Z) :- when(ground(X-Y),call(P,X,Y,Z)).
user:goal_expansion(lazy(P,X,Y,Z), freeze(Z,call(P,X,Y,Z))).
user:goal_expansion(patient(P,X,Y), when(ground(X),call(P,X,Y))).
user:goal_expansion(patient(P,X,Y,Z), when(ground(X-Y),call(P,X,Y,Z))).

:- meta_predicate map_sum(2,+,-), map_sum(3,+,+,-).
map_sum(P,X,Y,Sum) :- maplist(P,X,Y,Z), sum_list(Z,Sum).
map_sum(P,X,Sum) :- maplist(P,X,Y), sum_list(Y,Sum).
user:goal_expansion(map_sum(P,X,Sum), (maplist(P,X,Y), sum_list(Y,Sum))).
user:goal_expansion(map_sum(P,X,Y,Sum), (maplist(P,X,Y,Z), sum_list(Z,Sum))).
