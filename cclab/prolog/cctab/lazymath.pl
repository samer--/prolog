:- module(lazymath, [ add/3, sub/3, mul/3, max/3, min/3, stoch/2, log_e/2, surp/2, lse/3
                    , patient/3, patient/4, lazy/4, map_sum/3, map_sum/4]).

:- use_module(library(math), [stoch/3]).
:- use_module(library(insist)).

% lazy arithmetic predicates
max(X,Y,Z) :- when(ground(X-Y), Z is max(X,Y)).
min(X,Y,Z) :- when(ground(X-Y), Z is min(X,Y)).
add(X,Y,Z) :- when(ground(X-Y), Z is X+Y). %{Z=X+Y}.
sub(X,Y,Z) :- when(ground(X-Y), Z is Y-X). %{Z=X-Y}.
mul(X,Y,Z) :- when(ground(X-Y), Z is X*Y). %{Z=X*Y}.
stoch(X,Y) :- when(ground(X),   insist(stoch(X,Y,_))).
log_e(X,Y) :- when(ground(X),   Y is log(X)).
surp(P,LP) :- when(ground(P),   LP is -log(P)).
lse(X,Y,Z) :- when(ground(X-Y), log_sum_exp(X,Y,Z)).

log_sum_exp(-inf,Y,Y) :- !.
log_sum_exp(X,-inf,X) :- !.
log_sum_exp(X,Y,Z) :- M is max(X,Y), Z is M + log(exp(X-M) + exp(Y-M)).

:- meta_predicate lazy(3,?,?,-), patient(2,?,-), patient(3,?,?,-).
lazy(P,X,Y,Z) :- freeze(Z,call(P,X,Y,Z)).
patient(P,X,Y) :- when(ground(X),call(P,X,Y)).
patient(P,X,Y,Z) :- when(ground(X-Y),call(P,X,Y,Z)).
user:goal_expansion(lazy(P,X,Y,Z), freeze(Z,call(P,X,Y,Z))).
user:goal_expansion(patient(P,X,Y), when(ground(X),call(P,X,Y))).
user:goal_expansion(patient(P,X,Y,Z), when(ground(X-Y),call(P,X,Y,Z))).

:- meta_predicate map_sum(2,+,-), map_sum(3,+,+,-).
map_sum(P,X,Y,Sum) :- maplist(P,X,Y,Z), sumlist(Z,Sum).
map_sum(P,X,Sum) :- maplist(P,X,Y), sumlist(Y,Sum).
user:goal_expansion(map_sum(P,X,Sum), (maplist(P,X,Y), sumlist(Y,Sum))).
user:goal_expansion(map_sum(P,X,Y,Sum), (maplist(P,X,Y,Z), sumlist(Z,Sum))).
