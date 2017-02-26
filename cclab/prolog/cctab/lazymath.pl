:- module(lazymath, [add/3, mul/3, max/3, stoch/2, log_e/2]).

:- use_module(library(math), [stoch/3]).
:- use_module(library(insist)).

% lazy arithmetic predicates
max(X,Y,Z) :- when(ground(X-Y),Z is max(X,Y)).
add(X,Y,Z) :- when(ground(X-Y),Z is X+Y). %{Z=X+Y}.
mul(X,Y,Z) :- when(ground(X-Y),Z is X*Y). %{Z=X*Y}.
stoch(X,Y) :- when(ground(X), insist(stoch(X,Y,_))).
log_e(X,Y) :- when(ground(X), Y is log(X)).
