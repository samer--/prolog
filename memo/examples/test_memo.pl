:- use_module(library(memo)).
:- use_module(library(typedef)).

:- type maybe(A) ---> nothing; just(A).
:- volatile_memo sqrt1(+float, -maybe(float)).
:- persistent_memo sqrt2( +float, -float).

sqrt1(X,just(Y)) :- X>=0, !, Y is sqrt(X).
sqrt1(_,nothing).

% this will throw an exception if X<0
sqrt2(X,Y) :- Y is sqrt(X).

:- initialization 
		db_attach('test.db',[]),  % attach database for persistent memo
		debug(memo).              % tocheck memoisation operations
