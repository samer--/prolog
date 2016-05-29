:- module(math, 
	[	mul/3
	,	sub/3
	,	add/3
	,	divby/3
	,	equal/3
	,	prodlist/2
	,	recip/2
	,	stoch/3
	,	to_float/2
   ,  max/3
	]).

:- use_module(library(apply_macros)).

max(X,Y,Z)   :- Z is max(X,Y).
divby(X,Y,Z) :- Z is Y/X.
sub(X,Y,Z)   :- Z is Y-X.
add(X,Y,Z)   :- Z is Y+X.
mul(X,Y,Z)   :- Z is X*Y.
equal(X,Y,V) :- X=Y -> V=1; V=0.
recip(X,Y)     :- Y is 1/X.

prodlist(L,X) :- prodlist(L,1,X).
prodlist([],X,X) :- !.
prodlist([A|AX],X,Z) :- Y is A*X, prodlist(AX,Y,Z). 


%% stoch( +X:list(nonneg), -Y:list(nonneg), -Total:nonneg) is semidet.
%% stoch( +X:list(nonneg), -Y:list(nonneg)) is semidet.
%
%  Compute normalised probability distribution from histogram counts, with total.
%  Fails if total is less than or equal to zero.
stoch(H,P,N) :- sumlist(H,N), N>0, maplist(divby(N),H,P).

%% to_float( +Expr, -FloatVal:float) is det.
%  Return the floating point value of Expr as evaluated
%  by is/2.
to_float(X,Y) :- Y is float(X).
