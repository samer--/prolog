:- module(lazy,
	[	lazy_unfold/5
	,	lazy_unfold/4
	,	lazy_seqmap/5
	]).

:- meta_predicate 
		lazy_seqmap(4,?,?,?,?)
	,	lazy_unfold(4,?,?,?,?)
	,	lazy_unfold(3,?,?,?)
	.

%% lazy_unfold( +P:pred(-A1,-A2,+S,-S), -XX1:list(A1), -XX2:list(A2), +S1:S, -S2:S) is det.
lazy_unfold(P,[X1|XX],[Y1|YY],S1,S3) :-
	call(P,X1,Y1,S1,S2), 
	freeze(YY,lazy_unfold(P,XX,YY,S2,S3)).

%% lazy_unfold( +P:pred(-A1,+S,-S), -XX1:list(A1), +S1:S, -S2:S) is det.
lazy_unfold(P,[X1|XX],S1,S3) :-
	call(P,X1,S1,S2), 
	freeze(XX,lazy_unfold(P,XX,S2,S3)).

%% lazy_seqmap( +P:pred(A1,A2,S,S), XX1:list(A1), XX2:list(A2), S1:S, S2:S) is det.
lazy_seqmap(_,[],[],S,S).
lazy_seqmap(P,[X1|XX],[Y1|YY],S1,S3) :-
	call(P,X1,Y1,S1,S2), 
	freeze(YY,lazy_seqmap(P,XX,YY,S2,S3)).


