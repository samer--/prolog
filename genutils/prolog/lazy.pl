:- module(lazy,
	[	lazy_unfold/5
	,	lazy_unfold/4
	,	lazy_seqmap/5
	,	lazy_seqmap/4
	]).

:- meta_predicate 
		lazy_seqmap(4,?,?,?,?)
   ,	lazy_seqmap(3,?,?,?)
	,	lazy_unfold(4,?,?,?,?)
	,	lazy_unfold(3,?,?,?)
	.

%% lazy_unfold( +P:pred(-A1,-A2,+S,-S), -XX1:list(A1), -XX2:list(A2), +S1:S, -S2:S) is det.
%% lazy_unfold( +P:pred(-A1,+S,-S), -XX1:list(A1), +S1:S, -S2:S) is det.
%
%  Lazily unfold an infinite stream
lazy_unfold(P,[X1|XX],[Y1|YY],S1,S3) :-
	call(P,X1,Y1,S1,S2), 
	freeze(YY,lazy_unfold(P,XX,YY,S2,S3)).

lazy_unfold(P,[X1|XX],S1,S3) :-
	call(P,X1,S1,S2), 
	freeze(XX,lazy_unfold(P,XX,S2,S3)).

%% lazy_unfold_finite( +P:pred(-A1,+S,-S), -XX1:list(A1), +S1:S, -S2:S) is det.
%  Lazily unfold a finite list or infinite stream. If unfolding predicate fails,
%  then a finite list is produced.
lazy_unfold_finite(P,Xs,S1,S3) :-
   (  call(P,X1,S1,S2) 
   -> Xs=[X1|XX], freeze(XX,lazy_unfold_finite(P,XX,S2,S3))
   ;  Xs=[]
   ).

%% lazy_seqmap( +P:pred(A1,A2,S,S), +XX1:list(A1), -XX2:list(A2), S1:S, S2:S) is det.
%% lazy_seqmap( +P:pred(A1,A2,S,S), -XX1:list(A1), -XX2:list(A2), S1:S, S2:S) is nondet.
%% lazy_seqmap( +P:pred(A1,S,S), -XX1:list(A1), S1:S, S2:S) is nondet.
%  
%  Lazy versions of dcg_core:seqmap//{2,3} - can succeed for any lenght of list. 
%  Computation is frozen on last list, which is to be understood as an 'output'.
lazy_seqmap(_,[],[],S,S).
lazy_seqmap(P,[X1|XX],[Y1|YY],S1,S3) :-
	call(P,X1,Y1,S1,S2), 
	freeze(YY,lazy_seqmap(P,XX,YY,S2,S3)).

lazy_seqmap(_,[],S,S).
lazy_seqmap(P,[X1|XX],S1,S3) :-
	call(P,X1,S1,S2), 
	freeze(XX,lazy_seqmap(P,XX,S2,S3)).

