:- module(pairs,
          [ ffst/3, ffst//3
          , fsnd/3, fsnd//3
          , pair/3
          , select_key_value/4
          , select_key_default_value/5
          , map_select_key_value/5
          , map_select_key_default_value/6
			 ]).

:- meta_predicate fsnd(2,?,?), 
						ffst(2,?,?),
                  fsnd(4,?,?,?,?), 
						ffst(4,?,?,?,?),
						map_select_key_value(2,+,-,+,-),
						map_select_key_default_value(2,+,+,-,+,-).

%% pair(X:A, Y:B, Z:pair(A,B)) is det.
pair(X, Y, X-Y).

%% ffst(+P:pred(A,B), X:pair(A,C), Y:pair(B,C)) is det.
%% ffst(+P:pred(A,B,S,S), X:pair(A,C), Y:pair(B,C), S1:S, S2:S) is det.
%  Apply P to first element of pair. Two versions: one for normal use and
%  another for use in DCG goals.
ffst(P,Y-X,Z-X) :- call(P,Y,Z).
ffst(P,Y-X,Z-X) --> call(P,Y,Z).

%% fsnd(+P:pred(B,C), X:pair(A,B), Y:pair(A,C)) is det.
%% fsnd(+P:pred(B,C,S,S), X:pair(A,B), Y:pair(A,C), S1:S, S2:S) is det.
%  Apply P to second element of pair. Two versions: one for normal use and
%  another for use in DCG goals.
fsnd(P,X-Y,X-Z) :- call(P,Y,Z).
fsnd(P,X-Y,X-Z) --> call(P,Y,Z).

%% map_select_key_value(+P:pred(A,B), K:C, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is nondet.
%  True when L2 is L1 with an element K-X removed, and P maps X to Y.
map_select_key_value(P, K, Y, L1, L2) :- 
	select(K-X, L1, L2), call(P,X,Y).

%% map_select_key_default_value(+P:pred(A,B), K:C, Z:B, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is det.
%  If key K exists in pair list L1, extract value associated with it and apply P to get Y.
%  Otherwise unify default Z with Y.
map_select_key_default_value(P, K, Default, Y, L1, L2) :-
   (  select(K-X, L1, L2) -> call(P,X,Y)
   ;  Y=Default, L1=L2
   ).

%% select_key_value(K:C, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is nondet.
%  True when L2 is L1 with an element K-Y removed.
select_key_value(K, X, L1, L2) :- select(K-X, L1, L2).

%% select_key_default_value(K:C, Z:B, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is det.
%  If key K exists in pair list L1, extract value Y associated with it.
%  Otherwise unify default Z with Y.
select_key_default_value(K, Default, X, L1, L2) :-
   map_select_key_default_value((=), K, Default, X, L1, L2).
  
