:- module(callutils, [ (*)/4
							, (*)//4
							, (*:)//3
							, constf//3
							, pairf//3
							, op(600,yfx,*:)
					    	]).

/** <module> High-order utility predicates

Some high-order predicates to enable high-order 'point-free' and
lambda free composition of predicates.
*/

:- meta_predicate *(2,2,?,?)
                , *(4,4,?,?,?,?)
                , constf(3,?,?,?,?)
                , pairf(3,3,?,?,?)
                .

%% *(+P:pred(B,C,S,S), +Q:pred(A,B,S,S), X:A, Z:C, S1:S, S2:S) is det.
%  Pure and stateful predicate composition, order may look weird but
%  it follows the usual convention for function composition. Maybe I should
%  flip it round. Calls Q before P.
*(P,Q,X,Z) --> call(Q,X,Y), call(P,Y,Z).
*(P,Q,X,Z) :- call(Q,X,Y), call(P,Y,Z).

%% *:(+P:pred(A,B,S,S), +G:pred(A,S), X:B, S1:S, S2:S) is det.
% Stateful piping of generator G into function P. Calls G before P!
*:(P,G,Y) --> call(G,X), call(P,X,Y).

%% pairf(+F:pred(A,S,S), +G:pred(B,S,S), X:pair(A,B), S1:S, S2:S) is det.
%  Call F and G respectively on components of a pair.
pairf(F,G,X-Y) --> call(F,X), call(G,Y).

%% constf(+F:pred(A,S,S), Y:_, X:A, S1:S, S2:S) is det.
%  Call F on X ignoring argument Y.
constf(F,_,X) --> call(F,X).


