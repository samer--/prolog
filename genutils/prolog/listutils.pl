:- module(listutils, 
	[	natural/1		% test or enumerate natural numbers
	,	int/1				% test or enumerate integers
	,	take/3, takec/3, take_while/3
	,	drop/3, dropc/3, drop_while/3
	,  rep/3          % make a list of repeats of the same term
	,  cons/3         % list constructror
	,	decons/3       % list deconstructor
	,	print_list/1	% writes each element on a new line 
	,	printq_list/1	% as print_list but quotes atom as necessary
	,	print_numbered_list/1
   ,  zip/3
	]).

:- meta_predicate
		drop_while(1,?,?)
	,	take_while(1,?,?)
	.

%% natural(+N) is semidet.
%% natural(-N:natural) is multi.
%
% Means N is a natural number (includes 0). If N is
% a variable, succeeds an infinite number of times on backtracking,
% returning all natural numbers.
natural(N) :- (var(N) -> between(0,inf,N); integer(N), N>=0).


%% int(+N) is semidet.
%% int(-N:integer) is multi.
%
% Means N is an integer. If N is
% a variable, succeeds an infinite number of times on backtracking,
% returning all integers starting at zero and interleaving positive
% and negative values.
int(N)     :- nonvar(N), integer(N).
int(N)     :- var(N), (N=0; (between(1,inf,M), (N=M; N is -M))).

%% print_list( +L:list) is det.
%
%  Print a list, one item per line.
print_list([]) :- writeln('~'), nl.
print_list([H|T]) :- print(H), nl, print_list(T).

%% printq_list( +L:list) is det.
%
%  Print a list, one item per line, as with writeq/1.
printq_list([]) :- writeln('~'), nl.
printq_list([H|T]) :- writeq(H), nl, printq_list(T).

%% print_numbered_list( +L:list) is det.
%
%  Print a list with numbered lines.
print_numbered_list(L) :- 
	length(L,Max), 
	number_codes(Max,MC),
	length(MC,Width),
	print_num_list(Width,1,L).

print_num_list(_,_,[]) :- nl.
print_num_list(Width,N,[H|T]) :- succ(N,M),
	copy_term(H,H1),
	numbervars(H1,0,_),
   format('~` t~d~*+. ~q\n',[N,Width,H1]),
	print_num_list(Width,M,T).

% padleft(_,W,In,In) :- length(In,W).
% padleft(P,W,In,[P|Out]) :- succ(V,W), padleft(P,V,In,Out).

%% cons( ?Head:A, ?Tail:list(A), ?List:list(A)) is det.
%
%  List constructor.
cons(H,T,[H|T]).

%% decons( ?Head:A, ?List:list(A), ?Tail:list(A)) is det.
%
%  List deconstructor.
decons(H,[H|T],T).

%% rep( +N:natural, ?X:A, -L:list(A)) is det.
%% rep( -N:natural, ?X:A, -L:list(A)) is multi.
% Make a list consisting of N repeats of the same term. If called
% with N unbount, creates progressively longer and longer lists
% on backtracking.
rep(0,_,[]).
rep(N,A,[A|X]) :- 
	(	nonvar(N) 
	-> succ(M,N), rep(M,A,X)
	; rep(M,A,X), succ(M,N)
	).

%% drop( +N:natural, +In:list(A), -Out:list(A)) is semidet.
%  True if removing exactly N elements from the head of In results in Out.
%  Fails if there are fewer than N elements in In.
drop(N,X,T) :- length(H,N), append(H,T,X).

%% take( +N:natural, +In:list(A), -Out:list(A)) is semidet.
%  True when Out consists of the first N elements of Out.
%  Fails if In contains fewer than N elements.
take(N,X,H) :- length(H,N), append(H,_,X).

%% dropc( +N:natural, +In:list(A), -Out:list(A)) is det.
%  Unifies Out with the result of removing N elements from the head if In,
%  or the empty list if In has fewer than N elements. (The 'c' is for a
%  'complete' as opposed to a 'partial' function.)
dropc(0,T,T) :- !.
dropc(N,[_|T],V) :- !, succ(M,N), dropc(M,T,V).
dropc(_,[],[]).

%% takec( +N:natural, +In:list(A), -Out:list(A)) is semidet.
%  Unifies Out with the first N elements of In,
%  or the empty list if In has fewer than N elements. (The 'c' is for a
%  'complete' as opposed to a 'partial' function.)
takec(0,_,[]) :- !.
takec(_,[],[]) :- !.
takec(N,[X|XS],[X|YS]) :- succ(M,N), takec(M,XS,YS).

%% drop_while( +P:pred(A), +In:list(A), -Out:list(A)) is det.
%
%  Remove all elements from head of In that are accepted by P
%  and return the remained in Out.
drop_while(P,[X|T],V) :- call(P,X) -> drop_while(P,T,V); V=[X|T].
drop_while(_,[],[]).


%% take_while( +P:pred(A), +In:list(A), -Out:list(A)) is det.
%
%  Remove all elements from head of In that are accepted by P
%  and return them in Out.
take_while(P,[X|T],O) :- call(P,X) -> O=[X|V], take_while(P,T,V); O=[].
take_while(_,[],[]).

%% zip( +X:list(A), +Y:list(B), -Z:list(pair(A,B))) is det.
%% zip( -X:list(A), -Y:list(B), +Z:list(pair(A,B))) is det.
%% zip( ?X:list(A), ?Y:list(B), ?Z:list(pair(A,B))) is nondet.
%
%  True when Z is the list of pairs formed from corresponding
%  elements of X and Y. The pair type is =|pair(A,B) ---> A-B|=.
zip([],[],[]).
zip([X|XX],[Y|YY],[Z|ZZ]) :- Z=X-Y, zip(XX,YY,ZZ).


