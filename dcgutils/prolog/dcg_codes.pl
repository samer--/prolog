:- module(dcg_codes, [
		writedcg/1

   % Constants
	,	null//0
   ,	cr//0
   ,  sp//0
   ,  fs//0
	,	fssp/2
   ,	tb/2
   ,	comma/2
   ,  commasp/2

   % Writing Prolog data
	,	at//1
   ,	wr//1
   ,	str//1
   ,  fmt//2
	,	padint/5

   % Brackets
	,	brace//1
   ,	paren//1
   ,	sqbr//1

   % Quoting and escaping
	,	q//1
   ,	qq//1
	,	escape//2, escape_with//3

]).

/** <module> DCG utilities for list of character codes representation.
 
This module contains predicates for working with DCGs defined over
sequences of character codes. Some of the predicates can only
be used to generate sequences, not parse them.

*/

:- meta_predicate 
		writedcg(2)
	,	brace(//,?,?)
	,	paren(//,?,?)
	,	sqbr(//,?,?)
	,	qq(//,?,?)
	,	q(//,?,?)
	.

:- set_prolog_flag(double_quotes, codes).


%% writedcg(+P:phrase) is nondet.
%
%  Run the phrase P, which must be a standard list-of-codes DCG,
%  and print the output.
writedcg(Phrase) :-
	phrase(Phrase,Codes),
	format('~s',[Codes]).
		
%% null// is det.
%  Empty string.
null  --> "".

%% cr// is det.
%  Carriage return "\n".
cr    --> "\n".

%% sp// is det.
%  Space " ".
sp    --> " ".

%% fs// is det.
%  Full stop (period) ".".
fs    --> ".".

%% fssp// is det.
%  Full stop (period) followed by space.
fssp  --> ". ".

%% tb// is det.
%  Tab "\t".
tb    --> "\t".

%% comma// is det.
%  Comma ",".
comma   --> ",".

%% commasp// is det.
%  Comma and space ", ".
commasp --> ", ".

%% at(+X:atom)// is det.
%  Generate code list for textual representation of atom X.
at(A,C,T) :- atomic(A), with_output_to(codes(C,T),write(A)).

%% wr(+X:term)// is det.
%  Generate the list of codes for term X, as produced by write/1.
wr(X,C,T) :- ground(X), with_output_to(codes(C,T),write(X)).

%% wq(+X:term)// is det.
%  Generate the list of codes for term X, as produced by writeq/1.
wq(X,C,T) :- ground(X), with_output_to(codes(C,T),writeq(X)).

%% str(+X:term)// is det.
%  Generate the list of codes for string X, as produced by writeq/1.
str(X,C,T):- string(X), with_output_to(codes(C,T),write(X)).

%% fmt(+F:atom,+Args:list)// is det
%  Generate list of codes using format/3.
fmt(F,A,C,T) :- format(codes(C,T),F,A).

%% padint( +N:integer, +Range, +X:integer)// is det.
%% padint( +N:integer, +Range, -X:integer)// is nondet.
%
%  Write integer X padded with zeros ("0") to width N.
padint(N,..(L,H),X,C,T) :- 
	between(L,H,X), 
	format(codes(C,T),'~`0t~d~*|',[X,N]).

%% brace(+P:phrase)// is nondet.
%  Generate "{" before and "}" after the phrase P.
brace(A) --> "{", phrase(A), "}".

%% paren(+P:phrase)// is nondet.
%  Generate "(" before and ")" after the phrase P.
paren(A) --> "(", phrase(A), ")".

%% sqbr(+P:phrase)// is nondet.
%  Generate "[" before and "]" after the phrase P.
sqbr(A)  --> "[", phrase(A), "]".

%% q(+P:phrase)// is nondet.
%  Generate list of codes from phrase P, surrounds it with single quotes,
%  and escapes (by doubling up) any internal quotes so that the
%  generated string is a valid quoted string. Must be list of codes DCG.
q(X,[0''|C],T)  :- T1=[0''|T], escape_with(0'',0'',X,C,T1). 

%% qq(+P:phrase)// is nondet.
%  Generate list of codes from phrase P, surrounds it with double quotes,
%  and escapes (by doubling up) any double quotes so that the
%  generated string is a valid double quoted string.
qq(X,[0'"|C],T) :- T1=[0'"|T], escape_with(0'",0'",X,C,T1). 

%% escape(+Q:C, +P:phrase)// is nondet.
%
%  Runs phrase P to generate a list of elements of type C and
%  then escapes any occurrences of Q by doubling them up, e.g.,
%  =|escape(39,"some 'text' here")|= doubles up the single quotes
%  yielding =|"some ''text'' here"|=.
escape(Q,A) --> escape_with(Q,Q,A).

%% escape_with(+E:C, +Q:C, +P:phrase)// is nondet.
%
%  Runs phrase P to generate a list of elements of type C and
%  then escapes any occurrences of Q by prefixing them with E, e.g.,
%  =|escape_with(92,39,"some 'text' here")|= escapes the single quotes
%  with backslashes, yielding =|"some \'text\' here"|=.
escape_with(E,Q,Phrase,L1,L2) :-
	phrase(Phrase,L0,L2),
	escape_codes(E,Q,L0,L1,L2).

% escape difference list of codes with given escape character
escape_codes(_,_,A,A,A).
escape_codes(E,Q,[Q|X],[E,Q|Y],T) :-escape_codes(E,Q,X,Y,T).
escape_codes(E,Q,[A|X],[A|Y],T)   :- Q\=A, escape_codes(E,Q,X,Y,T).

% Not used, apparently.
% difflength(A-B,N) :- unify_with_occurs_check(A,B) -> N=0; A=[_|T], difflength(T-B,M), succ(M,N).

% % tail recursive version
% difflength_x(A-B,M)       :- difflength_x(A-B,0,M).
% difflength_x(A-B,M,M)     :- unify_with_occurs_check(A,B).
% difflength_x([_|T]-A,M,N) :- succ(M,L), difflength_x(T-A,L,N).

