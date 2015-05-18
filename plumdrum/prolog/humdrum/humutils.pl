/* Part of plumdrum
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

/** <module> Humdrum parsing utilities

   This module provides some utility predicates for parsing.
   They all operate as part of a DCG where the threaded state
   is a list of character codes.

   Parsers in this module use the following data types:
   ==
   recip ---> X:natural  % duration is 1/X except for X=0, then duration=2
            ; dots(X:natural,N:natural)   % add N dots, means (1/X)*(3/2)^N

   pitch_class ---> a ; b ; c ; d ; e ; f ; g
                  ; sharp(pitch_class)
                  ; flat(pitch_class).

   barline_attribute ---> double
                   ; ortho(list(barline_ortho))
                   ; number(natural)
                   ; number(natural,natural)
                   ; pause.

   barline_ortho ---> normal ; heavy ; partial1 ; partial2 ; invisible ; repeat.
   ==
*/
:- module(humutils,
		[	nat//1
		,	float//1
		,	charsex//4
		,	pitchclass//1
		,	pitchclass//2
		,	pitch//2
		,	recip//1
		,	bar//1
		,	recip_to_rational/2
		,	rep_shared//2
		,	peek//1
		]).

:- use_module(library(clp/bounds)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(snobol)).

:- set_prolog_flag(double_quotes,codes).

% --------------- DCG utilities ----------------------------------------

%% peek(+X:list(A))// is semidet.
%% peek(-X:list(A))// is nondet.
%  Works in a list(A) DCG and unifies X with the first one or two
%  elements of the list being parsed. Can be used in a DCG to peek
%  the first one or two elements of the unparsed %  list without
%  removing them.
peek([C],L,L) :- L=[C|_].
peek([C1,C2],L,L) :- L=[C1,C2|_].

%% charsex(+Type:char_type,+ExFirst:list(code),+Exclude:list(code),-Chars:list(code))// is nondet.
%
% Parses characters of char_type Type excluding characters in Exclude.
% In addition, the first character is not allowed to be in ExFirst.
% NB: ordering of clauses means charsex will read as much as possible
% and only get to shorter parses on backtracking.
% Character types as defined by char_type/2 but adding type text defined
% as the union of types graph and space.
charsex(T,Z1,Z,[X|Y]) --> [X], 
	{ ctype(X,T), \+member(X,Z1), \+member(X,Z) }, 
	charsex(T,[],Z,Y).
charsex(_,_,_,[]) --> [].

% Augmented char_type sort of thing
ctype(A,text) :- !, (char_type(A,graph); char_type(A,space)).
ctype(A,T) :- char_type(A,T).

%% nat(-Number:natural)// is nondet.
%  Parse or generate a natural number, ie a non-negative integer.
nat(I) --> 
	(	{integer(I)} -> {I>=0, number_codes(I,A)}, A
	;	charsex(digit,[],[],A), {A=[_|_], number_codes(I,A)}).

%% float(-X:float)// is semidet.
%  Can parse or generate a float as part of a list-of-codes DCG.
float(X) --> 
	(	{number(X)} -> {number_codes(X,A)}, A
	;	charsex(digit,[],[],A), 
		(	".",charsex(digit,[],[],B)
		->	{"."=[Dot],append(A,[Dot|B],C)}
		;	{C=A}
		),
		{C=[_|_], number_codes(X,C)}
	).

%% pitch(P:pitch_class, O:integer)// is semidet.
%  Parses a pitch in Humdrum Kern syntax. 
%  The grammar is
%  ==
%  pitch --> pitch_octave, modifiers.
%  pitch_octave --> {member(N,"abcdefg")}, +[N]  % each extra N goes up 1 octave
%                 ; {member(N,"ABCDEFG")}, +[N]. % each extra N goes down 1 octave
%  modifiers --> +"#"    % one or more sharps
%              ; +"-"    % one or more flats
%              ; "n"     % explicity natural
%              ; "".     % implicit natural
%
%  +X --> X ; X, +X.     % one or more copies of X.
%  ==
pitch(P,O) --> pitch_oct(N,O), wrap_mods(N,P).

wrap_mods(N,Q) --> sharpen(N,P), iterate(sharpen,P,Q), \+sharpen(Q,_).
wrap_mods(N,Q) --> flatten(N,P), iterate(flatten,P,Q), \+flatten(Q,_).
wrap_mods(N,N) --> maybe("n"), \+sharpen(N,_), \+flatten(N,_).

sharpen(A,sharp(A)) --> "#".
flatten(A,flat(A)) --> "-".

%% pitchclass(P:pitch_class)// is semidet.
%% pitchclass(C:oneof([lower,upper]), P:pitch_class)// is semidet.
%  Parses a pitch class in upper or lower case forms.
pitchclass(P) --> pitchclass(lower,P).
pitchclass(Case,P) --> pitchname(Case,N), wrap_mods(N,P).

% base pitch names in upper or lower case
pitchname(upper,N) --> [UNC], 
	{member(UNC,"ABCDEFG"), to_lower(UNC,NC), char_code(N,NC)}.
pitchname(lower,N) --> [NC], 
	{member(NC,"abcdefg"), char_code(N,NC)}.

% absolute pitch with octave but without sharps or flats
% parsing direction
pitch_oct(N,O)  --> {var(O)}, pitchname(lower,N), !, rep_shared(K,pitchname(lower,N)), {O is K+4}.
pitch_oct(N,O)  --> {var(O)}, pitchname(upper,N), !, rep_shared(K,pitchname(upper,N)), {O is 3-K}.

pitch_oct(N,O)  --> {nonvar(O)}, 
	(	{O >= 4}
	->	{K is O-3}, rep(K,pitchname(lower,N))
	; 	{K is 4-O}, rep(K,pitchname(upper,N))
	).


%% rep_shared(N:natural, P:phrase)// is nondet.
%  Equivalent to P repeated N times. Any variables in P are shared between iterations.
rep_shared(K,P) --> P, rep_shared(J,P), {succ(J,K)}.
rep_shared(0,_) --> [].

% -------------------- Durations --------------------------------------

%% recip_to_rational( +R:recip, -D:rational) is det.
%
%  Converts duration from recip format to rational number.
recip_to_rational(dots(R,N),D) :- !, 
	recip_to_rational(R,D0), 
	rep(N,mul(1 rdiv 2),1,C),
	D1 is D0*(2 - C),
	D=D1.

recip_to_rational(0,2)         :- !.
recip_to_rational(1,1)         :- !.
recip_to_rational(R,1 rdiv R)  :- R>0.

%% recip(-R:recip)// is semidet.
%  Parses a duration as a recip term. 
recip(D) --> nat(R), \+any("0123456789"), iterate(dottify,R,D).

dottify(dots(A,N),dots(A,M)) --> !, ".", {succ(N,M)}.
dottify(A,dots(A,1)) --> ".".

mul(X,Y,Z) :- Z is Y*X.



% ---------------- BAR lines -------------------


nth_lower(N,C) :- nth1(N,"abcdefghijklmnopqrstuvwxyz",C).

%% bar(-Attrs:list(bar_attributes))// is semidet.
%
%  Parses Humdrum bar signifiers. See module header for data type.
bar(Attrs) --> "=", run_left(bar_attrs,Attrs,[]).

bar_attrs -->
	maybe(bar_double), 
	maybe(bar_number),
	maybe(bar_pause),
	(bar_ortho; nop).

bar_double --> \> span("="), \< [double].
bar_number --> \> (nat(N), [C]), {nth_lower(M,C)}, \< [number(N,M)].
bar_number --> \> nat(N), \< [number(N)].
bar_pause --> \> ";",\< [pause].
bar_ortho --> \> seqmap(bar_ortho,O), {O\=[]}, \< [ortho(O)].

bar_ortho(normal)    --> "|".
bar_ortho(heavy)     --> "!".
bar_ortho(repeat)    --> ":".
bar_ortho(partial1)  --> "'".
bar_ortho(partial2)  --> "`".
bar_ortho(invisible) --> "-".
