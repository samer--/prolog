/* Part of dcgutils
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

:- module(dcg_pair, [
		(\<)//1
	,	(\>)//1
	,	run_left//3
	,	run_right//3
	,	(\#)//2

	,  op(900,fy,\<)
	,	op(900,fy,\>)
	,  op(900,xfy,\#)
   ]).

/** <module> Paired state DCG utilities
 
   This module contains predicates for using DCGs as a tool for stateful
   programming, where the state can be pair of arbitrary types. This enables
   DCGs to be 'composed'.

   The type =|pair(A,B)|= will be used to denote the type of terms
   with functor (-)/2 and arguments of types =|A|= and =|B|= respectively:
   ==
   pair(A,B) ---> A-B.
   ==
   This type is used to support a set of general purpose predicates
   for combining commands in two distinct DCGs into a single DCG 
   over a product space of states.
*/

:- meta_predicate 
		run_left(//,?,?,?,?)
	,	run_right(//,?,?,?,?)
	,	\<(//,?,?)
	,	\>(//,?,?)
	,	\#(?,//,?,?)
   .

:- op(900,fy,\<).
:- op(900,fy,\>).
:- op(900,xfy,\#).

%% \<(P:phrase(A), ?S1:pair(A,B), ?S2:pair(A,B)) is nondet.
%
%  Apply phrase P to left part of a paired state.
\<(P,A1-B,A2-B) :- call_dcg(P,A1,A2).

%% \>(P:phrase(B), ?S1:pair(A,B), ?S2:pair(A,B)) is nondet.
%
%  Apply phrase P which must be of type pred(B,B) to right
%  part of a paired state.
\>(P,A-B1,A-B2) :- call_dcg(P,B1,B2).

%% run_left(P:phrase(pair(A,B)), ?A1:A, ?A2:A, ?B1:B, ?B2:B) is multi.
%
%  Applies DCG phrase P to state formed by pairing A1 and A2 with
%  current DCG states B1 and B2. Phrase can use (\<) to access the
%  A state and (\>) to access the underlying B state.
run_left(P,S1,S2,T1,T2) :- call_dcg(P,S1-T1,S2-T2).

%% run_right(P:phrase(pair(A,B)), ?B1:B, ?B2:B, ?A1:A, ?A2:A) is multi.
%
%  Applies DCG phrase P to state formed by pairing A1 and A2 with
%  current DCG states B1 and B2. Phrase can use (\<) to access the
%  A state and (\>) to access the underlying B state.
run_right(P,S1,S2,T1,T2) :- call_dcg(P,T1-S1,T2-S2).

%% \#(N:natural, P:phrase(A), ?S1, ?S2) is nondet.
%
%  Apply phrase P to the Nth argument of state which must
%  be a compound term (with arbitrary functor), with the 
%  Nth argument of type A.
\#(N, P, S1, S2) :- with_nth_arg(N,P,S1,S2).


%% pushl(S:A,S1:B,S2:pair(A,B)) is det.
%  Create a paired state by putting S on the left and the
%  old state on the right.
pushl(S,S0,S-S0).

%% pushr(S:A,S1:B,S2:pair(B,A)) is det.
%  Create a paired state by putting S on the right and the
%  old state on the left.
pushr(S,S0,S0-S).

%% popl(S:A,S1:pair(A,B),S2:B) is det.
%  Unpair state by removing left state and unifying it with S.
popl(S,S-S0,S0).

%% popr(S:A,S1:pair(B,A),S2:B) is det.
%  Unpair state by removing right state and unifying it with S.
popr(S,S0-S,S0).


% --- internal utilities ----

with_nth_arg(K,P,T1,T2) :- 
	functor(T1,F,N),
	functor(T2,F,N),
	with_nth_arg(N,K,P,T1,T2).

with_nth_arg(K,K,P,T1,T2) :- 
	arg(K,T1,C1), call_dcg(P,C1,C2),
	arg(K,T2,C2), succ(N,K),
	copy_args(N,T1,T2).

with_nth_arg(N,K,P,T1,T2) :- 
	arg(N,T1,C), 
	arg(N,T2,C),
	succ(M,N), 
	with_nth_arg(M,K,P,T1,T2).

copy_args(0,_,_) :- !.
copy_args(N,T1,T2) :-
	succ(M,N), arg(N,T1,X), arg(N,T2,X), 
	copy_args(M,T1,T2).


