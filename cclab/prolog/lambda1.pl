:- module(lambda1, [ (\)/1, (\)/2, (\)/3, (\)/4, (\)/5, (\)/6, (\)/7
                   , (^)/3, (^)/4, (^)/5, (^)/6, (^)/7, (^)/8
                   , (<-)/2, (<-)/3, (<-)/4, (<-)/5, (<-)/6, (<-)/7
                   , op(200,xfy,~)
                   , op(600,xfx,<-)
                   ]).

/** <module> Two approaches to lambdas

This module implements two ways of implementing lambda terms. One is basically
the same as Ulrich Neumerkel's in the library(lambda), but with less checking and a different
notation for free variables which results in a simpler implementation. The result is
well typed and elegant. Sample lambda: =|\Free~X^Y^foo(Y,X,Free), \X^Y^succ(Y,X)|=

The other splits the arguments from the body at the top of the syntax tree representing
the lambda term, and naturally leads to an implementation based on lists of arguments, a
bit like library(yall). The result requires slightly less code (two clauses handle all
arities of argument binding) but relies on heterogenous lists of arguments and the equivalent
of apply/2, and is hence not well typed and, to my eyes, slightly less elegant.
Sample lambda: =| Free/[X,Y]<-foo(X,Y,Free), \[X^Y]<-succ(Y,X) |=.
*/

:- op(200,xfy,~).
:- meta_predicate \(0), \(1,?), \(2,?,?), \(3,?,?,?), \(4,?,?,?,?), \(5,?,?,?,?,?), \(6,?,?,?,?,?,?).
:- meta_predicate ^(?,0,?), ^(?,1,?,?), ^(?,2,?,?,?), ^(?,3,?,?,?,?), ^(?,4,?,?,?,?,?), ^(?,5,?,?,?,?,?,?).

:- set_prolog_flag(generate_debug_info, false).

\(Lambda) :- copy_lambda(Lambda,Copy), call(Copy).
\(Lambda,A1) :- copy_lambda(Lambda,Copy), call(Copy,A1).
\(Lambda,A1,A2) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2).
\(Lambda,A1,A2,A3) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3).
\(Lambda,A1,A2,A3,A4) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3,A4).
\(Lambda,A1,A2,A3,A4,A5) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3,A4,A5).
\(Lambda,A1,A2,A3,A4,A5,A6) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3,A4,A5,A6).

copy_lambda(M:G~Lam, M:Copy) :- !, copy_term_nat(G~Lam,G~Copy).
copy_lambda(M:Lam, M:Copy) :- copy_term_nat(Lam,Copy).

^(A,B,A) :- call(B).
^(A,B,A,V1) :- call(B,V1).
^(A,B,A,V1,V2) :- call(B,V1,V2).
^(A,B,A,V1,V2,V3) :- call(B,V1,V2,V3).
^(A,B,A,V1,V2,V3,V4) :- call(B,V1,V2,V3,V4).
^(A,B,A,V1,V2,V3,V4,V5) :- call(B,V1,V2,V3,V4,V5).

% ---------------- another version ---------------

:- op(600,xfx,<-).

:- meta_predicate <-(?,0), <-(?,1,?), <-(?,2,?,?), <-(?,3,?,?,?), <-(?,4,?,?,?,?), <-(?,5,?,?,?,?,?).
<-(Args,Body) :- copy_lambda(Args,Body,As,B), hat(As,B,[]). 
<-(Args,Body,A1) :- copy_lambda(Args,Body,As,B), hat(As,B,[A1]). 
<-(Args,Body,A1,A2) :- copy_lambda(Args,Body,As,B), hat(As,B,[A1,A2]). 
<-(Args,Body,A1,A2,A3) :- copy_lambda(Args,Body,As,B), hat(As,B,[A1,A2,A3]). 
<-(Args,Body,A1,A2,A3,A4) :- copy_lambda(Args,Body,As,B), hat(As,B,[A1,A2,A3,A4]). 
<-(Args,Body,A1,A2,A3,A4,A5) :- copy_lambda(Args,Body,As,B), hat(As,B,[A1,A2,A3,A4,A5]). 

copy_lambda(G/A,B,A1,B1) :- copy_term_nat(t(G,A,B),t(G,A1,B1)).
copy_lambda(\A,B,A1,B1) :- copy_term_nat(A/B,A1/B1).

% badly typed, uses univ operator!
hat([],B,Vs) :- G =.. [call, B | Vs], call(G).
hat([A|As],B,[A|Vs]) :- hat(As,B,Vs).

