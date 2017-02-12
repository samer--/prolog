:- module(lambda2, [ (\\)/1, (\\)/2, (\\)/3, (\\)/4, (\\)/5, (\\)/6, (\\)/7
                   , '`'/3, '`'/4, '`'/5, '`'/6, '`'/7, '`'/8
                   , op(1000,xfy,'`')
                   , op(1200,fx,\\)
                   ]).

/** <module> Lambdas, like Ulrich's but with lower precedence operators 
   Use (\\)/1 to introduce a lambda, an optional (|)/2 to separate free from bound variables, 
   and (`)/2 to separate arguments and body, eg =| \\F|X`Y`p(X,Y,F) |=.
   Operator precedences are low enough not to interfere with operator terms in the
   arguments or free variables sections.
*/

:- set_prolog_flag(back_quotes, symbol_char).
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate \\(0), \\(1,?), \\(2,?,?), \\(3,?,?,?), \\(4,?,?,?,?), \\(5,?,?,?,?,?), \\(6,?,?,?,?,?,?).
:- meta_predicate `(?,0,?), `(?,1,?,?), `(?,2,?,?,?), `(?,3,?,?,?,?), `(?,4,?,?,?,?,?), `(?,5,?,?,?,?,?,?).

:- op(1000,xfy,`).

\\(Lambda) :- copy_lambda(Lambda,Copy), call(Copy).
\\(Lambda,A1) :- copy_lambda(Lambda,Copy), call(Copy,A1).
\\(Lambda,A1,A2) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2).
\\(Lambda,A1,A2,A3) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3).
\\(Lambda,A1,A2,A3,A4) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3,A4).
\\(Lambda,A1,A2,A3,A4,A5) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3,A4,A5).
\\(Lambda,A1,A2,A3,A4,A5,A6) :- copy_lambda(Lambda,Copy), call(Copy,A1,A2,A3,A4,A5,A6).

copy_lambda(M:(G|Lam), M:Copy) :- !, copy_term_nat(G-Lam,G-Copy).
copy_lambda(M:Lam, M:Copy) :- copy_term_nat(Lam,Copy).

`(A,B,A) :- call(B).
`(A,B,A,V1) :- call(B,V1).
`(A,B,A,V1,V2) :- call(B,V1,V2).
`(A,B,A,V1,V2,V3) :- call(B,V1,V2,V3).
`(A,B,A,V1,V2,V3,V4) :- call(B,V1,V2,V3,V4).
`(A,B,A,V1,V2,V3,V4,V5) :- call(B,V1,V2,V3,V4,V5).

