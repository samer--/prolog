:- use_module('../prolog/typedef').

:- type pair(A,B) ---> A-B.
:- type either(A,B) ---> left(A); right(B).
:- type option(A) ---> none; some(A).

:- type expr(_).
:- type int(integer) :: expr(integer).
:- type float(float) :: expr(float).
:- type str(string) :: expr(string).
:- type expr(integer) + expr(integer) :: expr(integer).
:- type expr(float) + expr(float) :: expr(float).

test :- forall(current_type(T), check_type(T)).


:- debug(typedef).
:- test.
