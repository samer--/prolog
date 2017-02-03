:- module(cclistm, [run_list/2, choose/2]).
/** <module> Nondeterminism as list monad
   Provides nondeterministic choice as an effect using delimited
   continuations. The nondeterministic computation is refied as a
   list of possible answers, like the Haskell List monad.

   Use run_list/{1,2} to bracket a computation and use choose/2 (not
   Prolog nondeterministic constructs!) to make a nondeterministic choice.
*/

:- use_module(library(delimcc)).
:- use_module(library(lambda1)).

:- meta_predicate run_list(1,-), run_list(1,-,?).

%% run_list(+P:pred(-A), -X1:list(A)) is det.
%% run_list(+P:pred(-A), X1:list(A), X2:list(A)) is det.
%  Calls unary predicate P in a context where choose/2 provides
%  nondeterministic choice as an effect. run_list/3 produces
%  results as a difference list.
run_list(P,Xs) :- run_list(P,Xs,[]).
run_list(P) -->
   {p_reset(nondet, call(P, X), Status)},
   cont_list(Status, X).

cont_list(done, X) --> [X].
cont_list(susp(Ys-Y, Cont), X) --> foldl(expand1(\Y^X^Cont), Ys).

expand1(K, Y) --> run_list(call(K, Y)).

%% choose(+Ys:list(A), -Y:A) is det.
%  Nondeterministic choice as a control effect, not as Prolog nondeterminism.
choose(Ys,Y) :- p_shift(nondet, Ys-Y).
