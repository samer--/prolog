:- module(ccdcg, [run_dcg/3, run_prefix/2]).
/* <module> Playing with continuation based DCGs
*/
:- use_module(library(delimcc)).

run_dcg(P) -->
   {p_reset(prefix, P, Status)},
   cont_dcg(Status).

cont_dcg(done) --> [].
cont_dcg(susp(match(X),K)) --> [X], run_dcg(K).

run_prefix(_,[]) :- !.
run_prefix(P,L) :-
   p_reset(prefix, P, Status),
   cont_prefix(Status,L).

cont_prefix(done,[]).
cont_prefix(susp(match(X),K),[X|T]) :- run_prefix(K,T).

:- meta_predicate +(2).
+Lab :- call(Lab,Vals,[]), member(T,Vals), p_shift(prefix,match(T)).

% grammars using match/1 effect
% No left recursion!

s :- np, vp.

np :- +d, nom
    % ; np, pp
    ; +pn.

vp :- +iv
    ; +tv, np
    ; +dv, np, np
    % ; vp, pp
    ; +mv, s.

nom :- +n
     ; +adj, nom.

pp :- +p, np.

% specify preterminals as DCG that spits out alternatives
user:term_expansion(Lab | Body, Clause) :-
   dcg_translate_rule(Lab --> Body, Clause).

% preterminal declarations
adj | [hot,cold,thin,fat,disgusting,lovely].
pn  | [alice, bob, cuthbert, delia, edna].
d   | [the,a,some,my]. % ,every,no].
mv  | [knew,thought,believed,said].
dv  | [gave,made,baked].
tv  | [saw, ate, hated, baked, liked, walked, ran, loved, caught].
iv  | [lived, worked].
n   | [dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park].
p   | [with,on,under,in,without,by].
