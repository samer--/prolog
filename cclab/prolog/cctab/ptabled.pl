:- module(ptabled, [sent//0, s//0]).

/** <module> Test predicates for probabilistic tabling
   NB. this module expects cctabled/1 to be imported into user.
*/

:- use_module(library(ccmacros2)).

:- initialization debug(cctab), module(ptabled).

:- op(1200,xfx,~~>).
:- op(1150,xfx,~>).

:- cctable sent//0, sent1//1.
sent --> {dist([0.5-b,0.25-l,0.25-r], A)}, sent1(A).
sent1(b) --> {dist([0.6-cool,0.4-wicked], W)}, [W].
sent1(l) --> sent, [not].
sent1(r) --> [really], sent.

user:term_expansion(Lab | Body, Clause) :-
   prolog_load_context(module,Module),
   Lab =.. Args,   append(Args, [Module:Lab], Args1),
   Head =.. Args1, dcg_translate_rule(Head --> Body, Clause).

% sample terminal directly from switch
:- meta_predicate +(3,?,?).
+Lab --> [T], {T := Lab}.

user:goal_expansion(~>(S,Alts,L1,L2), Goals) :- 
   expand_alts(I,Alts,0,DCGGoals),
   dcg_translate_rule((h --> {I:=S}, DCGGoals), (h(L1,L2) :- Goals)).

expand_alts(K, (B; Bs), I, (G; Goals)) :- !, succ(I,J), expand_alt(K,B,J,G), expand_alts(K,Bs,J,Goals).
expand_alts(K, B, I, G) :- succ(I,J), expand_alt(K,B,J,G).
expand_alt(K, Goals, J, ({J=K}, !, Goals)).

:- cctable s//0, np//0, vp//0, pp//0, nom//0.

np  | iota(4).
vp  | iota(5).
nom | iota(2).

s --> np, vp.

np --> np ~> +d, nom
           ; +pn
           ; np, pp
           ; +pro.

vp --> vp ~> +iv
           ; +tv, np
           ; +dv, np, np
           ; vp, pp
           ; +mv, s.

nom --> nom ~> +n
             ; +adj, nom.

pp --> +p, np.

% preterminal switch declarations
adj | [hot,cold,thin,fat,disgusting,lovely].
pro | ['I','you'].
pn  | [alice, bob, cuthbert, delia, edna].
d   | [the,a,every,no,some,my].
mv  | [knew,thought,believed,said].
dv  | [gave,made,baked].
tv  | [saw, ate, hated, baked, liked, walked, ran, loved, caught].
iv  | [lived, worked].
n   | [dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park].
p   | [with,on,under,in,without,by].
v   | vals(dv), vals(tv), vals(iv).

iota(0) --> [].
iota(N) --> {succ(M,N)}, [N], iota(M).

user:portray(X) :- float(X), !, format('~5g',[X]).
