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

% user:goal_expansion(~>(S,Alts,L1,L2), call_dcg(Goals,L1,L2)) :- expand_alts(S,Alts,Goals).
user:goal_expansion(~>(S,Alts,L1,L2), Goals) :- 
   expand_alts(S,Alts,DCGGoals),
   dcg_translate_rule((h --> DCGGoals), (h(L1,L2) :- Goals)).

expand_alts(S, (B; Bs), (G; Goals)) :- !, expand_alt(S,B,G), expand_alts(S,Bs,Goals).
expand_alts(S, B, G) :- expand_alt(S,B,G).
expand_alt(S, P -> Goals, ({event((S->Goals), P)}, Goals)).

:- cctable s//0, np//0, vp//0, pp//0, nom//0.

s --> np, vp.

np --> np ~> 0.4 -> d, nom
           ; 0.3 -> pn
           ; 0.1 -> np, pp
           ; 0.2 -> pro.

vp --> vp ~> 0.3 -> iv
           ; 0.3 -> tv, np
           ; 0.1 -> dv, np, np
           ; 0.2 -> vp, pp
           ; 0.1 -> mv, s.

nom --> nom ~> 0.7 -> n
             ; 0.3 -> adj, nom.

pp --> p, np.

adj --> preterm(adj,[hot,cold,thin,fat,disgusting,lovely]).
pro --> preterm(pro,['I','you']).
pn --> preterm(pn,[alice, bob, cuthbert, delia, edna]).
d -->  preterm(d,[the,a,every,no,some,my]).
mv --> preterm(mv,[knew,thought,believed,said]).
dv --> preterm(mv,[gave,made,baked]).
tv --> preterm(tv,[saw, ate, hated, baked, liked, walked, ran, loved, caught]).
iv --> preterm(iv,[lived, worked]).
n  --> preterm(n,[dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park]).
p  --> preterm(p,[with,on,under,in,without,by]).

preterm(Lab,Ts) --> [T], {member(T,Ts), length(Ts,N), P is 1/N, event(Lab->T, P)}.

user:portray(X) :- float(X), !, format('~5g',[X]).
