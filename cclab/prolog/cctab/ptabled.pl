:- module(ptabled, [sent//0, s//0]).

/** <module> Test predicates for probabilistic tabling
   NB. this module expects cctabled/1 to be imported into user.
*/

:- use_module(library(tabling)).
:- use_module(library(ccmacros2)).
:- use_module(library(callutils), [(*)/4]).
:- use_module(library(listutils), [take/3]).

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

:- op(990,xfx,:==).
Lab :== X :- call(Lab,_,Xs,[]), member(X,Xs).

% sample terminal directly from switch
:- meta_predicate +(3,?,?).
+Lab --> [T], {Lab := T}.

user:goal_expansion(~>(S,Alts,L1,L2), Goals) :- 
   expand_alts(I,Alts,0,DCGGoals),
   dcg_translate_rule((h --> {S:=I}, DCGGoals), (h(L1,L2) :- Goals)).

expand_alts(K, (B; Bs), I, (G; Goals)) :- !, succ(I,J), expand_alt(K,B,J,G), expand_alts(K,Bs,J,Goals).
expand_alts(K, B, I, G) :- succ(I,J), expand_alt(K,B,J,G).
expand_alt(K, Goals, J, ({J=K} -> Goals)).

:- cctable s//0, np//0, vp//0, pp//0, nom//0.

np  | iota(3).
vp  | iota(5).
nom | iota(2).

s --> np, vp.

np --> np ~> +d, nom
           ; +pn
           ; np, pp.

vp --> vp ~> +iv
           ; +tv, np
           ; +dv, np, np
           ; vp, pp
           ; +mv, s.

nom --> nom ~> +n
             ; +adj, nom.

pp --> +p, np.

biased_sampler(fallback_sampler(LU,uniform_sampler)) :-
   make_lookup_sampler([(ptabled:nom)-[0.8,0.2], (ptabled:np)-[0.3,0.6,0.1]],LU).

:- cctable toss//0, flip//1, result//1.
flip(X,[X|T],T) :- dist([0.6,0.4],[heads,tails],X).
result(heads) --> ['I',win].
result(tails) --> [you, lose].
toss --> flip(T), result(T).

% preterminal switch declarations
adj | [hot,cold,thin,fat,disgusting,lovely].
pn  | [alice, bob, cuthbert, delia, edna].
d   | [the,a,some,my]. % ,every,no].
mv  | [knew,thought,believed,said].
dv  | [gave,made,baked].
tv  | [saw, ate, hated, baked, liked, walked, ran, loved, caught].
iv  | [lived, worked].
n   | [dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park].
p   | [with,on,under,in,without,by].

die | iota(4).
die(_) | iota(3).
:- cctable three_dice/1, two_dice/2, two_dice/1.
three_dice(X) :- length(Xs,3), maplist(:=(die), Xs), sumlist(Xs,X).
two_dice(X1,X2) :- die := X1, die := X2.
two_dice(X) :- die(1) := D1, die(2) := D2, X is D1+D2.

:- cctable dice/2.
dice(0,0).
dice(N,Z) :- succ(M,N), die := X, dice(M,Y), Z is X+Y.

% test handling of variables in answers
:- cctable ssucc/2.
ssucc(X, a(X)).
test(Y,Z) :- (X=1;X=2;X=3), ssucc(A,Y), A=X, ssucc(_,Z).

iota(0,L,L) :- !.
iota(N,L3,L1) :- succ(M,N), iota(M,L3,[N|L1]).

