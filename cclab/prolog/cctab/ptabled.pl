:- module(ptabled, [sent//0, s//0]).

/** <module> Test predicates for probabilistic tabling
   NB. this module expects cctabled/1 to be imported into user.
*/

:- use_module(library(tabling)).
:- use_module(library(ccmacros2)).
:- use_module(library(dcg_core), [rep//2]).

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
:- cctable three_dice/1, two_dice/2.
three_dice(X) :- length(Xs,3), maplist(:=(die), Xs), sumlist(Xs,X).

two_dice(X1,X2) :- die := X1, die := X2.

% test handling of variables in answers
:- cctable ssucc/2.
ssucc(X, a(X)).
test(Y,Z) :- (X=1;X=2;X=3), ssucc(A,Y), A=X, ssucc(_,Z).

iota(0,L,L) :- !.
iota(N,L3,L1) :- succ(M,N), iota(M,L3,[N|L1]).

user:portray(X) :- float(X), !, format('~5g',[X]).
user:portray(rbtree(T)) :- !,
   rb_visit(T,TT),
   write('<rbtree|'), print_term(TT,[]), write('>').
