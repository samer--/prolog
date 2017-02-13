:- module(tabled, [sent//0, pathl//0, pathr//0]).

/** <module> Test predicates for probabilistic tabling
   NB. this module expects cctabled/1 to be imported into user.
*/

:- use_module(library(ccmacros)).

:- initialization debug(cctab), module(ptabled).

:- cctable sent//0, sent1//1.
sent --> {dist([0.5-b,0.25-l,0.25-r], A)}, sent1(A).
sent1(b) --> {dist([0.6-cool,0.4-wicked], W)}, [W].
sent1(l) --> sent, [not].
sent1(r) --> [really], sent.

print_table(Var-Solns) :-
   nl, writeln(Var),
   maplist(writeln, Solns).
