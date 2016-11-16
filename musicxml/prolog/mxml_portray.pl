:- module(mxml_portray, []).
/** <module> Provides pretty portrayal of notes and chords
*/

:- use_module(library(listutils), [rep/3]).
:- use_module(library(mxml_chords), [ ivals_triad_exts/3 ]).

user:portray(flat(X)) :- sign(flat,S), format('~w~p',[S,X]).
user:portray(sharp(X)) :- sign(sharp,S), format('~w~p',[S,X]).
user:portray(a(N,A)) :- 
   accidentals(A,Chars,Sep), 
   (  number(N) 
   -> format('~s~w~w', [Chars,Sep,N])
   ;  format('~w~s', [N,Chars])
   ).

user:portray(chord(R,B,Is)) :-
   ivals_triad_exts(Is, Triad, Exts),
   format('~p ~w', [R,Triad]),
   maplist(space_print, Exts),
   ( R=B -> true
   ; write('/'), print(B)
   ).

space_print(X) :- format(' ~p',[X]).

bip(N,N,sharp) :- N>0.
bip(N,M,flat) :- N<0, M is -N.

accidentals(0, [], '').
accidentals(N, Chars, ' ') :- bip(N,M,S), sign(S,C), rep(M,C,Chars).

sign(ascii, sharp, #).
sign(ascii, flat, b).
sign(uni, sharp, ♯).
sign(uni, flat, ♭).
sign(uni, natural, ♮).
sign(uni, double_flat, 𝄫).
sign(uni, double_sharp, 𝄪).
sign(uni, crotchet, ♩).
sign(uni, quaver, ♪).
sign(uni, two_quavers, ♫).
sign(uni, two_semiquavers, ♬).

sign(Name,Char) :- sign(uni,Name,Char).
