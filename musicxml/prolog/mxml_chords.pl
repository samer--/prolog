:- module(mxml_chords, [ decode_pitch/4, decode_chord/2
                       , chord_pitches/2, ivals_triad_exts/3
                       , fifths_key/2, pc_name_num/2
                       , fifths_from_c/2
                       , pc_octave_midi/3
                       , pitch_midi/2
                       ]).

:- use_module(library(clpfd)).
:- use_module(library(listutils), [cons/3]).
:- use_module(library(pair), [ map_select_key_value/5
                             , map_select_key_default_value/6
                             , select_key_default_value/5
                             ]).

% MusicXML chord kinds, as DCG rules for spitting out intervals.
@N --> [a(N,0)].
\N --> [a(N,-1)].
+N --> [a(N,+1)].
other      --> [].
major      --> @1, @3, @5.
minor      --> @1, \3, @5.
diminished --> @1, \3, \5.
augmented  --> @1, @3, +(5).
'suspended-second' --> @1, @2, @5.
'suspended-fourth' --> @1, @4, @5.
'power'            --> @1, @5.

dominant        --> major, \7.
'major-seventh' --> major, @7.
'minor-seventh' --> minor, \7.
'augmented-seventh'  --> augmented, \7.
'diminished-seventh' --> diminished, [a(7,-2)].
'half-diminished'    --> diminished, \7.
'major-minor'    --> minor, @7.
'major-sixth'    --> major, @6.
'minor-sixth'    --> minor, @6.
'dominant-ninth' --> dominant, @9.
'augmented-ninth'--> 'augmented-seventh', @9.
'major-ninth'    --> 'major-seventh', @9.
'minor-ninth'    --> 'minor-seventh', @9.
'dominant-11th'  --> 'dominant-ninth', @11.
'major-11th'     --> 'major-ninth', @11. 
'minor-11th'     --> 'minor-ninth', @11.
'dominant-13th'  --> 'dominant-11th', @13.
'major-13th'     --> 'major-11th', @13.
'minor-13th'     --> 'minor-11th', @13.
% 'Neapolitan' -->
% 'Italian' -->
% 'French' -->
% 'German' -->
% 'pedal' --> 
% 'Tristan' -->

triad(T) :- member(T, [major, minor, diminished, augmented, 'suspended-second', 'suspended-fourth', 'power']).
ivals_triad_exts(Ivals, Triad, Exts) :-
   triad(Triad), phrase(Triad, Ivals, Exts).

expand_ival(I, a(I,0)) :- atomic(I).
expand_ival(flat(I),  a(D,A)) :- A #< 0, !, A1 #= A+1, expand_ival(I,a(D,A1)).
expand_ival(sharp(I), a(D,A)) :- A #> 0, !, A1 #= A-1, expand_ival(I,a(D,A1)).

decode_chord(Props, chord(Root, Bass, SortedIvals)) :-
   phrase(( map_select_key_value(phrase, kind, KindIvals),
            map_select_key_value(decode_pitch('root-step','root-alter'), root, Root),
            map_select_key_default_value(decode_pitch('bass-step','bass-alter'), bass, Root, Bass)
          ), Props, Props1),
   findall(D, member(degree-D, Props1), Degrees),
   foldl(edit_intervals(KindIvals), Degrees, KindIvals, Ivals),
   sort(Ivals, SortedIvals).

edit_intervals(KindIvals, DegreeProps, Is1, Is2) :-
   phrase(( select('degree-type'-Type),
            select('degree-value'-Deg),
            select_key_default_value('degree-alter', 0, Alter)
          ), DegreeProps, []), 
   apply_ival_mod(Type, Deg, Alter, KindIvals, Is1, Is2).

% K is the list of degrees coming from the chord kind.
apply_ival_mod(add,      7, A, _) --> !, {A1 is A-1}, cons(a(7,A1)).
apply_ival_mod(add,      D, A, _) --> cons(a(D,A)).
apply_ival_mod(subtract, D, A, K) --> {member(a(D,A0), K), A1 is A0 + A}, select(a(D,A1)).
apply_ival_mod(alter,    D, A, K) --> {member(a(D,A0), K), A1 is A0 + A}, select(a(D,A0)), cons(a(D,A1)).

decode_pitch(StepKey, AlterKey, Props, a(Nominal, Alter)) :-
   map_select_key_value((=), StepKey, Nominal, Props, _),
   select_key_default_value(AlterKey, 0, Alter, Props, _). 

% alter_pitch(0, Nom, Nom).
% alter_pitch(N, Nom, sharp(P)) :- N>0, M is N-1, alter_pitch(M,Nom,P).
% alter_pitch(N, Nom, flat(P))  :- N<0, M is N+1, alter_pitch(M,Nom,P).

% chord_pitches(end, []).
chord_pitches(chord(Root, Bass, Ivals), B-Pitches) :-
   pc_octave_midi(Root, 3, R),
   pc_octave_midi(Bass, _, B),
   R - 18 #=< B, B #< R - 6,
   maplist(ival_semis, Ivals, Semitones),
   maplist(plus(R), Semitones, Pitches).

pc_octave_midi(a(D,A), O, NN) :-
   nominal_semis(D, NN0),
   NN #= NN0 + A + 12*(O+1).

pitch_midi(pitch(PC,O), NN) :-pc_octave_midi(PC, O, NN).

ival_semis(a(I,A), Semis) :-
   degree_semis(I, Base),
   Semis #= Base + A.

nominal_semis('C', 0).
nominal_semis('D', 2).
nominal_semis('E', 4).
nominal_semis('F', 5).
nominal_semis('G', 7).
nominal_semis('A', 9).
nominal_semis('B', 11).

degree_semis(1, 0).
degree_semis(2, 2).
degree_semis(3, 4).
degree_semis(4, 5).
degree_semis(5, 7).
degree_semis(6, 9).
degree_semis(7, 11).
degree_semis(8, 12).
degree_semis(9, 14).
degree_semis(10, 16).
degree_semis(11, 17).
degree_semis(12, 19).
degree_semis(13, 21).
degree_semis(14, 23).
degree_semis(15, 24).

fifths_key(Fifths, Tonic-major) :- fifths_from_c(Fifths, Tonic).
fifths_key(Fifths, Tonic-minor) :- Rel #= Fifths+3, fifths_from_c(Rel, Tonic).


pc_name_num(a(N,A), Num) :-
   Num #= (Base + A) mod 12,
   int(Fifths), fifths_from_c(Fifths-2,  a(N,A)),
   nominal_semis(N,Base).

int(N) :- N=0; between(1,inf,M), (N=M; N is -M).

fifths_from_c(Fifths, a(N,A)) :-
   F #= (Fifths) mod 7,
   A #= (Fifths+1) div 7,
   nominal_fifths(N, F),
   label([Fifths]).

nominal_fifths('C', 0).
nominal_fifths('G', 1).
nominal_fifths('D', 2).
nominal_fifths('A', 3).
nominal_fifths('E', 4).
nominal_fifths('B', 5).
nominal_fifths('F', 6).

