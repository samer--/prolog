:- module(musicxml,
          [ musicxml_score/2
          , score_prop/2
          , score_part_prop/3
          , score_part_events/3
          , score_part_chords/3
          , score_part_slices/3
          , score_part_notes/3
          , slices_notes/2
          , events_slices/2
          ]).

/** <module> Reading and interpreting MusicXML files

This module provides tools for handling MusicXML files.
Some of the types used are defined as follows:
==
chord ---> chord(pitch_class, pitch_class, list(interval)).
pitch ---> rest; pitch(pitch_class, octave).

octave      == integer.
pitch_class == alterable(nominal).
interval    == alterable(degree).

alterable(A) ---> a(A, integer).

nominal ---> 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'.
degree  ---> 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14.
==
An 'alterable' in this context is something that can have a number of sharps 
or flats associated with it, including pitch classes and scale degrees. The 
=|event|= type is a pair of a time spec and a payload type. The time spec can 
be a number (a time point), a span (a pair of start and end time), or a tagged 
union of points and spans. Tied objects (notes) are specified by a pair of
bools indicating if the object is tied to the previous and next objects
respectively.
==
event(T, X) == pair(T, X).
ties == pair(bool, bool)
span == pair(number, number)
time_spec ---> point(number); span(span).
score_token ---> end; bar; change(chord); slice(list(tied(Pitch))).
tied(A) == pair(ties, A).
==
*/

:- use_module(library(xpath)).
:- use_module(library(zlib), [gzopen/3, zopen/3]).
:- use_module(library(sgml), [load_xml/3]).
:- use_module(library(insist)).
:- use_module(library(dcg_core), [get//1, trans//2, seqmap//2]).
:- use_module(library(dcg_pair)). % includes transduce/3
:- use_module(library(snobol), [arbno//1]).
:- use_module(library(math), [add/3]).
:- use_module(library(listutils), [cons/3, map_filter/3]).
:- use_module(library(mxml_chords), [pitch_midi/2, decode_chord/2, decode_pitch/4]).

%% musicxml_score(+Source:source, -Score:xml) is det.
%  Read MusicXML from Source and unify Score with XML data structure.
%  Source can be a file name or =|stream(StreamHandle)|=
%  as recognised by load_xml/3.
musicxml_score(File, Score) :-
   setup_call_cleanup(file_source(File, Source, Release),
                      load_xml(Source, Doc, [space(remove)]),
                      Release),
   findall(Score, xpath(Doc, //'score-partwise', Score), Scores),
   insist(Scores = [Score], multiple_scores_in(File)).

file_source(File, File, true) :- atom_concat(_, '.xml', File), !.
file_source(File, Stream, close(Stream)) :- atom_concat(_,'.xml.gz', File), !, 
   gzopen(File, read, Stream).
file_source(File, Uncompressed, close(Uncompressed)) :- atom_concat(_,'.mxl', File), !, 
   open(File, read, Compressed, []),
   zopen(Compressed, Uncompressed, [format(deflate)]).


%% score_prop(+S:xml, -P:score_prop) is nondet.
%  Valid props:
%  ==
%  score_prop ---> parts(list(pid))
%                ; title(atom)
%                ; software(atom)
%                ; date(atom).
%  ==
score_prop(S, parts(Ps)) :- findall(P, xpath(S, 'part-list'/'score-part'(@id), P), Ps).
score_prop(S, date(Date)) :- xpath(S, identification/encoding/'encoding-date'(text), Date).
score_prop(S, software(So)) :- xpath(S, identification/encoding/software(text), So).
score_prop(S, title(Title)) :- xpath(S, credit/'credit-words'(text), Title).


%% score_part_prop(+S:xml, +P:pid, -P:part_prop) is nondet.
%  Also verifies that measure numbers are contiguous. Valid props:
%  ==
%  part_prop ---> divisions(nat)   % time is counted in this fraction of a crochet.
%               ; fifths(integer)  % number of sharps (+ve) or flats (-ve) in key sig
%               ; n_measures(nat). % number of measures in part
%  ==
score_part_prop(S, PartId, Prop) :-
   xpath(S, 'part'(@id=PartId), Part),
   findall(A, xpath(Part, //attributes, A), As),
   insist(As=[Attribs], too_many_attributes(As)),
   ( attribs_prop(Attribs, Prop)
   ; part_prop(Part, Prop)
   ).

attribs_prop(Attribs, divisions(Div)) :- xpath(Attribs, divisions(number), Div).
attribs_prop(Attribs, fifths(Fifths)) :- xpath(Attribs, key/fifths(number), Fifths).

part_prop(Part, n_measures(NMeasures)) :-
   findall(M, part_numbered_measure(Part, M), NumberedMeasures),
   length(NumberedMeasures, NMeasures),
   pairs_keys_values(NumberedMeasures, MNums, _),
   insist(numlist(1,NMeasures,MNums), bad_measure_numbers(MNums)).

part_numbered_measure(Part, N-Content) :- 
   xpath(Part, measure, M),
   xpath(M, /self(@number(number)), N),
   M=element(_,_,Content).

% ----------- decoding MusicXML measures ---------------------------------

score_part_measures(Score, PartId, Measures) :-
   xpath(Score, 'part'(@id=PartId), Part),
   findall(M, xpath(Part, measure, element(_,_,M)), RawMeasures),
   maplist(decode1(measure, []), RawMeasures, Measures).

decode_record(Elements) -->
   \< [element(El, Ats, Content)],
   ( {member(El, Elements)}, \> [El-Val], {decode1(El, Ats, Content, Val)}
   ; {maplist(dif(El), Elements)}
   ).

decode1(El, Ats, Content, Val) :- insist(decodex(El, Ats, Content, Val)).

decodex(El, Ats, _, Ats) :- attr(El), !.
decodex(El, _, [Content], Val) :- leaf(El, Content, Val).
decodex(El, _, Content, Val) :- transduce(in(El), Content, Val).

attr(tie).

in(measure) --> decode_record([harmony, note, backup, forward]).
in(harmony) --> decode_record([root, kind, bass, degree, offset]).
in(root)    --> decode_record(['root-step', 'root-alter']).
in(bass)    --> decode_record(['bass-step', 'bass-alter']).
in(degree)  --> decode_record(['degree-value', 'degree-alter', 'degree-type']).
in(note)    --> decode_record([pitch, duration, chord, rest, tie, voice]).
in(backup)  --> decode_record([duration]).
in(forward) --> decode_record([duration]).
in(pitch)   --> decode_record([step, alter, octave]).

leaf(El,C,V) :- member(El, [alter, octave, duration, offset, voice, 'root-alter', 'degree-alter', 'bass-alter', 'degree-value']), atom_number(C,V).
leaf(El,C,C) :- member(El, [step, kind, 'root-step', 'bass-step', 'degree-type']).


%% events_slices(+Events:list(event(time_spec,score_token)), -Slices:list(event(span,list(tied(pitch))))) is det.
events_slices(Events, Slices) :- 
   map_filter(event_slice, Events, Slices).

events_chords(Events, Chords) :- 
   map_filter(event_chord_change, Events, Changes),
   extend_durations(Changes, Chords).

%% score_part_slices(+S:xml, +P:pid, -Slices:list(event(span, list(tied(pitch))))) is det.
score_part_slices(Score, PartId, Slices) :-
   score_part_events(Score, PartId, Events),
   events_slices(Events, Slices).

%% score_part_notes(+S:xml, +P:pid, -Notes:list(event(span, pitch))) is det.
score_part_notes(Score, PartId, Notes) :-
   score_part_slices(Score, PartId, Slices),
   slices_notes(Slices, Notes).

%% score_part_chords(+S:xml, +P:pid, -Chords:list(event(span, chord))) is det.
score_part_chords(Score, PartId, Chords) :-
   score_part_events(Score, PartId, Events),
   events_chords(Events, Chords).

event_chord_change(point(T)-change(C), T-C).
event_chord_change(point(T)-end, T-end).
event_slice(span(T)-slice(S), T-S).

extend_durations([T-D | Cs], Es) :- foldl(ext, Cs, Es, T-D, _).
ext(T2-D2, (T1-T2)-D1, T1-D1, T2-D2).


%% score_part_events(+S:xml, +P:pid, -Events:list(event(time_spec, score_token))) is det.
score_part_events(Score, PartId, Events) :-
   decode_measures(Score, PartId, Dur, Events, [point(Dur)-end]).

decode_measures(Score, PartId, Dur) -->
   { score_part_measures(Score, PartId, Measures),
     score_part_prop(Score, PartId, divisions(Div)) },
   run_left(foldl(transduce_measure(Div), Measures), 0, Dur).

transduce_measure(Div, MeasureItems, T1-[ span(T1-T2)-bar| L1], T2-L2) :-
   insist(arbno(musicxml:measure_item(Div), T1-(MeasureItems-L1), T2-([]-L2))).

measure_item(Div) -->
   trans(Start, End) <\> (simultaneous_notes(Propss) <\> [ span(Start-End)-slice(Tokens) ]),
   {  maplist(member(duration-Dur), Propss),
      insist(maplist(member(voice-1), Propss)),
      maplist(decode_tied_note, Propss, Notes),
      exclude(is_rest, Notes, Tokens),
      add(Dur rdiv Div, Start, End)
   }.

measure_item(Div) --> 
   get(T) <\> ([harmony-Props] <\> [point(TC)-change(Chord)]),
   {  select(offset-Offset, Props, Props1)
   -> TC is T + Offset rdiv Div
   ;  Props = Props1, TC = T
   },
   {insist(decode_chord(Props1, Chord))}.

measure_item(Div) -->
   trans(Start, End) <\> (\< [forward-Props]),
   {  member(duration-Dur, Props),
      add(Dur rdiv Div, Start, End)
   }.

measure_item(Div) -->
   trans(Start, End) <\> (\< [backup-Props]),
   {  member(duration-Dur, Props),
      add(-Dur rdiv Div, Start, End)
   }.

is_rest(_-rest).

chord_note(Chord, Props) --> 
   [note-Props], {membership(chord-_, Props,Chord)}.

simultaneous_notes([Props1 | Propss]) -->
   chord_note(false, Props1),
   seqmap(chord_note(true), Propss).

decode_tied_note(Props, Ties-Note) :-
   decode_ties(Props, Ties),
   decode_note(Props, Note).

decode_note(Props, pitch(PitchClass, Oct)) :-
   member(pitch-PProps, Props),
   \+member(rest-_, Props),
   decode_pitch(step, alter, PProps, PitchClass),
   member(octave-Oct, PProps).

decode_note(Props, rest) :-
   member(rest-_, Props),
   \+member(pitch-_, Props).

decode_ties(Props, TiedBack-TiedFwd) :-
   is_tied(stop, Props, TiedBack),
   is_tied(start, Props, TiedFwd).

is_tied(Type, Props, true) :- member(tie-TProps, Props), member(type=Type, TProps), !.
is_tied(_, _, false).

membership(X, Xs, F) :- member(X,Xs) -> F=true; F=false.

%% slices_notes(+Slices:list(event(span, list(tied(pitch)))), -Notes:list(event(span,pitch))) is det. 
%
%  Converts a sequence of time slices each containing multiple tied notes into a sequence of 
%  complete, possibly overlapping note events (without rests).
slices_notes(Slices, Notes) :- seqmap(process_slice, Slices, []-Notes, []-[]).
process_slice(T-Notes) --> seqmap(process_slice_note(T), Notes).
process_slice_note(T, (Back-Fwd)-Pitch) --> insist_dcg(process_ties(T, Back, Fwd, Pitch)).

process_ties(T1-T2, false, false, Pitch) --> \> [ (T1-T2)-Pitch ].
process_ties(T1-_, false, true, Pitch) --> {pitch_midi(Pitch, NN)}, cons(NN-(T1-T2)) <\> [ (T1-T2)-Pitch ].
process_ties(_-T2,  true, false, Pitch) --> {pitch_midi(Pitch, NN)}, \< select(NN-(_-T2)).
% process_ties(T1-_, false, true, Pitch) --> cons(Pitch-(T1-T2)) <\> [ (T1-T2)-Pitch ].
% process_ties(_-T2,  true, false, Pitch) --> \< select(Pitch-(_-T2)).
process_ties(_, true, true, _) --> []. % check pitch is pending?

insist_dcg(G, S1, S2) :- insist(call_dcg(G, S1, S2)).

