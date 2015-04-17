:- module(kernutils, 
		[	kern_get_notes/3
		,  kern_get_spine_notes/4
      ,  kern_get_events/2
      ,  compare_time_key/3
      ,  pitch_notenum/2
      ,  pitch_class/2
		]).

:- meta_predicate kern_get_events(2,-).

:- use_module(library(pairs)).

%% kern_get_events( +EventPred:pred(time,A), -Events:list(A)) is det.
%
%% Collect timed events and return as a temporally sorted list.
kern_get_events(EventPred,XXX) :-
	setof( T-Event, call(EventPred,T,Event), Events), 
	predsort(compare_time_key,Events,Sorted), 
	pairs_values(Sorted,XXX).


%% kern_get_notes( +Mod:module, +Decoder:kern_event_decoder(A), -Events:list(A)) is det.
%
%  Collect all notes in a kern file module and return as a list whose element type
%  is determined by Decoder.
kern_get_notes(Mod,Decoder,XXX) :-
   kern_get_events(note_event(Mod,Decoder),XXX).

%% kern_get_spine_notes( +Mod:module, +Decoder:kern_event_decoder(A), +Spine:spine, -Events:list(A)) is det.
%
%  Collect all notes one a given spine in a kern file module and return as a list whose element type
%  is determined by Decoder.
kern_get_spine_notes(Mod,Decoder,Spine,XXX) :-
   kern_get_events(note_event(Mod,Decoder,Spine),XXX).

note_event(Module,Decoder,Time,Info) :- note_event(Module,Decoder,_,Time,Info).
note_event(Module,Decoder,Spine,Time,Info) :- 
	Module:note(KernEv,Dur,Time,Spine), 
	call(Decoder,KernEv,Time,Dur,Info).

decode_note_pitch_dur(Event,T,Dur,(Note,Dur)) :- decode_note_pitch(Event,T,Dur,Note).
decode_note_nnum_dur(Event,T,Dur,(Note,Dur)) :- decode_note_nnum(Event,T,Dur,Note).
decode_time_dur_pitch(pitch(P),T,Dur,note(T,Dur,P)).
decode_time_dur_nn(pitch(P),T,Dur,note(T,Dur,NN)) :- pitch_notenum(P,NN).
decode_full(Event,T,Dur,note(T,Dur,Event)).

decode_note_pitch( pitch(P),_,_,pitch(P)).
decode_note_pitch( rest,    _,_,rest).

decode_note_nnum( pitch(P),_,_,nn(NN)) :- pitch_notenum(P,NN).
decode_note_nnum( rest,    _,_,rest).

decode_pitch( pitch(P),_,_,P).
decode_nnum( pitch(P),_,_,NN) :- pitch_notenum(P,NN).

%% compare_time_key( +R:relation, +X:pair(time,A), +Y:pair(time,B)) is semidet.
% compare_time_key( -R:relation, +X:time, +Y:time) is det.
compare_time_key(R,X-A,Y-B) :- (X<Y -> R=(<); X>Y -> R=(>); compare(R,A,B)).

%% pitch_notenum( +P:pitch, -NN:between(0,127)) is det.
%
% Compute MIDI note number (0--127) for a given Kern pitch
pitch_notenum(PC/Oct,NN) :- !, pc_nn(PC,NN1), oct_semis(Oct,O), NN is NN1 - O.
pitch_notenum(PC*Oct,NN) :- !, pc_nn(PC,NN1), oct_semis(Oct,O), NN is NN1 + O.
pitch_notenum(PC,NN) :- pc_nn(PC,NN).

oct_semis(oct^N,M) :- !, M is 12*N.
oct_semis(oct,12).

pc_nn(sharp(PC),N) :- !, pc_nn(PC,M), succ(M,N).
pc_nn(flat(PC),N) :- !, pc_nn(PC,M), succ(N,M).
pc_nn(c,60).
pc_nn(d,62).
pc_nn(e,64).
pc_nn(f,65).
pc_nn(g,67).
pc_nn(a,69).
pc_nn(b,71).

%% pitch_class(+P:pitch, -PC:pitch_class) is det.
%
%  Get pitch class from pitch by stripping octave modifiers.
pitch_class(P,_) :- must_be(nonvar,P), fail.
pitch_class(P/oct,PC) :- !, pitch_class(P,PC).
pitch_class(P*oct,PC) :- !, pitch_class(P,PC).
pitch_class(P,P).

