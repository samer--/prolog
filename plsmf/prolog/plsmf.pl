/*
 * Prolog part of standard MIDI file reading library
 * Samer Abdallah (2009)
*/
	  
:- module(plsmf,
	[	smf_read/2		
	,	smf_description/2
	,	smf_duration/2
	,	smf_events/2
	,	smf_events_between/4
	,	is_smf/1	
	]).
	
/** <module> Standard MIDI file reading

@author Samer Abdallah
*/

:-	use_foreign_library(foreign(plsmf)).


%% smf_read( +File:filename, -Ref:smf_blob) is semidet.
%
%  Attempts to read standard MIDI file named File and sets Ref
%  to an SMF blob atom which can be used to make further queries
%  about the file.

%% smf_duration( +Ref:smf_blob, -Dur:nonneg) is det.
%
%  Returns the duration of the MIDI file in seconds.

%% smf_description( +Ref:smf_blob, -Desc:atom) is det.
%
%  Sets Desc to an atom containing descriptive text about the
%  MIDI file, inluding the number of tracks and timing information.

%% smf_events( +Ref:smf_blob, -Events:list(smf_event)) is det.
%
%  Unifies Events with a list containing events in the MIDI file.
%  Not all types of events are handled, but most are. Events are
%  returned in a low-level numeric format containing the bytes
%  in the original MIDI data. The first argument of the smf
%  functor is always the time in seconds.
%
%  smf_event ---> smf( nonneg, byte)
%               ; smf( nonneg, byte, byte)
%               ; smf( nonneg, byte, byte, byte).
%
%  @see smf_events_between/4.

%% smf_events_between( +Ref:smf_blob, +T1:nonneg, +T2:nonneg, -Events:list(smf_event)) is det.
%
%  Unifies Events with a list containing events in the MIDI file
%  between the given times T1 and T2. See smf_events/2 for more
%  information about the format of the events list.

%% is_smf(+Ref) is semidet.
%
%  Determines whether or not Ref is a MIDI file BLOB as returned
%  by smf_read/2.


/*
	MIDI derived event types:

	midi(O,T,msg(A,B,C)) :- midi_send(O,A,B,C,T).
	midi(O,T,noteon(Ch,NN,V)) :- midi_send(O,144+Ch,NN,V,T).
	midi(O,T,noteoff(Ch,NN)) :- midi_send(O,128+Ch,NN,0,T).
	midi(O,T,prog(Ch,Prog)) :- midi_send(O,192+Ch,Prog,Prog,T).
	midi(O,T,prog(Ch,Prog,Bank)) :-
		MSB is Bank // 128,
		LSB is Bank mod 128,
		midi_send(O,176+Ch,0,MSB,T),
		midi_send(O,176+Ch,32,LSB,T),
		midi(O,T,prog(Ch,Prog)).
*/
