:- use_module(library(sc)).
:- use_module(library(plosc)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_shell)).
:- use_module(library(plrand)).
:- use_module(library(randbase)).
:- use_module(library(randpred)).
:- use_module(library(pitch)).
:- use_module(library(timedcg)).
:- use_module(library(lambda)).
:- use_module(library(plsc)).

:- dynamic osc_server/1, sc_server/1.
:- dynamic buffer_duration/2.


buffer_copy(Dest,Src,DPos,SPos,Len) :- buffer_gen(Dest,copy,[int(DPos),int(Src),int(SPos),int(Len)]).
buffer_circ_copy(Dest,Src,SPos,EPos,Len) :-
	buffer(Src,SrcLen,_,_),
	SPos1 is mod(SPos,SrcLen),
	EPos1 is mod(EPos,SrcLen),
	(	SPos1<EPos1
	-> Len is EPos1-SPos1,
		buffer_copy(Dest,Src,0,SPos,Len)
	;	EndChunk is SrcLen-SPos1,
		Len is EndChunk + EPos1,
		buffer_copy(Dest,Src,0,SPos1,EndChunk),
		buffer_copy(Dest,Src,EndChunk,0,EPos1)
	).



buffer_dur(Buf,Dur) :- buffer(Buf,Frames,_,FS), Dur is Frames/FS.

audio_thru_st   --> synth_ival('audio-thru-st',[]).
mixdown(In,Out) --> synth_ival('mixdown',[i_in:In, i_out:Out]).
panout(Bus,Id)  --> synth_ival('panout',[i_in:Bus, i_out:0],Id).
audio_mixdown_panout(Args,Id) --> synth_ival('audio-mixdown-panout',Args,Id).


rec_dur(Buf,Dur) --> synth_pt('recbuf-dur',[i_buf:Buf,i_in:8,i_dur:Dur]).
rec_once(Buf)    --> synth_gated('recbuf-gate-once',[i_buf:Buf,i_in:8,gate:1]).
rec_gated(Buf,I) --> synth_ival('recbuf-gate',[buf:Buf,i_in:8,gate:0],I).
rec_trig(Buf,I)  --> synth_ival('recbuf-trig',[buf:Buf,i_in:8],I).
rec_circ(Buf,In,Out) --> synth_ival('recbuf-circ',[i_buf:Buf,i_in:In,phase_out:Out]).

set_pan(I,Pan) --> synth_set(I,[pan:Pan]).
set_amp(I,Amp) --> synth_set(I,[amp:Amp]).
set_buf(I,Buf) --> synth_set(I,[buf:Buf]).
trig_start(I)  --> synth_set(I,[start:1,stop:0]).
trig_stop(I)   --> synth_set(I,[start:0,stop:1]).



onsets_trig(Id) -->
	mixdown(8,17) // synth_ival('onsets-trig',[i_in:17,i_fftbuf:0,meth=symbol(rcomplex)],Id).

onset_thresh(Id,Thresh) --> synth_set(Id,[thresh:Thresh]).

playbuf_circ(Buf) -->
	synth_gated('playbuf-circ',[i_buf:Buf,i_att:1,i_rel:1]).

playbuf(Buf) -->
	{buffer_duration(Buf,Dur)},
	playbuf_dur(Dur,Buf,[i_att:0.008,i_rel:0.01]).

playbuf_dur(Dur,Buf,Args) -->
	synth_pt('playbuf-dur',[i_buf:Buf,i_dur:Dur|Args]), adv(Dur). % i_att, i_rel, amp, pan

playbuf_st_dur(Dur,Buf) -->
	synth_pt('playbuf-st-dur',[i_buf:Buf,i_dur:Dur]), adv(Dur). % i_att, i_rel, amp

playbuf_dur_rate(Dur,Rate,Buf) -->
	synth_pt('playbuf-dur-rate',[i_buf:Buf,i_dur:Dur,rate:Rate]).
	% i_att, i_rel, amp

plsc:sc_handler( trigger, '/tr', [int(NodeID),int(TrigID),float(Val)], trigger_handler(NodeID,TrigID,Val)).
plsc:sc_handler( trigger_x, '/tr', [int(NodeID),int(TrigID),float(Val)], _, Time,
	trigger_handler(Time,NodeID,TrigID,Val)).


trigger_handler(NodeID,TrigID,Val) :-
	get_time(Time),
	(	buffer(TrigID,_,_,_) % it's a buffer duration message
	-> retract(buffer_duration(TrigID,_)),
		assert(buffer_duration(TrigID,Val)),
		% !! nb cannot be longer than length of buffer
		format('~g: buffer ~d duration = ~g s.\n',[Time,TrigID,Val])
	;	TrigID=67
	->	format('~g: onset.\n',[Time])
	;	format('~g: trigger ~w from ~w (~w)\n',[Time,TrigID,NodeID,Val])
	).

trigger_handler(Time,NodeID,TrigID,Val) :-
	format('trigger ~w at ~w from ~w (~w)\n',[TrigID,Time,NodeID,Val]).


% TO PORT
%
% Use
% 		respond to /tr [ _, int(bufid), float(dur) ]

piano_file(File,Level) :-
	piano_files(Files,Levels),
	nth1(N,Files,Suffix),
	nth1(N,Levels,Level),
	format(atom(File),'/Users/samer/src/hyperpiano/samples/p~s.wav',[Suffix]).

piano_files(
	[ '18','08','19','10','11', '12','20','14',
		'15','01','03','04','05','06','16', '17' ],
	[	1.5, 2.0, 1.5, 0.5, 0.5, 4.0, 1.5, 2.0,
		1.5, 1.0, 0.5, 0.5, 0.5, 1.0, 2.0, 1.5 ]).

alloc_buffers(N,Dur,Bufs,BufNumsBuf) :-
	sc_status, sc_sync,
	sc_server(Fs),
	buffer_alloc(0,512,1),      % fft buffer
	buffer_alloc(1,3*Dur*Fs,1), % circular buffer
	rep(N,alloc_buffer(Dur,Fs),(2,Bufs),(BufNumsBuf,[])),
	buffer_alloc(BufNumsBuf,N,1),
	buffer_setn(BufNumsBuf,0,Bufs).

alloc_buffer(Dur,FS) -->
	\< (get(Buf), succ),
	\> out(Buf),
	{	Len is Dur*FS,
		buffer_alloc(Buf,Len,1),
		assert(buffer_duration(Buf,Dur))
	}.

audio_onsets(Thresh,BusIn,BusOut,Id) -->
	synth_ival('onsets',
		[	meth = symbol(rcomplex)
		,	i_fftbuf : 0
		,	i_in     : BusIn
		,	i_out    : BusOut
		,	thresh   : Thresh
		], Id).

onsets_gate(BufNumBuf,OnsetBus,BufNumBus,GateBus,Id) -->
	synth_ival('onset_recbuf',
		[	onsets_in : OnsetBus
		,	gate_out  : GateBus
		,	buf_out   : BufNumBus
		,	bufs      : BufNumBuf
		], Id).

gated_recorder(InitBuf,AudioBus,BufNumBus,GateBus) -->
	get(T),
	synth_ival('recbuf-gate',[buf:InitBuf,i_in:AudioBus],Id),
	with(T,adv(0.01)>>synth_map(Id,[gate:GateBus,buf:BufNumBus])).

onset_record(onset_recorder(Detector,Gater)) -->
	{ MonoBus=17},
	{ OnsetBus=0, GateBus=1, BufNumBus=2},
	{ BufNumBuf=11, Buf0=2 },
	(	mixdown(8,MonoBus)
	// audio_onsets(0.4,MonoBus,OnsetBus,Detector)
	// onsets_gate(BufNumBuf,OnsetBus,BufNumBus,GateBus,Gater)
	// gated_recorder(Buf0,MonoBus,BufNumBus,GateBus)
	).

reset(onset_recorder(_,Gater)) --> synth_set(Gater,[reset:1]), adv(0.01), synth_set(Gater,[reset:0]).
enable(onset_recorder(_,Gater)) --> synth_set(Gater,[en:1]).
enable(onoff_recorder(_,Gater)) --> synth_set(Gater,[en:1]).
disable(onset_recorder(_,Gater)) --> synth_set(Gater,[en:0]).
disable(onoff_recorder(_,Gater)) --> synth_set(Gater,[en:0]).
thresh(onset_recorder(Detector,_),Thresh) --> synth_set(Detector,[thresh:Thresh]).

on_thresh(onoff_recorder(OnDetector,_),T) --> synth_set(OnDetector,[thresh:T]).
off_thresh(onoff_recorder(_,OffDetector),T) --> synth_set(OffDetector,[thresh:T]).

onoff_trigger(OnsetBus,PhaseBus,MonoBus,OffThresh,OffDetector) -->
	synth_ival('onset_offset_recbuf',
		[	onsets_in:OnsetBus
		,	phase_in:PhaseBus
		,	i_in:MonoBus
		,	thresh:OffThresh
		], OffDetector).

onoff_record(Cmd) -->
	{ MonoBus=17, CircBuf=1, OffThresh=0.001 },
	(	mixdown(8,MonoBus)
	//	rec_circ(CircBuf,MonoBus,PhaseBus)
	//	audio_onsets(0.4,MonoBus,OnsetBus,OnDetector)
	// onoff_trigger(OnsetBus,PhaseBus,MonoBus,OffThresh,OffDetector)
	// call(Cmd,onoff_recorder(OnDetector,OffDetector))
	).

/*
	recorder_state ---> recording(time,buf_pos,buf_id)
	                  ; waiting(buf_id)
							; disabled(buf_id).

	Pre=768  % number of samples before onset to copy
	Post=256 % number of samples after offse to copy

	recording(T,P,B) --[event(_disable)]--> disabled(B).
	recording(T1,P1,Buf1) --[event(T2,onset(P2))]--> recording(T2,P2,Buf2) :-
		(	overflow(T1,T2) -> nop % overflow if T2-T1 > circular buffer duration
		;	copy_circ((P1-Pre)..(P2+Post),Buf1,Len),
			set_buf_dur(Buf1,Len/FS),
			next_buf(Buf1,Buf2)
		).

	recording(T1,P1,Buf1) --[event(T2,offset(P2))]--> waiting(Buf2) :-
		(	overflow(T1,T2) -> nop
		;	copy_circ(P1..P2,Buf1,Len),
			set_buf_dur(Buf1,Len/FS),
			next_buf(Buf1,Buf2)
		).


	waiting(Buf) --[event(_,disable)]--> disabled(Buf).
	waiting(Buf) --[event(_,offset(_))]--> waiting(Buf).
	waiting(Buf) --[event(T,onset(P))]--> recording(T,P,Buf).

	disabled(B)  --[event(_,enable)]--> waiting(B).
	disabled(B)  --[_]--> disabled(B).


 db of buffer gains?
 */


note(Instr,Pitch) --> note(Instr,Pitch,[]).
note(Instr,Pitch,Args) -->
	{note_freq(Pitch,Freq)},
	(	{synth_class(Instr,sustain)}
	->	synth_gated(Instr,[freq:Freq|Args])
	;	synth_pt(Instr,[freq:Freq|Args]), set(_)
	).

pnote(Instr) --> pnote(Instr,[]).
pnote(Instr,Args) -->
	{synth_class(Instr,perc)},
	synth_pt(Instr,Args).

reich_degrees([\6,\7,\8,-,\7,-,\8,\7,\6,-,\7,-]).
degree_pitch(_,-,-).
degree_pitch(R,\I,\(R*I)).
rnote(-) --> nop.
rnote(\P) --> iso(note(piano,P)//adv(0.2)).

rnote(_,-) --> nop.
rnote(I,\P) --> iso(note(I,P)//adv(0.2)).

playseq(Dur,PX) --> seqmap_with_sep(adv(Dur),rnote,PX), adv(Dur).
playseq(I,Dur,PX) --> seqmap_with_sep(adv(Dur),rnote(I),PX), adv(Dur).


