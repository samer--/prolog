:- module(plsc,
	[	sc_synth_at/5
	,	synth_set//2
	,	synth_map//2

	,	group_new/2
	,	group_free/2

	,	buffer_zero/1
	,	buffer_free/1
	,	buffer_alloc/3
	,	buffer_set/3
	,	buffer_setn/3
	,	buffer_read/2
	,	buffer_read/4
	,	buffer_write/4
	,	buffer_write/6
	,	buffer_gen/3
	,	buffer/4

	,	sc_notify/1
	,	sc_quit/0
	,	sc_clear/0
	,	sc_status/0
	,	sc_sync/0
	,	sc_sync/1

	,	sc_add_handler/1
	,	sc_rm_handler/1

	,	synth_gated//2
	,	synth_gated//3
	,	synth_ival//2
	,	synth_ival//3
	,	synth_pt//2
	,	synth_pt//3

	,	with_server/2
	]).

:- use_module(library(sc)).
:- use_module(library(plosc)).
:- use_module(library(dcg_core)).
:- use_module(library(lambda)).

:- dynamic buffer/4, osc_server/1, sc_server/1.
:- multifile sc_handler/4.
:- multifile sc_handler/6.

/*
 *	Ideas
 *
 *	Resource bracketing
 *
 *	*	Use with_xxx(Spec,Goal) construct
 *		Uses setup_call_cleanup/3 to acquire resources, call
 *		Goal, and guarantee cleanup after Goal finishes.
 *		Timing is IMPLICIT.
 *
 *	*	Use interval temporal logic foo(Spec,T1,T2) construct
 *		Declarative description that resources are used from
 *		T1 to T2. Can use freeze(T2,Goal) to delay cleanup
 *		until T2 is known.
 *		Timing is EXPLICIT.
 *
 *	OSC message composition
 *
 *	*	Would like to be able to compose messages into a bundle
 *	*	Would prefer not to have to write timed and non timed versions
 *		of everything. Would like grammar for specifying messages,
 *		which can then be run in a timed context or an immediate
 *		context.
 *
 *	OSC message handling
 *
 *	*	Need some way to pass continuations along asynchronous
 *		message handling chain - may need to add and remove
 *		handlers a lot more than I do. OR use Prolog DB. Or
 *		use Prolog message queues somehow.
 */

% synth creation with control over in-group placement
sc_synth_at(head(G),    Nm,Args,ID,T) :- sc:build_args(Args,AX), sc_send_at(T,'/s_new',[string(Nm),int(ID),int(0),int(G)|AX]).
sc_synth_at(tail(G),    Nm,Args,ID,T) :- sc:build_args(Args,AX), sc_send_at(T,'/s_new',[string(Nm),int(ID),int(1),int(G)|AX]).
sc_synth_at(before(N),  Nm,Args,ID,T) :- sc:build_args(Args,AX), sc_send_at(T,'/s_new',[string(Nm),int(ID),int(2),int(N)|AX]).
sc_synth_at(after(N),   Nm,Args,ID,T) :- sc:build_args(Args,AX), sc_send_at(T,'/s_new',[string(Nm),int(ID),int(3),int(N)|AX]).
sc_synth_at(replace(N), Nm,Args,ID,T) :- sc:build_args(Args,AX), sc_send_at(T,'/s_new',[string(Nm),int(ID),int(4),int(N)|AX]).

synth_set(Id,Vals,T,T) :- synth_set(T,Id,Vals).
synth_map(Id,Vals,T,T) :- synth_map(T,Id,Vals).

synth_gated(Name,Args,T1,T2) :- synth_gated(Name,Args,_,T1,T2).
synth_gated(Name,Args,I,T1,T2) :-
	flag(id,I,I+1),
	sc_synth_at(tail(0),Name,Args,I,T1),
	freeze(T2,synth_set(T2,I,[gate:0])).

synth_ival(Name,Args,T1,T2) :- synth_ival(Name,Args,_,T1,T2).
synth_ival(Name,Args,I,T1,T2) :-
	flag(id,I,I+1),
	sc_synth_at(tail(0),Name,Args,I,T1),
	freeze(T2,synth_free(T2,I)).

synth_pt(Name,Args,T,T) :- synth_pt(Name,Args,_,T,T).
synth_pt(Name,Args,I,T,T) :-
	flag(id,I,I+1),
	synth_at(T,Name,Args,I).

gate(I,T1,T2) :-
	synth_set(T1,I,[gate:1]),
	freeze(T2,synth_set(T2,I,[gate:0])).

/* groups */

group_new(head(G),Id)    :- sc_send_now('/g_new',[int(Id),int(0),int(G)]).
group_new(tail(G),Id)    :- sc_send_now('/g_new',[int(Id),int(1),int(G)]).
group_new(before(N),Id)  :- sc_send_now('/g_new',[int(Id),int(2),int(N)]).
group_new(after(N),Id)   :- sc_send_now('/g_new',[int(Id),int(3),int(N)]).
group_new(replace(N),Id) :- sc_send_now('/g_new',[int(Id),int(4),int(N)]).

group_free(nodes,G) :- sc_send_now('/g_freeAll',[int(G)]).
group_free(deep,G)  :- sc_send_now('/g_deepFree',[int(G)]).

buffer_zero(Buf)  :- sc_send_now('/b_zero',[int(Buf)]).
buffer_query(Buf) :-
	sc_add_handler(buffer_info),
	sc_send_now('/b_query',[int(Buf)]).

buffer_free(Buf)  :-
	sc_send_now('/b_free',[int(Buf)]),
	sc_sync, buffer_query(Buf).

buffer_alloc(Buf,Frames,Channels) :-
	Frames1 is Frames,
	sc_send_now('/b_alloc',[int(Buf),int(Frames1),int(Channels)]),
	sc_sync, buffer_query(Buf).

buffer_read(Buf,File) :-
	sc_send_now('/b_allocRead',[int(Buf),string(File)]),
	sc_sync, buffer_query(Buf).

buffer_read(Buf,File,StartAt1,Frames1) :-
	StartAt is StartAt1,
	Frames is Frames1,
	sc_send_now('/b_allocRead',[int(Buf),string(File),int(StartAt),int(Frames)]),
	sc_sync, buffer_query(Buf).

buffer_write(Buf,File,HeaderFmt,SampleFmt) :-
	buffer_write(Buf,File,HeaderFmt,SampleFmt,0,-1).

buffer_write(Buf,File,HeaderFmt,SampleFmt,StartAt1,Frames1) :-
	Frames is Frames1,
	StartAt is StartAt1,
	member(HeaderFmt,[wav,aiff,next,ircam,raw]),
	member(SampleFmt,[int8,int16,int24,int32,float,double,mulaw,alaw]),
	sc_send_now('/b_write',[int(Buf),string(File),string(HeaderFmt),string(SampleFmt),int(Frames),int(StartAt)]).

buffer_set(Buf,PosX,ValX) :-
	seqmap(as_int_float,PosX,ValX,PosValX,[]),
	sc_send_now('/b_set',[int(Buf)|PosValX]).

buffer_setn(Buf,Pos1,Vals) :-
	length(Vals,N),
	seqmap(as_float,Vals,FloatVals,[]),
	sc_send_now('/b_setn',[int(Buf),int(Pos1),int(N)|FloatVals]).

as_float(Val) --> [float(Val)].
as_int_float(Pos,Val) --> [int(Pos),float(Val)].

buffer_gen(Buf,Cmd,Args) :- sc_send_now('/b_gen',[int(Buf),string(Cmd)|Args]).

sc_notify(on)  :- sc_send_now('/notify',[int(1)]).
sc_notify(off) :- sc_send_now('/notify',[int(0)]).
sc_quit        :- sc_send_now('/quit',[]).
sc_clear       :- sc_send_now('/clearSched',[]).

sc_sync :- flag(syncid,Id,Id+1), sc_sync(Id).
sc_sync(Id) :-
	thread_self(Thread),
	sc_add_handler(sync(Thread)),
	sc_send_now('/sync',[int(Id)]),
	thread_get_message(synced(Id)).

sc_status :-
	sc_add_handler(status),
	sc_send_now('/status',[]).

sc_status_handler(Gens,Synths,Groups,SynthDefs,AveCPU,PeakCPU,FS) :-
	sc_add_handler(status),
	format('\nSupercollider status.\n'),
	format('  audio rate: ~g Hz.\n',[FS]),
	format('  gens/~d - synths/~d - groups/~d - defs/~d\n',[Gens,Synths,Groups,SynthDefs]),
	format('  cpu: average/~g - peak/~g\n',[AveCPU,PeakCPU]),
	retractall(sc_server(_)),
	assert(sc_server(FS)).

sync_handler(Thread,Id) :-
	sc_rm_handler(sync(Thread)),
	thread_send_message(Thread,synced(Id)).

sc_handler( attach, '/attach',[], (attach_console,sc_rm_handler(attach))).
sc_handler( fail,   '/fail',  [string(Cmd),string(Msg)], format('Error in ~s -- ~s\n',[Cmd,Msg])).
sc_handler( n_go,   '/n_go',  [int(Node),int(_),int(_),int(_),int(_)], writeln(node_go(Node))).
sc_handler( n_end,  '/n_end', [int(Node),int(_),int(_),int(_),int(_)], writeln(node_end(Node))).

sc_handler( sync(Thread), '/synced', [int(Id)], sync_handler(Thread,Id)).
sc_handler( status,  '/status.reply',
	[int(_),int(Gens),int(Synths),int(Groups),int(SynthDefs),float(AveCPU),float(PeakCPU),float(FS),float(_)],
	sc_status_handler(Gens,Synths,Groups,SynthDefs,AveCPU,PeakCPU,FS)).

sc_handler( buffer_info,  '/b_info',
	[int(Buf),int(Frames),int(Channels),float(FS)],
	buffer_info_handler(Buf,Frames,Channels,FS)).

sc_handler( trigger, '/tr', [int(NodeID),int(TrigID),float(Val)], trigger_handler(NodeID,TrigID,Val)).

sc_handler( trigger_x, '/tr', [int(NodeID),int(TrigID),float(Val)], _, Time,
	trigger_handler(Time,NodeID,TrigID,Val)).


buffer_info_handler(Buf,Frames,Channels,FS) :-
	sc_rm_handler(buffer_info),
	retractall(buffer(Buf,_,_,_)),
	(	Frames>0 -> assert(buffer(Buf,Frames,Channels,FS)); true).


sc_add_handler(Name) :-
	sc_handler(Name,Path,Args,Handler),
	osc_server(Server),
	(var(Args) -> Args1=any; Args1=Args),
	osc_add_handler(Server,Path,Args1,\Path^Args^Handler).

sc_add_handler(Name) :-
	sc_handler(Name,Path,Args,Src,Time,Handler),
	osc_server(Server),
	(var(Args) -> Args1=any; Args1=Args),
	osc_add_handler_x(Server,Path,Args1,\Src^Time^Path^Args^Handler).

sc_rm_handler(Name) :-
	(	sc_handler(Name,Path,Args,_)
	;	sc_handler(Name,Path,Args,_,_,_)
	),
	osc_server(Server),
	osc_del_handler(Server,Path,Args).

null_handler(_,_).

with_server(Port,Goal) :-
	garbage_collect_atoms,
	osc_mk_server(Port,Server),
	osc_mk_address(localhost,Port,Me),
	setup_call_cleanup(
		assert(osc_server(Server)),
		(	maplist(sc_add_handler,[fail,n_go,n_end,trigger,attach]),
			osc_add_handler(Server,'/done', any, \_^[string(Cmd)|Args]^writeln(done(Cmd,Args))),
			osc_add_handler(Server, any, any, \P^A^(writeln(P:A),fail)),
			osc_send(Me,'/attach',[]),
			sc_notify(on),
			setup_call_cleanup(
				osc_start_server(Server),
				call(Goal,Server),
				osc_stop_server(Server)
			)
		),
		retract(osc_server(Server))
	).

