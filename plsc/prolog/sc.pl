:- module(sc, [
		sc_connect/2
	,	sc_connect/1
	,	synth/2
	,	synth/3
	,	synth_at/3
	,	synth_at/4
	,	synth_set/3
	,	synth_map/3
	,	synth_free/2
	,	synth_noid/2
	,	sc_send_at/3
	,	sc_send_now/2
	,	sc_current_server/1

	,	sc_note/5
	,	sc_note_off/2
	,	sc_note_on/4
	,	synth_class/2
	]).

:- use_module(library(plosc)).
:- use_module(library(dcg_core)).

:- dynamic osc_addr/3, reply_server/1.
:- initialization(osc_addr(_,_,_) -> true; assert(osc_addr(null,null,null))).

sc_current_server(S) :- reply_server(S).
sc_reply_to(Server:osc_server) :-
	retractall(reply_server(_)),
	assert(reply_server(Server)).

%% sc_connect( +Host:atom, +Port:integer) is det.
%% sc_connect( +Host:atom) is det.
%
%  Connect to Supercollider synthesiser process at Host on given port.
%  The connection will be used for all subsequent synthesiser messages.
%  Any previous connection is dropped. Default port for sc_connect/1
%  is 57110.

sc_connect(Host) :- sc_connect(Host,57110).
sc_connect(Host,Port) :-
	osc_mk_address(Host,Port,Addr),
	retractall(osc_addr(_,_,_)),
	assert(osc_addr(Host,Port,Addr)).


build_args(Maps,Args) :- seqmap(to_string_float,Maps,Args,[]).
build_int_args(Maps,Args) :- seqmap(to_string_int,Maps,Args,[]).

to_string_float(N:V) --> {V1 is V}, [string(N),float(V1)].
to_string_float(N=V) --> [string(N),V].

to_string_int(N:V) --> {V1 is V}, [string(N),int(V1)].
to_string_int(N=V) --> [string(N),V].


%% synth( +Name:atom, +Args:list(arg),+ID:integer) is det.
%% synth( +Name:atom, +Args:list(arg)) is det.
%
% Instantiate a new synth using given name and arguments. Arguments are
% of the form Name:Value. If present, the third argument is an integer
% ID which can be used to reference the synth node later. The default
% value is -1, which causes the server to autogenerate an ID.
synth(Name,Args) :- synth(Name,Args,-1).
synth(Name,Args,ID) :-
	build_args(Args,OSCArgs),
	osc_addr(_,_,Addr),
	osc_send(Addr,'/s_new',[string(Name),int(ID),int(0),int(1)|OSCArgs]).

%% synth_at( +T:time, +Name:atom, +Args:list(arg), +ID:integer) is det.
%% synth_at( +T:time, +Name:atom, +Args:list(arg)) is det.
%
% Instantiate a new synth at given time using given name and arguments. Arguments are
% of the form Name:Value. If present, the third argument is an integer
% ID which can be used to reference the synth node later. The default
% value is -1, which causes the server to autogenerate an ID.
synth_at(T,Name,Args) :- synth_at(T,Name,Args,-1).
synth_at(T,Name,Args,ID) :-
	build_args(Args,OSCArgs),
	osc_addr(_,_,Addr),
	osc_send(Addr,'/s_new',[string(Name),int(ID),int(0),int(0)|OSCArgs],T).

%% synth_set( +T:time, +ID:integer, +Args:list(arg)) is det.
%
%  Set values of controls on the given synth. ID is the integers identifier
%  assigned to the synth when it was created using synth/3 or synth_at/4.
%  Args is a list of Name:Vlaue pairs.
synth_set(T,ID,Args) :-
	build_args(Args,OSCArgs),
	osc_addr(_,_,Addr),
	osc_send(Addr,'/n_set',[int(ID)|OSCArgs],T).


%% synth_map( +T:time, +ID:integer, +Args:list(maparg)) is det.
%
%  Set values of controls on the given synth. ID is the integers identifier
%  assigned to the synth when it was created using synth/3 or synth_at/4.
%  Args is a list of Name:Vlaue pairs.

synth_map(T,ID,Args) :-
	build_int_args(Args,OSCArgs),
	osc_addr(_,_,Addr),
	osc_send(Addr,'/n_map',[int(ID)|OSCArgs],T).

% synth_mapa ??

%% synth_free( +Time:time, +ID:integer) is det.
%
%  Free the synth identified by ID at time T.
synth_free(T,ID) :- osc_addr(_,_,Addr), osc_send(Addr,'/n_free',[int(ID)],T).


%% synth_noid( +Time:time, +ID:integer) is det.
%
%  Release the identifier ID of a synth so that it can be reused.
synth_noid(T,ID) :- osc_addr(_,_,Addr), osc_send(Addr,'/s_noid',[int(ID)],T).



%% sc_send_at( +T:time, +M:atom, +Args:list(osc_arg)) is det.
sc_send_at(T,Msg,Args) :- osc_addr(_,_,Addr), osc_send(Addr,Msg,Args,T).

%% sc_send_now( +M:atom, +Args:list(osc_arg)) is det.
sc_send_now(Msg,Args) :- osc_addr(_,_,Addr), osc_send(Addr,Msg,Args).



% --- SC notes -----

% Node ids 0, 1, 2 never seem to work for some reason.
:- initialization(flag(scnid,_,3)).


sc_note(perc,Synth,T,_,Args) :-
	flag(scnid,ID,ID+1),
	synth_at(T,Synth,Args,ID).

sc_note(sustain,Synth,T,Dur,Args) :-
	T2 is T+Dur,
	sc_note_on(Synth,T,Args,ID),
	sc_note_off(T2,ID).

sc_note_off(T,ID) :- synth_set(T,ID,[gate:0]).
sc_note_on(Synth,T,Args,ID) :-
	flag(scnid,ID,ID+1),
	synth_at(T,Synth,Args,ID).


%% synth_class(+Name:atom, -Class:synth_class) is det.
%% synth_class(-Name:atom, -Class:synth_class) is nondet.
%
%  Return class of named Supercollider synth. Can be called
%  with Name unbound to unify N with known synth names.
%  synth_class can be perc (percussive) or sustain. Percussive
%  synths do not need explicit note end commands whereas
%  sustained ones do.

% unpitched percussive
synth_class(impulse,perc).
synth_class(kick0,perc).
synth_class(kick2,perc).
synth_class(kick3,perc).
synth_class(snare,perc).
synth_class(hat,perc).
synth_class(chat,perc).
synth_class(bar,perc).
synth_class(clap,perc).
synth_class(cling,perc).
synth_class(cling2,perc).

% pitched percussive
synth_class('SPE1',perc).
synth_class('SPE2',perc).
synth_class('SimpleSquare',perc).
synth_class('SimpleSaw',perc).
synth_class('SimpleSine',perc).
synth_class('VarSaw',perc).
synth_class(bling,perc).
synth_class(bell,perc).
synth_class(wood,perc).
synth_class(sines,perc).
synth_class(string,perc).
synth_class(drop,perc).
synth_class(glass,perc).
synth_class(crackle,perc).

% pitched sustained
synth_class('piano',sustain).
synth_class('SustainSaw',sustain).
synth_class('SustainSquare',sustain).
synth_class('SustainSine',sustain).
synth_class('ADSRSaw',sustain).
synth_class('SustainSawSlew',sustain).
synth_class('SustainSquareSlew',sustain).
synth_class('SustainSineSlew',sustain).
synth_class(fmsynth,sustain).
synth_class(synth,sustain).
synth_class(marimba,sustain).
synth_class(marimba2,sustain).
synth_class(elbass,sustain).
synth_class(bass,sustain).
synth_class(moog,sustain).
%synth_class(xylo,perc).
%synth_class(softwg,perc).
%synth_class(clarinet,perc).

