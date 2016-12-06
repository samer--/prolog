:- module(mididcg, [ midi//3
                   , noteon//3
                   , noteoff//2
                   , note//4
                   , prog//2
                   , prog//3
                   , prog//4
                   , pan//2
                   , volume//2
						 , tempo//1
						 , keysig//2
						 , timesig//1
                   , instr//2
                   ]).

:- use_module(library(clpfd)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_core), [get//1, set//1]).
:- use_module(library(genmidi), [gm/4]).

midi(Msg,Arg1,Arg2) --> get(T) <\> [msg(T,M,Arg1,Arg2)], {M #= Msg}.
midi(Msg,Arg1)      --> get(T) <\> [msg(T,M,Arg1)], {M #= Msg}.
meta(Msg,Bytes)     --> get(T) <\> [meta(T,M,N,Bytes)], {M #= Msg, length(Bytes,N)}.
text(Type,Text)     --> get(T) <\> [text(T,M,Text)], {text_type_code(M, Type)}.

text_type_code(0x01, text).
text_type_code(0x02, copyright).
text_type_code(0x03, track_name).
text_type_code(0x04, instrument).
text_type_code(0x05, lyric).
text_type_code(0x06, marker).
text_type_code(0x07, cue).

tempo(T)             --> meta(0x51, [B2, B1, B0]), {divmod(T,256,Z,B0), divmod(Z,256,B2,B1)}.
keysig(Sharps,major) --> meta(0x59, [Sharps, 0]).
keysig(Sharps,minor) --> meta(0x59, [Sharps, 1]).
timesig(Num/Denom)   --> meta(0x58, [Num, DenomPower, 24, 8]), { Denom #= 2^DenomPower }.

holding_time(P) --> \< get(T), call_dcg(P), \< set(T).

noteon(Ch,NN,V) --> midi(144+Ch,NN,V).
noteoff(Ch,NN) --> midi(128+Ch,NN,0).

note(Ch,Vel,Dur,NN) --> 
   {N1 #= NN, V1 #= Vel},
   noteon(Ch,N1,V1), \< adv(Dur),
   noteoff(Ch,N1).

prog(Ch,Prog) -->
	midi(192+Ch,Prog).
	
prog(Ch,Prog,Bank) -->
   { MSB #= Bank // 128, 
     LSB #= Bank mod 128
   },
	midi(176+Ch,0,MSB),
	midi(176+Ch,32,LSB),
	prog(Ch,Prog).

prog(Ch,Prog,MSB,LSB) -->
	midi(176+Ch,0,MSB),
	midi(176+Ch,32,LSB),
	prog(Ch,Prog).

instr(Ch,Instr) -->
   {gm(Instr,Prog,MSB,LSB)},
   prog(Ch,Prog,MSB,LSB).

pan(Ch,Pan) --> midi(176+Ch,10,Pan).

volume(Ch,Vol) --> midi(176+Ch,7,Vol).

adv(Dur,T1,T2) :- T2 #= T1 + Dur.
