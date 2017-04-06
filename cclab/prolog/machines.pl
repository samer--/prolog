:- module(machines, [ iterate/4, unfold/2, unfold_finite/2, scanner/3, scan0/4, (**)//2
                    , (>>)/3, iterator/3, unfolder/3, mapper/3, moore/5, progress/2, subsample/3, drop/3]).

:- use_module(library(dcg_progress), [seqmap_with_progress//3]).
:- use_module(library(lazy),         [lazy_unfold/4, lazy_unfold_finite/4]).

% running machines
:- meta_predicate unfold(1,-), unfold_finite(1,-), iterate(1,?,+,-).
unfold(MakeMachine, Stream) :- call(MakeMachine,unfolder(T,S)), lazy_unfold(T,Stream,S,_).
unfold_finite(MakeMachine, Stream) :- call(MakeMachine,unfolder(T,S)), lazy_unfold_finite(T,Stream,S,_).
iterate(Setup, LPs) --> {call(Setup, Step)}, seqmap_with_progress(1,Step,LPs).

% bulding unfolding predicates
:- meta_predicate scanner(2,1,-), scan(2,3,-,+,-), scan0(2,?,?,?), >>(1,2,-).
scanner(Sel, Setup, machines:scan(Sel, Step)) :- call(Setup,Step).
scan(Sel,Trans,X,P1,P2) :- call(Trans,LP,P1,P2), call(Sel,t(LP,P1,P2),X).
scan0(Trans,S1,S1,S2)   :- call(Trans,S1,S2).

%% >>(+P:pred(-A), +Q:pred(+A,-B), -Y:B) is det.
% machine composition: generator >> transducer --> generator
>>(U,T,M) :- call(U, Unfolder), call(T, Unfolder, M).

% some predicates for building machings
:- meta_predicate unfolder(3,?,-), mapper(2,+,-), moore(3,2,?,+,-), iterator(1,+,-).

%% unfolder(+T:pred(-X,+S,-S), S0:S, -G:unfolder(X,S)) is det.
unfolder(T,S,unfolder(T,S)).

%% mapper(+F:pred(+X,-Y), +G:unfolder(X,S), -T:unfolder(Y,S)) is det.
mapper(F, unfolder(TA,SA), unfolder(machines:map_step(TA,F), SA)).
map_step(T,F,Y) --> call(T,X), {call(F,X,Y)}.

%% moore(+T:pred(+X,+SB,-SB), +O:pred(+SB,-Y), S0:SB, +G:unfolder(X,SA), -G2:unfolder(Y,pair(SA-SB))) is det.
moore(TB,OB,SB, unfolder(TA,SA), unfolder(machines:moore_step(TA,TB,OB), SA-SB)).
moore_step(TA,TB,OB, Out, SA1-SB1, SA2-SB2) :- call(TA,OA,SA1,SA2), call(TB,OA,SB1,SB2), call(OB,SB2,Out).

%% iterator(+I:pred(-pred(-X,+S,-S)), S0:S, -G:unfolder(pair(X,S),S)) is det.
iterator(Setup, S0, unfolder(machines:it_step(Step),S0)) :- call(Setup,Step).
it_step(Step,X-S1,S1,S2) :- call(Step,X,S1,S2).

%% drop(+N:natural +G:unfolder(X,S), -T:unfolder(X,S)) is det.
drop(N, unfolder(T,S0), unfolder(T,S1)) :- length(X,N), foldl(T,X,S0,S1).

subsample(N, unfolder(T,S0), unfolder(skip(N,T),S0)).
skip(N,T,X,S1,S2) :- length([X|Y],N), foldl(T,[X|Y],S1,S2).

progress(M1,M2) :- moore(prog_step,snd,0-_,M1,M2).
prog_step(X,I-_,J-X) :- J is I+1, format('~d: ~w\n',[J,X]).

**(G,N) --> {var(N)} -> rep_var(N,G); rep_nonvar(N,G).
rep_nonvar(N,G) --> {N=<0} -> []; {M is N-1}, call_dcg(G), rep_nonvar(M,G).
rep_var(N,G) --> {N=0}; rep_var(M,G), call_dcg(G), {N is M+1}.
