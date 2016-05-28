:- module(timedcg,
	[  now//1
	,	sync//0
	,	wait//0
	,	wait//1
	,	adv//1
	,	cue//1
	,	cue//2
	,	run_cued//2
	,	run_cued//3
	,	run_cued/2
	,	run_cued/3
	,	sleep_till/1
	]).

/** <module> Time DCG

Some of the predicates below are designed to operate in a DCG where
the state represents a time as a number of seconds since a particular
epoch (see get_time/1). The enables the write of code in a sort
of temporal modal logic form, where predicates can implicitly get
at the current time (using dcg_core:get//1) or move to other times by
chaging the DCG state.

*/

:- meta_predicate run_cued(+,//,+,-), run_cued(+,+,//,+,-).
:- meta_predicate run_cued(+,//), run_cued(+,+,//).

:- use_module(library(utils)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).


% -------------------------------- Time DCG -------------------------------

%% now(-T:time)// is det.
%
%  Unifies T with the current real time as returned by get_time/1.
now(T,S,S) :- get_time(T).

%% wait// is semidet.
%% wait(+Pre:nonneg)// is semidet.
%  Waits (blocks) until the real world time catches up with the
%  current DCG time. If Pre is given, then wait//1 sleeps until
%  Pre seconds before the current DCG time.
wait(T,T) :- sleep_till(T).
wait(Pre,T,T) :- T1 is T-Pre, sleep_till(T1).

%% sleep_till(+T:time) is semidet.
%  Compares T with the current real time (get_time/1); if T is
%  in the past, then fail, otherwise, blocks until approximately
%  T. Accuracy of timing cannot be guaranteed.
sleep_till(T) :- 
	get_time(T0), T@>T0, DT is T-T0, sleep(DT).

%% sync// is det.
%  Runs in time DCG and sets the current time to the current real time as returned
%  by now//1 and get_time/1.
sync --> now(T), set(T).

%% adv(+T:nonneg)// is det.
%  Advances current time by T seconds. Runs in time DCG.
adv(DT)  --> trans(T1,T2), {T2 is T1+DT}.

%% quant(+Q:nonneg)// is det.
%  Advances current time to the next multiple of Q.
quant(Q) --> trans(T1,T2), {T2 is Q*ceil(T1/Q)}.

%% cue(+D:nonneg)// is det.
%  Sets the current DCG time to D seconds after current real time.
cue(D)   --> now(T), set(T), adv(D).

%% cue(+D:nonneg,+Q:nonneg)// is det.
%  Sets the current DCG time to next multiple of Q at least
%  D seconds after current real time.
cue(D,Q) --> now(T), set(T), adv(D), quant(Q).

cue(D,Q,O) --> now(T), set(T), adv(D), quant(Q), adv(O).
lag(L)    --> now(T0), get(T), {L is T-T0}.


%% run_cued( +DT:nonneg, +Q:nonneg, +Cmd:phrase((time,S)), ?S1:S, ?S2:S) is nondet.
%% run_cued( +DT:nonneg, +Cmd:phrase((time,S)), ?S1:S, ?S2:S) is nondet.
%% run_cued( +DT:nonneg, +Q:nonneg, +Cmd:phrase(time)) is nondet.
%% run_cued( +DT:nonneg, +Cmd:phrase(time)) is nondet.
%
%  Run command Cmd in DT seconds. Cmd must operate in (time,S) DCG and is called
%  with the current real time plus DT seconds. If Q is supplied, the time
%  passed to Cmd is quantised upwards in units of Q seconds.
run_cued(DT,Cmd) --> run_left((\< cue(DT), call_dcg(Cmd)),_,_).
run_cued(DT,Q,Cmd) --> run_left((\< cue(DT,Q), call_dcg(Cmd)),_,_).
run_cued(DT,Cmd) :- call_dcg((cue(DT), call_dcg(Cmd)),_,_).
run_cued(DT,Q,Cmd) :- call_dcg((cue(DT,Q), call_dcg(Cmd)),_,_).

