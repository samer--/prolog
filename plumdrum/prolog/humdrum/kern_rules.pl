:- module(kern_rules,
		[	spine/1     	% spine( spine).
		,	spine/2     	% spine( spine, record).
		,	next_spine/2	% next_spine( spine, spine).
		,	colinear/2  	% colinear( spine, spine).
		,	fwd/2        	% fwd( position, position).

		,	all_spines/2	% all_spines( pred(spine,record), record).
		,	xinterp/3   	% xinterp( xinterp, spine, record).
		,	interp/2     	% interp( spine, record).
		,	tempo/2   		% tempo( nonneg, record).
		,	barlines/2  	% barlines( bar_attr, record).
		,	barline/3   	% barline( bar_attr, spine, record).
		,	barline/2   	% barline( spine, record).
      ,  articulation/3 % articulation( artic, spine, record).
		,	data/2       	% data( spine, record).
		,	note/4      	% note( kern_note, duration, time, spine).
		,	kern_note/5 	% kern_note( kern_note, duration, tie, spine, record).

		% not dependent on db module
		,	data_token/2
		,	metre_to_bar_duration/2

		,	time//1  % time( time, position, position).
		,	token//1 % token( token, position, position).
		]).

/** <module> Kern inference rules

	---++ Usage

	This module is designed to be called from a module containing a
	database describing a humdrum object, which may be created from
	a Humdrum file by using assert_humdrum/3 in humdrum_world.pl

	The following predicates must be defined in the calling module.

	==
		spine/4          % spine( xinterp, spine, record, record). 
		ref/3            % ref(refcode, lang, text).
		duration/1       % duration( duration).
		num_spines/1     % num_spines( natural).
		num_records/1    % num_records( natural).

		time/2           % time( time, record).
		duration/2       % duration( duration, record).
		new_spine/2      % new_spine( spine, record).
		init_spine/3     % init_spine( xinterp, spine, record).
		change_spine/5   % change_spine( xinterp, xinterp, spine, spine, record).
		term_spine/3     % term_spine( xinterp, spine, record).
		join_spines/4    % join_spines( spine, spine, spine, record).
		split_spines/4   % split_spines( spine, spine, spine, record).

		interp/3         % interp( interp, spine, record).
		data/3           % data( data, spine, record).
	==


	---++ Performance interpretation 

	dynamics
	*	p, f, mp, mf etc - static level
	*	crescendo, diminuendo - dynamic process
	*	sforzando - loud then quiet
	*	subito - suddenly

	phrasing, compound events

	*	tied notes
	*	slur (open, close)
	*	phrase (open, close)
	*	glissando (open, close)
	*	elision?

	articulation

	*	staccato
	*	spiccato
	*	pizzicato
	*	attacca
	*	tenuto
	*	accent
	*	harmonic
	*	sordino
	*	sforzando
	*	up_bow
	*	down_bow
	*	arpeggio
	*	generic
	*	pause
	*	breath

	ornament(_)

	*	trill(whole)
	*	trill(semi)

			Check date of composition.
			If before 1800, start on pitch above and trill \/\/\/\
			If after 1800, start on notated pitch /\/\/\/
			Speed?

	*	mordent(whole)    	/\___
	*	mordent(semi)			/\___
	*	inv_mordent(whole)	\/---
	*	inv_mordent(semi) 	\/---
		
			Speed?

	*	turn			\\/
	*	wagnerian_turn
	*	ending_turn

	*	generic

	grace(_)

	*	acciaccutura - does not steal time, very short, just before ot on the beat
	*	appoggiatura - steal time from next note (post_appoggiatura?)
	*	groupetto
	*	post_appoggiatura


	---++++ TODO

	-	slurs, glissandi, phrasing (using supercollider?)
	-  grace notes, groupettos etc.
	-  elided phrases and slurs
 	- 	expansion lists
	-	live intervention

	-	trail stack and local stack size growth
*/

:- module_transparent
			spine/1
		,	spine/2
		,	next_spine/2
		,	tempo/2
		,	colinear/2
		,	fwd/2
		,	interp/2
		,	xinterp/3
		,	barlines/2
		,	barline/3
		,	barline/2
      ,  articulation/3
		,	data/2
		,	all_spines/2
		,	note/4
		,	kern_note/5
		,	tied_note//5
		,	next_tied_note/7

		,	time//1
		,	token//1
		.

%:- use_module(library(math)).
:- use_module(library(humdrum/kern)).

%% spine( +S:spine, +R:record) is semidet.
%% spine( -S:spine, -R:record) is nondet.
%
%  True if spine S exists at record R. Enumerates all
%  spines at all records on backtracking.
spine(S,R)     :- xinterp(_,S,R).

%% spine( +S:spine) is semidet.
%% spine( -S:spine) is nondet.
%
%  True if spine S exists anywhere in the current database.
%  Enumerates all spines on backtracking.
spine(S)       :- context_module(M), M:spine(_,S,_,_).


%% all_spines(P:pred(spine,record), +R:record) is semidet.
%% all_spines(P:pred(spine,record), -R:record) is nondet.
%
%  True if P is true for all spines of record R.
all_spines(P,R) :-
	setof(S,call(P,S,R),SX), % find all spines for which P is true at R
	setof(S,spine(S,R),SX).  % make sure this equals all spines at R



%% colinear( +S1:spine, +S2:spine) is semidet.
%% colinear( +S1:spine, -S2:spine) is nondet.
%
%  Succeeds if S2 is either the same spine as S1 or can be
%  reached going forward in time via splits, joins, or
%  representation changes.

colinear(S,S) :- spine(S).

colinear(S1,S2) :-
	spine(S1),
	context_module(M),
	colinear1(M,[S1],S2).

colinear1(M,SX1,S2) :-
	next_spine_set(M,SX1,SX2),
	(	member(S2,SX2)
	;	(	var(S2) -> SX3=SX2
		;	include(after(M,S2),SX2,SX3)
		),
		colinear1(M,SX3,S2)
	).

after(M,S1,S2) :-
	M:spine(_,S1,S1_starts,_),
	M:spine(_,S2,_,S2_ends),
	S1_starts > S2_ends.



%% next_spine( -S1:spine, -S2:spine) is nondet.
%
%  True if spine S1 evolves into S2 without any intervening spines.
next_spine(S1,S2) :- context_module(M), next_spine(M,S1,S2).

next_spine(M,S1,S2) :- M:change_spine(_,_,S1,S2,_).
next_spine(M,S1,S2) :- M:split_spines(S1,S1a,S1b,_), (S2=S1a;S2=S1b).
next_spine(M,S1,S2) :- M:join_spines(S1,_,S2,_); M:join_spines(_,S1,S2,_).

next_spine_set(M,SX1,SX2) :-
	setof(SS, S1^(member(S1,SX1), next_spine(M,S1,SS)), SX2).


%% fwd( +Pos:position, -Pos:position) is nondet.
%
%  Step forwards one record following current spine.
%  ==
%  position ---> (spine,record).
%  ==
fwd((S1,R1),(S2,R2)) :-
	succ(R1,R2),
	spine(S2,R2),
	colinear(S1,S2).



%% xinterp( -X:xinterp, +S:spine, +R:record) is semidet. 
%% xinterp( -X:xinterp, -S:spine, -R:record) is nondet. 
%
%  True if X is the exclusive interpretation for spine S 
%  which must exists at record R.
xinterp(X,S,R) :- context_module(M), M:spine(X,S,I,J), between(I,J,R).


%% interp( -S:spine, -R:record) is nondet.
%
%  True if record R is an interpretation record and S 
%  is a spine that exists at record R.
interp(S,R)  :- context_module(M), M:interp(_,S,R).


%% barline( -B:bar_attr, +S:spine, +R:record) is semidet.
%% barline( -B:bar_attr, -S:spine, -R:record) is nondet.
%
%  True if spine S at record R is a bar line  with attributes B.
barline(B,S,R) :- context_module(M), M:data(tok(bar(B)),S,R).


%% barlines( -B:bar_attr, +R:record) is semidet.
%% barlines( -B:bar_attr, -R:record) is nondet.
%
%  True if all spines at record R are bar lines  with attributes B.
barlines(B,R)  :- all_spines(data(tok(bar(B))),R).


%% articulation( -A:artic, +S:spine, +R:record) is semidet.
%% articulation( -A:artic, -S:spine, -R:record) is nondet.
%
%  True if spine S at record R includes articulation marking A.
articulation(A,S,R) :- 
   context_module(M), 
   M:data(tok(Toks),S,R),
   member(articulation(A), Toks).


%% tempo( -T:nonneg, +R:record) is semidet.
%% tempo( -T:nonneg, -R:record) is nondet.
%
%  True if all spines at record R are metronome interpretations
%  with tempo T beats per second.
tempo(Tempo,R) :- all_spines(interp(metro(Tempo)),R).


%% barline( +S:spine, +R:record) is semidet.
%% barline( -S:spine, -R:record) is nondet.
%
%  True if spine S at record R is a bar line.
barline(S,R) :- barline(_,S,R).


%% data( +S:spine, +R:record) is semidet.
%% data( -S:spine, -R:record) is nondet.
%
%  True if spine S at record R is a data token.
data(S,R)    :- context_module(M), M:data(_,S,R).



%% note( -P:kern_note, -D:duration, -T:time, -S:spine) is nondet.
%
%  True if a note with pitch P and duration D is initiated at
%  time T in spine S. This also applies to tied notes which 
%  may continue in the same spine or a colinear spine. If
%  a tied note continues in a non-colinear spine, a warning is
%  printed.
%
%  @see colinear/2
note(Note,Total_Dur,Time,S) :-
	context_module(M),
	kern_note(Note,Dur,Tie,S,R),
	M:time(Time,R),
	(	Tie=not_tied -> Total_Dur=Dur
	;	Tie=open 
	-> (	tied_note(cont, Note, Time, Dur, S, 0, Total_Dur) -> true
		;	format('* ERROR: tied note (~w) at ~w NOT FOUND.\n',[Note,(S/R)]),
			Total_Dur=Dur
		)
	).


tied_note( close, _, _, Dur, _) --> !, add(Dur).
tied_note( cont, Note, Time, Dur, S) --> 
	% jump ahead exactly Dur in time
	% and look for a colinear data token
	add(Dur),
	{	%context_module(M),
		add(Dur,Time,Time1),
		(	next_tied_note(colinear, Note, Time1, S, S1, Dur1, Tie)
		;	next_tied_note(kern_rules:true, Note, Time1, S, S1, Dur1, Tie)
		), !
	},
	tied_note(Tie,Note,Time1,Dur1,S1).

next_tied_note(Constraint, Note, Time1, S,S1, Dur1, Tie) :-
	context_module(M),
	M:time(Time1,R1), 
	M:data(Data,S1,R1), Data\=tok(bar(_)),
	call(Constraint,S,S1),
	kern_note(Note,Dur1,Tie,S1,R1),
	(Tie=cont;Tie=close).

true(_,_).



%% kern_note( -P:kern_note, -D:duration, -T:tie, -S:spine, -R:record) is nondet.
%
%  True if data token at spine S in record R is a kern note with pitch
%  P and duration D. T indicates whether the note is part of a tie as follows:
%  ==
%  kern_note ---> rest; pitch(pitch).
%
%  tie ---> not_tied   % atomic note, not part of a tie
%         ; open       % onset of tied note
%         ; cont       % continuation of tied note
%         ; close      % final segment of tied note
%         .
%  ==

kern_note(Event,Dur,Tie,S,R) :-
	context_module(M),
	M:data(Data,S,R),
	xinterp(kern,S,R),
	data_token(Data,Tok),
	(	kern_pitch(Tok,Pitch) -> Event=pitch(Pitch)
	;	kern_rest(Tok)        -> Event=rest),
	kern_duration(Tok,Dur),
	kern_tie(Tok,Tie).


kern_tie(Sigs,Tie) :- (member(par(Tie,tie),Sigs) -> true; Tie=not_tied).

%% data_token( +D:data, -T:token) is nondet.
%
%  True when T is a token or subtoken in the given data term.
data_token(tok(T),T) :- !, T\=bar(_).
data_token(sub(TX),T) :- member(T,TX).

%% metre_to_bar_duration( +M:metre, -D:duration) is det.
%
%  Compute duration in whole notes of a bar in the given metre.
metre_to_bar_duration(metre(N,D),BD) :- BD is N rdiv D. 


% (Slice,Record) DCG 

%% time( -Time:rational, +Loc:spine_rec, -Loc:spine_rec) is det.
time(T,(S,R),(S,R)) :- context_module(M), M:time(T,R).

%% token( -Token, +Loc:spine_rec, -Loc:spine_rec) is det.
token(T,(S,R),(S,R)) :- context_module(M), M:data(X,S,R), data_token(X,T).

