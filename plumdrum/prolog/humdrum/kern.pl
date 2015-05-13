:- module(kern,
		[	kern_duration/2
		,	kern_pitch/2
		,	kern_rest/1
		]).

/** <module> Kern spine format for Humdrum objects

   This defines hooks into the humdrum module to enable it to parse
   Kern format spines. The predicates exported are hum_data_hook//2
   and hum_duration_hook/3.
*/

:- use_module(library(dcg_core)).
:- use_module(library(dcg_macros)).
:- use_module(library(humdrum)).
:- use_module(library(humdrum/humutils)).
:- use_module(library(apply_macros)).

:- set_prolog_flag(double_quotes,codes).

% hooks to enable humdrum module to interpret kern data
humdrum:hum_data_hook(kern,Sigs) --> !, seqmap(kern,Sigs).

humdrum:hum_duration_hook(kern,tok(E),D) :- !, kern_duration(E,D).
humdrum:hum_duration_hook(kern,sub(EX),D) :- !,
	(	maplist(dur_token(D),EX) -> true
	;	throw(kern_semantics(subtoken_duration_mismatch(EX)))
	).

dur_token(Dur,Token) :- (kern_duration(Token,Dur) -> true; Dur=none).

%% kern_duration( +S:data, -D: rational) is det.
%  Determines the duration of a given Kern data term.
kern_duration(S,0) :- member(grace(Gr),S), (Gr=acciaccatura;Gr=appoggiatura), !.
kern_duration(S,R) :- member(recip(D),S), !, recip_to_rational(D,R).
kern_duration(_,0).

%% kern_pitch( +S:data, -P:pitch) is nondet.
%  True when P is one of the pitches contained in a Kern data term.
kern_pitch(Sigs,Pitch) :- member(pitch(Note,Oct),Sigs), !, oct_note_pitch(Oct,Note,Pitch).

%% kern_rest( +S:data) is nondet.
%  True when Kern data term S signifies a rest.
kern_rest(Sigs) :- memberchk(rest,Sigs).

oct_note_pitch(4,Note,Note)      :- !.
oct_note_pitch(5,Note,Note*oct)  :- !.
oct_note_pitch(3,Note,Note/oct)  :- !.
oct_note_pitch(O1,Note,Note*(oct^O)) :- O1>5, !, O is O1-4.
oct_note_pitch(O1,Note,Note/(oct^O)) :- O1<3, !, O is 4-O1.

% --------------- signifier and signifier components -----------------

kern(P) --> {kern(P)}, P.

% all the phrase that can occur in a kern data token
kern(recip(_)).
kern(pitch(_,_)).
kern(rest).
kern(articulation(_)).
kern(ornament(_)).
kern(grace(_)).
kern(par(_,_)).
kern(beam(_)).
kern(stem(_)).
kern(editorial).
kern(undefined(_)).

rest --> "r";"rr".

% articulation and phrasing (kern)
par(open,tie)   --> "[".
par(cont,tie)   --> "_".
par(close,tie)  --> "]".
par(open,slur-0)  --> "(".
par(close,slur-0) --> ")".
par(open,phrase-0)   --> "{".
par(close,phrase-0)  --> "}".
par(open,glissando)   --> "H".
par(close,glissando)  --> "h".
par(Op,Type-N) --> peek("&"), rep_shared(N,"&"), par(Op,Type-0).

articulation(staccato)  --> "'".
articulation(spiccato)  --> "s".
articulation(pizzicato) --> "\"".
articulation(attacca)   --> "`".
articulation(tenuto)    --> "~".
articulation(accent)    --> "^".
articulation(generic)   --> "I".
articulation(harmonic)  --> "o".
articulation(sordino)   --> "U".
articulation(sforzando) --> "z".
articulation(down_bow)  --> "u".
articulation(up_bow)    --> "v".
articulation(arpeggio)  --> ":".
articulation(pause)     --> ";".
articulation(breath)    --> ",".

ornament(turn) --> "S".
ornament(wagnerian_turn) --> "$".
ornament(trill(whole)) --> "T".
ornament(trill(semi)) --> "t".
ornament(mordent(whole)) --> "M".
ornament(mordent(semi)) --> "m".
ornament(inv_mordent(whole)) --> "W".
ornament(inv_mordent(semi)) --> "w".
ornament(ending_turn) --> "R".
ornament(generic) --> "O".


grace(acciaccatura)      --> "q".
grace(appoggiatura)      --> "P".
grace(groupetto)         --> "Q".
grace(post_appoggiatura) --> "p".

/*
	relevant only to score typesetting

	k	partial beam extending leftward
	kk	two partial beams extending leftward
	J	end beam
	JJ	end two beams
	K	partial beam extending rightward
	KK	two partial beams extending rightward
	L	start beam
	LL	start two beams

	/	up-stem
	\	down-stem
*/

beam(B) --> ("kk";"k";"JJ";"J";"KK";"K";"LL";"L")//list(Codes), {atom_codes(B,Codes)}.
stem(S) --> ("/";"\\")//list(Codes), {atom_codes(S,Codes)}.

/*
	x	editorial interpretation; immediately preceding signifier is interpreted
	xx	editorial interpretation; entire data token is interpreted
	X	editorial intervention; immediately preceding signifier is an editorial addition; see also x
	XX	editorial intervention; entire data token is an editorial addition
	y	editorial mark: invisible symbol; unprinted note, rest, or
	Y	editorial mark: sic marking; information is encoded
	?	editorial mark: immediately preceding signifier has footnote in an ensuing comment
	??	editorial mark: entire preceding data token has footnote in an ensuing comment
*/
editorial --> "xx";"x";"y";"XX";"X";"Y";"??";"?".

% !!!RDF**piano: X=hands cross, left over right

undefined(XX) --> [X], {member(X,"VijlNZ@+|<>"), char_code(XX,X)}.
undefined('%') --> "%".


prolog:message(kern_semantics(subtoken_duration_mismatch(Terms))) -->
	{ maplist(dur_token,Durs,Terms) },
	[ 'Subtoken duration mismatch in **kern spine'-[], nl,
		'Subtokens are ~w'-[Terms], nl,
		'Durations are ~w'-[Durs], nl ].
