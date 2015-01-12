:- module(humdrum, [ 
		hum_read/2			% Load a Humdrum file to list of records
	,	hum_read/3			% with selectable encoding
	,	hum_has/3			% Reference information for list of records
	,	hum_prop_desc/2   % Description of reference property

	,	apply_xinterps/6  % stateful
	,	apply_pathops/6	% stateful version
	,	event_duration/3
	,	beats_to_secs/3
	,	recip_to_rational/2
	,	slice_duration/5
	]).

:- meta_predicate apply_pathops(+,3,?,?,?,?).
:- meta_predicate apply_xinterps(+,3,?,?,?,?).

/** <module> Humdrum file format reading

This module provides facilities for reading and decoding Humdrum files.
It provides an extensible framework whereby new exclusive interpretation types
can be added by adding clauses to the multifile predicates hum_data_hook//2,
hum_interp_hook//1 and hum_duration_hook/3.

Types:
==

record --> ref(atom,atom,atom)
         ; comment(atom)
         ; comment(list(atom))
         ; xinterps(list(interp))
         ; interps(list(tandem))
         ; pathops(list(pathop))
         ; data(list(data))
         .

interp ---> null ; x(xinterp) ; t(list(code)) ; p(pathop).

pathop ---> null  ; term ; new 
          ; split ; join ; exch 
          ; init(xinterp) 
          ; chx(xinterp).

spine_ed(A) == pred( path_action(A), S, S).

path_action(A) ---> term(A)
                  ; split(A,A,A)
                  ; join(A,A,A)
                  ; new(A)
                  ; init(xinterp,A)
                  ; chx(xinterp,A)
                  .
==

*/

:- dynamic current_humdrum_file/3.
:- multifile 
		hum_interp_hook//1
	,	hum_data_hook//2
	,	hum_duration_hook/3
	.

:- use_module(library(dcg_core)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_macros)).
:- use_module(library(fileutils)).
:- use_module(library(humdrum/humutils)).
:- use_module(library(humdrum/refcodes)).
:- use_module(library(humdrum/interps)).
:- use_module(library(humdrum/reps)).
:- use_module(library(apply_macros)).

:- set_prolog_flag(double_quotes,codes).

%% hum_interp_hook(-I:interp)// is det.
%
%  This should be a DCG predicate that parses characters from a
%  Humdrum token that signify an interpretation and returns a
%  term encoding the interpretaiton. See interps.pl


%% hum_data_hook(+Rep:xinterp, -D:data)// is det.
%
%  This should be a DCG predicate that parses characters from a
%  Humdrum data token.

%% hum_duration_hook(+Rep:xinterp, +D:data, -Dur:rational) is semidet.
%
%  This should compute the duration of the data term if it has one
%  or fail otherwise.

%% event_duration( +Rep:rep, +Token, -Dur:number) is semidet.
%
%  Compute duration of token, fail if it has no duration.

event_duration(Rep,Data,Dur) :- hum_duration_hook(Rep,Data,Dur).

%% hum_read(+FileName,-HumdrumObject) is semidet.
%% hum_read(+FileName,+Encoding,-HumdrumObject) is semidet.
%
%  Read a humdrum file and return a big term representing its contents.
%  Default encoding is iso_latin_1. If you are having problems reading
%  a Humdrum file, it might be an encoding problem - try utf8 instead.
%  See encoding/1 for more information.
hum_read(File,Records2) :- hum_read(File,iso_latin_1,Records2).
hum_read(File,Enc,Records2) :-
	with_stream(Str, open(File,read,Str,[encoding(Enc)]), read_stream_to_codes(Str,Codes)),
	catch((
			humdrum(Records1,0-Codes,NumLines-[]), !, % level 1 parse
			debug(humdrum,'% Read ~w lines from ~w.\n',[NumLines,File]), 
			seqmap(nofail(count(humdrum)),Records1,Records2, 0-[], _-[]), !
		), % level 2 parse
		Ex, throw(error_in_file(File,Ex))
	).


nofail(P,A,B,C,D) :-
	(	catch(call(P,A,B,C,D),Ex,throw(exception(Ex,call(P,A,B,C,D)))) -> true
	;	throw(failed(call(P,A,B,C,D)))
	).

%% hum_has(+Records,?Prop,?Val) is nondet.
%
%  Searches reference records of all loaded humdrum files.
hum_has(Recs,Prop,Val) :- member(ref(Prop,_,Val),Recs).

%% hum_prop_desc( ?RefCode, ?Description) is nondet.
%
%  Enumerate all known three letter reference codes and their textual descriptions.
hum_prop_desc(Prop,Desc) :- refcode(Prop,_,Desc).



count(P) --> \> P, \< succ.
count(P,X) --> \> call(P,X), \< succ.
count(P,X,Y) --> \> call(P,X,Y), \< succ.

% --------------- humdrum file --------------------------------------------

% Level 1 parse - transforms list of charactes to list of records
% which are terms, one of:
%    comment(list(char))        ~ a single string
%    comments(list(list(char))) ~ one string per spine
%    interps(list(list(char)))  ~ interpretations, one per spine 
%    data(list(list(char)))     ~ data tokens, one per spine
%
% The top level rules assume that the DCG state is (LineNum,DLState)
% where DLState the usual DCG difference list state thingy. It
% pushes a new variable into the state to keep track of the current
% number of spines, which must go from zero to zero.
humdrum(Records) --> records(Records).

eol([],[]).

% read records while keeping track of the line number
% in the dcg state: (LineNumber, DLState)
records([L1|LX]) --> \> (rec(L1), cr), !, \< succ, records(LX).
records([L1])    --> \> (rec(L1), eol), !, \< succ, 
	{print_message(warning,humdrum_syntax(end_of_file))}.

records([])      --> \> eol.
records(_)       --> 
	\< (succ,get(LineNumber)), 
	\> charsex(text,[],"\n",Codes),
	{ throw(humdrum_syntax(unknown(LineNumber,Codes))) }.

rec(comment(A))  --> peek("!!"), !, global_comment(A).
rec(comments(A)) --> peek("!"),  !, for_spines(local_comment,A).
rec(interps(A))  --> peek("*"),  !, for_spines(interp,A).
rec(data(A))     --> for_spines(token,A).

for_spines(P,[A|AX]) --> call(P,A), for_spines1(P,AX).
for_spines1(P,[A|AX]) --> tb, !, call(P,A), for_spines1(P,AX).
for_spines1(_,[]) --> [].

global_comment(A)  --> "!!", charsex(text,[],"\n",A).
local_comment(A)   --> "!", charsex(text,"!","\t\n",A).

token(null)   --> ".".
token(A)      --> {A=[_|_]},   charsex(graph,"!*."," \t\n",A).
token(s(A))   --> {A=[_,_|_]}, seqmap_with_sep(" ",subtoken,A).
subtoken(A)   --> {A=[_|_]},   charsex(graph,"!*"," \t\n",A).
interp(A)     --> "*", int1(A).

int1(x(A))  --> "*", !, charsex(graph,[],"\t\n",Codes), {atom_codes(A,Codes)}.
int1(t(A))  --> {A=[_|_]}, charsex(text,"*+-v^x","\t\n",A).
int1(p(A))  --> [C],{str_pathop([C],A)}.
int1(null)  --> charsex(graph,"*","\t\n",[]).

str_pathop("-",term).
str_pathop("+",new).
str_pathop("^",split).
str_pathop("x",exch).
str_pathop("v",join).


% -----------------------------------------------------------------------
% Level 2 parse - transforms list of records to list of parsed records.
%
% The parse checks that the spine structure is properly adhered to
% and fails if the wrong number of spines is encountered at any point.


humdrum(comment(A),  ref(Prp,Lng,Vl)) --> {phrase(ref(Prp,Lng,Codes),A)}, !, {atom_codes(Vl,Codes)}.
humdrum(comment(A),  comment(B))  --> {atom_codes(B,A)}.
humdrum(comments(A), comments(B)) --> map_spines(atom_codes,B,A).
humdrum(data(A),     data(B))     --> once(map_spines_with_reps(data,A,B)).
humdrum(interps(A),  interps(B))  --> {maplist(tandem,A)}, !, map_spines_with_reps(tinterp,A,B). 
humdrum(interps(A),  xinterps(A)) --> {memberchk(x(_),A)}, !, apply_xinterps(A).
humdrum(interps(A),  pathops(B))  --> {memberchk(p(_),A)}, !, apply_pathops(A,B).

	

% ............... reference comments .............................
ref(Code,Lang,Text) -->  
	"!", !, charsex(text,[],":@",CC), lang(Lang),
	":", charsex(space,[],[],_), charsex(text," \t",[],Text),
	{atom_codes(Code1,CC), upcase_atom(Code1,Code)}.

% NB. according to the spec, language codes should have 3 characters, but
% in practice, many seem to have only two.
lang(def) --> [].
lang(pri-Lang) --> "@@", charsex(text,[],":@",L), {atom_codes(Lang,L)}.
lang(sec-Lang) --> "@", charsex(text,[],":@",L), {atom_codes(Lang,L)}.
% ............... interpretations .......................

% check that interpretation is null or tandem only
tandem(null).
tandem(t(_)).

% check that interpreation is null or path op only
% and convert to stripped path op
pathop(null,null).
pathop(p(Op),Op).

% ............... spines and paths and x interps .......................

% apply exclusive interpretations to current spine list
apply_xinterps(Ints,SX1,SX2)   :- apply_xinterps(Ints,run_pathop,SX1,SX2,[],[]).

% apply path operation interpretations to current spine list
% and create list of path-ops for later use.
apply_pathops(Ints,Ops,L1,L2) :- 
	map_spines(pathop,Ints,Ops,L1,L1), 
	apply_pathops(Ops,run_pathop,L1,L2,[],[]).


% run path operation with state X.
run_pathop( term(_),      X, X).
run_pathop( new(null(_)), X, X).
run_pathop( split(R,R,R), X, X).
run_pathop( join(R,R,R),  X, X).
run_pathop( init(R,R),    X, X).
run_pathop( chx(R,_,R), X, X).

% ............... data tokens .............................


data( _, null, null) :- !.
data( null(_), _, _) :- !, throw(humdrum_semantics(no_xinter)).
data( Rep, s(ST), sub(Terms)) :- maplist(data1(Rep),ST,Terms), !.
data( Rep, s(ST), unknown(Rep,sub(ST))) :- !.
data( Rep, Codes, tok(Term))  :- data1(Rep,Codes,Term), !.
data( Rep, Codes, unknown(Rep,Codes)).

data1( _, Codes, bar(Sigs)) :- peek("=",Codes,_), !, bar(Sigs,Codes,[]).
data1( Rep, Codes, Term) :- hum_data_hook(Rep,Term,Codes,[]).

% ............... tandem interpretations .............................

tinterp( _, null, null) :- !.
tinterp( Rep, t(Codes), Term) :- atom(Rep), tinterp(Codes,Term).

tinterp( Codes, Term) :- hum_interp_hook(Term,Codes,[]), !.
tinterp( Codes, unknown(Codes)).

map_spines_with_reps(P,A,B,SX,SX) :- maplist(P,SX,A,B).
map_spines(P,A,B,S,S) :- length(S,N), length(A,N), maplist(P,A,B).

% --------------- spine operations --------------------------

%% apply_xinterps( +I:list(interp), +E:spine_ed(A), ?S1:list(A), ?S2:list(A))// is det.
%
%  Apply list of exclusive interpretations to list of spines using
%  give spine editor interpreter predicate.

apply_xinterps(Ints,P,[],SX)   --> !, seqmap(init_rep(P),Ints,SX).
apply_xinterps(Ints,P,SX1,SX2) --> !, seqmap(apply_rep(P),Ints,SX1,SX2).

init_rep(P,x(R),S) --> call(P,init(R,S)).
apply_rep(P,x(R),S1,S2) --> call(P,chx(R,S1,S2)).
apply_rep(_,null,S,S) --> [].

%% apply_pathops( +I:list(pathop), +E:spine_ed(A), ?S1:list(A), ?S2:list(A))// is det.
%
%  Apply list of path ops to a list of spines using given spine editor.
apply_pathops(Ops,Interp,L1,L2) --> once(edit_spines(Interp,no,Ops,L1,L2)).

% --------------- paths ----------------------------------------------

% edit_spines(+E:spine_ed(A), +Ops:list(pathop(A)),+In:list(A),-Out:list(A))// is det.
%
% Relates the spine configurations before and after a path operation.
%

% exch can exchange non-adjactent spines
% join can join multiple adjacent spines
% State indicates the state of the current exchange operation if there is one,
% and can be one of
% * no - There is no exchange operation and one can occur.
% * S1/S2 - spine S1 is being exchanged with S2 when it is found.
% * ex - An exchange has happened and no more can occur.

%:- index(edit_spines(0,1,1,0,0,0,0)).

edit_spines(_, no, [],[],[]) --> [].
edit_spines(_, ex, [],[],[]) --> [].

edit_spines(P, Ex, [null |OX], [S|In], [S|Out])     --> edit_spines(P,Ex,OX,In,Out).
edit_spines(P, Ex, [term |OX], [S|In], Out)         --> call(P,term(S)), edit_spines(P,Ex,OX,In,Out).
edit_spines(P, Ex, [split|OX], [S|In], [S1,S2|Out]) --> call(P,split(S,S1,S2)), edit_spines(P,Ex,OX,In,Out).
edit_spines(P, Ex, [new  |OX], [S|In], [S,T|Out])   --> call(P,new(T)), edit_spines(P,Ex,OX,In,Out).

edit_spines(P, S/T,[exch       |OX], [T|In],   [S|Out])   --> edit_spines(P,ex,OX,In,Out).
edit_spines(P, no, [exch, exch |OX], [S,T|In], [T,S|Out]) --> !, edit_spines(P,ex,OX,In,Out).
edit_spines(P, no, [exch, Op   |OX], [S,U|In], [T,V|Out]) --> edit_spines(P,S/T,[Op|OX],[U|In],[V|Out]).

edit_spines(_, no, [join],[S1],[S1]) --> []. 
edit_spines(_, ex, [join],[S1],[S1]) --> []. 
edit_spines(P, Ex, [join,join|OX],[S,T|In],[U|Out]) --> !, call(P,join(S,T,V)),
	edit_spines(P,Ex,[join|OX],[V|In],[U|Out]).
edit_spines(P, Ex, [join,Op  |OX],[S|In],[S|Out]) --> edit_spines(P,Ex,[Op|OX],In,Out).



% ----------------- DURATIONS --------------------


%% beats_to_secs( +T:tempo, +B:number, -S:float) is det.
%
%  Convert beats to seconds given tempo in beats-per-minute.
beats_to_secs(Tempo,Beats,Secs) :- Secs is 60*Beats/Tempo.

%% slice_duration( +Reps:list(xinterp), +Evs:list(data), -DT:rational, +P1:pending, -P2:pending) is det.
%
%  Computes the duration of a time slice given the representation (xinterp) of each
%  spine and the data in each spine. The last two arguments maintain a list of pending
%  durations of notes begun in earlier slices. NB =|pending = list(rational)|=.
slice_duration(Reps,Events,DT1,P1,P2) :-
	events_durations(Reps,Events,Durations),
	append(Durations,P1,P1a),
	minlist(P1a,none,DT),
	(	DT=none -> P2=P1, DT1=0
	;	filter_durations(DT,P1a,P2), DT1=DT
	).
	
events_durations([],[],[]).
events_durations([R|RX],[E|EX],DX1) :-
	(event_duration(R,E,Dur) -> DX1=[Dur|DX] ; DX1=DX),
	events_durations(RX,EX,DX).

minlist([],T,T).
minlist([none|XT],T1,T3) :- !, minlist(XT,T1,T3).
minlist([X|XT],none,T) :- !, minlist(XT,X,T).
minlist([X|XT],T1,T3) :- (X<T1 -> T2=X; T2=T1), minlist(XT,T2,T3).

filter_durations(_,[],[]).
filter_durations(DT,[D1|DX1],DDX2) :-
	D2 is D1-DT,
	(D2>0 -> DDX2=[D2|DX2]; DDX2=DX2),
	filter_durations(DT,DX1,DX2).



% ------------------ MESSAGES --------------------

prolog:message(humdrum_syntax(dot_null_interp)) -->
	[ 'Use of "." as null interpretation - use "*" instead.'-[], nl].

prolog:message(humdrum_syntax(end_of_file)) -->
	[ 'Last line of Humdrum file not properly terminated.'-[], nl].

prolog:message(humdrum_syntax(unknown(LineNum,Codes))) -->
	[ 'Humdrum syntax error on line ~w:'-[LineNum], nl, '~s'-[Codes], nl].

prolog:message(error_in_file(File,Error)) -->
	[ 'Error in file ~w'-[File], nl],
	prolog:message(Error).

prolog:message(failed(Goal)) -->
	[ 'Goal not allowed to fail: ~q'-[Goal], nl].

prolog:message(exception(Ex,Goal)) -->
	[ 'While calling ~q'-[Goal], nl],
	prolog:message(Ex).
