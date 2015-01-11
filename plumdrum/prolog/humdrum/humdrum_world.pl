:- module(humdrum_world, [	assert_humdrum/3, retract_humdrum/1 ]).

:- use_module(library(dcg_core)).
%:- use_module(library(utils)).
%:- use_module(library(argutils)).
:- use_module(library(humdrum)).
%:- use_module(library(data/env)).

humdrum_predicates(
		[	spine/4          % spine( xinterp, spine, record, record). 
		,	ref/3            % ref(refcode, lang, text).
		,	duration/1       % duration( duration).
		,	num_spines/1     % num_spines( natural).
		,	num_records/1    % num_records( natural).

		,	time/2           % time( time, record).
		,	duration/2       % duration( duration, record).
		,	new_spine/2      % new_spine( spine, record).
		,	init_spine/3     % init_spine( xinterp, spine, record).
		,	change_spine/5   % change_spine( xinterp, xinterp, spine, spine, record).
		,	term_spine/3     % term_spine( xinterp, spine, record).
		,	join_spines/4    % join_spines( spine, spine, spine, record).
		,	split_spines/4   % split_spines( spine, spine, spine, record).

		,	interp/3         % interp( interp, spine, record).
		,	data/3           % data( data, spine, record).
	]).

:- use_module(library(apply_macros)).
:- use_module(library(dcg_macros)).

%% retract_humdrum(+Module) is det.
%
%  Reverses the action of assert_humdrum/3, abolishing all the predicates
%  contained in it.
retract_humdrum(Mod) :-
	humdrum_predicates(Preds),
	Mod:maplist(abolish,Preds).


%% assert_humdrum( +BaseModule, +HumdrumObject, +NewModule) is det.
%
%  The procedure takes a Humdrum object as returned by hum_read/3 and creates
%  a predicate-based representation of it in a new dynamically created module
%  with the given name NewModule. Before loading, the module specified by BaseModule
%  (as you would supply to use_module/1) is loaded into NewModule.
%  The process can be reversed using retract_humdrum/1.
%
%  The newly created module contains a number of predicates which can be used
%  to access information in the Humdrum object. They can all be used with any
%  instantiation pattern. The predicates are:
%
%  $ duration( D:rational) :
%    The total duration of the Humdrum object
%  $ num_spines( N:natural) :
%    The total number of spines used.
%  $ num_records( N:natural) :
%    The number of records in the obect
%  $ ref( R:refcode, L:lang, T:text) :
%    Table of reference comments, with a standard code, language identifier, and comment text.
%  $ time( T:rational, R:natural) :
%    The time at the beginning of record R.
%  $ data( D, S:spine, R:record) :
%    Indicates that spine S contains data D at record R. Format depends on xinterp of S.
%  $ spine( X:xinterp, S:spine, R1:natural, R2:natural) :
%    Table of spines, their exclusive interpretation. R1 and R2 give the start and end records
%    of the spine.
%  $ duration( -D:rational, +R:natural) :
%    The duration of record R.
%  $ new_spine( -S:spine, -R:natural) :
%    Indicates a new spine S is created at record R.
%  $ init_spine( X:xinterp, S:spine, R:record) :
%    Indicates that a new spine is given its exclusive interpretation at record R.
%  $ change_spine( xinterp, xinterp, spine, spine, record) :
%    I don't know what this is.
%  $ term_spine( X:xinterp, S:spine, R:record) :
%    Indicates that spine S terminates are record R.
%  $ join_spines( S1:spine, S2:spine, S3:spine, R:record) :
%    Indicates that spines S1 and S2 join to become spine S2 at record R.
%  $ split_spines( S1:spine, S2:spine, S3:spine, R:record) :
%    Indicates that spine S1 splits to become spines S2 and S3 at record R.
%  $ interp( I:interp, S:spine, R:record) :
%    Indicates that spine S gets an interpretation I at record R.

assert_humdrum(BaseMod,Recs,Mod) :-
	Mod:use_module(BaseMod),
	declare_predicates_in(Mod),
	with_env(
		ins_key(module,Mod) >>
		run_records(Recs,N,R,T) >> 
		module_assert(num_spines(N)) >>
		module_assert(num_records(R)) >>
		module_assert(duration(T))
	),
	T1 is float(T),
	debug(humdrum,'% spines:~w ~15| records:~w ~30| time:~w\n',[N,R,T1]). 
%	format('\n------------------------------\n'),
%	format('~tspines consumed ~20|: ~w.\n',[N]),
%	format(   '~telapsed time ~20|: ~w.\n',[T1]),
%	format(   '~tnumber of records ~20|: ~w.\n',[R]).


declare_predicates_in(Mod) :-
	humdrum_predicates(Preds),
	Mod:maplist(dynamic,Preds).

run_records(Recs,N,R,T) -->
	ins_keys(  
		[	(time,0) 
		,	(timebase,none)
		,	(tied,[]), (pending,[])
		,	(numspines,0)
		,	(spines,[]), (records,0)
		]),

	seqmap(count(exec),Recs),

	sel_keys( 
		[	(numspines,N) 
		,	(pending,_),	(tied,_)
		,	(timebase,_),	(time,T)
		,	(spines,_), (records,R)
		]).

count(P,X,S1,S3) :- 
   with_key(records,succ,S1,S2), 
   catch( call(P,X,S2,S3), Ex,
      (  get_key(records,N,S2,_),
         throw(level2_parse_error(N,Ex))
      )).

prolog:message(level2_parse_error(Line,Ex)) -->
   ['Level 2 parse error on record ~d'-[Line], nl],
   prolog:message(Ex).

exec(comment(_))  --> [].
exec(comments(_)) --> [].

exec(ref(Prop,Lang,Val)) --> module_assert(ref(Prop,Lang,Val)).

exec(xinterps(X)) --> 
	get_key(spines,R1),
	apply_xinterps(X,x_pathop,R1,R2), 
	set_key(spines,R2).

exec(pathops(X))  --> 
	get_key(spines,R1),
	apply_pathops(X,x_pathop,R1,R2), 
	set_key(spines,R2).

exec(interps(X))  --> 
	get_key(spines,S1),
	(	{X=[X1|_]}, global_interp(X1,X), !
	;	seqmap(local_interp,X,S1,S2),
		set_key(spines,S2)
	).

exec(data(X))     --> 
	get_key(spines,Spines),
	get_key(time,T),
	record_assert(time(T)),
	seqmap(assert_spine_data,Spines,X),

	(	key_val( timebase, delta(DT)) -> nop
	;	with_key( pending, delta_time(Spines,X,DT)),
		record_assert(duration(DT))
	),
	with_key( time, add(DT)).


delta_time(Spines,Events,DT,P1,P2) :-
	maplist(spine_rep,Spines,Reps),
	slice_duration(Reps,Events,DT,P1,P2).

global_interp(tb(TB),All)       --> 
	{	maplist(=(tb(TB)),All) -> true
	;	throw(humdrum_semantics(timebase_mismatch(All))) 
	},	
	{recip_to_rational(TB,DT)}, 
	set_key(timebase,delta(DT)).

local_interp(Interp,S,S) --> spine_assert(S,interp(Interp)).

module_assert(Fact) -->
	get_key(module,Mod),
	{ assert(Mod:Fact) }.

record_assert(Fact) -->
	get_key(records,R),
	{add_arg(R,Fact,Fact1)},
	module_assert(Fact1).

spine_assert(sp(N,_,_),Fact) -->
	{add_arg(N,Fact,Fact1)},
	record_assert(Fact1).
	
assert_spine_data(S,X) --> spine_assert(S,data(X)).
assert_spine_rep(S) --> { spine_rep(S,R) }, spine_assert(S,xinterp(R)).


% ------------------------------------------------


next_spine(N) --> with_key(numspines, (succ, get(N))).

spine_rep(sp(_,R,_),R).

% path op token to spine operation mapping
x_pathop( new(sp(N,null,null))) --> 
	next_spine(N), 
	record_assert(new_spine(N)).

x_pathop( init(R,sp(N,R,I))) --> 
	next_spine(N), 
	get_key(records,I), 
	record_assert(init_spine(R,N)).

x_pathop( term(sp(N,R,I)))   --> 
	get_key(records,J), {succ(JJ,J)}, 
	record_assert(term_spine(R,N)),
	(	{R\=null}
	->	module_assert(spine(R,N,I,JJ))
	;	nop).

x_pathop( chrep(R,sp(N,R0,I),sp(M,R,J))) --> 
	next_spine(M),
	get_key(records,J), {succ(JJ,J)},
	record_assert(change_spine(R0,R,N,M)),
	(	{R0\=null} 
	-> module_assert(spine(R0,N,I,JJ))
	;	nop).

x_pathop( join(sp(N1,R,I1),sp(N2,R,I2),sp(M,R,J))) --> 
	next_spine(M), 
	get_key(records,J), {succ(JJ,J)},
	record_assert(join_spines(N1,N2,M)),
	(	{R\=null}
	->	module_assert(spine(R,N1,I1,JJ)),
		module_assert(spine(R,N2,I2,JJ))
	;	nop).

x_pathop( split(sp(N,R,I),sp(M1,R,J),sp(M2,R,J))) --> 
	next_spine(M1), next_spine(M2), 
	get_key(records,J), {succ(JJ,J)},
	record_assert(split_spines(N,M1,M2)),
	(	{R\=null}
	->	module_assert(spine(R,N,I,JJ))
	;	nop).

add(_,none,none) :- !.
add(X,Y,Z) :- Z is Y+X.

