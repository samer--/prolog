:- module(memo,
    [ memo/1,    memo/2
    , browse/1,  browse/2
    , clear_all/1,   clear_all/2
    , compute/1, compute/2
    , recompute_all/3
    , (volatile_memo)/1
    , (persistent_memo)/1
    , call_with_mode/2
    , current_mode/1
    , op(1150,fx,volatile_memo)
    , op(1150,fx,persistent_memo)
    , modally/1
    , meta_property/2
    , memo_attach/2
    ]).

/** <module> Memoisation of deterministic predicates

   This module provides the ability to store the results of expensive to run
   deterministic predicates to save recomputation. The memo table can be volatile
   (in memory only) and therefore lost when SWI terminates, or stored persistently
   on disk using the facilities provided by library(persistency).

   Suppose you have a predicate expensive/4 which is deterministic with two inputs 
   and two outputs, eg, its PlDoc comment could look like this:
   ==
   %% expensive(+In1:atom, +In2:int, -Out1:float, -Out2:compound) is det.
   ==
   Then a volatile memo could be declared with the following directive:
   ==
   :- volatile_memo expensive( +atom, +int, -float, -compound).
   ==
   This causes a dynamic predicate expensive/5 to be declared (the extra
   argument is for metadata about the computation that produced the result).
   It also causes the definition of the predicate expensive/4 to be renamed to
   compute_expensive/4, and a new expensive/4 is generated which looks in the
   memo table (expensive/5) to avoid recomputations.

   To declare a persistently memoised predicate, you would use
   ==
   :- persistent_memo expensive( +atom, +int, -float, -compound).
   ==
   This causes a persistent predicate expensive/5 to be declared via
   facilities provided by library(persistency). Otherwise, things look
   the same externally (there are some internal differences).
   It is the caller's responsibility to attach a persistent database.

   ---+++ Argument specifiers

   Each volatile_memo/1 or persistent_memo/1 directive can contain one or more predicate
   declarations, where each argument of the predicate declaration term specifies
   an input or out argument of the predicate to be memoised. The predicate will
   be treated as semi-deterministic in the sense that it will succeed at most
   once for each combination of inputs. Each argument specifier must look like one of these:
      *  +Type  
      An input of type Type.
      *  +Name:Type  
      An input named Name of type Type.
      *  -Type
      An output of type Type.
      *  -Name:Type  
      An output named Name of type Type.

   ---+++ Meta-level interface

   Memoised predicates can be accessed at a 'meta-level' using some
   meta-predicates which you can think of as modal operators.
   
   First of all, each memoised call to the base predicate has some recorded
   meta-data whose type is =|metadata|=, defined below:
   ==
   metadata    == pair(comp_event,result).
   pair(A,B)  ---> A-B.
   result     ---> ok ; fail ; ex(any).
   comp_event ---> comp(hostname,time,duration)
                 ; comp(hostname,duration)
                 ; none.
   hostname == ground.
   time     == float.
   duration == float.
   ==
   Thus, the meta-data can record the computer on which the computation was
   done, the time it was called (as returned by get_time/1), the duration of
   the computation in seconds, and whether or not the computation succeeded, 
   failed, or threw an exception. The fact that failures and exception-throwings
   are recorded means that object level interface (eg expensive/4 in the example
   above) avoids repeating unsuccessful computation as well as successful ones.
   
   The meta-level browse/1 and browse/2 allow you to access this information.
   browse(Goal) is a nondeterministic predicate which unifies Goal with all the
   successful calls in the memo table. browse(Goal,Meta) matches all calls in the
   memo table including unsuccessful ones.

   The meta-level clear_all/1 and clear_all/2 allow you to delete memo table entries.
   The meta-level memo(Goal) is the same as the object level Goal, failing or
   throwing an expection depending on what the underlying predicate does, but
   the meta-level memo(Goal,Meta) catches failures and exceptions, returning the
   meta-data in Meta.

   Finally, the meta-level compute/1 by-passes all the memoisation machinery
   and calls the original underlying predicate directly. This is useful in
   cases where you absolutely need any side-effects caused by the original
   computation. compute/2 is the same except that failures and exceptions are
   reified as meta-data.

   ---+++ Setting the host name

   The meta-data includes the host name of the computer used to do the 
   computations. On a Unix system, this should get picked up from the environment
   automatically, using =|getenv('HOSTNAME',Hostname)|= (see getenv/2) OR,
   if this fails, by calling the shell command 'hostname'.

   TODO:

   - check for duplicate declarations
   - check for missing predicate definitions
*/
:- meta_predicate 
      modally(0),
      call_with_mode(+,0),
      memo(0), memo(0,-), 
      browse(0), browse(0,-), 
      clear_all(0),  clear_all(0,-),
      compute(0), compute(0,-),
      recompute_all(0,-,-).

:- use_module(library(persistency)).
:- use_module(library(typedef)).
:- use_module(library(settings)).
:- use_module(library(sandbox)).

:- multifile memoised/4, asserter/4, retracter/4, computer/4.

:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(back_quotes,codes).

:- type pair(A,B) ---> A-B.
:- type module   == atom.
:- type time     == float.
:- type duration == float.
:- type hostname == ground.
:- type metadata == pair(comp_event,result).
:- type result     ---> ok ; fail ; ex(any).
:- type comp_event ---> comp(hostname,time,duration)
                      ; comp(hostname,duration)
                      ; none.


:- setting(confirmation_style, list(ground), [bold,fg(red)], "Confirmation message style options.").
:- setting(confirmation_threshold, integer, 1, "Maximum entries for silent clear_all deletion.").
:- setting(db_directory, text, '.', "Directory prefix for database files via memo_attach/2").


user:term_expansion(init_hostname,hostname(H)) :-
   (  getenv('HOSTNAME',H) -> true
   ;  setup_call_cleanup(open(pipe(hostname),read,S),
                         read_line_to_codes(S,Codes),
                         close(S)), 
      atom_codes(H,Codes)
   ),
   format("% memo: setting hostname to '~w'.\n",[H]).

:- dynamic hostname/1.
init_hostname.


%% memo_attach(+File:text, +Options:options) is det.
%
%  Convenience wrapper for db_attach/2. Attaches a persistent database file with the
%  given name in the directory specified in the memo:db_directory setting. Options
%  is passed to db_attach/2. If File is an absolute path, then the db_directory setting
%  is ignored.
memo_attach(File,Opts) :-
   setting(db_directory,Dir),
   directory_file_path(Dir,File,Path),
   db_attach(Path,Opts).


%% volatile_memo(+Spec) is det.
%
%  Directive to declare memoised predicates. Spec can be a single memo predicate
%  specifier or several comma separated ones, as with dynamic/1 etc, for example,
%  ==
%  :- volatile_memo pred1( ArgSpec1, ArgSpec2), 
%          pred2(ArgSpec3,ArgSpec4,ArgSpec5).
%  ==
%  where ArgSpec1 etc are argument specifiers= as defined in the module
%  header comment. It must look like +Type, -Type, +Name:Type or -Name:Type.
%  Name, if given is not used.
volatile_memo(Spec) :- throw(error(context_error(nodirective, volatile_memo(Spec)), _)).

%% persistent_memo(+Spec) is det.
%
%  Directive to declare memoised predicates. Spec can be a single memo predicate
%  specifier or several comma separated ones, as with dynamic/1 etc, for example,
%  ==
%  :- persistent_memo pred1( ArgSpec1, ArgSpec2), 
%                     pred2( ArgSpec3, ArgSpec4, ArgSpec5).
%  ==
%  where ArgSpec1 etc are argument specifiers= as defined in the module
%  header comment. It must look like +Type, -Type, +Name:Type or -Name:Type.
%  Name, if given is only used for the declaration of the persistent memo table.
persistent_memo(Spec) :- throw(error(context_error(nodirective, persistent_memo(Spec)), _)).

%% browse(+Goal:callable, ?Meta:metadata) is nondet.
%% browse(+Goal:callable) is nondet.
%
%  Looks up previous computations of memoised predicate Goal. browse/1 unifies Goal
%  with all successful computations. browse/2 unifies Goal and Meta with all computations,
%  including failed or exception-throwing computations. browse(Goal) is equivalent
%  to browse(Goal, _-ok).
browse(Module:Head) :- browse(Module:Head,_-ok).
browse(Module:Head,Meta) :- 
   must_be(module,Module),
   memoised(Module,Head,Meta,MemoHead), 
   call(Module:MemoHead).

%% clear_all(@Goal:callable, @Meta:metadata) is det.
%% clear_all(@Goal:callable) is det.
%
%  Clears all matching entries from the memo tables that unify with Goal. clear_all/2 additionally
%  allows selection on the basis of meta-data. Deletion is NOT undone on backtracking.
%
%  Asks for confirmation if more than N entries are to be deleted, where N is determined
%  by the setting memo:confirmation_threshold (see library(settings)). The default is 1.
%  The confirmation message is printed with ansi_format/3 using a style determined by setting
%  memo:confirmation_style, whose default is [bold,fg(red)]. The user must type "yes" and
%  press return continue, otherwise an =|operation_cancelled|= is thrown.
clear_all(Module:Head) :- clear_all(Module:Head,_).
clear_all(Module:Head,Meta) :- 
   must_be(nonvar,Head), 
   memoised(Module,Head,Meta,MemoHead),
   retracter(Module,Head,Meta,RetractHead), 
   aggregate_all(count,Module:MemoHead,Count),
   setting(confirmation_threshold,Thresh),
   (Count>Thresh -> confirm(format("Will delete ~d entries.",[Count])); true),
   call(Module:RetractHead).

confirm(Printer) :-
   setting(confirmation_style,Style),
   ansi_format(Style,"~@ Type 'yes' [return] to proceed: ",[Printer]), 
   read_line_to_string(user_input,Response),
   (Response \= "yes" -> throw(operation_cancelled); true).

%% compute(+Goal:callable) is semidet.
%
%  Calls the original un-memoised predicate without checking or modifying memo-tables.
compute(Module:Head) :- 
   computer(Module,Head,Spec,ComputeHead),
   type_and_mode_check(Spec,Head),
   call(Module:ComputeHead).

%% compute(+Goal:callable, -Meta:metadata) is det.
%
%  Calls the original un-memoised predicate without checking or modifying memo-tables.
%  If the underlying predicate fails or throws an exception, Meta is set accordingly.
compute(Module:Head,Meta) :- 
   memoised(Module,Head,Meta,_),
   computer(Module,Head,Spec,ComputeHead),
   type_and_mode_check(Spec,Head),
   timed(reify(Module:ComputeHead,Res),Comp),
   Meta=Comp-Res.


%% recompute_all(+Goal:callable, @Meta:metadata, @Res:result) is semidet.
%
%  Recomputes all memoised computations matching Goal and Meta. This will recompute all
%  the entries that would have been returned by browse/2. The only extra condition is that
%  Goal cannot be unbound on entry -- only one determinate predicate can be recomputed at a time.
%  The old entry is removed only after the new computation produces a result that unifies
%  with Res. Otherwise, the recomputed version is discarded.
recompute_all(Module:Head,Meta,Res) :- 
   must_be(nonvar,Head),
   memoised(Module,Head,Meta,MemoHead),
   retracter(Module,Head,Meta,RetractHead), % to remove old computation
   computer(Module,Head,Spec,_), % just to get Spec
   forall( Module:MemoHead, (
      unbind_outputs(Spec,Head,Head1),
      computer(Module,Head1,_,ComputeHead),
      debug(memo,"recomputing ~q...",[Module:Head1]),
      (  timed(reify(Module:ComputeHead,Res),Comp) 
      -> debug(memo,"storing (~w) ~q...",[Res,Module:Head1]),
         asserter(Module,Head1,Comp-Res,AssertHead), 
         call(Module:RetractHead), % ideally these would be atomic
         call(Module:AssertHead)
      ;  debug(memo,"rejecting (~w) ~q...",[Res,Module:Head1])
      )
   )).


% copies Head0 to Head1, but leaves output arguments unbound.
unbind_outputs(Type,Head0,Head1) :-
   Type=..[Name|Types],
   Head0=..[Name|Args0],
   maplist(copy_if_input,Types,Args0,Args1),
   Head1=..[Name|Args1].

copy_if_input(+_,X,X). 
copy_if_input(-_,_,_).

%% memo(+Goal:callable, -Meta:metadata) is semidet.
%% memo(+Goal:callable) is semidet.
%
%  Calls memoised predicate if it has not been called before with these input arguments, storing
%  the result, or looks up previous matching computations if they exist.
%  Goal must be sufficiently instantiated to satisfy the underlying memoised predicate. 
%  memo/1 and memo/2 behave differently if the underlying predicate fails or throws an
%  exception. memo/1 _reflects_ this behaviour, failing or throwing the same exception, even
%  if the computation was not actually repeated but was retrieve from the memo table.
%  memo/2 _reifies_ this behaviour, returing information in Meta.
%
%  Note that the type and mode are checked strictly. An Input argument X declared with +T
%  must satisfy must_be(T,X). An output argument declared with a -T must an unbound
%  variable. 
memo(Module:Head) :- 
   freeze(Res,reflect(Res)), % this will prevent storage on failure or exception
   memo(Module:Head,_-Res).
memo(Module:Head,Meta) :-
   memoised(Module,Head,Meta,MemoHead),
   computer(Module,Head,Spec,ComputeHead),
   type_and_mode_check(Spec,Head),
   (  call(Module:MemoHead) *-> true
   ;  debug(memo,"computing ~q...",[Module:Head]),
      timed(reify(Module:ComputeHead,Res),Comp),
      asserter(Module,Head,Meta,AssertHead),
      Meta=Comp-Res, 
      debug(memo,"storing (~w) ~W...",[Res,Module:Head,[quoted(true),max_depth(6)]]),
      call(Module:AssertHead)
   ).

reflect(ok) :- !.
reflect(fail) :- !, fail.
reflect(ex(Ex)) :- throw(Ex).


:- nb_setval(memo_mode,memo).

%% current_mode(-Mode:oneof([memo,browse,compute])) is det.
%  Gets the current memoisation mode.
current_mode(Mode) :- nb_current(memo_mode,Mode), Mode\=[], !.
current_mode(memo).


sandbox:safe_meta(memo:browse(_),[]).
sandbox:safe_meta(memo:call_with_mode(browse,_),[]) :- !.
sandbox:safe_meta(memo:call_with_mode(_,G),[G]).
sandbox:safe_meta(memo:modally(Module:Head),[Module:ComputeHead]) :-
   computer(Module,Head,_,ComputeHead).


modally(Module:Head) :-
   current_mode(Mode),
   call(Mode,Module:Head).

%% call_with_mode(+Mode:oneof([memo,browse,compute]), +Goal:callable) is nondet.
%
%  Executes an arbitrary Prolog goal with the current memo-evaluation-mode set
%  to Mode. Thus, any calls to memoised predicates result in calls to memo/1,
%  browse/1 or compute/1 respectively.
call_with_mode(Mode,Goal) :-
   must_be(oneof([memo,compute,browse]),Mode),
   current_mode(Mode0),
   b_setval(memo_mode,Mode),
   catch(Goal,E,(b_setval(memo_mode,Mode0),throw(E))),
   b_setval(memo_mode,Mode0).

compile_memo(_,Var, _) --> { var(Var), !, instantiation_error(Var) }.

compile_memo(Type, (A,B), Module) --> !,
   compile_memo(Type, A, Module),
   compile_memo(Type, B, Module).

compile_memo(volatile, Spec, Module) -->
   {  % strip any arg names from spec
      debug(memo,"registering volatile memo predicate ~q...",[Spec]),
      Spec =.. [Name|ArgSpecs],
      maplist(strip_name,ArgSpecs,ArgTypes),
      Type =.. [Name|ArgTypes],

      functor(Spec, Name, Arity),   
      length(Args, Arity),
      build_term([Name], Args,        Head),
      build_term([Name], [Meta|Args], MemoHead),
      functor(MemoHead, MemoName, MemoArity),

      build_term([MemoName], [MetaA|Args], AssertHead),
      build_term([MemoName], [Meta|Args], RetractHead),
      build_term(['compute_',Name],Args, ComputeHead),
      debug(memo,"-  backed  by ~q...",[MemoName/MemoArity]),
      debug(memo,"-  computed by ~q...",[ComputeHead]),
      hostname(Host), MetaA=comp(Host,_,_)-_
   },
   [ :- dynamic(MemoName/MemoArity),

     memo:memoised(Module, Head, Meta, MemoHead),
     memo:computer(Module, Head, Type, ComputeHead),
     memo:asserter(Module, Head, MetaA, assertz(Module:AssertHead)),
     memo:retracter(Module, Head, Meta, retractall(Module:RetractHead)),
     (Head :- memo:modally(Module:Head))
   ].

compile_memo(persistent, Spec, Module) -->
   {  % strip any arg names from spec
      debug(memo,"registering persistent memo predicate ~q...",[Spec]),
      Spec =.. [Name|ArgSpecs],
      maplist(strip_name,ArgSpecs,ArgTypes),
      Type =.. [Name|ArgTypes],

      functor(Spec, Name, Arity),   
      length(Args, Arity),
      build_term([Name], Args, Head),
      build_term([Name], [Meta|Args], MemoHead),
      functor(MemoHead, MemoName, _),

      build_term(['compute_', Name],     Args,        ComputeHead),
      build_term(['assert_',  MemoName], [MetaA|Args], AssertHead),
      build_term(['retractall_', MemoName], [Meta|Args], RetractHead),

      maplist(mtype_to_ptype, ArgSpecs, PTypes),
      PersistencySpec =.. [MemoName,meta:metadata | PTypes],
      debug(memo,"-  backed  by ~q...",[PersistencySpec]),
      debug(memo,"- computed by ~q...",[ComputeHead]),
      expand_term( (:- persistent(PersistencySpec)), PersistClauses),
      hostname(Host), MetaA=comp(Host,_,_)-_
   },
   phrase(PersistClauses),
   [ memo:memoised(Module, Head, Meta, MemoHead),
     memo:computer(Module, Head, Type, ComputeHead),
     memo:asserter(Module, Head, MetaA, Module:AssertHead),
     memo:retracter(Module, Head, Meta, Module:RetractHead),
     (Head :- memo:modally(Module:Head))
   ].

build_term(NameParts,Args,Term) :- 
   atomic_list_concat(NameParts,Name), 
   Term =.. [Name|Args].

mtype_to_ptype( +N:T, N:T) :- !.
mtype_to_ptype( -N:T, N:partial(T)) :- !.
mtype_to_ptype( +T, _:T).
mtype_to_ptype( -T, _:partial(T)).
strip_name(+_:T,+T) :- !.
strip_name(-_:T,-T) :- !.
strip_name(S,S) :- !.

type_and_mode_check(Type,Head) :-
   forall( arg(I,Type,ArgSpec), 
      (  arg(I,Head,Arg), 
         (  ArgSpec = +ArgType -> must_be(ArgType,Arg)
         ;  ArgSpec = -_ -> must_be(var,Arg)))).

user:term_expansion((:- volatile_memo(Spec)), Clauses) :-
   prolog_load_context(module, Module),
   phrase(compile_memo(volatile, Spec, Module), Clauses).

user:term_expansion((:- persistent_memo(Spec)), Clauses) :-
   prolog_load_context(module, Module),
   phrase(compile_memo(persistent, Spec, Module), Clauses).

user:term_expansion((Head :- Body),(ComputeHead :- Body)) :-
   prolog_load_context(module, Module),
   computer(Module,Head,_,ComputeHead).

user:term_expansion(Head,ComputeHead) :-
   prolog_load_context(module, Module),
   computer(Module,Head,_,ComputeHead).

timed(Goal,comp(_,T1,DT)) :- 
   get_time(T1), call(Goal), 
   get_time(T2), DT is T2-T1.

reify(Goal,R) :- catch((Goal -> R=ok ; R=fail), Ex, R=ex(Ex)).


%% meta_property(+Meta:metadata,-Prop:meta_property) is nondet.
%
%  True when Prop is a meta-property of a memoised computation, where Meta
%  is the metadata term retrieved from memo/2 or browse/2.
%  Valid properties are:
%     *  duration(number)
%        The duration of the computation in seconds.
%     *  time(timestamp)
%        The time at which the computation was initiated, as returned by get_time/1
%     *  host(atom)
%        The name of host on which the computation was performed.
%     *  result(result)
%        The success/fail status of the result
meta_property(comp(_,_,D)-_, duration(D)).
meta_property(comp(_,D)-_,   duration(D)).
meta_property(comp(_,T)-_,   time(T)).
meta_property(comp(H,_,_)-_, host(H)).
meta_property(comp(H,_)-_,   host(H)).
meta_property(_-R,           result(R)).
