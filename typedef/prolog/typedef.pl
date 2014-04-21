:- module(typedef,
    [ op(1150,fx,type)
    , op(1120,xfx,--->)
    , (type)/1
    ]).

/** <module> Type definition framework

   This module provides a way to declare new types that hook into the
   must_be/2 framework of library(error).

   The type definition language is designed to be close to the one in 
   Tom Schrijver's type_check package, supporting type synonyms, such as
   ==
   :- type natnum == nonneg.
   ==
   and algebraic type definitions like
   ==
   :- type nat ---> zero ; succ(nat).
   ==

   A built in type =|partial(T)|= is defined, which is satisfied by variables
   as well as terms that satisfy type T. This should be probably be extended to
   include all partial instantiations of type T, eg =|s(s(_))|= should satisfy
   =|partial(nat)|=, which it _does not_ at the moment.

   The result types can be used with must_be/2 and therefore in the record
   declarations provided by library(record) and the peristency declarations
   provided by library(persistency).
*/

:- multifile user_type_def/1, user_type_constructor/2.
:- op(1150,fx,type).
:- op(1130,xfx, --->).

% true if the module whose terms are being read has specifically
% imported library(typedef).
wants_typedef :-
    prolog_load_context(module, Module),
    Module \== typedef,  % we don't want typedef sugar ourselves
    predicate_property(Module:type(_),imported_from(typedef)).

%% type(Spec).
%  Declares a new type. Spec can be one of two forms:
%  ==
%  NewType ---> Constructor1 ; Constructor2 ; ... .
%  NewType == OldType
%  ==
%  NewType can included type variables, which can be used in the constructor
%  terms. The arguments of constructor terms are the types of the required
%  arguments for that constructor. The second form declares a type synonym,
%  so NewType is equivalent to OldType.
%
%  This is directive. It cannot be called.
type(Spec) :- throw(error(context_error(nodirective, type(Spec)), _)).

user:term_expansion(:- type(Type == Syn), Clause) :-
    wants_typedef,
    Clause = (
        error:has_type(Type, Value) :-
            error:has_type(Syn, Value)
    ).
user:term_expansion(:- type(Type ---> Defs), Clauses) :-
    wants_typedef,
    type_def(Type,Defs,Clauses,[]).

type_def(Type,Defs) -->
   [ typedef:user_type_def(Type) ],
   [ error:has_type(Type, Value) :- typedef:has_type(Type,Value) ],
   constructors(Type,Defs).

constructors(Type,C1;CX) --> !, constructor(Type,C1), constructors(Type,CX).
constructors(Type,CZ) --> constructor(Type,CZ).
constructor(Type,C) --> [ typedef:user_type_constructor(Type,C) ].

error:has_type(partial(Type),Term) :- !,
   var(Term) -> true; error:has_type(Type,Term).

has_type(Type,Term) :-
   user_type_def(Type), !, nonvar(Term),
   user_type_constructor(Type,Cons),
   (  atomic(Cons) -> Cons=Term
   ;  functor(Cons,F,A),
      functor(Term,F,A),
      forall( arg(N,Cons,ArgType), (arg(N,Term,ArgVal), error:has_type(ArgType,ArgVal)))
   ).
