:- module(pldb_terms, 
         [  term_to_codes/2
         ,  string_to_term/3
         ,  strings_to_terms/2
         ,  term_pattern/3
         ]).

:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).

% sql_string(Term) --> 
%    "'", esc(run_left(sql),Codes), "'",
%    {  read_term_from_codes(Codes,Term,[back_quoted_string(false),double_quotes(string),variable_names(Bindings)]),
%       maplist(bind_var,Bindings)
%    }.

% ----- Dealing with terms ---------------------------------------------------

%% term_to_codes(?X,L:list(code)) is det.
%
% This formats any term such that it can be read back. 
% It MUST be a ground term. Atoms and functors are quoted properly. 
% '$VAR'(_) terms are written as with numbervars(true) option to write_term.
% Operator declarations are not used.
%
% NB. we do not use numbervars or write_canonical/1 here, because it will
% probably be more useful to label all the variables in one row together,
% so that variables shared across muliple columns will be properly labelled.
%
% !!! TODO: make sure that variables shared across multiple columns are
% properly reinstated all together in the aftermath of SELECT.
%
% NB. Strings are always written using double quotes.
term_to_codes(V,L) :-
	(ground(V) -> true; throw(error(pldb:nonground(V)))),
	with_output_to(codes(L), write_canonish(V)).

string_to_term(Text,Term,Bindings) :- 
   read_term_from_atom(Text,Term,[back_quoted_string(false),double_quotes(string),variable_names(Bindings)]).

strings_to_terms(Strings,Terms) :-
   maplist(string_to_term,Strings,Terms,Bindings),
   maplist(maplist(includes_binding(_)),Bindings).

includes_binding(Master,Name=Var) :- memberchk(Name=Var,Master).
bind_var(Name='$VAR'(Name)).

%% term_pattern(?X)// is det.
%  Generates a valid SQL LIKE pattern for matching string representation of terms.
%  The term X is any Prolog term including variables. The database is assumed to contain
%  string values representing Prolog terms. This DCG rule generates a string corresponding to
%  the textual representation of X, except that each variable is replaced with a LIKE wildcard
%  character '%', and any wildcard characters present literally in X ('%', '_', or '\')
%  are escaped with a preceding '\'.
%
%  Operator declaration are ignored: terms are written in their canonical form.
term_pattern(Term,Pattern,Tail) :- 
   term_variables(Term,Vars),
   with_output_to(codes(C1),write_with_variables_as(1,Vars,Term)),
   with_output_to(codes(C2),write_with_variables_as(2,Vars,Term)),
   escape_term_pattern(C1-(C2-Pattern),[]-([]-Tail)).

write_with_variables_as(X,Vars,Term) :-
   maplist(=(X),Vars),
   write_canonish(Term),
   fail.
write_with_variables_as(_,_,_).

write_canonish(Term) :- 
   write_term(Term,[quoted(true),ignore_ops(true),numbervars(true),back_quotes(symbol_char)]).

escape_term_pattern --> \< eos, !.
escape_term_pattern --> "1" <\> ("2" <\> "%"), !, escape_term_pattern. 
escape_term_pattern --> [_] <\> pattern, !, escape_term_pattern.

eos([],[]).

% escape string for use in SQL LIKE pattern (generate only, not parse)
% (still needs escaping for writing as quoted SQL literal.)
pattern --> "%" <\> "\\%", !.
pattern --> "_" <\> "\\_", !.
pattern --> "\\" <\> "\\\\", !.
pattern --> [X] <\> [X].

