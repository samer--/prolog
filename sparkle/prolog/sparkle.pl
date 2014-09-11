:- module(sparqlweb,[
      sparql_endpoint/2
   ,  sparql_endpoint/3
   ,  current_sparql_endpoint/5
   ,  query_goal/2  % Context, Opts
   ,  query_goal/3  % Endpoint, Context, Opts
   ,  query_phrase/2
   ,  query_phrase/3
   ,  (??)/1
   ,  (??)/2
   ,  op(1150,fx,??)
   ,  op(1150,xfx,??)
	]).

/** <module> Query to SPARQL endpoints with a more Prolog-like syntax
 
  Samer Abdallah, Dept. of Computer Science, UCL (2014)
  Based on Yves Raimond's swic package, but completely re-written.

  To do: 
  *   query multiple endpoints in parallel
  *   process results incrementally.
  *   Documentation!
*/

:- use_module(library(semweb/sparql_client)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(sparql_dcg).

:- dynamic sparql_endpoint/5.
:- multifile sparql_endpoint/5.
:- set_prolog_flag(double_quotes, codes).

:- setting(limit,integer,100,'Default SPARQL SELECT limit').

??(Goal) :- setting(limit,L), query_goal(Goal,[limit(L)]).
??(EP,Goal) :- setting(limit,L), query_goal(EP,Goal,[limit(L)]).

/*
 * Assert/declare a new sparql end point
 */

sparql_endpoint(EP,Url) :- sparql_endpoint(EP,Url,[]).
sparql_endpoint(EP,Url,Options) :-
   url_endpoint(Url,Host,Port,Path), 
	(	sparql_endpoint(EP,Host,Port,Path,_)
   -> format('% WARNING: Updating already registered SPARQL end point ~w.\n',[Url]),
      retractall(sparql_endpoint(EP,Host,Port,Path,_))
   ),
	debug(sparkle,'Asserting SPARQL end point ~w: ~w ~w ~w ~w.',[EP,Host,Port,Path,Options]),
   assert(sparql_endpoint(EP,Host,Port,Path,Options)).

user:term_expansion(:-(sparql_endpoint(EP,Url)), Expanded) :- 
   endpoint_declaration(EP,Url,[],Expanded).
user:term_expansion(:-(sparql_endpoint(EP,Url,Options)), Expanded) :- 
   endpoint_declaration(EP,Url,Options,Expanded).

endpoint_declaration(EP,Url,Options, sparqlweb:sparql_endpoint(EP,Host,Port,Path,Options)) :-
	debug(sparkle,'Declaring SPARQL end point ~w: ~w ~w ~w ~w.',[EP,Host,Port,Path,Options]),
   url_endpoint(Url,Host,Port,Path).

url_endpoint(Url,Host,Port,Path) :-
	parse_url(Url,Parsed),
	member(host(Host),Parsed),
	member(path(Path),Parsed),
	(member(port(Port),Parsed);Port=80).

current_sparql_endpoint(EP,Host,Port,Path,Options) :-
   sparql_endpoint(EP,Host,Port,Path,Options).


% ----------------------------------------------------
% Goal-based queries 
% These get translated into phrase-based queries.

query_goal(Goal,Opts) :- 
   goal_to_phrase(Goal,Opts,Phrase,Result),
   query_phrase(Phrase,Result).

query_goal(EP,Goal,Opts) :- 
   goal_to_phrase(Goal,Opts,Phrase,Result),
   query_phrase(EP,Phrase,Result).

goal_to_phrase(Goal,Opts,Phrase,Vars) :-
   term_variables(Goal,Vars),
   (  Vars=[] % if no variables, do an ASK query, otherwise, SELECT
   -> Phrase=ask(Goal)
   ;  Phrase=select(Vars,Goal,Opts)
   ).

% ----------------------------------------------------
% Phrase-based queries 
% These get translated into SPARLQ queries and executed.

query_phrase(Phrase,Result) :- 
   phrase_to_query(Phrase,Query),
   sparql_endpoint(EP,_,_,_,_),
   catch( run_query(EP,Query,Result),
          Ex, (print_message(warning,Ex), fail)).

query_phrase(EP,Phrase,Result) :- 
   phrase_to_query(Phrase,Query),
   run_query(EP,Query,Result).


phrase_to_query(Phrase,Query) :-
   term_variables(Phrase,Vars),
   copy_term(t(Vars,Phrase),t(Vars1,Phrase1)),
   numbervars(Vars1,0,_),
   (  phrase(Phrase1,QueryCodes) -> true
   ;  throw(unrecognised_query(Phrase))
   ),
   string_codes(Query,QueryCodes),
   debug(sparkle,'SPARQL query: ~s',[Query]).

% ----------------------------------------------------
% In the end, everything comes through this.

run_query(EP,Query,Result) :-
   sparql_endpoint(EP,Host,Port,Path,EPOpts),
   debug(sparkle,'Querying endpoint http://~w:~w~w',[Host,Port,Path]),
   sparql_query(Query,Row,[host(Host),port(Port),path(Path)|EPOpts]),
   Row =.. [row|Result].


% Forget about provenance for now...
% provenance(Context,Provenance). % fill in the Provenance argument to rdf/4
% provenance([],_).
% provenance([rdf(_,_,_)|T],Provenance) :- provenance(T,Provenance).
% provenance([rdf(_,_,_,Provenance)|T],Provenance) :-
% 	provenance(T,Provenance).
