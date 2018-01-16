/* Part of sparkle
	Copyright 2014-2015 Samer Abdallah (UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(sparkle,[
      sparql_endpoint/2
   ,  sparql_endpoint/3
   ,  current_sparql_endpoint/5
   ,  query_goal/3     % Endpoint, Context, Opts
   ,  query_phrase/3   % Endpoint, QueryPhrase, Result
   ,  query_sparql/3 % Endpoint,QueryText,Result
   ,  create_sparql_select/2
   ,  create_sparql_select/3
   ,  create_sparql_construct/3
   ,  create_sparql_construct/4
   ,  (??)/1
   ,  (??)/2
   ,  op(1150,fx,??)
   ,  op(1150,xfy,??)
	]).

/** <module> Query to SPARQL endpoints with a more Prolog-like syntax
 
  Samer Abdallah, Dept. of Computer Science, UCL (2014)
  Based on Yves Raimond's swic package, but completely re-written.

   This module provides a little language for expressing SPARQL queries
   and a database of known SPARQL endpoints. Queries can be executed
   across multiple endpoints in parallel. When using auto-paging,
   multiple queries are made automatically to fetch new bindings as
   they are needed. For example, 
   ==
   EP ?? rdf(A,B,C).
   ==
   will retrieve all triples from all endpoints in parallel, fetching
   100 bindings at a time from each endpoint (assuming the setting
   sparkle:limit takes it's default value of 100).
*/

:- use_module(library(sandbox)).
:- use_module(library(settings)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(sparql_dcg).
:- use_module(concurrency).


:- dynamic sparql_endpoint/5.
:- multifile sparql_endpoint/5.
:- set_prolog_flag(double_quotes, codes).

:- setting(limit,integer,100,'Default SPARQL SELECT limit').
:- setting(select_options,list,[distinct(true)],'Default select options').

:- meta_predicate query_phrase(+,//,-).

sandbox:safe_meta(sparql_dcg:phrase_to_sparql(Phr,_),[Phr]).
sandbox:safe_primitive(sparql_dcg:select(_,_,_,_,_)).
sandbox:safe_primitive(sparql_dcg:describe(_,_,_,_)).
sandbox:safe_primitive(sparql_dcg:describe(_,_,_)).
sandbox:safe_primitive(sparql_dcg:ask(_,_,_)).

%% '??'(+Goal:sparql_goal) is nondet.
%  Equivalent to _ ?? Goal. Will query all endpoints
%  in parallel. Identical bindings may be returned multiple times.
%  See query_goal/3 for details.
??(Spec) :- ??(_,Spec).

%% '??'(EP,+Goal:sparql_goal) is nondet.
%  Equivalent to query_goal(EP,Goal,Opts) where Opts is the value of
%  the setting sparkle:select_options. See query_goal/3 for details.
%  IF EP is unbound on entry, it is bound to the endpoint from which
%  the current bindings were obtained.
??(EP,Spec) :-
   rewrite_goal(Spec,SpecRewrite),
   debug(sparkle,'Rewritten goal: ~w',[SpecRewrite]),
   spec_goal_opts(SpecRewrite,Goal,Opts),
   setting(select_options,Opts0),
   merge_options(Opts,Opts0,Opts1),
   query_goal(EP,Goal,Opts1).

spec_goal_opts(Opts ?? Goal, Goal, Opts) :- !.
spec_goal_opts(Goal,Goal,[]).

rewrite_goal(In,Out) :- rewrite_goal(In,Out,1).

% terminals
rewrite_goal(T, T,_) :- T=rdf(_,_,_), !.
rewrite_goal(T, T,_) :- T=rdf(_,_,_,_), !.
rewrite_goal(filter(A), filter(A),_) :- !.

% TODO: consider adding semantics
rewrite_goal(rdf(S,P,O), rdf_has(S,P,O),_) :- !.

% rdfs terminals
rewrite_goal(rdf_where(Q), rdf_where(Q), _) :- !.
rewrite_goal({Q}, {Q}, _) :- !.
rewrite_goal(rdf_has(S,P,O), rdf(S,P,O),_) :- !.
rewrite_goal(rdfs_subclass_of(C,P), rdf(C,oneOrMore(rdfs:subClassOf),P),_) :- !.
rewrite_goal(rdfs_subproperty_of(C,P), rdf(C,oneOrMore(rdfs:subPropertyOf),P),_) :- !.
rewrite_goal(rdfs_individual_of(I,C), (rdf(I,rdf:type,X),rdf(X,zeroOrMore(rdfs:subClassOf),C)),_) :- !.

% non-terminals
rewrite_goal((A,B),(A2,B2),D) :-
        !,
        rewrite_goal(A,A2,D),
        rewrite_goal(B,B2,D).

rewrite_goal((A;B),(A2;B2),D) :-
        !,
        rewrite_goal(A,A2,D),
        rewrite_goal(B,B2,D).
rewrite_goal(\+A, \+A2, D) :-
        !,
        rewrite_goal(A,A2,D).

rewrite_goal(A,A2,D) :-
        increase_depth(D,D2),
        setof(Clause,clause(A,Clause),Clauses),
        list_to_disj(Clauses,X),
        rewrite_goal(X,A2,D2).

list_to_disj([X],X) :- !.
list_to_disj([X|T],(X;T2)) :- list_to_disj(T,T2).


increase_depth(D,_) :-
        D > 10,
        !,
        throw(error(max_depth_exceeded(D))).
increase_depth(D,D2) :-
        D2 is D+1.

        
        
        


/*
 * Assert/declare a new sparql end point
 */

%% sparql_endpoint(+EP:ground, +URL:atom, +Options) is det.
%% sparql_endpoint(+EP:ground, +URL:atom) is det.
%
%  Declares EP as a short name for a SPARQL endpoint with the given URL.
%  No options are defined at the moment.
sparql_endpoint(EP,Url) :- sparql_endpoint(EP,Url,[]).
sparql_endpoint(EP,Url,Options) :-
   url_endpoint(Url,Host,Port,Path),
   !,
   retract_declared_endpoint(EP,Url),     
   debug(sparkle,'Asserting SPARQL end point ~w: ~w ~w ~w ~w.',[EP,Host,Port,Path,Options]),
   assert(sparql_endpoint(EP,Host,Port,Path,Options)).

retract_declared_endpoint(EP,Url) :-
   sparql_endpoint(EP,Host,Port,Path,_),
   format('% WARNING: Updating already registered SPARQL end point ~w.\n',[Url]),
   retractall(sparql_endpoint(EP,Host,Port,Path,_)),
   !.
retract_declared_endpoint(_,_).

user:term_expansion(:-(sparql_endpoint(EP,Url)), Expanded) :- 
   endpoint_declaration(EP,Url,[],Expanded).
user:term_expansion(:-(sparql_endpoint(EP,Url,Options)), Expanded) :- 
   endpoint_declaration(EP,Url,Options,Expanded).

endpoint_declaration(EP,Url,Options, sparkle:sparql_endpoint(EP,Host,Port,Path,Options)) :-
	debug(sparkle,'Declaring SPARQL end point ~w: ~w ~w ~w ~w.',[EP,Host,Port,Path,Options]),
   url_endpoint(Url,Host,Port,Path).

url_endpoint(Url,Host,Port,Path) :-
	parse_url(Url,Parsed),
	member(host(Host),Parsed),
	member(path(Path),Parsed),
	(member(port(Port),Parsed);Port=80).


%% current_sparql_endpoint(-EP:ground,-Host:atom,-Port:natural,-Path:atom,-Options:list) is nondet.
%
%  Succeeds once for each known endpoint.
current_sparql_endpoint(EP,Host,Port,Path,Options) :-
   sparql_endpoint(EP,Host,Port,Path,Options).


% ----------------------------------------------------
% Goal-based queries 
% These get translated into phrase-based queries.

%% query_goal(+EP,+Goal:sparql_goal,+Opts) is nondet.
%% query_goal(-EP,+Goal:sparql_goal,+Opts) is nondet.
%
%  Runs a SPARQL query against one or more SPARLQ endpoints.
%  Goal is converted into a textual SPARQL query using the DCG
%  defined in sparql_dcg.pl. 
%
%  If EP is ground on entry, the query is run against the specified endpoint.
%  If EP is unbound on entry, the query is run agains all endpoints
%  in parallel, possibly returning multiple results from each.
%
%  (The following applies only to queries that return bindings, not
%  to simple boolean questions, which return only true or false.)
%  Options are as follows:
%     *  limit(L:natural)
%        At-most this many bindings will be returned per SPARQL call.
%     *  offset(O:natural)
%        Begin returning bindings from the Oth result on.
%     *  autopage(Auto:bool)
%        If false, a single SPARQL call is made using any limit and offset
%        options if supplied. If true, the the offset option is ignored
%        and multiple SPARQL queries are made as necessary to supply
%        results, using the limit option to determine the number of results
%        retrieved from the endpoint at a time.
%  Other options are passed to phrase_to_sparql/2.

query_goal(EP,Goal,Opts) :- 
   findall(EP,sparql_endpoint(EP,_,_,_,_),EPs),
   term_variables(Goal,Vars),
   (  Vars = [] % if no variables, do an ASK query, otherwise, SELECT
   -> phrase_to_sparql(ask(Goal),SPARQL),
      parallel_query(simple_query(SPARQL),EPs,EP-true)
   ;  Result =.. [row|Vars],
      setting(limit,DefaultLimit),
      call_dcg((  option_default_select(limit(Limit),DefaultLimit),
                  option_default_select(autopage(Auto),true),
                  (  {Auto=true}
                  -> {Query = autopage_query(Limit,SPARQL)},
                     option_default_select(offset(_),_)
                  ;  {Query = simple_query(SPARQL)},
                     cons(limit(Limit))
                  ) 
               ), Opts, Opts1),
      phrase_to_sparql(select(Vars,Goal,Opts1),SPARQL),
      parallel_query(Query,EPs,EP-Result)
   ).

%% create_sparql_select(+Goal,-SPARQL,+Opts) is det.
%% create_sparql_select(+Goal,-SPARQL) is det.
%
% Generates a sparql SELECT or ASK statement for a 
% prolog goal without executing it.
%
% Goal can be any prolog goal consisting of based 
% rdf/3 or rdf/4 statements, filters, or terms
% that can be rewritten in this way
create_sparql_select(Goal,SPARQL) :-
   create_sparql_select(Goal,SPARQL,[]).

create_sparql_select(Goal,SPARQL,Opts) :-
   rewrite_goal(Goal,Goal2),
   debug(sparkle,'Rewritten goal: ~w',[Goal2]),        
   term_variables(Goal2,Vars),
   (  Vars = [] % if no variables, do an ASK query, otherwise, SELECT
   -> phrase_to_sparql(ask(Goal2),SPARQL)
   ;  setting(limit,DefaultLimit),
      call_dcg((  option_default_select(limit(Limit),DefaultLimit),
                  option_default_select(autopage(Auto),true),
                  (  {Auto=true}
                  -> {Query = autopage_query(Limit,SPARQL)},
                     option_default_select(offset(_),_)
                  ;  {Query = simple_query(SPARQL)},
                     cons(limit(Limit))
                  ) 
               ), Opts, Opts1),
      phrase_to_sparql(select(Vars,Goal2,Opts1),SPARQL)).

%% create_sparql_construct(+Head,+Goal,-SPARQL,+Opts) is det.
%% create_sparql_construct(+Head,+Goal,-SPARQL) is det.
%
% Generates a sparql CONSTRUCT statement for a 
% prolog goal without executing it.
%
% Goal or Head can be any prolog goal consisting of based 
% rdf/3 or rdf/4 statements, filters, or terms
% that can be rewritten in this way
%
% the Head forms the head part of the CONSTRUCT
create_sparql_construct(Head,Goal,SPARQL) :-
   create_sparql_construct(Head,Goal,SPARQL,[]).
create_sparql_construct(Head,Goal,SPARQL,Opts) :-
   rewrite_goal(Goal,Goal2),
   rewrite_goal(Head,Head2),
   debug(sparkle,'Rewritten: ~w <- ~w',[Head2,Goal2]),        
   phrase_to_sparql(construct(Head2,Goal2,Opts),SPARQL).


cons(X,T,[X|T]).
option_default_select(Opt,Def,O1,O2) :- select_option(Opt,O1,O2,Def).
simple_query(SPARQL,EP,EP-Result) :- query_sparql(EP,SPARQL,Result).
autopage_query(Limit,SPARQL,EP,EP-Result) :- autopage(EP,SPARQL,Limit,0,Result).

autopage(EP,SPARQL,Limit,Offset,Result) :-
   format(string(Q),'~s LIMIT ~d OFFSET ~d',[SPARQL,Limit,Offset]),
   findall(R,query_sparql(EP,Q,R),Results),
   (  member(Result,Results)
   ;  length(Results,Limit),     % no next page if length(Results) < Limit
      Offset1 is Offset + Limit, % next batch of results
      autopage(EP,SPARQL,Limit,Offset1,Result)
   ).

parallel_query(_,[],_) :- !, fail.
parallel_query(P,[X],Y) :- !, call(P,X,Y).
parallel_query(P,Xs,Y) :-
   maplist(par_goal(P,Y),Xs,Goals),
   concurrent_or(Y,Goals,[on_error(continue)]).

par_goal(P,Y,X,call(P,X,Y)).



%% query_phrase(+EP,+Q:sparqle_phrase(R),R) is nondet.
%% query_phrase(-EP,+Q:sparqle_phrase(R),R) is nondet.
%
% Phrase-based queries using the DCG defined in sparql_dcg.pl.
% The return type depends on the query:
% ==
% select(V:list(var), sparql_goal, options) :: sparql_phrase(row(N)) :- length(V,N).
% describe(resource,sparql_goal)            :: sparql_phrase(rdf).
% describe(resource)                        :: sparql_phrase(rdf).
% ask(sparql_goal)                          :: sparql_phrase(bool).
%
% rdf  ---> rdf(resource,resource,object).
% bool ---> true; false.
% ==
% =|row(N)|= is the type of terms of functor row/N.

query_phrase(EP,Phrase,Result) :- 
   phrase_to_sparql(Phrase,SPARQL),
   query_sparql(EP,SPARQL,Result).


phrase_to_sparql(Phrase,SPARQL) :-
   term_variables(Phrase,Vars),
   copy_term(t(Vars,Phrase),t(Vars1,Phrase1)),
   numbervars(Vars1,0,_),
   (  phrase(Phrase1,Codes) -> true
   ;  throw(unrecognised_query(Phrase))
   ),
   string_codes(SPARQL,Codes),
   debug(sparkle,'SPARQL query: ~s',[SPARQL]).

% ----------------------------------------------------
% In the end, everything comes through this.

%% query_sparql(?EP,SPARQL,-Result) is nondet.
%
%  Runs textual SPARQL query against an endpoint, exactly as
%  with sparql_query/3. If EP is unbound on entry, all known
%  endpoints will be tried sequentially. 
query_sparql(EP,SPARQL,Result) :-
   sparql_endpoint(EP,Host,Port,Path,EPOpts),
   debug(sparkle,'Querying endpoint http://~w:~w~w',[Host,Port,Path]),
   sparql_query(SPARQL,Result,[host(Host),port(Port),path(Path)|EPOpts]).

