:- module(sindice,
   [  sindice_url/3
   ,  si_with_graph/4
   ,  si_with_result/5
   ,  si_facet/2
   ]).

/** <module> Inteface to Sindice semantic web search engine

   This module provides the ability to formulate queries to the
   Sindice semantic web search engine, and to analyse the results
   obtained. It is based on an original module by Yves Raimond,
   but mostly rewritten by Samer Abdallah.

   Sindice queries have serveral components:
      1. A keyword based query, which may use + and - operators
         to mark terms that are required or must be excluded.
         It may also use boolean operators AND, OR and NOT, though
         note that NOT has the semantics of set difference, not
         the set complement. NOT is a binary operator in Sindice
         queries.
      2. A triple based query which is built using Boolean operators
         from RDF triples. In this queries, a '*' denotes an
         constrained URI or literal
      3. One or more filters, which specify certain simple test
         to be applied to the returned objects.

   Other parameters determine what and how much information is returned:
      1. The page parameter determines which page of a multipage query is returned.
      2. The sortbydate paramater affects the order of results (the default
         is to sort by relevance).
      3. The field parameter determines what information is returned about
         each object.

   ---+++ Results

   Results are retreived as a named RDF graph.
   To interpret this, it is necessary to understand the Sindice
   ontology. The results consist of a set of resources of the class
   sindice:Result. Each item has the following properties:

      *  dc:title       :: literal
      *  dc:created
      *  sindice:cache
      *  sindice:link   :: url
      *  sindice:rank   :: xsd:integer
      *  sindice:explicit_content_length
      *  sindice:explicit_content_size
      *  si_field:format
      *  sindice:class
      *  sindice:ontology
      *  sindice:property

   As well as information about each item, the results also contain 
   data about the search itself, which is represented as a resource of
   class sindice:Query, and data about the returned page, represented as
   a resource of class sindice:Page.
   The sindice:Query has the following properties

      *  sindice:totalResults :: xsd:integer
      *  dc:title
      *  dc:creator
      *  dc:date
      *  sindice:searchTerms  :: literal
      *  sindice:totalResults :: literal(integer)
      *  sindice:itemsPerPage :: literal(integer)
      *  sindice:first        :: sindice:Page
      *  sindice:last         :: sindice:Page
      *  result :: sindiceResult [nondet]
      
   The sindice:Page has the following properties:
      *  dc:title          :: literal
      *  sindice:next      :: sindice:Page
      *  sindice:previous  :: sindice:Page
      *  sindice:startIndex :: literal


   ---+++ Running queries

   The core predicate for running a Sindice query is si_with_graph/4, which formulates
   a query from a term of type =|sindice_req|= and a list of options, and then
   loads into the RDF store, temporarily, a named graph containing the results.
   The last argument to si_with_graph/4 is a goal which is called with the results
   graph in context. The graph is only available to this goal, and is unloaded after
   si_with_graph/4 finished. You may use any RDF-related predicates to interrogate
   the graph.

   On top of this is built a high-abstraction: si_with_result/5, which hides the
   details of large, multi-page result sets and calls a supplied goal once (disjunctively)
   for each result, automatically issuing multiple Sindice requests to iterate through
   multiple pages. You may interrogate the properties of each result only within
   the supplied goal. For convenience, the si_facet/2 allows a number of properties
   to be extracted from the RDF graph with type conversions from
   RDF literals to Prolog values where appropriate.


   ---+++ Building queries

   The three main parts of a Sindice query are represented by a term of
   type =|sindice_request|=, which has several forms. Currently, these
   are 
   ==
   si_request ---> keyword(atom) 
                 ; keywords(list(atom))
                 ; uri(resource).
   ==
   A resource can be an atomic URI or a Prefix:Suffix term as understood
   by rdf_global_id/2. Eventually, Sindice's full query syntax, including
   ntriple queries and Boolean operators, will be implemented.
   
   @seealso http://sindice.com/

   Samer Abdallah, UCL, University of London
   Yves Raimond, C4DM, Queen Mary, University of London
 */

:- meta_predicate si_with_graph(+,+,-,0).
:- meta_predicate si_with_result(+,+,-,-,0).
:- meta_predicate rdf_call_with_graph(+,+,-,0).

:- rdf_meta rdf_number(r,r,?).
:- rdf_meta rdf_number(r,r,?,?).


:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_http_plugin')).
:- use_module(library(aggregate)).
:- use_module(library(dcg_core)).


:- rdf_register_prefix(sindice,'http://sindice.com/vocab/search#').
:- rdf_register_prefix(si_field,'http://sindice.com/vocab/fields#').
:- rdf_register_prefix(si_search,'http://api.sindice.com/v3/search?').


%% sindice_url(+Req:si_request,+Opts:options,-URL:atom) is det.
%  Formulates a Sindice query URL from a request and options.
%  Recognised options:
%     *  sort_by_date(B:boolean)
%        If true, then results are sorted by date rather than relevance
%     *  fields(F:list(atom))
%        Specify which fields are returned for each result.
%     *  count(P:nonneg)
%        Number of results per page. (Incompatible with from option.)
%     *  page(P:nonneg)
%        Request a given page number. (Incompatible with from option.)
%     *  from(Offset:nonneg,Count:nonneg)
%        Starts from result number Offset+1, with Count results per page.
%        Incompatible with count and page options.
%  The resulting URL can be loaded with rdf_load/2 or rdf_load/3.

sindice_url(Req,Opts,URL) :-
   phrase( request_params(Req) >> seqmap(option_params,Opts), Params,[]),
	parse_url(URL,
      [  protocol(http)
      ,  host('api.sindice.com')
      ,  path('/v3/search')
      ,  search(Params)]).


%% si_with_graph(+Req:si_request,+Opts:options,-Graph:atom,+Goal:callable) is det.
%
%  Formulates a Sindice query and temporarily loads the resulting RDF graph.
%  Graph must be a variable; it is unified with the name of the loaded graph
%  and then Goal is called. The graph is not available outside Goal.
si_with_graph(Req,Opts,Graph,Goal) :-
   sindice_url(Req,Opts,URL),
   rdf_call_with_graph(URL,[],Graph,Goal).

% builds HTTP parameters from si_request term.
request_params(keyword(K)) --> {must_be(atomic,K)}, [q=K].
request_params(keywords(KS)) --> {atomic_list_concat(KS,' ',K)}, [q=K].
request_params(uri(URI)) --> [q=Query],
   {  URI=_:_ -> rdf_global_id(URI,Query)
   ;  must_be(atomic,URI), Query=URI
   }.

% builds HTTP parameters from request options
option_params(fields(Fields)) --> seqmap(field,Fields).
option_params(sort_by_data(B)) --> [sortbydata(B)].
option_params(page(P)) --> [page(P)].
option_params(count(N)) --> [count(N)].
option_params(from(I,N)) --> [start(I),count(N)].

field(F) --> [field(F)].

% sindice options
sindice_opt(keyword,text,q).
sindice_opt(ntriple,_,nq).
sindice_opt(filter,_,fq).


%% si_with_result(+Req:si_request, +Opts:options, -Prog:progress, -R:resource, +Goal:callable) is nondet.
%
%  For each result produced by the query, R is unified with the URI of the sindice:Result
%  and Goal is called. Multi-page result sets are traversed automatically and on demand.
%  The graph containing the query results is not available outside Goal and is unloaded
%  when si_with_result/5 is finished. Progress is a term of the form I/Total, where Total
%  is the total number of results and I is the rank of the result currently bound to R.

si_with_result(Req,Opts,I/N,R,Goal) :-
   % first, make sure that rank will be included with results
   (  select_option(fields(Fs),Opts,Opts1)
   -> union([rank],Fs,Fs1), 
      Opts2=[fields(Fs1)|Opts1]
   ;  Opts2=Opts % default field set already includes rank
   ),

   (  var(I) % means we are browsing results with auto-paging 
   -> catch( autopaged_result(Req,Opts2,1,I/N,R,Goal),no_more, fail)
   ;  succ(I0,I), % otherwise, go straight to Ith result
      si_with_graph(Req,[from(I0,1)|Opts2],G,
         (  rdf_number(_,sindice:totalResults,N,G),
            rdf_number(R,sindice:rank,I,G),
            call(Goal)))
   ).

% recursive predicate for traversing multi-page result sets.
autopaged_result(Req,Opts,P,Progress,R,Goal) :-
   (  si_with_graph(Req,[page(P)|Opts],G,page_result(Progress,R,G,Goal))
   ;  succ(P,P1), autopaged_result(Req,Opts,P1,Progress,R,Goal)
   ).

% This extracts information about the total number of results and the 
% current page, and then uses page_result/6 to nondeterministically bind
% R to each result in the current page, in order of rank I. 
% A no_more expection is thrown from page_result/6 if the end of the result
% set is reached.
page_result(I/N,R,G,Goal) :-
   rdf_number(_,sindice:totalResults,N,G),
   aggregate(set(I-R)-max(I),rdf_number(R,sindice:rank,I,G),Results-Last),
   (  member(I-R,Results), call(Goal)
   ;  Last>=N -> throw(no_more)
   ).


%% si_facet(-R:resource,-F:si_facet) is nondet.
%
%  True when search result R has facet F.
%  Current facets are:
%  ==
%  si_facet ---> link(url)
%              ; cache(url)
%              ; rank(nonneg)
%              ; title(atom)
%              ; class(resource)
%              ; predicate(resource)
%              ; formats(list(atom))
%              ; explicit_content_size(nonneg)
%              ; explicit_content_length(nonneg)
%              .
%  ==
%  Note that the class and predicate facets have the unfortunate tendency
%  to refer to RDF namespaces with there being any mechanism for establishing
%  which namespaces are assumed by the Sindice server. For example, a predicate
%  might be returned as the atom 'dcterms:date', but there is no way to 
%  recognise conclusively when such a namespace is being used, or to find out
%  what the Sindice server thinks these should expand to.

si_facet(R, link(L))      :- rdf(R,sindice:link,L).
si_facet(R, cache(C))     :- rdf(R,sindice:cache,literal(C)).
si_facet(R, rank(I))      :- rdf_number(R,sindice:rank,I).
si_facet(R, title(T))     :- rdf(R,dc:title,literal(T)).
si_facet(R, class(C))     :- rdf(R,si_field:class,literal(C)).
si_facet(R, predicate(P)) :- rdf(R,si_field:predicate,literal(P)).
si_facet(R, formats(Fs))  :- setof(F,rdf(R,si_field:format,literal(F)),Fs).
si_facet(R, explicit_content_size(I)) :- rdf_number(R,sindice:explicit_content_size,I).
si_facet(R, explicit_content_length(I)) :- rdf_number(R,sindice:explicit_content_length,I).


% -- potentially general RDF utilities --

rdf_number(S,P,Num) :-
   (  var(Num) 
   -> rdf(S,P,literal(Atom)), atom_number(Atom,Num)
   ;  atom_number(Atom,Num), rdf(S,P,literal(Atom))
   ).
rdf_number(S,P,Num,G) :-
   (  var(Num) 
   -> rdf(S,P,literal(Atom),G), atom_number(Atom,Num)
   ;  atom_number(Atom,Num), rdf(S,P,literal(Atom),G)
   ).

rdf_call_with_graph(URL,Opts,Graph,Goal) :-
   setup_call_cleanup( rdf_load(URL,[graph(Graph)|Opts]), call(Goal),
                       rdf_unload_graph(Graph)).
