:- module(musicbrainz,[
      mb_query/4
   ,  mb_query/5
   ,  mb_search/5 
   ,  mb_browse/4
   ,  mb_lookup/3
   ,  mb_facet/2
   ,  mbid/2
	]).

/** <module> Interface to Musicbrainz XML web service

   This module provides client methods for the Musicbrainz XML service.
   The predicates it provides fall broadly into two categories: those for
   composing and querying the Musicbrainz web service, and those for
   decoding the resulting XML documents. Then there are a few higher
   level predicates that combine the two for common query patterns,
   insulating (mostly) the user from the idiosyncracies of XML..

   ---+++ Queries

   The Musicbrainz XML web service is described at 
   http://musicbrainz.org/doc/Development/XML_Web_Service/Version_2 .
   A query is logically composed of three parts: 

   1. An entity type, which determines what kind of Musicbrainz entity is
      returned, for example, =|artist|=, =|release|=, =|work|= etc.
   2. A query type, which is a 'lookup', a 'browse', or a 'search'. Each of
      these has its associated parameters and is represented in this library
      as a Prolog term.
   3. Generic options, which control how much information is returned about
      each entity, and how many entities are returned.

   ---++++ Entity types

   The core predicate mb_query/4 can deal with any entity type that the Musicbrainz
   services recognises. The name is simply represented as an atom of type =|mb_class|=. 
   The core entity 
   types are: =|artist, release, 'release-group', label, recording, work, area, url|=.

   ---++++ Query types

   The three query types are represented using three Prolog functors:

   *  lookup(+ID:atom)
      Looks up the Musicbrainz entity with the given ID.   
      Returns an element.
   *  browse(+LinkT:mb_class,+LinkID:atom)
      Returns a list of entities which are linked directly to the entity of
      class LinkT with ID LinkT. For example, the query =|browse(artist,ArtistID)|=
      applied to a core entity type =|release|= retrieves all the releases associated
      with the given artist. Returns a list of elements and a number giving the total number 
      of matches.
   *  search(+SeachTerm:text)
      Full text search for the given text (an atom or string).
      Returns a list of elements and a number giving the total number of matches.
      If the search term is not atomic and the module lucene is
      loaded, then Term will be interpreted as a term describing a Lucene search
      as implemented in the module lucene.pl.

   ---+++ Options

   The following options are recognised:

   *  limit(+N:integer)
      For browse and search requests only - limits the number of returned entities.
   *  offset(+N:integer)
      For browse and search requests only - determines the offset of the returned
      list of entities relative to the full query results.
   *  inc(+I:text)
      For lookup and browse requests only - fills in the inc parameter of the
      query URL. See http://musicbrainz.org/doc/Development/XML_Web_Service/Version_2 for
      more information.

   If any inappropriate options are supplied for a given query, an exception is thrown.


   ---+++ XML Decoding

   The system for decoding XML documents is based on the idea of 'facets': the
   main result document consists of a list of items of the requested entity type.
   Each entity has several facets, each of which can be thought of as a logical
   statement which is true relative to that item. Each facet is represented by
   a term with some functor and arguments of the appropriate types. For example,
   the facet name/1 applies to artists, countries, labels etc looks like
   =|name(Name)|=, where Name is an atom. The name facet is extracted from the
   XML sub-element 'name', but other facets might result from more comlicated
   processing of the XML element representing the item. The predicate facet/4
   determines which facets are recognised and how they are computed.

   The predicate mb_facet/2 relates an XML element with its facets.

   ---+++ Multi-page Queries

   Browse and search queries produce a list of answers, a subset of which is returned
   depending on the limit and offset options. A higher level predicate mb_query/5 gives a
   general way of accessing the elements of the result set, returning each item one by
   one on backtracking, along with a progress indicator, which is a term that looks
   like N/T, which means 'N th out of T'. The option =|auto_page(boolean)|=
   controls how large result sets are handled. If false, only one HTTP request is
   fired off, yielding a window into the full result set determined by the options
   =|limit(integer)|= and =|offset(integer)|=. If true (the default), multiple queries
   are executed transparently, yielding the full result set in chunks determined by
   the limit option. This defaults to the value of the setting =|limit|=.

  (C) Samer Abdallah, UCL (2014)
 */

:- use_module(library(http/http_client)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(xpath)).


:- setting(limit,integer,20,'Default limit for Musicbrainz search and browse queries').

%% mb_search(+T:mb_class, +Term:text, -Score:number, -Id:atom, -Item:element(T)) is nondet.
%
%  Searches for entities of type T using arbitrary text. Multiple matches are yielded
%  on backtracking, with Score giving a goodness of fit between 0 and 100, Id giving the
%  Musicbrainz ID of the proposed item, and Item containing all the information returned
%  about the item, which can be examined using mb_facet/2.
%  Executes multiple queries to page through an arbitrary number of results.
mb_search(T,Term,Score,Id,Item) :-
   mb_query(T,search(Term),[],_,Item),
   maplist(mb_facet(Item),[id(Id),score(Score)]).


%% mb_browse(+T:mb_class, +LinkT:mb_class, +LinkId:atom, -Item:element(T)) is nondet.
%
%  Finds entities of type T which are directly linked with the entity of type
%  LinkT with ID LinkId. Multiple items are returned on backtracking. 
%  Executes multiple queries to page through an arbitrary number of results.
mb_browse(T,LinkT,LinkId,Item) :- mb_query(T,browse(LinkT,LinkId),[],_,Item).

%% mb_lookup(+T:mb_class, +ID:atom, -Item:element(T)) is semidet.
%
%  Lookup Musicbrainz entity of the given type with the given ID.
mb_lookup(Class,Id,Item) :- mb_query(Class,lookup(Id),[],Item).

%% mb_query(+T:mb_class,+Req:request(T,items(T)),+Opts:options,-P:progress,-E:element(T)) is nondet.
%
%  Executes a query that produces multiple results and binds E to each of the items
%  in turn on backtracking. This predicate accepts the option =|auto_page(boolean)|=.
%  If true (the default), any =|offset(N)|= option is ignored, and the the full result
%  set is returned one by one, on backtracking, executing multiple queries if necessary.
%  Otherwise, the results for a single query with the given offset (default 0) is produced.
%
%  Progress through the whole result set is given in P.
%  The elements returned can be examined using mb_facet/2.

mb_query(Class,Req,Opts,I/Total,Item) :- ground(I), !,
   select_option(auto_page(_),Opts,Opts1,true),
   select_option(limit(_),Opts1,Opts2,1),
   select_option(offset(_),Opts2,Opts3,0),
   succ(Offset,I),
   mb_query(Class,Req,[limit(1),offset(Offset)|Opts3],Total-[Item]).

mb_query(Class,Req,Opts,I/Total,Item) :-
   setting(limit,DL),
   select_option(auto_page(Auto),Opts,Opts1,true),
   select_option(limit(L),Opts1,Opts2,DL),
   (  Auto=false 
   -> option(offset(Offset),Opts2,0),
      mb_query(Class,Req,[limit(L)|Opts2],Total-Items),
      nth1(J,Items,Item), I is Offset+J
   ;  select_option(offset(_),[limit(L)|Opts2],Opts3,0),
      items(Class,Req,Opts3,0,Total-Items),
      lazy_nth1(I,Items,0,Total-items(Class,Req,Opts3),Item)
   ).


items(Class,Req,Opts,Offset,Items) :-
   mb_query(Class,Req,[offset(Offset)|Opts],Items).

%% lazy_nth1( List:list(A), Seen:integer, TM:pair(integer, More:pred(+Seen:integer,-:Total:integer,-Items:list(A))), -I:integer, -X:A) is nondet.
lazy_nth1(I, [X|_],  M,_,    X) :- succ(M,I).
lazy_nth1(I, [_|Xs], M,TMore, X) :- succ(M,M1), lazy_nth1(I,Xs,M1,TMore,X).
lazy_nth1(I, [],     M,T-More, X) :-
   T>M, call(More,M,T1-Xs),
   lazy_nth1(I,Xs,M,T1-More,X).



%% mb_query(+T:mb_class,+Req:request(T,A),+Opts:options,-Result:A) is det.
%
%  Execute a query against the Musicbrainz server, requesting entities of class
%  T. The request term Req is a ground term specifying a lookup, browse, or search query.
%  Supplied options must be appropriate for the given query type.
%  Each request is associated with a return type, which is the type of Result returned by query/4.
%
%  The request terms and their types are:
%  ==
%  lookup(atom)          :: request(A,element(A)).
%  browse(mb_class,atom) :: request(A,items(A)).
%  search(text)          :: request(A,items(A)).
%
%  items(A) == pair(natural,list(element(A))).
%  pair(X,Y) ---> X-Y.
%  ==
%  =|element(A)|= is the type of XML element terms like =|element(A,_,_)|=.
mb_query(Class,Req,Opts,Return) :-
   request_params(Req,Opts,Decode,PathParts,Params),
   concat_atom(['/ws/2/',Class|PathParts],Path),
   get_xml([host('musicbrainz.org'), path(Path), search(Params)], [Root]),
   (  Root=element(error,_,_) -> throw(mb_error(Root))
   ;  call(Decode,Class,Root,Return)
   ).


%% request_params(+R:request(A), +O:list(option), +Decode:pred(+atom,+dom,-A), -T:list(atom), -P:list(param)) is det.
%
%  Takes a request and a list of Name=Value pairs and produces a URL path and list of parameters
%  for that request. Only options valid for the given request are permitted.
request_params(lookup(Id),       Opts, doc_item,  ['/',Id], Params)  :- process_options([inc],Opts,Params).
request_params(browse(Class,Id), Opts, doc_items, [], [Class=Id|Params]) :- process_options([limit,offset,inc],Opts,Params).
request_params(search(Query),    Opts, doc_items, [], [query=Q|Params]) :- 
   (  atomic(Query) -> Q=Query 
   ;  (  current_module(lucene) -> lucene:lucene(Query,Q)
      ;  throw(error(lucene_module_not_loaded)))),
   process_options([limit,offset],Opts,Params).

% Convert list of valid Name=Value pairs and produce params for HTTP query.
process_options(ValidOpts,Opts,Params) :- process_options(ValidOpts,Opts,Params,[]).
process_options([],Opts) --> ({Opts=[]} -> []; {throw(unrecognised_options(Opts))}).
process_options([Nm|NS],O1) -->
   ({O=..[Nm,Val], select_option(O,O1,O2)} -> [Nm=Val];{O2=O1}),
   process_options(NS,O2).

doc_item(Class,Root,Item) :- xpath(Root,Class,Item).
doc_items(Class,Root,Total-Items) :-
   atom_concat(Class,'-list',ListElem),
   xpath(Root,ListElem,List),
   mb_facet(List,count(Total)),
   List=element(_,_,Items).

% would like to use http_open, but it doesn't handle MBZ error documents properly.
get_xml(URLSpec,Doc) :- 
   debug(musicbrainz,'Query URL: ~q',[URLSpec]),
   http_get([port(80)|URLSpec],Doc,[content_type('text/xml'),dialect(xml)]).

% get_xml(URLSpec,Doc) :-
%    debug(musicbrainz,'Query URL: ~w',[URLSpec]),
%    setup_call_cleanup( 
%       http_open(URLSpec,Stream,[]),
%       load_xml(Stream,Doc,[]),
%       close(Stream)).

%% mb_facet( +E:element, ?Facet:facet) is nondet.
%
%  This predicate implements a scheme for extracting information from an XML
%  element. The idea is that attributes and sub-elements of an element represent
%  'facets', which can be thought of modal predicates which a true relative
%  to this element. Each facet is therefore like a Prolog predicate, with a name,
%  arity, and typed arguments.
%
%  If Facet is unbound on entry, then all facets which true relative to element E
%  are produced on backtracking.
%
%  If Facet is nonvar on entry, then the element is scanned to test/bind any variables
%  in the facet. 

mb_facet(E,Facet) :- var(Facet), !,
   % if Facet is unbound, then this goal ordering results in an
   % orderly scan through all the components of the element.
   (Spec=attr(_,_); Spec=elem(_,_,_)),
   call(Spec,E), 
   (  facet(Facet,Spec,Goal) *-> call(Goal)
   ;  debug(musicbrainz,'Unrecognised information: ~q',[Spec]),
      fail
   ).

mb_facet(E,Facet) :- 
   % if Facet is bound, then this goal ordering goes directly to the info.
   facet(Facet,Spec,Goal), 
   call(Spec,E),
   call(Goal).


% goals for extracting attributes and subelements from an element
attr(Name,Value,element(_,Attrs,_)) :- member(Name=Value,Attrs). 
elem(Name,A,C,element(_,_,Elems))   :- member(element(Name,A,C),Elems).

%% facet( F:facet, X:pred(element), G:pred) is nondet.
%
%  Database of known facets. Each facet has a goal for extracting information
%  from an element and a goal to bind elements in the facet's head.
facet( count(Y),   attr(count,       X), atom_number(X,Y)).
facet( offset(Y),  attr(offset,      X), atom_number(X,Y)).
facet( id(X),      attr(id,          X), true).
facet( score(Y),   attr('ext:score', X), atom_number(X,Y)).
facet( type(X),    attr(type,        X), true).
facet( name(Y),    elem(name,     _, X), get_text(X,Y)).
facet( gender(Y),  elem(gender,   _, X), get_text(X,Y)).
facet( country(Y), elem(country,  _, X), get_text(X,Y)).
facet( born(Y),    elem('life-span', _, X), xp(X,begin(text),Y)).
facet( died(Y),    elem('life-span', _, X), xp(X,end(text),Y)).
% facet( dead,       elem('life-span', _, X), xp(X,ended(text),true)).
facet( title(Y),   elem(title,    _, X), get_text(X,Y)).
facet( date(Y),    elem(date,     _, X), get_text(X,Y)).
facet( length(Y),  elem(length,   _, [X]), atom_number(X,Y)).
facet( alias(Y),          elem('alias-list',_, X),    xp(X,alias(text),Y)).
facet( sort_name(Y),      elem('sort-name', _, X),    get_text(X,Y)).
facet( disambiguation(Y), elem(disambiguation, _, X), get_text(X,Y)).
facet( area(Id,Facets),   elem(area,As,Es),           get_area(As,Es,Id,Facets)).
facet( credit(artist,E),  elem('artist-credit',_, X),  xp(X,'name-credit'/artist,E)).
facet( release(E),        elem('release-list',_, X),  xp(X,release,E)).

get_text(Elems,Text) :- xp(Elems,/self(text),Text).
get_area(As,Es,Id,F2) :-
   findall(F,mb_facet(element(area,As,Es),F),F1),
   select(id(Id),F1,F2).
xp(Elems,Selector,Val) :- xpath(element(e,[],Elems),Selector,Val).

%% mbid(+E:element(_), -Id:atom) is semidet.
%  Short accessor for entity Id.
mbid(E,Id) :- mb_facet(E,id(Id)).

%% mb_class(-T:mb_class) is nondet.
%  Registry of core entity types.
mb_class(label).
mb_class(artist).
mb_class(work).
mb_class(recording).
mb_class(release).
mb_class('release-group').
mb_class(area).
mb_class(url).

% For more convenient display of elements.
user:portray(E) :-
   E=element(T,_,_), mb_class(T),
   mb_facet(E,id(Id)),
   (  mb_facet(E,name(Name))
   -> format('<mb:~w/~w|~w>',[T,Id,Name])
   ;  mb_facet(E,title(Title))
   -> format('<mb:~w/~w|~w>',[T,Id,Title])
   ;  format('<mb:~w/~w>',[T,Id])
   ).
