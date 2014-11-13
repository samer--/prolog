:- module(musicbrainz,[
      mb_query/4
   ,  mb_query/5
   ,  mb_search/4 
   ,  mb_browse/3
   ,  mb_lookup/3
   ,  mb_lookup/2
   ,  mb_facet/2
   ,  mb_id/2
   ,  mb_id_uri/3
   ,  mb_uri/2
   ,  mb_class/2
   ,  mb_lazy_query/4
	]).

/** <module> Interface to Musicbrainz XML web service

   This module provides client methods for the Musicbrainz XML service.
   The predicates it provides fall broadly into two categories: those for
   composing and querying the Musicbrainz web service, and those for
   decoding the resulting XML documents. Then there are a few higher
   level predicates that combine the two for common query patterns,
   insulating (mostly) the user from the idiosyncracies of XML. This module
   can also use the Lucene module (lucene.pl) to compose Lucene searches.

   ---+++ Quick start

   A simple search returning a 'goodness of match' in Score, a Musicbrainz ID
   in ID, and an XML element in E, then extracting info from E with mb_facet/2:
   ==
   ?- mb_search(artist,'John Coltrane',Score,E), forall(mb_facet(E,F),writeln(F)).
   ==
   Search for releases with 'trane' in the title, using general purpose mb_query/5 
   to get progress info:
   ==
   ?- mb_query(release,search(trane),[],Prog,E).
   ==
   Search for artist then browse releases:
   ==
   ?- mb_search(artist,'John Coltrane',_,A),
      mb_browse(release,A,E),
      forall(mb_facet(E,F),(print(F),nl)).
   ==
   Lucene search for male artist then direct lookup all releases (with debug
   on to report unrecognised fields):
   ==
   ?- debug(musicbrainz).
   ?- mb_search(artist,[coltrane, gender:male],_,A), 
      mb_lookup(A,[inc([releases])],Item), 
      forall(mb_facet(Item,F),(print(F),nl)).
   ==

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

   An entity can be referred to either as a pair Class-Id, or an element as returned
   by a previous query:
   ==
   eref :< pair(mb_class,atom).
   eref :< element(T) :- mb_class(T).
   ==

   ---++++ Query types

   The three query types are represented using three Prolog functors:

   *  lookup(+ID:atom)
      Looks up the Musicbrainz entity with the given ID.   
      Returns an element.
   *  browse(+Link:eref)
      Returns a list of entities which are linked directly to the referenced entity.
      For example, the query =|browse(artist-ArtistID)|=
      applied to a core entity type =|release|= retrieves all the releases associated
      with the given artist. Returns a list of elements and a number giving the total number 
      of matches.
   *  search(+SeachTerm:text)
      Full text search for the given text (an atom or string).
      Returns a list of elements and a number giving the total number of matches.
      If the search term is not atomic, loaded, then Term will be interpreted as a 
      term describing a Lucene search as implemented in the module lucene.pl.

   ---+++ Options

   The following options are recognised by mb_query/4 and mb_query/5:

      *  limit(+N:integer)
         For browse and search requests only - limits the number of returned entities.
      *  offset(+N:integer)
         For browse and search requests only - determines the offset of the returned
         list of entities relative to the full query results.
      *  inc(+I:list(atom))
         Contributes to the inc parameter of the query URL. 
         See http://musicbrainz.org/doc/Development/XML_Web_Service/Version_2 for
         more information.
      *  rels(+I:list(mb_class))
         Contributes 'xxx-rels' components to the inc parameter of the query URL. 
      *  lrels(+I:list(mb_class))
         Contributes 'xxx-level-rels' components to the inc parameter of the query URL. 


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

   This module defines a user:portray/2 clause for terms like =|element(Type,_,_)|=
   where type is one of the core Musicbrainz entity types.
   It extracts the facets =|id(_)|= and either =|name(_)|= or =|title(_)|= (whichever
   is present, and displays the element in the form
   ==
   <mb:Type/Id|NameOrTitle>
   ==

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

  @author Samer Abdallah, UCL (2014)
 */

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(http/json)).
:- use_module(library(xpath)).
:- use_module(library(error)).
:- use_module(library(dcg_core)).
:- use_module(lucene).


:- setting(limit,integer,20,'Default limit for Musicbrainz search and browse queries').
:- setting(min_wait,number,0.5,'Minimum time between Musicbrainz requests').

% for rate limiting.
:- initialization set_state(next_request_time,0). 

:- dynamic state/2.
set_state(Name,Value) :- retractall(state(Name,_)), assert(state(Name,Value)).
get_state(Name,Value) :- state(Name,Value).

%% mb_search(+T:mb_class, +Term:text, -Score:between(0,100), -Item:element(T)) is nondet.
%
%  Searches for entities of type T using arbitrary text. Multiple matches are yielded
%  on backtracking, with Score giving a goodness of fit between 0 and 100 and 
%  Item containing all the information returned
%  about the item, which can be examined using mb_facet/2.
%  Executes multiple queries to page through an arbitrary number of results.
mb_search(T,Term,Score,Item) :-
   mb_query(T,search(Term),[],_,Item),
   mb_facet(Item,score(Score)).


%% mb_browse(+T:mb_class, +Link:eref, -Item:element(T)) is nondet.
%
%  Finds entities of type T which are directly linked with the entity Link.
%  Multiple items are returned one by one on backtracking. 
%  Executes multiple queries to page through an arbitrary number of results.
mb_browse(T,Link,Item) :- mb_query(T,browse(Link),[],_,Item).

%% mb_lookup(+E:eref, +Opts:options, -Item:element(T)) is semidet.
%% mb_lookup(+E:eref, -Item:element(T)) is semidet.
%
%  Lookup a Musicbrainz entity. The entity E can be specified either as a pair Type-Id
%  or a previously returned XML element.
mb_lookup(Class-Id,Opts,Item) :- mb_query(Class,lookup(Id),Opts,Item).
mb_lookup(E,Opts,Item) :- mb_class(E,T), mb_id(E,Id), mb_query(T,lookup(Id),Opts,Item).
mb_lookup(E1,E2) :- mb_lookup(E1,[],E2).

%% mb_query(+T:mb_class,+Req:request(T,items(T)),+Opts:options,-P:progress,-E:element(T)) is nondet.
%
%  Executes a query that produces multiple results and binds E to each of the items
%  in turn on backtracking. This predicate accepts the option =|auto_page(boolean)|=.
%  If true (the default), any =|offset(N)|= option is ignored, and the the full result
%  set is returned one by one, on backtracking, executing multiple queries if necessary.
%  Otherwise, the results for a single query with the given offset (default 0) is produced.
%
%  Progress through the whole result set is given in P, which is a term I/N, where
%  N is the total number of items in the result set and I is the index of the current
%  item. If I is bound on input, that single item is fetched directly.
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

% Query results as a lazy list rather than nondet predicate
mb_lazy_query(Class,Req,Opts1,Items) :-
   setting(limit,DL),
   select_option(limit(L),Opts1,Opts2,DL),
   select_option(offset(O),[limit(L)|Opts2],Opts3,0),
   freeze(Items,grow_tail(items(Class,Req,Opts3),1,O,Items)).

grow_tail(More,Total,Seen,Items) :-
   (  Total=<Seen -> Items=[]
   ;  call(More,Seen,Total1-Chunk), % it's ok if Total1 \= Total 
      append(Chunk,Tail,Items), 
      length(Chunk,N), 
      Seen1 is Seen + N,  
      freeze(Tail,grow_tail(More,Total1,Seen1,Tail))
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
%  Each request is associated with a return type, which is the type of Result.
%  All queries eventually come through this predicate. The address of the Musicbrainz
%  web service is hard coded here.
%
%  The request terms and their types are:
%  ==
%  lookup(atom) :: request(A,element(A)).
%  browse(eref) :: request(A,items(A)).
%  search(text) :: request(A,items(A)).
%
%  items(A) == pair(natural,list(element(A))).
%  pair(X,Y) ---> X-Y.
%  ==
%  =|element(A)|= is the type of XML element terms like =|element(A,_,_)|=.
%
%  @throws mb_error(E:element(error)) 
%  If the Musicbrainz server returns an error term. E is an XML
%  element containing supplementary information returned by the server.
mb_query(Class,Req,Opts,Return) :-
   select_option(fmt(Fmt),Opts,Opts1,xml),
   insist(mb_class(Class),unrecognised_class(Class)),
   request_params(Req,Class,Opts1,Decode,PathParts,Params),
   concat_atom(['/ws/2/'|PathParts],Path),
   wait_respectfully,
   get_doc(Fmt, [host('musicbrainz.org'), path(Path), search([fmt=Fmt|Params])], Doc),
   (  decode_error(Fmt,Doc,Msg)
   -> throw(mb_error(q(Class,Req,Opts),Msg))
   ;  call(Decode,Fmt,Class,Doc,Return)
   ).

decode_error(xml,[element(error,_,E)],Msg) :- get_text(E,Msg).
decode_error(json,Dict,Msg) :- get_dict(error,Dict,Msg).

% this allows us to respect the rate limit on Musicbrainz requests
% using a minimum time interval between requests and the next allowable
% time to make the next request.
wait_respectfully :-
   get_time(Now),
   setting(min_wait,TMin),
   get_state(next_request_time,T0), T1 is max(Now,T0) + TMin,
   set_state(next_request_time,T1),
   (  Now>=T0 -> true
   ;  DT is T0-Now, 
      debug(musicbrainz,"Sleeping for ~f seconds to respect rate limit",[DT]),
      sleep(DT)
   ).

%% request_params(+R:request(A), +T:mb_class, +O:list(option), +Decode:pred(+atom,+atom,+dom,-A), -T:list(atom), -P:list(param)) is det.
%
%  Takes a request and a list of Name=Value pairs and produces a URL path and list of parameters
%  for that request. Only options valid for the given request are permitted.
%
%  @throws unrecognised_options(Opts:list(option))
%  if the given request type does not recognise any of the supplied options.
request_params(lookup(Id),   C, O, doc_item,  [C,'/',Id], Params)  :- process_options([inc(C)],O,Params).
request_params(browse(Link), C, O, doc_items, [C], [LC=Id|Params]) :- 
   (Link=LC-Id; mb_id(Link,Id), mb_class(Link,LC)),
   process_options([inc(C),limit,offset],O,Params),
   insist(link(C,LC),invalid_link(C,LC)).
request_params(search(Query), C, O, doc_items, [C], [query=Q|Params]) :- 
   process_options([inc(C),limit,offset],O,Params),
   (  atom(Query) -> Q=Query 
   ;  string(Query) -> atom_string(Q,Query)
   ;  class_fields(C,Fields),
      lucene_codes(Query,[fields(Fields)],Cs), 
      atom_codes(Q,Cs)
   ).

% Convert list of valid Name=Value pairs and produce params for HTTP query.
process_options(ValidOpts,Opts,Params) :- process_options(ValidOpts,Opts,Params,[]).

process_options([],Opts) --> 
   ({Opts=[]} -> []; {throw(unrecognised_options(Opts))}).
process_options([Spec|SS],O1) -->
   ({opt(Spec,Param,O1,O2)} -> [Param];{O2=O1}),
   process_options(SS,O2).

opt(limit,limit=L) --> select_option(limit(L)), {must_be(between(1,inf),L)}.
opt(offset,offset=O) --> select_option(offset(O)), {must_be(between(0,inf),O)}.
opt(inc(C),inc=I) --> 
   % first get include, relation, and level-relation lists,
   % then translate these into MBZ include keywords and accumulate,
   % finally stick them all together with + (if not empty list).
   seqmap(select_list_option, [inc(Is),rels(Rs),lrels(LRs)]), 
   {phrase( seqmap(checked_seqmap,[inc(C),rel,lrel(C)],[Is,Rs,LRs]), Incs)},
   {Incs\=[], atomics_to_string(Incs,"+",I)}.

inc(C,I)  --> [I], { class_incs(C,Incs), insist(member(I,Incs),invalid_inc(C,I)) }.
rel(R)    --> [I], { insist(mb_class(R), invalid_rel(R)), string_concat(R,"-rels",I) }.
lrel(C,R) --> [I], { insist(C=release, invalid_level_rels),
                     insist(member(R,[recording,work]), invalid_level_rel(R)),
                     string_concat(R,"-level-rels",I) }.

select_list_option(Opt,O1,O2) :- select_option(Opt,O1,O2,[]).
checked_seqmap(P,L) --> {must_be(list,L)}, seqmap(P,L).

doc_item(xml,Class,[Root],Item) :- xpath(Root,Class,Item).
doc_item(json,Class,Dict,Dict) :- is_dict(Dict,Class).

doc_items(xml,Class,[Root],Total-Items) :-
   atom_concat(Class,'-list',ListElem),
   xpath(Root,ListElem,List),
   mb_facet(List,count(Total)),
   List=element(_,_,Items).

doc_items(json,Class,Dict,Total-Items) :-
   atom_concat(Class,'s',ItemsField),
   get_dict(count,Dict,Total),
   get_dict(ItemsField,Dict,Items),
   maplist(tag_dict(Class),Items).

tag_dict(Tag,Dict) :- is_dict(Dict,Tag).

% would like to use http_open, but it doesn't handle MBZ error documents properly.
get_doc(xml,URLSpec,Doc) :- 
   http_get([port(80)|URLSpec],Doc,[content_type('text/xml'),dialect(xml)]).

get_doc(json,URLSpec,Doc) :-
   setup_call_cleanup( 
      http_open(URLSpec,Stream,[request_header('Accept'='application/json')]),
      json_read_dict(Stream,Doc),
      close(Stream)).

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
   ;  print_message(warning,unrecognised_property(Spec)), fail
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
facet( barcode(Y), elem('barcode',_,X),  get_text(X,Y)).
facet( asin(Y),    elem('asin',_,X),     get_text(X,Y)).
facet( length(Y),  elem(length,   _, [X]), atom_number(X,Y)).
facet( credit(E),  elem('artist-credit',_, X),  xp(X,'name-credit'/artist,E)).
facet( text_repn(L,S), elem('text-representation',_,X), (xp(X,language,L),xp(X,script,S))).
facet( alias(Y),          elem('alias-list',_, X),    xp(X,alias(text),Y)).
facet( sort_name(Y),      elem('sort-name', _, X),    get_text(X,Y)).
facet( disambiguation(Y), elem(disambiguation, _, X), get_text(X,Y)).
facet( area(Id,Facets),   elem(area,As,Es),           get_area(As,Es,Id,Facets)).
facet( status(Y),    elem(status,  _, X),    get_text(X,Y)).
facet( packaging(Y), elem(packaging,  _, X), get_text(X,Y)).
facet( group(Y),     elem('release-group',As,Es), Y=element('release-group',As,Es)).
facet( language(L),  elem(language, _, [L]), true). 
facet( release(E),       elem('release-list',_, X),  xp(X,release,E)).
facet( release_event(Y), elem('release-event-list',_,X), xp(X,'release-event',Y)).
facet( medium(Y),        elem('medium-list',_,X), xp(X,'release-event',Y)).
facet( label_info(Y),    elem('label-info-list',_,X), xp(X,'label-info',Y)).
facet( label_code(Y),    elem('label-code',_,X), get_text(X,Y)).
facet( relation(E,R), elem('relation-list',As,Es), decode_relations(As,Es,E,R)).
facet( tags(Tags),    elem('tag-list',_,Es), maplist(get_tag,Es,Tags)).
facet( iswc(Y),    elem('iswc-list',_,X), xp(X,iswc(text),Y)).
facet( iswc(Y),    elem('iswc',_,X), get_text(X,Y)).
facet( isrc(Y),    elem('isrc-list',_,X), xp(X,isrc(text),Y)).
facet( isrc(Y),    elem('isrc',_,X), get_text(X,Y)).

get_tag(E,N-CC) :- 
   xpath(E,name(text),N),
   xpath(E,/self(@count),C), 
   atom_number(C,CC). 

decode_relations(As,Rs,E,Rel) :-
   member('target-type'=Type,As),
   member(R,Rs),
   xpath(R,/self(@type),Name),
   xpath(R,direction(content),[Dir]),
   xpath(R,Type,Val),
   (  Dir=backward -> Rel=..[Name,Val,E]
   ;  Dir=forward  -> Rel=..[Name,E,Val]
   ).

get_text(Elems,Text) :- xp(Elems,/self(text),Text).
get_area(As,Es,Id,F2) :-
   findall(F,mb_facet(element(area,As,Es),F),F1),
   select(id(Id),F1,F2).
xp(Elems,Selector,Val) :- xpath(element(e,[],Elems),Selector,Val).

%% mb_id(+E:element(_), -Id:atom) is semidet.
%  Short accessor for entity Id.
mb_id(E,Id) :- mb_facet(E,id(Id)).

%% mb_class(+E:element(_), -T:mb_class) is semidet.
%  Short accessor for entity class.
mb_class(element(T,_,_),T).

%% mb_id_uri(+T:mb_class,+ID:atom,-URI:atom) is det.
%% mb_id_uri(-T:mb_class,-ID:atom,+URI:atom) is semidet.
%
%  Gets the Musicbrainz URI for the given entity type and ID.
%  It can also work in reverse: given a URI, it can return the
%  entity type and ID.
mb_id_uri(Class,Id,URI) :- var(URI), !,
   format(atom(URI),'http://musicbrainz.org/~w/~w#_',[Class,Id]).
mb_id_uri(Class,Id,URI) :- 
   atomic_list_concat(['http:','','musicbrainz.org',Class,IdHash],'/',URI),
   atom_concat(Id,'#_',IdHash).

%% mb_uri(+E:element(_),-URI:atom) is det.
%  Get Musicbrainz URI for a given element. This can be used, for example,
%  to query the linkedbrainz.org SPARQL endpoint.
mb_uri(E,URI) :- 
   E=element(T,_,_),
   mb_facet(E,id(Id)), 
   mb_id_uri(T,Id,URI).

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

mb_non_core(rating).
mb_non_core(tag).
mb_non_core(collection).
mb_non_core(discid).
mb_non_core(isrc).
mb_non_core(iswc).

% For more convenient display of elements.
user:portray(E) :-
   E=element(T,_,_), mb_class(T), 
   mb_facet(E,id(Id)), !,
   (  mb_facet(E,name(Name)), truncate(40,Name,SName)
   -> format('<mb:~w/~w|~w>',[T,Id,SName])
   ;  mb_facet(E,title(Title)), truncate(40,Title,STitle)
   -> format('<mb:~w/~w|~w>',[T,Id,STitle])
   ;  format('<mb:~w/~w>',[T,Id])
   ).

% for dicts
user:portray(Dict) :-
   is_dict(Dict,T), nonvar(T), mb_class(T), 
   get_dict(id,Dict,Id), !,
   (  get_dict(name,Dict,Name), truncate(40,Name,SName)
   -> format('<mb:~w/~w|~w>',[T,Id,SName])
   ;  get_dict(title,Dict,Title), truncate(40,Title,STitle)
   -> format('<mb:~w/~w|~w>',[T,Id,STitle])
   ;  format('<mb:~w/~w>',[T,Id])
   ).

truncate(Max,S,S) :- string_length(S,L), L<Max, !.
truncate(Max,S1,S3) :-
   L is Max-3,
   sub_string(S1,0,L,_,S2),
   string_concat(S2,"...",S3).

% tables used for validating requests.
link(url,resource).
link(label,release).
link(C1,C2) :- links(C1,Cs), member(C2,Cs).
links(artist,[recording,release,'release-group',work]).
links(recording,[artist,release]).
links(release,[artist,label,recording,'release-group']).
links('release-group',[artist,release]).

class_fields( artist, 
   [  area,beginarea,endarea,arid,artist,artistaccent,alias,begin,comment
   ,  country,end,ended,gender,ipi,sortname,tag,type]).
class_fields('release-group', 
   [  arid,artist,artistname,comment,creditname,primarytype,rgid,releasegroup
   ,  releasegroupaccent,releases,release,reid,secondarytype,status,tag,type ]).
class_fields( release,
   [  arid,artist,artistname,asin,barcode,catno,comment,country,creditname
   ,  date,discids,discidsmedium,format,laid,label,lang,mediums,primarytype
   ,  puid,quality,reid,release,releaseaccent,rgid,script,secondarytype,status
   ,  tag,tracks,tracksmedium,type ]).
class_fields( recording,
   [  arid,artist,artistname,creditname,comment,country,date,dur,format,isrc
   ,  number,position,primarytype,puid,qdur,recording,recordingaccent,reid
   ,  release,rgid,rid,secondarytype,status,tid,tnum,tracks,tracksrelease
   ,  tag,type,video ]).
class_fields( label,
   [  alias,area,begin,code,comment,country,end,ended,ipi,label,labelaccent
   ,  laid,sortname,type,tag ]).
class_fields( work,
   [  alias,arid,artist,comment,iswc,lang,tag,type,wid,work,workaccent ]).
class_fields( annotation, [text,type,name,entity]).
class_fields('FreeDB', [artist,title,discid,cat,year,tracks]).

class_incs(artist, [recordings,releases,'release-groups',works]).
class_incs(label,  [releases]).
class_incs(recording, [artists,releases]).
class_incs(release, [artists,labels,recordings,'release-groups']).
class_incs('release-group',[artists,releases]).
class_incs(_,[discids,media,isrcs,'artist-credits','various-artists']).
class_incs(_,[aliases,annotation,tags,ratings,'user-tags','user-ratings']).

insist(G,Ex) :- call(G) -> true; throw(Ex).
prolog:message(unrecognised_class(C)) --> ["'~w' is not a recognised Musibrainz entity class."-[C]].
prolog:message(unrecognised_property(Spec)) --> ["No facet for property ~q"-[Spec]].
prolog:message(invalid_link(C1,C2)) --> ["Cannot browse class ~w via links to '~w'."-[C1,C2]].
prolog:message(invalid_inc(C,I)) --> ["~w in not a valid inc parameter for ~w resources."-[I,C]].
prolog:message(invalid_level_rels) --> ["Work- or recording-level relationships can only be requested for releases"].
prolog:message(invalid_level_rel(I)) --> ["~w relationships cannot be requested."-[I]].
prolog:message(mb_error(_,E)) --> {xpath(E,text(text),Text)}, ["MBZ error: ~w"-[Text]].
