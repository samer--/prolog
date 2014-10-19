:- module(lucene, 
   [	lucene//1
   ,  lucene_codes/3
   ,  op(200,fy,@)
   ]).

/** <module> A DCG for generating Lucene searches

   Right. First off, forget everything you know about Lucene's search syntax.
   Especially the boolean operators, which do not work in any logical way.
   This library is based on a data type which, as far as I can determine,
   represents the internal structure of a Lucene query. Basically, a query
   is a triple of a modifier (Lucene +, -, or <none>), a numerical boost (Lucene ^
   operator), and a, for want of a better name, a 'part'. I could have called it
   a query 'component', but 'part' is a shorter word that means the same thing.
   A part is either a primitive
   term coupled with a field name( (:)/2 constructor), or a composite part 
   consisting of a list of sub-queries (comp/1 constructor). So, we have:
   ==
   :- type query ---> q(modifier,boost,part).
   :- type part  ---> comp(list(query))
                    ; field:prim.

   :- type modifier ---> plus, minus, none.
   :- type boost == nonneg.
   ==
   Note that the 'field' argument of the (:)/2 part constructor is inherently
   defaulty: if no field is specified, the search agent fills it in with an
   application specific default.

   The primitives cover all those obtainable using the Lucene syntax
   and are as follows:
   ==
   :- type prim  ---> word(word)           % bare, unquoted literal word
                    ; glob(pattern)        % word with wildcards * and ?
                    ; re(pattern)          % regular expression /.../
                    ; fuzzy(word,integer)  % fuzzy word match <...>~N
                    ; range_inc(word,word) % inclusive range [A TO B]
                    ; range_exc(word,word) % exclusive range {A TO B}
                    ; phrase(list(word),integer) % quoted multi word with slop
                    .
   ==

   Building queries out of these constructors is a bit of a chore, so next
   we have an term language and associated evaluator which takes an expression
   and produces a valid query term. This can be thought of as a set of functions
   which return queries. Every function in the language produces a value of type
   =|query|=. Some of them leave the field and modifier arguments unbound. If they
   are unbound at the end of the process, they take on default values. 
   The functions and literals are as follows:
   ==
   <any atomic literal> :: query    % primitive word with unbound modifier and field
   (@)  :: atomic -> query          % wildcard pattern with unbound modifier and field
   (\)  :: atomic -> query          % regular expression with unbound modifier and field
   (/)  :: atomic, number -> query  % fuzzy match with unbound modifier and field 
   (//) :: list(atomic), number -> query  % quoted phrase with unbound modifier and field
   (+)  :: atomic, atomic -> query  % inclusive range with unbound modifier and field
   (-)  :: atomic, atomic -> query  % exclusive range with unbound modifier and field
   (+)  :: query -> query           % unifies query modifier with plus
   (-)  :: query -> query           % unifies query modifier with minus
   (:)  :: atom, query              % unifies all field arguments recursively
   (^)  :: query, number            % multiplies boost factor
   list(query) :: query             % a list of queries evaluates to a composite query 
                                    % with unbound modifier.
   ==
   A few notes are in order. 

      1. Unlike Lucene's ~ postfix operator, the (/)/2 operator must have a number for
         the edit distance parameter. Lucene's default is 2.
      2. Unlike Lucene's bare quoted term, the (//)/2 must have a number to use as a 
         'slop' parameter. Supplying zero replicates Lucene's treatment of a bare quoted phrase.
      3. (+)/1 and (-)/1 cannot be composed: this is a contradiction that results in 
         error. (+)/2 and (-)/2 are both idempotent.
      4. Prolog's (-)/1 and (+)/2 operates bind tighter than (:)/2 operators, which is the
         wrong way round
         for Lucene queries: Prolog reads =|-F:E|= as =|(-F):E|=, but my expression language
         needs to see =|-(F:E)|=. Hence, there is little kludge in the evaluator to catch
         such terms and re-group the operators.
      5. Two different field names cannot both be applied to the primitive. Considering that
         the (:)/2 operator is applied recursively into sub-queries, this means that each
         node in the syntax tree can have at most one field name on the path from it to the root.
         This is different from Lucene's parser, which allows one field name to override
         another.
      6. I've taken the liberty of making multiple boosts on the same query combine multiplicatively,
         much like ordinary mathematical exponentiation. This is different from Lucene, where
         a later boost overrides an earlier boost. I think this way makes more sense.
   Thus, the type of such expression can be defined as:
   ==
   qexpr ---> @atomic; \atomic 
            ; atomic/number
            ; list(atomic)//number
            ; atomic + atomic
            ; atomic - atomic
            ; +qexpr ; -qexpr
            ; atom:qexpr
            ; qexpr^number
            ; list(qexpr)
            .
   atomic :< qexpr.  % any atomic is a qexpr
   query  :< qexpr.  % any query  is a qexpr
   ==

   ---+++ Quoting and escaping policy
   
   Ugh. Let us take the 4 different cases in turn:
      1. Plain search terms. The Prolog interface takes any sequence of characters
         and automatically escapes them for Lucene.
      2. Wildcard terms (@W). These can contain any characters but * and ? are 
         interpreted as wildcards. The can be escaped with \ to be interpreted
         literally. \ must be escaped with \. A single \ is illegal.
      3. Regexp terms (\RE). Any / characters are escaped automatically. Otherwise,
         Many characters have special meaning in regular expressions and must
         be escaped with \ to be inserted literally. Infact, ANY character can be
         escaped with \ and inserted literally. A single \ is illegal.
      4. Field names. These do not support escape sequences and cannot contain
         any special characters (ie " /+-&|!(){}[]^\"~?:\").

   So that's the basics of it. There might still be some problems in the DCG
   when it comes to handling character escapes. Somewhat suprisingly, the DCG
   seems to parse much of the Lucene query syntax more or less correctly,
   except for the boolean operators, which Lucene does not handle in any sensible way
   and are best avoided. Also, it does not parse field names applied to componound queries
   or the postfix '~' operator.

   See (though I warn you it will not be especially enlightening) 
      https://lucene.apache.org/core/4_3_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description
*/

:- meta_predicate bidi(?,//,0,?,?).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- set_prolog_flag(double_quotes, codes).

%% lucene_codes(+Q:qexpr, +Opts:options, -C:list(code)) is det.
%% lucene_codes(+Q:qexpr, -C:list(code)) is det.
%
%  Format a Lucene query. This predicate can accept a term of type
%  =|query|= or an expression of type =|qexpr|= and produces a query as a list
%  of character codes. It accepts the following options:
%
%  *  fields(+F:list(atom))
%     F is list of valid field names that can be used in the query. If absent,
%     then no field name checking is done; otherwise, an exception is thrown if 
%     an illegal field name is found in the query.
%
%  See lucene//1 for more details.
%
%  @throws failed(G) If an expression contains a type errors, or any contradictory
%  operators, G is the failing type check.
lucene_codes(E,Codes) :- lucene_codes(E,[],Codes).
lucene_codes(E,Opts,Codes) :-
   eval(E,Q),
   (option(fields(F),Opts) -> check_fields(F,Q); true),
   once(phrase(lucene(Q),Codes,[])).

eval(W,     q(_,1,_:word(W))) :- atomic(W).
eval(@(G),  q(_,1,_:glob(G))) :- insist(atomic(G)).
eval(\(RE), q(_,1,_:re(RE))) :- insist(atomic(RE)).
eval(W/S,   q(_,1,_:fuzzy(W,S))) :- atomic(W), insist(number(S)).
eval(Ws//D, q(_,1,_:phrase(Ws,D))) :- insist(maplist(atomic,Ws)), insist(number(D)).
eval(Min-Max, q(_,1,_:range_exc(Min,Max))) :- insist(atomic(Min)), insist(atomic(Max)).
eval(Min+Max, q(_,1,_:range_inc(Min,Max))) :- insist(atomic(Min)), insist(atomic(Max)).

eval(+F:E, C) :- eval(+(F:E),C). % kludge to fix operator precedence
eval(-F:E, C) :- eval(-(F:E),C). % kludge to fix operator precedence
eval(F:E, C) :- eval(E,C), apply_field(F,C).
eval(Es, q(_,1,comp(Cs2))) :- is_list(Es), maplist(eval,Es,Cs2).
eval(E1^B, q(M,B2,Q)) :- eval(E1,q(M,B1,Q)), B2 is B1*B.
eval(+E, q(plus,B,Q)) :- eval(E,q(M,B,Q)), insist(M=plus).
eval(-E, q(minus,B,Q)) :- eval(E,q(M,B,Q)), insist(M=minus).
eval(q(M,B,Q),q(M,B,Q)).

apply_field(F,q(_,_,F:_)).
apply_field(F,q(_,_,comp(Cs))) :- maplist(apply_field(F),Cs).


check_fields(Fields,q(_,_,Part)) :- check_part(Part,Fields).
check_part(comp(Queries),Fields) :- maplist(check_fields(Fields),Queries).
check_part(Field:_, Fields) :- insist((var(Field);member(Field,Fields)),invalid_field(Field)).

insist(G) :- insist(G,failed(G)).
insist(G,Ex) :- call(G) -> true; throw(Ex).

%% lucene(+Q:query)// is nondet.
%% lucene(-Q:query)// is nondet.
%
%  Top level DCG goal for Lucene queries. Can be non-deterministic in
%  either direction because of defaulty-ness, but usually, it is best to 
%  accept only the first result.
%  It can parse Lucene queries not involving AND, OR and NOT, but it cannot handle
%  fields applied to sub-queries (eg "name:(foo bar)"). You must distribute the
%  field over the contents of the sub-query (ie "(name:foo name:bar)").
lucene(Top) --> query(Top).

query(q(Mod,Boost,Part)) --> mod(Mod), part(Part), boost(Boost).

mod(none) --> "".
mod(plus) --> "+".
mod(minus) --> "-".

boost(Boost) --> {Boost=1}; "^", number(Boost).

part(default(_):Prim) --> prim(Prim).
part(Field:Prim)    --> field(Field), ":", prim(Prim).
part(comp(Clauses)) --> "(", seqmap_with_sep(" ",query,Clauses), ")".

prim(word(W))   --> word(W).
prim(glob(G))   --> bidi(G,esc(glob,C),string_codes(G,C)).
prim(re(RE))    --> "/", bidi(RE,esc(regexp,C),string_codes(RE,C)), "/".
prim(fuzzy(W,P)) --> word(W), "~", integer(P).
prim(range_inc(Min,Max)) --> sqbr((word(Min), " TO ", word(Max))).
prim(range_exc(Min,Max)) --> brace((word(Min), " TO ", word(Max))).
prim(phrase(Words,D)) --> 
   "\"", seqmap_with_sep(" ",word,Words), "\"",
   ( {D=0}; "~", integer(D)).

word(W) --> bidi(W,esc(word,C),string_codes(W,C)).
field(F) --> bidi(F,esc(field,C),string_codes(F,C)).

% beginnings of parsing ability...
% this orders the two goals depending on the instantiation state of Sem.
bidi(Sem,Phrase,Unify) -->
   {var(Sem)} -> Phrase, {Unify}; {Unify}, Phrase.

% -- escape sequence parsers for different entities

word([C|T],T) --> [0'\\,C], {member(C," /+-&|!(){}[]^\"~:\\*?")}. % auto escape these
word([C|T],T) --> [C], {\+member(C," /+-&|!(){}[]^\"~:\\*?")}. % pass the rest

glob([0'\\,C|T],T) --> [0'\\,C], {member(C,"*?\\")}. % manual escape these
glob([C|T],T) --> [0'\\,C], {member(C," /+-&|!(){}[]^\"~:")}. % auto escape these 
glob([C|T],T) --> [C], {\+member(C," /+-&|!(){}[]^\"~:\\")}. 

% escape sequences preserved in string, \ must be escaped,
% / auto escaped
regexp([0'\\,C|T],T) --> [0'\\,C].
regexp([0'/|T],T) --> "\\/".
regexp([C|T],T) --> [C], {\+member(C,"/\\")}.

% no escape sequences, no funny characters allowed.
field([C|T],T) --> [C], {\+member(C," /+-&|!(){}[]^\"~?:\\")}.

prolog:message(invalid_field(F)) --> 
   ['Fieldname ~w is not recognised in the current Lucene query context.'-[F]].
