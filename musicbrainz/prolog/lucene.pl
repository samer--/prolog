:- module(lucene, [	lucene//1, lucene/2 ]).

/** <module> A DCG for generating Lucene searches

	See https://lucene.apache.org/core/4_3_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package_description

   This code is very preliminary, for two reasons:

   Firstly, it cannot yet parse because (a) the grammar has a left-recursive production rule,
   and (b) deterministic parsing of operators with precedence rules
   combined with parentheses for disambiguation would 
   mean that generation is nondeterministic.

   Secondly, the Lucene grammar is just not making any sense
   to me at the moment. It seems very illogical and ill-defined.

   News just in: Lucene's parser is totally fucked up. Look at these--they show how
   various queries are parsed. Boolean operators get reduced to - and + in a _really_
   bizarre way.
   ==
   $ qlucene "(apple AND bear) OR (cherry AND donut) AND (echo OR jelly)"
   Searching for: (+apple +bear) +(+cherry +donut) +(echo jelly)

   $ qlucene "(apple AND bear) OR ((cherry AND donut) AND (echo OR jelly))"
   Searching for: (+apple +bear) (+(+cherry +donut) +(echo jelly))

   $ qlucene "((apple AND bear) OR (cherry AND donut)) AND (echo OR jelly)"
   Searching for: +((+apple +bear) (+cherry +donut)) +(echo jelly)

   $ qlucene "((apple AND bear) OR (cherry AND donut)) AND NOT (echo OR jelly)"
   Searching for: +((+apple +bear) (+cherry +donut)) -(echo jelly)

   $ qlucene "((apple AND bear) OR (cherry AND donut))  NOT (echo OR jelly)"
   Searching for: ((+apple +bear) (+cherry +donut)) -(echo jelly)
   ==
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- set_prolog_flag(double_quotes, codes).

lucene(E,String) :-
   (  var(String) -> phrase(lucene(E),Codes,[]), string_codes(String,Codes)
   ;  throw(error(lucene_parsing_not_supported)),
      string_codes(String,Codes), phrase(lucene(E),Codes,[])
   ).

% Bypass all processing, just spit out a literal atom or string
lucene(Atom) --> {atomic(Atom)}, !, atom(Atom).
lucene(Expr) --> query(Q).

query(Clauses) --> "(", seqmap_with_sep(" ",cl,Clauses), ")".

cl($(T))          --> word(T).
cl(F:V)           --> field_name(F), ":", query(V).
cl(re(Regexp))    --> "/", regexp(Regexp), "/".
cl(fuzzy(Word))   --> word(Word), "~".
cl(fuzzy(Word,P)) --> word(Word), "~", integer(P).
cl(prox(Words,D)) --> "\"", seqmap_with_sep(" ",word,Words), "\"", "~", integer(D).
cl(range_inc(Min,Max)) --> sqbr((word(Min), " TO ", word(Max))).
cl(range_exc(Min,Max)) --> brace((word(Min), " TO ", word(Max))).

cl(E^Boost) --> query(E), "^", number(Boost).
cl((E1,E2)) --> query(E1), " AND ", query(E2).
cl((E1;E2)) --> query(E1), " OR ", query(E).
cl(\+(E))   --> "NOT ", query(E).
cl(inc(E))  --> "+", cl(E).
cl(exc(E))  --> "-", cl(E).
cl(phr(Ws)) --> "\"", seqmap_with_sep(" ",word,Ws), "\"".

regexp(RE) --> 
   (  {var(RE)} 
   -> re_codes(Codes), {string_codes(RE,Codes)}
   ;  {string_codes(RE,Codes)}, re_codes(Codes)
   ).

word(W) --> 
   (  {var(W)} 
   -> word_codes(Codes), {\+keyword(Codes), string_codes(W,Codes)}
   ;  {string_codes(W,Codes), \+keyword(Codes)}, word_codes(Codes)
   ).

field_name(F) --> 
   (  {var(F)} 
   -> field_name_codes(Codes), {\+keyword(Codes), string_codes(F,Codes)}
   ;  {atom_codes(F,Codes), \+keyword(Codes)}, field_name_codes(Codes)
   ).

re_codes(Codes) --> escaped_codes(back_slash,"/\\",Codes).
word_codes([C1|Cs]) --> escaped_codes(back_slash," +-&|!(){}[]^\"~*?:\\",[C1|Cs]).
field_name_codes([C1|Cs]) --> escaped_codes(fail," +-&|!(){}[]^\"~*?:\\",[C1|Cs]).

keyword("OR").
keyword("AND").
keyword("NOT").
keyword("TO").

%% escaped_codes(+Esc:esc,+Special:list(code),+Codes:list(code))// is det.
%% escaped_codes(+Esc:esc,+Special:list(code),-Codes:list(code))// is nondet.
%
%  Parser for a sequence of characters. Characters in the Special list
%  are not allowed unless they are escaped.
%  Escape sequences are recognised by the predicate Esc, whose type is
%  ==
%  esc == pred(list(codes),list(codes))//.
%  ==
%  The DCG goal esc(H,T) matches an escape sequence and unifies H-T with
%  a difference list representing the codes in the escape sequence. esc//2
%  must not place any constraints on the difference list tail T..
%
%  Starts with the longest possible match and retrieves shorter
%  matches on backtracking.

escaped_codes(Esc,S,Cs) --> call(Esc,Cs,T), !, escaped_codes(Esc,S,T). 
escaped_codes(Esc,S,[C1|Cs]) --> [C1], { \+member(C1,S) }, escaped_codes(Esc,S,Cs). 
escaped_codes(_,_,[]) --> [].

%% back_slash(+H:list(code), @T:list(code))// is det.
%% back_slash(-H:list(code), @T:list(code))// is semidet.
%
%  DCG goal representing escape sequences consisting of a back-slash
%  followed by any character. 
back_slash([0'\\,C|T],T) --> [0'\\,C].

%% fail(@H,@T) is semidet.
%  goal representing no escape sequences: it always fails.
fail(_,_) --> {fail}.

