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

:- module(sparql_dcg,[
      select//3
   ,  describe//1
   ,  describe//2
   ,  ask//1
	]).

/** <module> A simple DCG for generating a subset of SPARQL

   ==
   sparql_goal ---> (sparql_goal, sparql_goal) % conjunction
                  ; (sparql_goal; sparql_goal) % disjunction
                  ; rdf(resource,resource,object)
                  ; filter(cond)
                  .

   resource :< object. % any resource is an object

   literal(+literal)   :: object.  % any ground literal is an object
   atomic              :< literal. % any atomic can be a plain literal
   lang(atom,atom)     :: literal. % literal with language
   type(resource,atom) :: literal. % typed literal

   object   :< expr. % any object is an expr
   number   :< expr. % Prolog numerical values can also be expressions

   condition ---> (cond , cond)
                ; (cond ; cond)
                ; \+ cond
                ; expr == expr
                ; expr \= expr
                ; expr >= expr
                ; expr =< expr
                ; expr < expr
                ; expr > expr
                ; between(expr,expr,expr)
                ; in(object,list(object))
                ; regex(pattern,value)
                ; bound(object)
                ; blank(resource)
                ; uri(object)
                ; literal(object)
                .

   expr ---> expr + expr
           ; expr - expr
           ; expr * expr
           ; expr / expr
           ; +expr
           ; -expr
           ; str(expr)
           ; lang(expr)
           ; datatype(expr)
           .

   var ---> '$VAR'(integer)
          ; '@'        % anonymous blank node
          ; '@'(atom)  % nonymous blank node 
          .

   var :< resource
   var :< literal

   ==   

   Samer Abdallah, Dept. of Computer Science, UCL (2014)
 */

:- use_module(library(semweb/rdf_db), [rdf_global_object/2, rdf_global_id/2]).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- set_prolog_flag(double_quotes, codes).

%% select(+Vars:list(expr), +Goal:sparql_goal, +Options:list(option))// is det.
%
%  Any variables in the query must be represented by '$VAR'/1 terms
%  as generated by numbervars/3.
%
select(Vars,Goal,Options) -->
   "SELECT ", 
   if_option(distinct(Distinct), if(Distinct=true, " DISTINCT "),Options,O1),
   seqmap_with_sep(" ",expr,Vars), " ",
   where(Goal),
   if_option(order_by(OB), (" ORDER BY ", expr(OB)), O1,O2),
   if_option(limit(Limit), (" LIMIT ", at(Limit)), O2,O3),
   if_option(offset(Offs), (" OFFSET ", at(Offs)), O3,O4),
   {check_remaining_options(O4)}.

check_remaining_options([]) :- !.
check_remaining_options(Opts) :- throw(unrecognised_options(Opts)).

if_option(Opt,Phrase,O1,O2) -->
   ( {select_option(Opt,O1,O2)} -> call_dcg(Phrase); {O2=O1}).

%% ask(+Goal:sparql_goal)// is det.
%
%  Format an ASK query.
ask(Goal) --> "ASK ", brace(goal(Goal)).


%% describe(+Resource:resource)// is det.
%% describe(+Resource:resource,+Goal:sparql_goal)// is det.
describe(R) --> "DESCRIBE ", resource(R).
describe(RS,Goal) --> 
   "DESCRIBE ", 
   seqmap_with_sep(" ",resource,RS),
   where(Goal).

%% where(+Goal:sparql_goal)// is det.
where(Goal) --> "WHERE ", brace(goal(Goal)).

%% goal(+Goal)// is det.
goal(G1;G2)   --> brace(goal(G1)), " UNION ", brace(goal(G2)).
goal(\+G)     --> "FILTER NOT EXISTS ", brace(goal(G)). %NB consider MINUS { ... } also
goal((G1,G2)) --> goal(G1), " . ", goal(G2).
goal(conj(GS)) --> seqmap_with_sep(" , ",goal,GS).

goal(rdf(S,P,O)) -->
   { rdf_global_object(O,OO) },
   resource(S), " ",
   resource(P), " ",
   object(OO).

goal(filter(Cond)) --> "FILTER ", cond(Cond).

:- op(1150,fx,p).
p(X) --> paren(X).

cond(\+C)   --> p  "! ", cond(C).
cond((X,Y)) --> p cond(X), " && ", cond(Y).
cond((X;Y)) --> p cond(X), " || ", cond(Y).
cond(X==Y)  --> p expr(X), " = ", expr(Y).
cond(X\=Y)  --> p expr(X), " != ", expr(Y).
cond(X=<Y)  --> p expr(X), " <= ", expr(Y).
cond(X>=Y)  --> p expr(X), " >= ", expr(Y).
cond(X>Y)   --> p expr(X), " > ", expr(Y).
cond(X<Y)   --> p expr(X), " < ", expr(Y).
cond(between(L,U,X)) --> cond((L=<X,X=<U)).
cond(in(X,Ys))     --> p expr(X), " in ", (p seqmap_with_sep(", ",expr,Ys)).
cond(regex(P,V))   --> "regex(", object(V), ",", quote(at(P)), ")".
cond(regex(P,V,F)) --> "regex(", object(V), ",", quote(at(P)),  ",", quote(at(F)), ")".
cond(bound(V))     --> "bound(", object(V), ")".
cond(uri(V))       --> "isURI(", object(V), ")".
cond(blank(V))     --> "isBLANK(", object(V), ")".
cond(literal(V))   --> "isLITERAL(", object(V), ")".

expr(str(V))       --> "STR(", object(V), ")".
expr(lang(V))      --> "LANG(", object(V), ")".
expr(count(X))     --> "COUNT(", expr(X), ")".
expr(datatype(V))  --> "DATATYPE(", object(V), ")".

expr(+X) -->  p "+ ", expr(X), ")".
expr(-X) -->  p "- ", expr(X), ")".
expr(X+Y) --> p expr(X), " + ", expr(Y).
expr(X-Y) --> p expr(X), " + ", expr(Y).
expr(X*Y) --> p expr(X), " * ", expr(Y).
expr(X/Y) --> p expr(X), " / ", expr(Y).
expr(X) --> {number(X)}, at(X).
expr(X) --> object(X).

resource(R) --> variable(R).
resource(R) --> {rdf_global_id(R,RR)}, uri(RR).

object(literal(Lit)) --> literal(Lit).
object(Resource) --> resource(Resource).

literal(lang(Lang,Val)) --> quote(at(Val)), "@", at(Lang).
literal(type(Type,Val)) --> quote(wr(Val)), "^^", resource(Type).
literal(Lit) --> {atomic(Lit)}, quote(at(Lit)).

uri(U) --> {atom(U)}, "<", at(U), ">".
quote(P) --> "\"", escape_with(0'",0'\\,P), "\"".
variable(V)  --> {var_number(V,N)}, "?v", at(N).
variable(@V) --> "_:", {atomic(V) -> N=V; var_number(V,N)}, at(N).
variable(@)  --> "[]".

