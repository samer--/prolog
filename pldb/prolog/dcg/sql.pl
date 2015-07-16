:- module(sql,
      [  sql_tokenise/2
      ,  sql_generate/3, sql_generate/2
      ,  sql_parse/2
      ,  statement//1
      ,  expr//2
      ]).

/** <module> SQL DCG with semantic representation

   This module provides an SQL DCG based on that implemented in library(dcg/sql92),
   but with extra arguments to carry a semantic representation of the SQL syntax,
   as well a some other modifications. It is primarily intended for generating SQL
   statements, but it can also be used for parsing. 

   ---++ TYPES

   These types are specified using a mixture of algebraic date types (=|Type ---> Constructor, Constructor ... .|=),
   generalised algebraic data types for types with an open set of constructors
   (=|Constructor :: Type.|=), and type equivalences (=|Type == TypeExpression|=).
   Type expressions can involve undiscriminitated unions (=|Type == Type1 | Type2|=).
   Open unions can be expressed using subtype declarations (=|SubType :< SuperType|=).
   Finally, =|{A, B, C, ...}|= is shorthand for an anonymous type T defined as
   =|T ---> A; B; C; ... .|=.

   Transactions
   ==
   begin    :: statement.
   commit   :: statement.
   rollback :: statement.
   set_transaction(list(transaction_setting)) :: statement.

   transaction_setting ---> isolation_level(isolation_level)
                          ; writable(boolean)
                          ; diagnostics_size(simple_value). % non-negative integer

   isolation_level ---> read_committed; read_uncommitted; repeatable_read; serializable.
   ==

   Table and view creation
   ==
   drop_table(table_name) :: statement.
   create_table(table_name, list(table_element), maybe({delete,preserve})) :: statement.
   create_view(table_name, maybe(list(column_name)), query, maybe(maybe({cascaded,local}))) :: statement.
   set_constraints_mode(maybe(list(qualified_name)), {immediate,deferred}) :: statement.

   table_element ---> column(column_name, type, maybe(default),list(column_constraint), maybe(collation))
                    ; constraint(maybe(qualified_name),pair(list(column_name), constraint_spec),check_time).

   default ---> literal(literal(_)); null; current_user; session_user; system_user.

   column_constraint ---> constraint(maybe(qualified_name), 
                                     constraint_spec | {not_null}, 
                                     check_time, maybe(boolean)).

   constraint_spec ---> unique; primary_key; references(table_name, maybe(list(column_name))).

   check_time ---> maybe({immediate,deferred}).
   ==

   Queries
   ==
   ordered_query(query,maybe(list(ordering))) :: statement.
   select_into(list(target), select) :: statement. 

   select ---> select( maybe(quantifier),            % all or distinct
                       list(select_sublist),
                       maybe(list(table_reference)), % from 
                       maybe(condition),             % where
                       maybe(list(grouping)),        % group by
                       maybe(condition)).            % having

   quantifier ---> all; distinct.

   select_sublist ---> sel(expr(_),maybe(column_name))
                     ; all(table_name).

   where_spec ---> all; cond(condition); current(identifier).
   grouping    ==  pair(column_reference, maybe(collation)).
   ordering   ---> ord(column, maybe(collation), maybe({asc,desc})).
   column     ---> named(column_name)
                 ; numbered(natural).

   table_reference ---> op(join_op, table_reference, table_reference)
                      ; table(table_name, maybe(correlation_spec))
                      ; subquery(query,correlation_spec).

   join_op --> cross_join 
             ; join(join_type, maybe(join_spec)).

   op(setop, query, query)    :: query.
   values(list(expr(row(_)))) :: query.
   table(table_name)          :: query.
   query(query)               :: query.
   select                     :< query.

   setop ---> setop({union,except,intersect}, boolean, maybe(maybe(list(column_name))))

   join_type ---> inner; union; left; right; full.
   join_spec ---> on(condition); using(list(column_name)); natural.

   correlation_spec  == pair(identifier,maybe(list(column_name))).
   ==

   Data mutation
   ==
   update(table_name, list(set_clause), where_spec) :: statement.
   insert(table_name, insert_spec) :: statement.
   delete(table_name, where_spec) :: statement.

   set_clause ---> column_name=(expr(_) | {default}).
   insert_spec ---> defaults; query(maybe(list(column_name)), query).
   ==

   Conditions
   ==
   condition == expr(boolean).

   exists(query) :: condition.
   and(condition, condition) :: condition.
   or(condition, condition)  :: condition.
   not(condition)            :: condition.
   is(negatable_predicate,boolean) :: condition.

   cmp( comparison_op, expr(T), expr(T))                 :: condition.
   cmpq(comparison_op, expr(_), {all,some,any}, query)   :: condition.
   match(expr(_), boolean, maybe({partial,full}), query) :: condition.
   overlaps(expr(interval), expr(interval))              :: condition.

   expr(boolean)={true,false,unknown} :: negatable_predicate.
   null(expr(_))                      :: negatable_predicate.
   between(expr(T),expr(T),expr(T))   :: negatable_predicate.
   in(expr(T),in_spec(T))             :: negatable_predicate.

   like(expr(string(char)),expr(string(char)),expr(string(char))) :: negatable_predicate.

   comparison_op ---> '='; '<'; '>'; '<>'; '<='; '>='. 
   in_spec(T) ---> subquery(query); list(T).
   ==

   Values and expressions
   ==

   simple_value ---> param(identified)
                   ; embedded(identifier)
                   ; literal(literal(_)).

   general_value ---> param(identifier,maybe(identifier))
                    ; embedded(identifier,maybe(identifier)).
                    ; dynamic
                    ; current_user
                    ; session_user
                    ; system_user
                    ; user
                    ; value.

   literal(T)              :< expr(T).
   row(hlist(Types))       :: expr(row(Types)).
   gen(T,general_value)    :: expr(T).
   col(T,column_reference) :: expr(T).
   nullif(expr(T),expr(T)) :: expr(T).
   coalesce(list(expr(T))) :: expr(T).
   case(expr(S), list(when(expr(S),expr(T))), maybe(value(T))) :: expr(T).
   case(list(when(condition,expr(T))), maybe(value(T)))        :: expr(T).
   cast(expr(_),type) :: expr(_).
   aggr(aggregate) :: expr(numeric).
   fn(function(T)) :: expr(T).
   subquery(query) :: expr(_).

   expr(numeric) + expr(numeric) :: expr(numeric).
   expr(numeric) - expr(numeric) :: expr(numeric).
   expr(numeric) * expr(numeric) :: expr(numeric).
   expr(numeric) / expr(numeric) :: expr(numeric).

   + expr(numeric) :: expr(numeric).
   - expr(numeric) :: expr(numeric).

   expr(interval) + expr(datetime) :: expr(datetime).
   expr(datetime) + expr(interval) :: expr(datetime).
   expr(datetime) - expr(interval) :: expr(datetime).
   expr(interval) + expr(interval) :: expr(interval).
   expr(interval) - expr(interval) :: expr(interval).
   
   expr(numeric) * expr(interval) :: expr(interval).
   expr(interval) * expr(numeric) :: expr(interval).
   expr(interval) / expr(numeric) :: expr(interval).

   + expr(interval) :: expr(interval).
   - expr(interval) :: expr(interval).

   iq(interval_qualifier, interval) :: interval.
   iq(interval_qualifier, datetime, datetime) :: interval.
   at(time_zone,datetime)           :: datetime.

   concat(expr(string(T)), expr(string(T)))     :: expr(string(T)).
   collate(qualified_name, expr(string(char)))) :: expr(string(char)).

   when(T,R) ---> when(T,value(R)).

   time_zone ---> local; interval(expr(interval)).

   aggregate ---> count(maybe(pair(maybe(quantifier),expr(_))))
                ; agg({avg,max,min,sum}, pair(maybe(quantifier),expr(numeric))).
               
   length({char,character,octet,bit}, expr(string(_))) :: function(numeric).
   position(expr(string(char)), expr(string(char)))    :: function(string(char)).
   upper(expr(string(char)))                           :: function(string(char)).
   lower(expr(string(char)))                           :: function(string(char)).
   convert(expr(string(char)), qualified_name)         :: function(string(char)).
   translate( expr(string(char)), qualified_name)      :: function(string(char)).
   extract(dt_field | {timezone_hour, timezone_minute}, extract_source) :: function(numeric).
   substring(expr(string(T)), expr(string(T)), expr(string(T)))         :: function(string(T)).
   trim(maybe({leading,trailing,both}), maybe(expr(string(char))), expr(string(char))) :: function(string(char)). 

   extract_source ---> expr(datetime | interval). % datetime or interval
   ==

   Literals
   ==
   null                                     :: literal(_).
   unsigned(numeric)                        :: literal(numeric).
   signed({+,-},numeric)                    :: literal(numeric).
   string(T:{char,hex,bit},list(code))      :: literal(string(T)).
   timestamp(date,time,maybe(time_zone_interval)) :: literal(datetime).
   current_date                             :: literal(datetime).
   current_time(natural)                    :: literal(datetime).
   current_timestamp(natural)               :: literal(datetime).
   interval(maybe(sign), year_month | date_time, interval_qualifier) :: literal(interval).

   date :< literal(datetime).
   time :< literal(datetime).

   date    ---> date(integer,integer,integer).
   time    ---> time(integer,integer,integer,maybe(numeric)).
   numeric ---> int(integer); fixed(integer, natural); float(integer, integer).

   interval_qualifier ---> dt_field_with_precision - dt_field % !!! messy types here.
                         ; [dt_field_with_precision].

   dt_field ---> year; month; day; hour; minute; second; second(natural).
   dt_field_with_precision ---> dt_field ^ maybe(natural).
   ==

   Identifiers
   ==
   identifier       == atom | string.
   column_name      == identifier.
   table_name       == qualified_name | name_in_module.
   column_reference == qualified(table_name).
   collation        == qualified_name.
   qualified_name   == qualified(qualified(identifier)) % [[Catalog/]Schema/]Name
   qualified(T)     == identifier | slash_name(T). % name with optional qualifier of type T
   name_in_module  ---> module(identifier).
   slash_name(T)   ---> T/identifier.
   ==

   Basics
   ==
   maybe(X)  ---> nothing; just(X).
   pair(X,Y) ---> X-Y.
   boolean   ---> true; false.

   % heterogenous list type - needed for rows
   [T1|hlist(Ts)]  :: hlist([T1|Ts]).
   []              :: hlist([]).
   ==

   Tokens
   ==
   token ---> t(delimiter_class, token_class).

   delimiter_class ---> n; d.

   token_class ---> charset(maybe(list(code)),list(code))
                  ; string(string_class, list(code))
                  ; identifier(atom|string)
                  ; unsigned(numeric)
                  ; operator(atom).
   ==
 */

:- use_module(library(dcg_core), except([if//2, if//3])).
:- use_module(library(dcg_codes), except([comma//0, q//1, qq//1, paren//1])).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(snobol)).
:- use_module(library(dcg/algebra)).

:- op(900,fy,?).
:- op(900,xfy,?).
:- op(200,fy,@).

:- set_prolog_flag(double_quotes, codes).

:- meta_predicate
         ?(//,?,?)
      ,  *(//,?,?)
      ,  +(//,?,?)
      ,  +(//,//,?,?)
      ,  clist(//,?,?)
      ,  paren(//,?,?)
      ,  maybe_list(:,-,?,?)
      ,  >(//,+,-,?,?)
      .

% -- Meta rules --

>(Phrase,Sem,Sem) --> call_dcg(Phrase).
?(Phrase) --> []; call_dcg(Phrase).
+(Phrase) --> call_dcg(Phrase); call_dcg(Phrase), +(Phrase).
*(Phrase) --> []; +(Phrase). 
+(Sep,Phrase) --> call_dcg(Phrase); call_dcg(Phrase), call_dcg(Sep), +(Sep,Phrase).
% +(Sep,Phrase) --> call_dcg(Phrase), *((Sep,Phrase)).

% greedy versions for parsing
**(Phrase) --> ++(Phrase); [].
++(Phrase) --> call_dcg(Phrase), (++(Phrase);[]).
++(Sep,Phrase) --> call_dcg(Phrase), (++((Sep,Phrase));[]).

nothing ? _ --> [].
just(Sem) ? Phrase --> call(Phrase,Sem).

if(Bool,Phrase) --> if(Bool,Phrase,[]).

if(false,_,P2) --> call_dcg(P2).
if(true,P1,_) --> call_dcg(P1).

maybe(_,_,nothing) --> [].
maybe(T,Phrase,just(T)) --> call_dcg(Phrase).

maybe_list(_,[]) --> [].
maybe_list(M:[Head^P1|Ps],[Head|Tail]) --> call_dcg(M:P1), maybe_list(M:Ps,Tail).

opt_default(_,Default,Default) --> [].
opt_default(Pred,_,X) --> call(Pred,X).

*(Phrase,Things) --> seqmap(Phrase,Things).
clist(Phrase,Things) --> seqmap_with_sep(comma,Phrase,Things).
clist(Phrase,Xs,Ys) --> seqmap_with_sep(comma,Phrase,Xs,Ys).

clist(Phrase) --> +(comma,Phrase).
paren(Phrase) --> o('('), Phrase, o(')').

term_expansion(set_members(Name,Items),Clauses) :- seqmap(set_member(Name),Items,Clauses,[]).
set_member(Name,Item) --> [Clause], {Clause =.. [Name,Item]}.

% ================ STATEMENTS ================

statements(XX) --> seqmap_with_sep(o(;),statement,XX).

%% statement(+S:statement)// is det.
%% statement(-S:statement)// is nondet.
%
%  Parse or generate a sequence of tokens for an SQL statement.
statement(ordered_query(Q,Order)) --> query_expr(Q), Order?order_by_clause.
statement(delete(T,WhereSpec)) --> @delete, @from, table_name(T), where_spec(WhereSpec).
statement(insert(T,InsertSpec)) --> @insert, @into, table_name(T), insert_spec(InsertSpec).
statement(update(T,Cs,WhereSpec)) --> @update, table_name(T), @set, clist(set_clause,Cs), where_spec(WhereSpec).

statement(begin) --> @begin.
statement(commit) --> @commit, ? @work.
statement(rollback) --> @rollback, ? @work.
statement(set_transaction(Settings)) --> @set, @transaction, clist(transaction_setting,Settings).

statement(open_cursor(X)) --> @open, cursor_name(X).
statement(close_cursor(X)) --> @close, cursor_name(X).
statement(fetch(X,Orient,Targets)) -->
   @fetch, Orient?fetch_orientation, @from, cursor_name(X), @into, clist(target_spec,Targets).

statement(drop_table(T)) --> @drop, @table, table_name(T).
statement(create_table(T,Elements,OnCommit)) -->
   @create, @table, table_name(T), paren(clist(table_element,Elements)),
   maybe(X, (@on, @commit, @([delete,preserve],X), @rows), OnCommit).
statement(create_view(T,ColSpec,Q,CheckSpec)) -->
   @create, @view, table_name(T),
   maybe(Columns, column_names(Columns), ColSpec),
   @(as), query_expr(Q),
   maybe(CTO, (@with, maybe(CT, @([cascaded,local],CT), CTO), @check, @option), CheckSpec).
statement(set_constraints_mode(CSpec,Mode)) -->
   @set, @constraints,
   CSpec ? clist(constraint_name),
   @([deferred,immediate],Mode).
statement(declare_cursor(CN,Insensitive,Scroll,Statement)) -->
   @declare, cursor_name(CN),
   if(Insensitive, @insensitive),
   if(Scroll, @scroll),
   @cursor, @for, cursor_statement(Statement).

statement(select_into(Targets,Select)) --> select(just(Targets),Select).

collate_clause(X) --> @collate, qualified_name(X).

table_element(column(CN,TorD,Default,Constraints,Collation)) -->
   column_name(CN), type_or_domain(_,TorD),
   Default ? default_clause,
   seqmap(column_constraint_defn,Constraints),
   Collation ? collate_clause.

table_element(constraint(Name,Constraint,CTS)) -->
   Name ? constraint_name_defn,
   table_constraint(Constraint),
   CTS ? constraint_check_time.

type_or_domain(TC,T) --> data_type(TC,T).
type_or_domain(_,D) --> domain_name(D).

default_clause(D) --> @default, default_option(D).
default_option(X) --> literal(_,X).
default_option(X) --> @X, {member(X,[user, current_user, session_user, system_user])}. % null is now literal

column_constraint_defn(constraint(CN,Constraint,CheckTime,Deferrable)) -->
   CN ? constraint_name_defn,
   column_constraint(Constraint),
   constraint_attributes(CheckTime,Deferrable).

constraint_name_defn(X) --> @constraint, constraint_name(X).
constraint_name(X) --> qualified_name(X).

column_constraint(not_null) --> @not, @null.
column_constraint(Constraint) --> unique_spec(Constraint).
column_constraint(references(T,ColSpec))--> references_spec(T,ColSpec).

constraint_attributes(nothing,nothing) --> [].
constraint_attributes(just(CT),Deferrable) --> constraint_check_time(CT), Deferrable ? deferrable.
constraint_attributes(CheckTime,just(D)) --> deferrable(D), CheckTime ? constraint_check_time.

deferrable(D) --> if(D,[],@not), @deferrable.

constraint_check_time(CT) --> @initially, (@immediate;@deferred) // @CT.

table_constraint(Cs-Constraint) --> unique_spec(Constraint), column_names(Cs).
table_constraint(Cs-references(T,ColSpec)) --> @foreign, @key, column_names(Cs), references_spec(T,ColSpec).

references_spec(T,ColSpec) --> @references, table_name(T), maybe(Cs,column_names(Cs),ColSpec).
unique_spec(primary_key) --> @primary, @key.
unique_spec(unique) --> @unique.

% -- Data modification ----
insert_spec(defaults) --> @default, @values.
insert_spec(query(Columns,Q)) -->
   maybe(Cs, column_names(Cs), Columns),
   query_expr(Q).

where_spec(all) --> [].
where_spec(cond(Cond)) --> where_clause(Cond).
where_spec(current(Cursor)) --> @where, @current, @of, cursor_name(Cursor).

set_clause(C=V) --> column_name(C), o(=), set_value(V).
set_value(X) --> expr(_,X).
set_value(default) --> @default.


% -- Query ----
where_clause(Cond) --> @where, condition(Cond).
order_by_clause(Specs) --> @order, @by, clist(sort_spec,Specs).
sort_spec(ord(Column,Collation,Direction)) -->
   (  {Column=named(C)}, column_name(C)
   ;  {Column=numbered(N)}, unsigned_int(N)
   ),
   Collation ? collate_clause,
   Direction ? @[asc,desc].

subquery(Q) --> paren(query_expr(Q)).
query_expr(Q) --> algebra([in(y,x)-setop(2), in(y,x)-setop(1)],query_primary,Q).
setop(L,setop(Op,All,CS)) --> @Op, {setop(L,Op)}, if(All,@all), CS?corresponding_spec.

setop(2,union).
setop(2,except).
setop(1,intersect).

query_primary(S)            --> select(nothing,S).
query_primary(values(Vals)) --> @values, clist(expr(row(_)),Vals). 
query_primary(table(T))     --> @table, table_name(T).
% query_primary(join(J))    --> joined_table(J). 
query_primary(Q)            --> paren(query_expr(Q)).

table_reference(X) --> table_expr(tref(_),X).
table_expr(T,X) --> typed_algebra(1,table_op,table_primary,T,X).

table_op(1, in(y<S1:tref(_),y<S2:tref(_)), cross_join(S1,S2):tref(joined), (@cross,@join)).
table_op(1, custom(PA), join(JT,JS,S1,S2):tref(joined), join(PA,JT,JS,S1,S2)).

join(PA,JT,JS,T1,T2) --> 
   call(PA,y<T1:tref(_)),
   (  join_op(JT),
      call(PA,y<T2:tref(_)),
      JS?join_spec
   ;  {JS=just(natural)}, @natural, join_op(JT),
      opt_default(join_type,inner,JT), @join,
      call(PA,y<T2:tref(_))
   ).

table_op(1,in(y:tref(_),y:tref(_)),tref(joined),cross_join) --> @cross, @join.
table_op(1,custom(PA),tref(joined),op(join(JT,JS),T1,T2)) --> 
   call(PA,y,tref(_),T1),
   (  join_op(JT),
      call(PA,y,tref(_),T2),
      JS?join_spec
   ;  {JS=just(natural)}, @natural, join_op(JT),
      call(PA,y,tref(_),T2)
   ).

join_op(JT) --> opt_default(join_type,inner,JT), @join.

join_type(X) --> @([inner,union],X).
join_type(X) --> @([left,right,full],X), ? @outer.
join_spec(on(Cond)) --> @on, condition(Cond).
join_spec(using(Cs)) --> @using, column_names(Cs).

table_primary(tref(primary),table(T,CorrSpec)) --> table_name(T), CorrSpec ? correlation_spec.
table_primary(tref(primary),subquery(Q,CorrSpec)) --> subquery(Q), correlation_spec(CorrSpec).
table_primary(tref(joined),J) --> paren(table_expr(tref(joined),J)).

correlation_spec(ID-ColSpec) --> ? @(as), identifier(ID), {\+reserved(ID)}, maybe(Cs,column_names(Cs), ColSpec).
corresponding_spec(ColSpec) --> @corresponding, maybe(Cs, (@by, column_names(Cs)), ColSpec).

% incomplete list, enough to avoid incorrect parses of join expressions
% due to reserved words being parsed as correlation names.
set_members(reserved,[outer,inner,left,right,full,union,natural,using,on,as]).

select(Targets,select(Quant,SList,From,Cond,GroupBy,Having)) -->
   @select, Quant ? set_quantifier,
   select_list(Targets,SList),
   maybe(Ts, (@from, clist(table_reference,Ts)), From),
   Cond ? where_clause,
   maybe(Gs, (@group, @by, clist(grouping_spec,Gs)), GroupBy),
   maybe(C, (@having, condition(C)), Having).

select_list(nothing,[]) --> star.
select_list(nothing,SList) --> clist(select_sublist,SList).
select_list(just(Targets),SList) -->
   clist(select_sublist,SList),
   @into, clist(target_spec,Targets).

set_quantifier(Q) --> @([all,distinct],Q).
select_sublist(sel(X,CN)) --> expr(_,X), maybe(C, (@(as), column_name(C)), CN).
select_sublist(all(Q)) --> table_name(Q), dot, star.

grouping_spec(C-Collation) -->
   column_reference(C),
   Collation ? collate_clause.

target_spec(param(X,Ind)) --> with_indicator(param_name,X,Ind).
target_spec(embedded(X,Ind)) --> with_indicator(embedded_variable_name,X,Ind).

% ------------------ CURSORS ------------------
cursor_statement(query(Q,Order,Updatability)) --> cursor_spec(Q,Order,Updatability).
cursor_statement(statement(N)) --> identifier(N).

cursor_spec(Q,Order,Updatability) --> query_expr(Q), Order?order_by_clause, Updatability?updatability_clause.
updatability_clause(U) -->
   @for,
   (  {U=read_only}, @read, @only
   ;  {U=update(CSpec)}, @update,
      maybe(Cs, (@of, clist(column_name,Cs)), CSpec)
   ).
fetch_orientation(X) --> @([next,prior,first,last],X).
fetch_orientation(abs(X)) --> @absolute, simple_val(X).
fetch_orientation(rel(X)) --> @relative, simple_val(X).
cursor_name(X) --> identifier(X).

% ------------------ TRANSACTIONS --------------------
transaction_setting(isolation_level(IL)) --> @isolation_level, isolation_level(IL).
transaction_setting(writable(Bool))      --> @read, if(Bool, @write, @only).
transaction_setting(diagnostics_size(X)) --> @diagnostics, @size, simple_val(X).

isolation_level(read_committed)   --> @read, @committed.
isolation_level(read_uncommitted) --> @read, @uncommitted.
isolation_level(repeatable_read) --> @repeatable, @read.
isolation_level(serializable)    --> @serializable.

% ======================== EXPRESSIONS =====================
condition(Cond) --> expr(boolean,Cond). 

expr(T,X) --> typed_algebra(9,expr_op,primary,T,X).
expr(L,T,X) --> typed_algebra(L,expr_op,primary,T,X).

% operator database for typed_algebra//5, also extended with boolean operators and predicates
expr_op(9, in(y<S1:boolean,x<S2:boolean), or(S1,S2):boolean, @or).
expr_op(8, in(y<S1:boolean,x<S2:boolean), and(S1,S2):boolean, @and).
expr_op(7, pre(x<S1:boolean),     not(S1):boolean, @not).
expr_op(6, post(x<S1:boolean),    is(S1=Val,Pos):boolean, boolean_test(is(Val,Pos))).

expr_op(5, in(x<S1:T,x<S2:T),       overlaps(S1,S2)    :boolean, @overlaps) :- T=interval.
expr_op(5, in(x<S1:T,x<S2:T),       cmp(O,S1,S2)       :boolean, comp_op(O)).
expr_op(5, post(x<S1:_),            cmpq(O,S1,Quant,Q) :boolean, query_comparison(O,Quant,Q)).
expr_op(5, post(x<S1:_),            is(null(S1),Pos)   :boolean, (@(is), neg(Pos), @null)).
expr_op(5, post(x<S1:T),            is(between(L,U,S1) ,Pos):boolean, between(T,L,U,Pos)).
expr_op(5, post(x<S1:T),            is(in(S1,Set),Pos) :boolean, (neg(Pos), @in, in_spec(T,Set))).
expr_op(5, post(x<S1:_),            match(S1,U,P,Q)    :boolean, match(U,P,Q)).
expr_op(5, post(x<S1:string(char)), is(like(S1,Y,Esc),Pos):boolean, like(Y,Esc,Pos)).

expr_op(4, in(y<S1:numeric,x<S2:numeric), (S1+S2):numeric, plus).
expr_op(4, in(y<S1:numeric,x<S2:numeric), (S1-S2):numeric, minus).
expr_op(3, in(y<S1:numeric,x<S2:numeric), (S1*S2):numeric, star).
expr_op(3, in(y<S1:numeric,x<S2:numeric), (S1/S2):numeric, slash).
expr_op(2, pre(x<S1:numeric), -S1:numeric, minus).
expr_op(2, pre(x<S1:numeric), +S1:numeric, plus).

expr_op(2, in(x<S1:string(T),y<S2:string(T)),concat(S1,S2):string(T),concat).
expr_op(1, post(x<S1:string(char)),collate(C,S1):string(char),collate_clause(C)).

expr_op(4, in(y<S1:interval,x<S2:datetime), (S1+S2):datetime, plus).
expr_op(4, in(y<S1:datetime,x<S2:interval), (S1+S2):datetime, plus).
expr_op(4, in(y<S1:datetime,x<S2:interval), (S1-S2):datetime, minus).
expr_op(4, in(y<S1:interval,x<S2:interval), (S1+S2):interval, plus).
expr_op(4, in(y<S1:interval,x<S2:interval), (S1-S2):interval, minus).
expr_op(4, in(y<S1:datetime,x<S2:datetime), (S1-S2):preival, minus).

expr_op(3, in(x<S1:numeric,y<S2:interval), S1*S2:interval, star).
expr_op(3, in(y<S1:interval,x<S2:numeric), S1*S2:interval, star).
expr_op(3, in(y<S1:interval,x<S2:numeric), S1/S2:interval, slash).
 
expr_op(2, pre(x<S1:interval),    -S1:interval, minus).
expr_op(2, pre(x<S1:interval),    +S1:interval, plus).
expr_op(1, post(x<S1:interval),   iq(Q,S1):interval,        interval_qualifier(Q)).
expr_op(1, post(x<D1-D2:preival), ival(Q,D1,D2):interval,   interval_qualifier(Q)).
expr_op(1, post(x<S1:datetime),   at(TZ,S1):datetime, (@at, time_zone_spec(TZ))).

boolean_test(is(Val,Pos)) --> @(is), neg(Pos), @([true,false,unknown],Val).
query_comparison(O,Quant,Q) --> comp_op(O), @([all,some,any],Quant), subquery(Q).
between(T,L,U,Pos) --> neg(Pos), @between, expr(4,T,L), @and, expr(4,T,U).
like(Y,Esc,Pos) -->
   neg(Pos), @like, expr(2,string(char),Y), 
   maybe(E, (@escape, expr(2,string(char),E)), Esc).
match(Unique,Partiality,Q) -->
   @match, if(Unique, @unique), 
   Partiality ? @[partial,full], 
   subquery(Q).
in_spec(_,subquery(Q)) --> subquery(Q).
in_spec(T,list(Items)) --> paren(clist(expr(T),Items)).
comp_op(O) --> o(O), {member(O,['=','<>','<','>','<=','>='])}.
neg(Pos) --> if(Pos, [], @not).

time_zone_spec(local) --> @local.
time_zone_spec(interval(X)) --> @time, @zone, expr(interval,X).

primary(T,X)           --> {dif(T,signed)}, literal(T,X).
primary(T,gen(T,X))    --> general_val(X).
primary(T,col(T,X))    --> column_reference(X).
primary(T,X)           --> case(T,X).
primary(T,nullif(X,Y)) --> @nullif, paren((expr(T,X), comma, expr(T,Y))).
primary(T,coalesce(X)) --> @coalesce, paren(clist(expr(T),X)).
primary(T,cast(X,DT))  --> cast(T,X,DT).
primary(T,aggr(X))     --> set_function(T,X).
primary(T,fn(X))       --> function(T,X).
primary(row(Types),row(Vals)) --> paren(clist(expr,Types,Vals)).
primary(boolean,exists(X))    --> @exists, subquery(X).
primary(T,X)                  --> paren(expr(T,X)).
primary(_,subquery(X))        --> subquery(X).

% -- date/time --------------------
interval_qualifier(X-Y) --> start_field(X), @to, end_field(Y).
interval_qualifier(Q) --> datetime_field_with_precision(Q).
interval_qualifier(second(FPrec)^just(LPrec)) --> @second, paren((unsigned_int(LPrec),comma,unsigned_int(FPrec))).

start_field(F^LPrec)      --> {dif(F,second)}, datetime_field_with_precision(F^LPrec).
end_field(second(FPrec))  --> @second, paren(unsigned_int(FPrec)).
end_field(Field)          --> datetime_field(Field).
datetime_field_with_precision(F^LP) --> datetime_field(F), maybe(P,paren(unsigned_int(P)),LP).
datetime_field(F) --> @([year,month,day,hour,minute,second],F).

% -------------------- FUNCTIONS ---------------------
function(string(T),substring(X,Y,N)) --> 
   @substring, paren((expr(string(T),X), @from, expr(numeric,Y), @for, expr(numeric,N))).
function(string(char),upper(X)) --> @upper, paren(expr(string(char),X)).
function(string(char),lower(X)) --> @lower, paren(expr(string(char),X)).
function(string(char),convert(Y,X)) --> @convert, paren((expr(string(char),Y), @using, qualified_name(X))).
function(string(char),translate(Y,X)) --> @translate, paren((expr(string(char),Y), @using, qualified_name(X))).
function(string(char),trim(Spec,Char,X)) --> @trim, paren((trim_params(Spec,Char),expr(string(char),X))).
function(numeric,position(X,Y)) --> @position, paren((expr(string(char),X), @in, expr(string(char),Y))).
function(numeric,extract(Field,Source)) -->  @extract, paren((extract_field(Field), @from, extract_source(Source))).
function(numeric,length(CT,X)) --> @([char,character,octet,bit],CT), @length, paren(expr(string(_),X)).

extract_field(F) --> datetime_field(F); @([timezone_hour,timezone_minute],F).
extract_source(X) --> expr(T,X), {T=datetime;T=interval}.

trim_params(nothing,nothing) --> [].
trim_params(Spec,Char) -->
   Spec ? @[leading,trailing,both], 
   Char ? expr(string(char)),
   @from.

% -- Aggregate functions -----------
set_function(numeric,count(Spec)) --> @count, paren(({Spec=nothing}, star; {Spec=just(Q)}, quant(Q))).
set_function(T,agg(F,Q)) --> @([avg,max,min,sum],F), paren(quant(T,Q)).

quant(T,qual(Q,X)) --> Q?set_quantifier, expr(T,X).

cast(T,X,TorD) --> @cast, paren((expr(_,X), @(as), type_or_domain(T,TorD))). % includes null

case(T,case(X,Whens,Else)) -->
   @case, expr(TX,X), 
   seqmap(when_clause(TX,T),Whens),
   maybe(Y, else(expr(T,Y)), Else), 
   @end.

case(T,case(Whens,Else)) -->
   @case, 
   seqmap(when_clause(T),Whens),
   maybe(Y, else(expr(T,Y)), Else), 
   @end.

when_clause(TX,TR,when(X,R)) --> when(expr(TX,X),expr(TR,R)). 
when_clause(TR,when(C,R))  --> when(condition(C),expr(TR,R)). 

when(Phrase,Result) --> @when, Phrase, @then, Result.
else(Result) --> @else, Result.

% ------------------- IDENTIFIERS -------------------
table_name(X) --> qualified_name(X).
table_name(module(X)) --> @module, dot, identifier(X).
column_name(X) --> identifier(X).
column_reference(Y) --> column_name(Y).
column_reference(X/Y) --> table_name(X), dot, column_name(Y).
qualified_name(Y) --> {identifier(Y)}, identifier(Y).
qualified_name(X/Y) --> schema_name(X), dot, identifier(Y).
schema_name(Y) --> {identifier(Y)}, identifier(Y).
schema_name(X/Y) --> identifier(X), dot, identifier(Y).
identifier(X) --> t(identifier(X)).

column_names(Cs) --> paren(clist(column_name,Cs)).
% --------------------- LITERALS ---------------------
literal( numeric,   unsigned(X)) --> t(unsigned(X)).
literal( signed,    signed(S,X)) --> sign(S), t(unsigned(X)).
literal( string(T), string(T,X)) --> t(string(T,X)).
literal( datetime,  date(Y,M,D))        --> @date, qt(date(Y,M,D)).
literal( datetime,  time(H,M,S,FS))     --> @time, qt(time(H,M,S,FS)).
literal( datetime,  timestamp(D,T,TZ))  --> @timestamp, qt(date_time_tz(D,T,TZ)).
literal( interval,  interval(Sign,X,Q)) --> @interval, Sign?sign, qt(year_month_or_day_time(X)), interval_qualifier(Q).
literal( datetime,  current_date)         --> @current_date.
literal( datetime,  current_time(N))      --> @current_time, paren(unsigned_int(N)).
literal( datetime,  current_timestamp(N)) --> @current_timestamp, paren(unsigned_int(N)).
literal( boolean,   true)  --> @true.
literal( boolean,   false) --> @false.
literal( _,         null). % !!! I have added this to simplify elsewhere.

unsigned_int(X)--> t(unsigned(int(X))).
qt(Phrase) --> string(char,Phrase).

year_month_or_day_time(year_month([Y|Parts])) --> 
   years(Y), maybe_list([ M^("-", months(M))], Parts).
year_month_or_day_time(day_time([D|Parts])) --> 
   days(D), 
   maybe_list([ H^(" ",hours(H)), M^(":",minutes(M)), S^(":",seconds(S))], Parts).

date_time_tz(date(Y,M,D),time(H,MM,S,FS),TZ) --> date(Y,M,D), " ", time(H,MM,S,FS), TZ?time_zone_interval.
time_zone_interval(tz(S,H,M)) --> sign_codes(S), hours(H), ":", minutes(M).
date(Y,M,D) --> years(Y), "-", months(M), "-", days(D).
time(H,M,S,FS) --> hours(H), ":", minutes(M), ":", seconds(S), FS?fraction.
years(Y)   --> unsigned_int_codes(Y).
months(M)  --> unsigned_int_codes(M).
days(D)    --> unsigned_int_codes(D).
hours(H)   --> unsigned_int_codes(H).
minutes(M) --> unsigned_int_codes(M).
seconds(S) --> unsigned_int_codes(S).
fraction(fixed(D,S)) --> peek("."), token(_,unsigned(fixed(D,S))).

unsigned_int_codes(X) --> 
   (  {nonvar(X)} 
   -> {number_codes(X,Codes)}, digits(Codes)
   ;  digits(Codes), {number_codes(X,Codes)}
   ).

% -- HOST LANGUAGE INTERFACE -------

simple_val(param(X)) --> param_name(X).
simple_val(embedded(X)) --> embedded_variable_name(X).
simple_val(literal(X)) --> literal(_,X). % !!! can this be NULL?

general_val(param(X,Ind)) --> with_indicator(param_name,X,Ind).
general_val(embedded(X,Ind)) --> with_indicator(embedded_variable_name,X,Ind).
general_val(dynamic) --> o(?).
general_val(X) --> @X, {member(X,[user,value,current_user,session_user,system_user])}.

with_indicator(Phrase,Var,MaybeInd) --> call(Phrase,Var), maybe(Ind,(@indicator,call(Phrase,Ind)),MaybeInd).
param_name(X) --> colon, identifier(X).
embedded_variable_name(X) --> colon, host_identifier(X).
domain_name(X)     --> identifier(X).
host_identifier(X) --> identifier(X).

% ------------- BASICS -------------
comma --> o(',').
star  --> o(*).
slash --> o(/).
plus  --> o(+).
minus --> o(-).
colon --> o(:).
dot   --> o(.).
concat --> o('||').
sign  --> plus; minus.
sign(+) --> plus.
sign(-) --> minus.


o(Op)    --> [t(d,operator(Op))].
@Word    --> [t(n,identifier(Word))].
@(Keywords,K) --> {member(K,Keywords)}, @K.
string(T,Phrase) --> [t(_,string(T,Codes))], {freeze(Codes,phrase(Phrase,Codes))}.
t(Class) --> [t(_,Class)].

% ============== TOKENISATION ===========

%% sql_tokenise(+Input:string, -Tokens:list(token)) is det.
%% sql_tokenise(+Input:list(code), -Tokens:list(token)) is det.
%
%  Take a string or list of codes and tokenise to produce a list of SQL tokens.
sql_tokenise(Input,Tokens) :- 
   (  string(Input) -> string_codes(Input,Codes) ; Codes=Input),
   phrase(tokens(parse,Tokens),Codes), !.

% tokens(+Pred:phrase(delim_class,token,string,list(codes)), +Tokens:list(token))// is det.
% tokens(+Pred:phrase(delim_class,token,string,list(codes)), -Tokens:list(token))// is det.
%
% NB., when processing a list of tokens, we cut immediately after each token since:
% (a) when parsing, the first parse is the only correct one
% (b) when generating, the first success is the canonical one.
tokens(Pred,[t(D1,T1)|Ts]) --> ?separator, call(Pred,D1,T1), !, tokens(Pred,D1,Ts).
tokens(Pred,D1,[t(D2,T2)|Ts]) --> inter_token(D1,D2), call(Pred,D2,T2), !, tokens(Pred,D2,Ts).
tokens(_,_,[]) --> ?separator.

% handles between token separators on the basis of their delimiter class
inter_token(d,_) --> [].
inter_token(n,d) --> [].
inter_token(_,_) --> separator.


separator --> +(any(" \n\t");(parsing,comment)).
comment --> "--", *(char//notany("\n\r")), any("\n\r").

scheme(D,T) --> {ground(T)}, !, token(D,T).
scheme(n,C) --> fmt("[~@]",[(write_canonical(C),fail;true)]).

parse(D,T) --> token(D,T).

%% token(?DC:delim_class, ?TC:token_class)// is nondet.
%
%  Parse or generate one token. NB the order of these clauses is important;
%  Using only definite clauses,Some character sequences can be parsed in several 
%  ways, but with this ordering of clauses, only the first parse is the correct one.

token(n,charset(Sch,Nm))    --> "_", maybe(S,(regular_ident(S), "."),Sch), regular_ident(Nm), not(".").
token(d,string(char,Codes)) --> string_of(char,Codes).
token(n,string(char,Codes)) --> ("n";"N"), string_of(char,Codes).
token(n,string(hex,Codes))  --> ("x";"X"), string_of((digit;any("abcdefABCDEF")),Codes).
token(n,string(bit,Codes))  --> ("b";"B"), string_of(("0";"1"),Codes).
token(D,identifier(I)) --> (  {ground(I)}
                           -> {identifier_codes(D,I,Codes)}, identifier(D,Codes)
                           ;  identifier(D,Codes), {identifier_codes(D,I,Codes)}
                           ).

token(n,unsigned(X))   --> (  {ground(X)}
                           -> {number_number(X,Y)}, number(Y)
                           ;  number(Y), {number_number(X,Y)}
                           ).
token(d,operator(Op))  --> (  {nonvar(Op)}
                           -> {atom_codes(Op,Codes)}, list(Codes), {operator(Codes,[])}
                           ;  operator//dlist(Codes), {atom_codes(Op,Codes)}
                           ).

% -- Identifiers ----------
identifier(A) :- freeze(A, atom(A);string(A)).
identifier_codes(d,ID,Codes) :- string_codes(ID,Codes), string(ID).
identifier_codes(n,ID,Codes) :- atom_codes(ID,Codes), atom(ID).

regular_ident --> letter, **(letter;"_";digit).
regular_ident(Codes) -->
   (  parsing
   -> regular_ident//seqmap(lower,Codes), ! % only first parse
   ;  seqmap(lower,Codes), {regular_ident(Codes,[])}
   ).

identifier(n,Codes) --> regular_ident(Codes).
identifier(d,Codes) --> "\"", esc(esc_qq,Codes), "\"".

% true when Lower is the lower case version of the next character in the sequence.
lower(Lower) --> [Either], {to_lower(Either,Lower)}.

to_lower(Char,Char) :- code_type(Char,graph), \+code_type(Char,upper).
to_lower(Upper,Lower) :- code_type(Lower,lower(Upper)).

% handles escaped sequences inside double quotes
esc_qq([0'"|Cs],Cs) --> "\"\"".
esc_qq([C|Cs],Cs) --> [C]//char//notany("\"").

% -- Numbers -------------

% this rule handles the _syntactic_ form of an unsigned numeric literal.
number(number(Before,After,Exponent)) --> 
   exact_number(Before,After), 
   (  {Exponent=nothing}, not("eE")
   ;  {Exponent=just(ESign:EDigits)}, ("e";"E"), 
      ESign ? sign_codes,
      digits(EDigits)
   ).

exact_number(Before,After) --> digits(Before), ({After=[]}, not("."); ".", (digits(After);{After=[]})).
exact_number([],After) --> ".", digits(After).

% this converts between semantic and syntactic representation of numbers
number_number(int(N),number(Before,[],nothing)) :- Before=[_|_], number_codes(N,Before).
number_number(fixed(X,Scale),number(Before,After,nothing)) :- 
   After=[_|_], length(After,Scale),
   (  nonvar(X) 
   -> number_codes(X,Digits), append_zeros(Before,After,Digits) 
   ;  append(Before,After,Digits), number_codes(X,Digits)
   ).

number_number(float(Mant,Exp),number(Before,After,just(ESign:EDigits))) :-
   (  After=[], Scale=0, number_codes(Mant,Before)
   ;  number_number(fixed(Mant,Scale),number(Before,After,nothing))
   ),
   (  nonvar(Exp)
   -> plus(Scale,Exp,E),
      number_abs_sign(E,AbsE,SignE), 
      number_codes(AbsE,EDigits), 
      sign_sign(SignE,ESign)
   ;  sign_sign(SignE,ESign),
      number_codes(AbsE,EDigits), 
      number_abs_sign(E,AbsE,SignE),
      plus(Scale,Exp,E)
   ).

append_zeros(X,Y,Z) :- append(X,Y,Z).
append_zeros([],Y,Z) :- +("0",Y,Z).

number_abs_sign(X,AbsX,SignX) :- nonvar(X), !, AbsX is abs(X), (X<0 -> SignX=(-); SignX=(+)).
number_abs_sign(X,AbsX,-)   :- X is -AbsX.
number_abs_sign(AbsX,AbsX,+).

sign_sign(-,just(-)).
sign_sign(+,nothing).
sign_sign(+,just(+)).

% -- Strings -----------------------

% handles multi-part single quoted strings
string_of(Class,Codes) --> run_left(chunks(Class),Codes,[]), not("'").
chunks(Class) --> \> parsing -> ++(\>separator, chunk(Class)); chunk(Class).
chunk(Class) --> \> "'", **(esc_q(Class)), \> "'".
esc_q(Class) --> "'"//Class <\> "''". 
esc_q(Class) --> [C]//notany("'") <\> [C]//Class.

% -- Primitives --------------------
digits(Codes) --> 
   (  {is_list(Codes)} 
   -> list(Codes), {phrase(++(digit),Codes)}
   ;  ++(digit) // dlist(Codes)
   ).

digit  --> any("0123456789").
letter --> [X], {code_type(X,alpha)}.
char   --> [X], {code_type(X,graph); code_type(X,space)}.
sign_code(+) --> "+".
sign_codes(-) --> "-".

dlist(Cs,L1,L2) :- is_list(Cs), !, append(Cs,L2,L1).
dlist([],L1,L2) :- L1==L2, !.
dlist([C|Cs],L1,L3) :- must_be(nonvar,L1), L1=[C|L2], dlist(Cs,L2,L3).

operator  --> "<="; ">="; "<>"; "||"; ".."
            ;  ">", not("="); "<", not(">="); ".", not("."); "-", not("-")
            ;  any(",+*/&?%:;=()[]").

not(Cs,S,S) :- freeze(S,(S=[];S=[X|_],freeze(X,\+member(X,Cs)))).
peek(Phrase,S,S) :- call_dcg(Phrase,S,_).
parsing(S,S) :- nonvar(S), S=[C|_], nonvar(C).

% --------------------------- NOT PROPERLY IMPLEMENTED ------------------------
data_type(numeric,T) --> @([integer,real],T).
data_type(string(char),T) --> @([text, varchar],T).
data_type(datetime,T) --> @([datetime,date,time],T).
data_type(boolean,boolean) --> @boolean.

% ================= TOP LEVEL UTILITIES ============
phrase_to_tokens(Phrase,Tokens,Opts) :- limit_size(max_length,Tokens,Opts), phrase(Phrase,Tokens).
tokens_to_string(Tokens,String) :- phrase_string(tokens(scheme,Tokens),String).

%% sql_parse(+Phrase:phrase(list(token)),+String:string) is nondet.
%
%  Parse a string or list of codes using an arbitrary phrase in the SQL
%  grammar.
sql_parse(Phrase,String) :- 
   sql_tokenise(String,Tokens), 
   phrase(Phrase,Tokens).

%% sql_generate(+Phrase:phrase(list(token)), -String:string, +Opts:options) is nondet.
%
%  Generate SQL string from an SQL phrase.
sql_generate(Phrase,String) :- sql_generate(Phrase,String,[]).
sql_generate(Phrase,String,Opts) :-
   phrase_to_tokens(Phrase,Tokens,Opts),
   once(tokens_to_string(Tokens,String)).

% delayed length checker, useful for generating from grammars.
max_length(N,L) :- freeze(L,(L=[]->true;L=[_|T],succ(M,N),max_length(M,T))).

limit_size(Limiter,Term,Opts) :-
   option(step(DD),Opts,15),
   option(tries(DT),Opts,5),
   between(1,DT,I), Limit is DD*I,
   debug(sql,'Trying with ~w(~d)',[Limiter,Limit]),
   call(Limiter,Limit,Term).

