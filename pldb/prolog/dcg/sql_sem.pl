:- module(sql_sem,
      [  sql_tokenise/2
      ,  sql_generate/3, sql_generate/2
      ,  sql_parse/2
      ,  statement//1
      ]).

/** <module> SQL DCG with semantic representation

   This module provides an SQL DCG based on that implemented in library(dcg/sql92),
   but with extra arguments to carry a semantic representation of the SQL syntax,
   as well a some other modifications. It is primarily intended for generating SQL
   statements, but it can also be used for parsing. 

   In both directions, the depth of
   the search tree must be limited to prevent infinite recursion: in the parsing
   direction, the depth of parsed semantic representation can be limited using
   max_depth/2 to prevent infinite left-recursion. In the generating directions,
   the length of the resulting list of tokens can be limited using max_length/2.

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
   create_table(table_name, list(table_element), maybe({delete,preserve})) :: statement.
   create_view(table_name, maybe(list(column_name)), query, maybe(maybe({cascaded,local}))) :: statement.
   set_constraints_mode(maybe(list(qualified_name)), {immediate,deferred}) :: statement.

   table_element ---> column(column_name, type, maybe(default),list(column_constraint), maybe(collation))
                    ; constraint(maybe(qualified_name),pair(list(column_name), constraint_spec),check_time).

   default ---> literal(literal); null; current_user; session_user; system_user.

   column_constraint ---> constraint(maybe(qualified_name), 
                                     constraint_spec | {not_null}, 
                                     check_time, maybe(boolean)).

   constraint_spec ---> unique; primary_key; references(table_name, maybe(list(column_name))).

   check_time ---> maybe({immediate,deferred}).
   ==

   Queries
   ==
   ordered_query(query,maybe(list(ordering))) :: statement.
   select(select_variant, select) :: statement. 

   select ---> select( maybe(quantifier),            % all or distinct
                       list(select_sublist),
                       maybe(list(table_reference)), % from 
                       maybe(condition),             % where
                       maybe(list(grouping)),        % group by
                       maybe(condition)).            % having

   quantifier ---> all; distinct.

   select_variant ---> general; single(list(target)).
   select_sublist ---> sel(expr,maybe(column_name))
                     ; all(table_name).

   where_spec ---> all; cond(condition); current(identifier).
   grouping    ==  pair(column_reference, maybe(collation)).
   ordering   ---> ord(column, maybe(collation), maybe({asc,desc})).
   column     ---> named(column_name)
                 ; numbered(natural).

   table_reference ---> table(table_name, maybe(correlation_spec))
                      ; subquery(query,correlation_spec)
                      ; join.

   query ---> setop({union,except,intersect}, boolean, maybe(maybe(list(column_name))))
            ; values(list(row_value))
            ; table(table_name)
            ; query(query)
            ; select(select).

   join ---> op(join_op,table_reference,table_reference).

   join_op --> cross_join 
             ; join(maybe(join_type), maybe(join_spec)).


           ; natural(table_reference, table_reference, boolean, maybe(join_type), maybe(join_spec))
           ; paren(join).

   join_type ---> inner; union; left; right; full.
   join_spec ---> on(condition); using(list(column_name)); natural.

   correlation_spec  == pair(identifier,maybe(list(column_name))).
   ==

   Data mutation
   ==
   update(table_name, list(set_clause), where_spec) :: statement.
   insert(table_name, insert_spec) :: statement.
   delete(table_name, where_spec) :: statement.

   set_clause ---> column_name=value.
   insert_spec ---> defaults; query(maybe(list(column_name)), query).
   ==

   Conditions
   ==
   condition ---> op(binary_boolean_op, condition, condition)
                ; op(unary_boolean_op, condition)
                ; cmp(row_value, comparison_op, row_value)
                ; cmp(row_value, comparison_op, {all,some,any}, query)
                ; exists(query)
                ; overlaps(row_value, row_value)
                ; match(row_value, query, boolean, maybe({partial,full}))
                ; is_not(negatable_predicate)
                ; is(negatable_predicate)
                ; paren(condition).

   binary_boolean_op ---> or; and.
   unary_boolean_op ---> not; is(trinary); is_not(trinary).
   trinary ---> true; false; unknown.

   negatable_predicate ---> null(row_value)
                          ; between(row_value,row_value,row_value)
                          ; in(row_value, in_spec) % in_spec = query | list(expr) ?
                          ; like(expr, expr, maybe(expr)). 

   comparison_op ---> '='; '<'; '>'; '<>'; '<='; '>='. 
   ==

   Values and expressions
   ==
   row_value ---> row(list(value)); element(value); subquery(query).
   value     ---> val(expr); default; null.
   result    ---> null; val(expr).

   simple_value ---> param(identified)
                   ; embedded(identifier)
                   ; literal(literal).

   general_value ---> param(identifier,maybe(identifier))
                    ; embedded(identifier,maybe(identifier)).
                    ; dynamic
                    ; current_user
                    ; session_user
                    ; system_user
                    ; user
                    ; value.

   expr ---> expr + expr
           ; expr - expr
           ; expr * expr
           ; expr / expr
           ; -expr
           ; +expr

           ; right(concat, expr, expr)
           ; post(collation, expr) 
         
           ; at(expr, time_zone)
           ; expr : interval_qualifier

           ; lit(literal)
           ; gen(general_value)
           ; col(column_reference)
           ; case(case_spec)
           ; cast(expr,type)
           ; aggr(aggregate)
           ; fn(function)
           ; subquery(query)
           ; expr(expr).

   case_spec ---> nullif(expr,expr)
                ; coalesce(list(expr))
                ; case(expr,list(when(expr)), maybe(result))
                ; case(list(when(condition)), maybe(result)).

   when(T) ---> when(T,result).


   time_zone ---> local; interval(expr).

   aggregate ---> count(maybe(pair(maybe(quantifier),expr)))
                ; agg({avg,max,min,sum}, pair(maybe(quantifier),expr)).
               
   function ---> substring(expr, expr, expr)
               ; upper(expr)
               ; lower(expr)
               ; convert(expr, qualified_name)
               ; translate( expr, qualified_name)
               ; trim(maybe({leading,trailing,both}), maybe(expr), expr)
               ; position(expr, expr)
               ; extract(dt_field | {timezone_hour, timezone_minute}, extract_source) 
               ; length({char,character,octet,bit}, expr).

   extract_source ---> expr. % datetime or interval
   ==

   Literals
   ==
   unsigned(numeric)                        :: literal.
   signed({+,-},numeric)                    :: literal.
   string({char,hex,bit},list(code))        :: literal.
   timestamp(date,time,maybe(time_zone_interval)) :: literal.
   current_date                             :: literal.
   current_time(natural)                    :: literal.
   current_timestamp(natural)               :: literal.
   interval(maybe(sign), year_month | date_time, interval_qualifier) :: literal.

   date :< literal.
   time :< literal.

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

*(Phrase,Things) --> seqmap(Phrase,Things).
clist(Phrase,Things) --> seqmap_with_sep(comma,Phrase,Things).

clist(Phrase) --> +(comma,Phrase).
paren(Phrase) --> o('('), Phrase, o(')').

:- setting(max_left_recursion,nonneg,3,'Maximum depth for parsing left-recursive operators').

algebra(Ops,Base,S) --> {setting(max_left_recursion,M)}, algebra(M,M,Ops,Base,S).
algebra(_,_,[],Base,S) --> call(Base,S).
algebra(M,_,[_|Ops],Base,S) --> algebra(M,M,Ops,Base,S).
algebra(M,M0,[O|Ops],Base,S) --> {O=Class-Op}, a_app(Class,Op,a(M0,[O|Ops],M,Base),S).
a_app(pre(A1),Op,State,op(SO,S1))  --> call(Op,SO), a_arg(State,A1,S1).
a_app(post(A1),Op,State,op(SO,S1)) --> a_arg(State,A1,S1), call(Op,SO).
a_app(in(A1,A2),Op,State,op(SO,S1,S2)) --> a_arg(State,A1,S1), call(Op,SO), a_arg(State,A2,S2). 
a_arg(a(M0,Ops,M,Base),A1,S1) --> {assoc(M,M0,Ops,A1,M1,Ops1)}, algebra(M,M1,Ops1,Base,S1).

algebra2(Ops,Base,S) --> {setting(max_left_recursion,M)}, algebra2(M,M,Ops,Base,S).
algebra2(_,_,[],Base,S) --> call(Base,S).
algebra2(M,M0,[O|Ops],Base,S) --> {O=Class-Op}, a2_app(Class,Op,a(M0,[O|Ops],M,Base),S).
a2_app(pre(A1),Op,State,op(SO,S1))  --> call(Op,SO), a2_arg(State,A1,S1).
a2_app(Class,Op,State,S) --> {class_assoc1(Class,A1)}, a2_arg(State,A1,S1), a2_cont(A1,Class,Op,State,S1,S).
a2_cont(x,_,_,_,S,S) --> [].
a2_cont(A1,post(A1),Op,_,S1,op(SO,S1)) --> call(Op,SO).
a2_cont(A1,in(A1,A2),Op,State,S1,op(SO,S1,S2)) --> call(Op,SO), a2_arg(State,A2,S2). 
a2_arg(a(M0,Ops,M,Base),A1,S1) --> {assoc(M,M0,Ops,A1,M1,Ops1)}, algebra2(M,M1,Ops1,Base,S1).

% this is to avoid parsing arg 1 as y if the current op class precludes it
class_assoc1(_,x).
class_assoc1(post(y),y).
class_assoc1(in(y,_),y).


assoc(M,_,[_|Ops],x,M,Ops).
assoc(_,M0,Ops,y,M1,Ops) :- succ(M1,M0).

% Untyped algebra
:- meta_predicate algebra8(+,5,3,-,?,?).
algebra8(L,DB,Base,Sem) --> 
   {setting(max_left_recursion,M)},
   a8_top(L,M,a(M,DB,Base),Sem).

a8_top(0,_,Alg,Sem) --> {Alg=a(_,_,Base)}, call(Base,Sem).
a8_top(L,M,Alg,Sem) --> {L>0}, a8_x(L-M,Alg,Sem).

% ideally, 3rd clause would not bother with A1=y if no operator will accept it.
a8_x(LM,Alg,op(SO,S1)) --> a8_op(Alg,LM,pre(A1),SO), a8_arg(LM,Alg,A1,S1).
a8_x(LM,Alg,Sem) --> a8_op(Alg,LM,custom(a8_arg(LM,Alg)),Sem).
a8_x(LM,Alg,Sem) --> a8_arg(LM,Alg,A1,S1), a8_cont(A1,LM,Alg,S1,Sem). 

a8_cont(x,_,_,S1,S1) --> [].
a8_cont(A1,LM,Alg,S1,op(SO,S1))  --> a8_op(Alg,LM,post(A1),SO).
a8_cont(A1,LM,Alg,S1,op(SO,S1,S2)) --> a8_op(Alg,LM,in(A1,A2),SO), a8_arg(LM,Alg,A2,S2).

a8_arg(LM,Alg,A,S) --> {Alg=a(M,_,_), assoc(M,LM,A,L1-M1)}, a8_top(L1,M1,Alg,S).
a8_op(a(_,DB,_),L-_,Class,SO) --> call(DB,L,Class,SO).

% Typed algebra
:- meta_predicate algebra9(+,6,4,-,-,?,?).
algebra9(L,DB,Base,Type,Sem) --> 
   {setting(max_left_recursion,M)},
   a9_top(L,M,a(M,DB,Base),Type,Sem).

a9_top(0,_,Alg,Type,Sem) --> {Alg=a(_,_,Base)}, call(Base,Type,Sem).
a9_top(L,M,Alg,Type,Sem) --> {L>0}, a9_x(L-M,Alg,Type,Sem).

% ideally, 3rd clause would not bother with A1=y if no operator will accept it.
a9_x(LM,Alg,Type,op(SO,S1)) --> a9_op(Alg,LM,pre(A1:T1),Type,SO), a9_arg(LM,Alg,A1,T1,S1).
a9_x(LM,Alg,Type,Sem) --> a9_op(Alg,LM,custom(a9_arg(LM,Alg)),Type,Sem).
a9_x(LM,Alg,Type,Sem) --> a9_arg(LM,Alg,A1,T1,S1), a9_cont(A1,LM,Alg,T1,S1,Type,Sem).

a9_cont(x,_,_,T1,S1,T1,S1) --> [].
a9_cont(A1,LM,Alg,T1,S1,TO,op(SO,S1))  --> a9_op(Alg,LM,post(A1:T1),TO,SO).
a9_cont(A1,LM,Alg,T1,S1,TO,op(SO,S1,S2)) --> a9_op(Alg,LM,in(A1:T1,A2:T2),TO,SO), a9_arg(LM,Alg,A2,T2,S2).

a9_arg(LM,Alg,A,T,S) --> {Alg=a(M,_,_), assoc(M,LM,A,L1-M1)}, a9_top(L1,M1,Alg,T,S).
a9_op(a(_,DB,_),L-_,Class,TO,SO) --> call(DB,L,Class,TO,SO).

assoc(M,L-_,x,L1-M) :- succ(L1,L).
assoc(_,L-M,y,L-M1) :- succ(M1,M).

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

statement(select(Variant,Select)) --> select(Variant,Select).

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
default_option(literal(X)) --> literal(_,X).
default_option(X) --> @X, {member(X,[user, null, current_user, session_user, system_user])}.

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

set_clause(C=V) --> column_name(C), o(=), value(V).

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

query_expr(Q) --> algebra9(2,setop,query_primary,query,Q).

setop(Level,in(y:query,x:query),query,setop(Op,All,CS)) --> 
   @Op, {setop_level(Op,Level)}, if(All,@all), CS?corresponding_spec.

setop_level(union,2).
setop_level(except,2).
setop_level(intersect,1).

query_primary(query,select(S))    --> select(general,S).
query_primary(query,values(Vals)) --> @values, clist(row_val,Vals).
query_primary(query,table(T))     --> @table, table_name(T).
% query_primary(query,join(J))    --> joined_table(J). 
query_primary(query,Q)            --> paren(query_expr(Q)).

table_reference(X) --> table_expr(tref(_),X).
table_expr(T,X) --> algebra9(1,table_op,table_primary,T,X).

table_op(1,in(y:tref(_),y:tref(_)),tref(join),cross_join) --> @cross, @join.
table_op(1,custom(PA),tref(joined),op(join(JT,JS),T1,T2)) --> 
   call(PA,y,tref(_),T1),
   (  JT?join_type, @join,
      call(PA,y,tref(_),T2),
      JS?join_spec
   ;  {JS=just(natural)}, @natural, 
      JT?join_type, @join,
      call(PA,y,tref(_),T2)
   ).

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
reserved(outer).
reserved(inner).
reserved(left).
reserved(right).
reserved(full).
reserved(union).
reserved(natural).
reserved(using).
reserved(on).
reserved(as).

select(Variant,select(Quant,SList,From,Cond,GroupBy,Having)) -->
   @select, Quant?set_quantifier,
   select_list(Variant,SList),
   maybe(Ts, (@from, clist(table_reference,Ts)), From),
   Cond ? where_clause,
   maybe(Gs, (@group, @by, clist(grouping_spec,Gs)), GroupBy),
   maybe(C, (@having, condition(C)), Having).

select_list(general,[]) --> star.
select_list(general,SList) --> clist(select_sublist,SList).
select_list(single(Targets),SList) -->
   clist(select_sublist,SList),
   @into, clist(target_spec,Targets).

set_quantifier(Q) --> (@all; @distinct) // @Q.
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

% =================== SEARCH CONDITIONS ===================
condition(Cond) --> 
   % algebra9(5,bool_op,bool_primary,boolean,Cond).
   algebra2( [  in(y,x)-(@or>or)
             ,  in(y,x)-(@and>and)
             ,  pre(x) -(@not>not)
             ,  post(x)-boolean_test
             ], predicate, Cond).

boolean_test(is(Val,Pos)) --> @(is), neg(Pos), @([true,false,unknown],Val).

predicate(exists(X)) --> @exists, subquery(X).
predicate(X) --> paren(condition(X)).
predicate(Sem) --> row_val(X), predicate_continue(X,Sem).
predicate(op(is(like(X,Esc),Pos),X,Y)) --> 
   expr(string(char),X), neg(Pos), @like, expr(string(char),Y), 
   maybe(E, (@escape, expr(string(char),E)),Esc).

predicate_continue(X,op(cmp(O),X,Y))--> comp_op(O), row_val(Y).
predicate_continue(X,op(cmp(O,Quant,Q),X)) --> comp_op(O), @([all,some,any],Quant), subquery(Q).
predicate_continue(X,op(overlaps,X,Y)) --> @overlaps, row_val(Y).
predicate_continue(X,op(is(null,Pos),X)) --> @(is), neg(Pos), @null.
predicate_continue(X,op(is(between(L,U),Pos),X)) --> neg(Pos), @between, row_val(L), @and, row_val(U).
predicate_continue(X,op(is(in(Set),Pos),X)) --> neg(Pos), @in, in_spec(Set).
predicate_continue(X,op(match(Q,Unique,Partiality),X)) --> 
   @match, if(Unique, @unique), 
   Partiality ? @[partial,full], 
   subquery(Q).

% bool_op(5, in(y:boolean,x:boolean), boolean, or)  --> @or.
% bool_op(4, in(y:boolean,x:boolean), boolean, and) --> @and.
% bool_op(3, pre(x:boolean),          boolean, not) --> @not.
% bool_op(2, post(x:boolean),         boolean, is(Val,Pos)) --> 
%    @(is), neg(Pos), @([true,false,unknown],Val).

% bool_op(1, in(x:row,x:row), boolean, cmp(O))    --> comp_op(O).
% bool_op(1, in(x:row,x:row), boolean, overlaps)  --> @overlaps.
% bool_op(1, post(x:row),     boolean, cmpq(O,Quant,Q)) --> comp_op(O), @([all,some,any],Quant), subquery(Q).
% bool_op(1, post(x:row),     boolean, match(Q,Unique,Partiality)) -->
%    @match, if(Unique, @unique), 
%    Partiality ? @[partial,full], 
%    subquery(Q).
% bool_op(1, post(x:row), boolean, is(Pos,null)) --> @(is), neg(Pos), @null.
% bool_op(1, post(x:row), boolean, is(Pos,between(L,U))) --> neg(Pos), @between, row_val(L), @and, row_val(U).
% bool_op(1, post(x:row), boolean, is(Pos,in(Set))) --> neg(Pos), @in, in_spec(Set).
% bool_op(1, custom(_),   boolean, op(is(Pos,like(Esc)),X,Y)) --> 
%    expr(string(char),X), neg(Pos), @like, expr(string(char),Y), 
%    maybe(E, (@escape, expr(string(char),E)),Esc).

% bool_primary(row,X) --> row_val(X).

in_spec(subquery(Q)) --> subquery(Q).
in_spec(list(Items)) --> paren(clist(expr(_),Items)).
comp_op(O) --> o(O), {member(O,['=','<>','<','>','<=','>='])}.
neg(Pos) --> if(Pos, [], @not).

% ======================== EXPRESSIONS =====================
:- discontiguous expr//2, term//2, factor//2.

row_val(row(Elements)) --> paren(clist(value,Elements)).
row_val(element(X))    --> value(X).
row_val(subquery(Q))   --> subquery(Q).

value(val(X)) --> expr(_,X).
value(default) --> @default.
value(null) --> @null.

expr(T,X) --> algebra9(4,expr_op,primary,T,X).

expr_op(4,in(y:numeric,x:numeric),numeric,S) --> sign(S).
expr_op(3,in(y:numeric,x:numeric),numeric,*) --> star.
expr_op(3,in(y:numeric,x:numeric),numeric,/) --> slash.
expr_op(2,pre(x:numeric),numeric,S) --> sign(S).

expr_op(2,in(x:string(T),y:string(T)),string(T),concat) --> concat.
expr_op(1,post(x:string(char)),string(char),collate(C)) --> collate_clause(C).

expr_op(4,in(y:interval,x:datetime),datetime,+) --> plus.
expr_op(4,in(y:datetime,x:interval),datetime,S) --> sign(S).
expr_op(4,in(y:interval,x:interval),interval,S) --> sign(S).
expr_op(4,in(y:datetime,x:datetime),preival,ival) --> minus.

expr_op(3,in(x:numeric,y:interval),interval,*) --> star.
expr_op(3,in(y:interval,x:numeric),interval,*) --> star.
expr_op(3,in(y:interval,x:numeric),interval,/) --> slash.

expr_op(2,pre(x:interval),interval,S) --> sign(S).
expr_op(1,post(x:interval),interval,qual(Q)) --> interval_qualifier(Q).
expr_op(1,post(x:preival),interval,qual(Q)) --> interval_qualifier(Q).
expr_op(1,post(x:datetime),datetime,at(TZ)) --> @at, time_zone_spec(TZ).

time_zone_spec(local) --> @local.
time_zone_spec(interval(X)) --> @time, @zone, expr(interval,X).

primary(T,lit(X)) --> {dif(T,signed)}, literal(T,X).
primary(T,gen(T,X)) --> general_val(X).
primary(T,col(T,X)) --> column_reference(X).
primary(T,case(X)) --> case(T,X).
primary(T,cast(X,DT)) --> cast(T,X,DT).
primary(T,aggr(X))   --> set_function(T,X).
primary(T,fn(X))      --> function(T,X).
primary(T,expr(X)) --> paren(expr(T,X)).
primary(_,subq(X)) --> subquery(X).

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

cast(T,X,TorD) --> @cast, paren(((expr(_,X) ; @null), @(as), type_or_domain(T,TorD))).

case(T,nullif(X,Y)) --> @nullif, paren((expr(T,X), comma, expr(T,Y))).
case(T,coalesce(X)) --> @coalesce, paren(clist(expr(T),X)).
case(T,case(X,Whens,Else)) -->
   @case, expr(TX,X), 
   seqmap(when_clause(TX,T),Whens),
   maybe(Y, else(result(T,Y)), Else), 
   @end.

case(T,case(Whens,Else)) -->
   @case, 
   seqmap(when_clause(T),Whens),
   maybe(Y, else(result(T,Y)), Else), 
   @end.

when_clause(TX,TR,when(X,R)) --> when(expr(TX,X),result(TR,R)). 
when_clause(TR,when(C,R))  --> when(condition(C),result(TR,R)). 

when(Phrase,Result) --> @when, Phrase, @then, Result.
else(Result) --> @else, Result.
result(_,null) --> @null.
result(T,val(X)) --> expr(T,X).


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

% ================= TOP LEVEL UTILITIES ============
tokens_to_phrase(Phrase,Tokens,Opts) :- limit_size(max_depth,Phrase,Opts), phrase(Phrase,Tokens).
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
max_depth(N,T) :- freeze(T,check_depth(N,T)).

check_depth(_,T) :- atomic(T), !.
check_depth(N,T) :- T=..[_|Args], succ(M,N), maplist(max_depth(M),Args).

limit_size(Limiter,Term,Opts) :-
   option(step(DD),Opts,15),
   option(tries(DT),Opts,5),
   between(1,DT,I), Limit is DD*I,
   debug(sql,'Trying with ~w(~d)',[Limiter,Limit]),
   call(Limiter,Limit,Term).

