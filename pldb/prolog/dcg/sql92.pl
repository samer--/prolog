:- module(sql92,[]).

/** <module> SQL '92 reference grammar

   This module provides a reference implementation of the SQL'92 grammar as
   described here: http://savage.net.au/SQL/sql-92.bnf.html

   The grammar operates at the level of tokens.
   The rules are based directly on the SQL'92 BNF, but there a few minor
   changes:

      1. DCG rules with parameters have been used to organise the productions
         used to manage expressions. For example <value_expression>, <character_value_expression>,
         <numeric_value_expression> etc have been replaced with expr//1, parameterised
         by type. One effect of this is that expressions are now slightly stricter
         with regard to the legal types of sub-expressions.

      2. A few correctness-preserving simplifications have been made, for example,
         the rules for query expressions no longer require a disctinction between
         join and non-join sub-expressions and terms.

      3. The meta-rule algebra//2 is introduced to take care of heirarchies of
         operator precedence in conditions, value expressions, and query expressions.

      4. For the sake of being able to run the grammar, left recursive rules have been
         made right recursive, with the correct left recursive rule commented out.
         This means that the grammar will parse some operator expressions incorrectly

      5. Because of certain problems in the grammar, I have classified the two character
         operators <= >= <> and || as delimiter tokens, not non-delimiter tokens.

   Some remaing questions about the underlying language:

      1. It's not clear whether the rules for query expression are correct, because
         some DBMSs, eg Postgres, do not recognise join expressions as valid queries.

      2. It's not clear how strict the typing rules for expressions should be.

      3. It's not clear whether or not expressions should be allowed in GROUP BY or
         ORDER BY constructs.

      4. In a VALUES query_expr, can row_val be a single value or must it be a tuple?

      5. The tokenisation rules are a bit broken: "x'...'", "b'....'" etc
         are non-delimiter tokens and the standard requires a space before a following
         non-delimiter token, but DBMSs seem to parse eg "n'hello'as x" without
         any trouble. Also, the rules state that two delimiter tokens can follow one-another
         without a space, but 'foo''bar' is not parsed as "foo" and "bar", but as "foo'bar".

      6. The tokenisation rules are a bit unclear about the status of character set specifiers.

      7. The tokenisation rules are a bit unclear about how numeric tokens interact with "." and
         ".." tokens. Under what circumstances is the "." absorbed into the numeric token?
         Eg, can "2.34.foo" be parsed as "2.34", ".", "foo"? What about "2..10"?

      8. Not sure about the legality of an un-parenthesised joined_table as a query_expr.
 */

:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes), except([comma//0, q//1, qq//1, paren//1])).
:- use_module(library(snobol)).

:- op(900,fy,?).
:- op(200,fy,@).

:- set_prolog_flag(double_quotes, codes).

:- meta_predicate
         ?(//,?,?)
      ,  *(//,?,?)
      ,  +(//,?,?)
      ,  +(//,//,?,?)
      ,  clist(//,?,?)
      ,  paren(//,?,?)
      ,  q(//,?,?)
      ,  string_of(//,?,?)
      ,  algebra(+,//,?,?)
      ,  with_indicator(//,?,?)
      .

% -- Meta rules --

?(Phrase) --> []; Phrase.
+(Phrase) --> Phrase, *(Phrase).
*(Phrase) --> []; +(Phrase). % greedy for parsing
+(Sep,Phrase) --> Phrase, *((Sep,Phrase)).

% greedy versions for parsing
**(Phrase) --> ++(Phrase); [].
++(Phrase) --> Phrase, (++(Phrase);[]).
++(Sep,Phrase) --> Phrase, (++((Sep,Phrase));[]).

clist(Phrase) --> +(comma,Phrase).
paren(Phrase) --> o('('), Phrase, o(')').

algebra(Ops,Base) --> algebra(Ops,Ops,Base).
algebra([],All,Base) --> Base; paren(algebra(All,Base)).
algebra([_|Ops],All,Base) --> algebra(Ops,All,Base).
% algebra([left(Op)|Ops],All,Base) -->  algebra([left(Op)|Ops],All,Base), Op, algebra(Ops,All,Base). % !!! Left recursive
algebra([left(Op)|Ops],All,Base) --> algebra(Ops,All,Base), Op, algebra([left(Op)|Ops],All,Base).
algebra([right(Op)|Ops],All,Base) --> algebra(Ops,All,Base), Op, algebra([right(Op)|Ops],All,Base).
algebra([pre(Op)|Ops],All,Base) --> Op, algebra(Ops,All,Base).
algebra([post(Op)|Ops],All,Base) --> algebra(Ops,All,Base), Op.

% ================ STATEMENTS ================

statement --> 
   table_defn; view_defn; insert; delete; update; select_multi_row; select_single_row;
   declare_cursor; open_statement; close_statement; fetch_statement; 
   set_transaction; commit_statement; rollback_statement;
   set_constraints_mode_statement.

collate_clause --> @collate, qualified_name.

% -- Schema definition ----
table_defn --> @create, @table, table_name, paren(clist(table_element)),
               ?((@on, @commit, (@delete;@preserve), @rows)).

view_defn --> @create, @view, table_name, ?paren(clist(column_name)), @(as), query_expr.
%                ? (@with, ?(@cascaded; @local), @check, @option).

table_element --> column_name, (data_type(_) ; domain_name), ?default_clause, ?column_constraint_defn, ?collate_clause
                ; ?constraint_name_defn, table_constraint, ?constraint_check_time.

default_clause --> @default, default_option.
default_option --> literal(_)
                 ; @user; @current, @user
                 ; @session, @user, @system, @user, @null.

column_constraint_defn --> ?constraint_name_defn, column_constraint, ?constraint_attributes.

constraint_name_defn --> @constraint, constraint_name.
constraint_name --> qualified_name.
column_constraint --> @not, @null ; unique_spec; references_spec.
constraint_attributes --> constraint_check_time, ? (? @not, @deferrable)
                        ; ? @not, @deferrable, ?constraint_check_time.
constraint_check_time --> @initially, (@immediate;@deferred).

table_constraint --> unique_spec, paren(clist(column_name))
                   ; @foreign, @key, paren(clist(column_name)), references_spec.

references_spec --> @references, table_name, ?paren(clist(column_name)).
unique_spec --> @unique; @primary, @key.

% -- Data modification ----
delete --> @delete, @from, table_name, (?where_clause; where_current).
insert --> @insert, @into, table_name, (?paren(clist(column_name)), query_expr ; @default, @values).
update --> @update, table_name, @set, clist(set_clause), (?where_clause; where_current).

set_clause --> column_name, o(=), (expr(_); @null; @default).
where_current --> @where, @current, @of, cursor_name.

% -- Query ----
where_clause --> @where, condition.
order_by_clause --> @order, @by, clist(sort_spec).
sort_spec --> (column_name;unsigned_int), ?collate_clause, ?(@asc;@desc). % can also be expr in some dbmss!

subquery --> paren(query_expr).

query_expr --> algebra( [ ((@union;@except), ? @all, ?corresponding_spec)
                        , (@intersect, ? @all, ?corresponding_spec)
                        ], query_primary).

query_primary --> select
                ; @values, clist(row_val)
                ; @table, table_name
                ; joined_table % NB not recognised py postgres
                ; paren(query_expr).

joined_table --> table_reference, @cross, @join, table_reference
               ; table_reference, ? @natural, ?join_type, @join, table_reference, ?join_spec
               ; paren(joined_table).

join_type --> @inner ; (@left;@right;@full), ? @outer; @union.
join_spec --> @on, condition ; @using, paren(clist(column_name)).

table_reference --> table_name, ?correlation_spec
                  ; subquery, correlation_spec
                  ; paren(joined_table). % !!! Paren to prevent left recursive

correlation_spec --> ? @(as), identifier, ?paren(clist(column_name)).
corresponding_spec --> @corresponding, ? ( @by, paren(clist(column_name))).

select_multi_row --> query_expr, ?order_by_clause.

select_single_row -->
   @select, ?set_quantifier,
   clist(select_sublist),
   @into, clist(target_spec),
   table_expr.

select --> @select, ?set_quantifier, (star ; clist(select_sublist)), table_expr.

set_quantifier --> @all; @distinct.
select_sublist --> expr(_), ? (@(as), column_name) ; qualifier, dot, star.
table_expr -->
     @from, clist(table_reference),
     ?where_clause,
     ? (@group, @by, clist((column_reference, ?collate_clause))),
     ? (@having, condition).

target_spec --> with_indicator(param_name); with_indicator(embedded_variable_name).

% ------------------ CURSORS ------------------
declare_cursor -->
   @declare, cursor_name, ? @insensitive, ? @scroll,
   @cursor, @for, (cursor_spec;statement_name).

cursor_spec --> query_expr, ?order_by_clause, ?updatability_clause.
updatability_clause --> @for, (@read, @only; @update, ? (@of, clist(column_name))).
open_statement --> @open, cursor_name.
close_statement --> @close, cursor_name.
fetch_statement --> @fetch, ? (?fetch_orientation, @from), cursor_name, @into, clist(target_spec).
fetch_orientation --> @next; @prior; @first; @last; (@absolute;@relative), simple_val.
cursor_name --> identifier.

% ------------------ TRANSACTIONS --------------------
commit_statement --> @commit, ? @work.
rollback_statement --> @rollback, ? @work.
set_constraints_mode_statement --> @set, @constraints, (@all; clist(constraint_name)), (@deferred;@immediate).
set_transaction --> @set, @transaction, clist(isolation_level;trans_access_mode;diagnostics_size).
isolation_level --> @isolation, @level, (@read, (@uncommitted;@committed); @repeatable, @read; @serializable).
trans_access_mode --> @read, (@only;@write).
diagnostics_size --> @diagnostics, @size, simple_val.

% =================== SEARCH CONDITIONS ===================
condition --> algebra([left(@or), left(@and), pre(@not), post(boolean_test)],(predicate;paren(condition))).

boolean_test --> @(is), ? @not, (@true;@false;@unknown).

predicate --> row_val, comp_op, row_val
            ; row_val, ? @not, @between, row_val, @and, row_val
            ; row_val, ? @not, @in, (subquery ; paren(clist(expr(_))))
            ; expr(string(char)), ? @not, @like, expr(string(char)), ? (@escape, expr(string(char)))
            ; row_val, @(is), ? @not, @null
            ; row_val, comp_op, (@all;@some;@any), subquery
            ; @exists, subquery
            ; row_val, @match, ? @unique, ?(@partial;@full)
            ; row_val, @overlaps, row_val
            .

comp_op --> o(=); o('<>'); o(<); o(>); o('<='); o('>=').

% ======================== EXPRESSIONS =====================
:- discontiguous expr//1, term//1, factor//1.

row_val --> row_val_el ; paren(clist(row_val_el)) ; subquery.
row_val_el --> expr(_) ; @null ; @default.

primary(T) --> {dif(T,signed)}, literal(T)
             ; general_val
             ; column_reference
             ; case(T)
             ; cast(T)
             ; set_function(T)
             ; function(T)
             ; paren(expr(T))
             ; subquery.

set_function(numeric) --> @count, paren(star; (?set_quantifier, expr(_))).
set_function(T) --> (@avg;@max;@min;@sum), paren((?set_quantifier, expr(T))).

cast(T) --> @cast, paren(((expr(_) ; @null), @(as), (domain_name;data_type(T)))).

case(T) --> @nullif, paren((expr(T), comma, expr(T)))
          ; @coalesce, paren(clist(expr(T)))
          ; @case, expr(T), +(when(expr(T),result(R))), ? (else(result(R))), @end
          ; @case, when(condition,result(R)), ? (else(result(R))), @end.

when(Phrase,Result) --> @when, Phrase, @then, Result.
else(Result) --> @else, Result.
result(T) --> expr(T) ; @null.

% -- NUMERIC --------------------
expr(numeric) --> term(numeric); term(numeric), sign, expr(numeric).  % expr(numeric), sign, term(numeric). % !!! Left recursive
term(numeric) --> factor(numeric); factor(numeric), (star;slash), term(numeric).  % term(numeric), (star;slash), factor(numeric). % !!! Left recursive
factor(numeric) --> ?sign, primary(numeric).

% -- STRINGS --------------------
expr(string(char)) --> algebra([right(concat), post(collate_clause)],primary(string(char))).
expr(string(T)) --> {T=bit;T=hex}, algebra([right(concat)],primary(string(T))).

% -- DATE/TIME --------------------
expr(datetime) --> term(datetime)
                 ; expr(interval), plus, term(datetime)
                 % ; expr(datetime), sign, term(interval). % !!! Left recursive
                 ; term(datetime), sign, term(interval)
                 ; expr(interval), plus, term(datetime), sign, term(interval).

expr(interval) --> term(interval)
                 % ; expr(interval), sign, term(interval)  % !!! Left recursive
                 ; term(interval), sign, expr(interval)
                 ; paren((expr(datetime), minus, term(datetime))), interval_qualifier. % !!!

term(datetime) --> primary(datetime), ? (@at, time_zone_spec).
term(interval) --> factor(interval)
                 % ; term(interval), (star;slash), factor(numeric) % !!! Left recursive
                 ; term(numeric), star, factor(interval).

factor(interval) --> ?sign, primary(interval), ?interval_qualifier.

time_zone_spec --> @local; @time, @zone, expr(interval).
interval_qualifier --> start_field, @to, end_field ; start_field; end_field.
start_field --> non_second_datetime_field, ?paren(unsigned_int).
end_field --> non_second_datetime_field; @second, paren(unsigned_int).
non_second_datetime_field --> @year;@month;@day;@hour;@minute.

% -------------------- FUNCTIONS ---------------------
function(string(T)) --> @substring, paren((expr(string(T)), @from, expr(numeric), @for, expr(numeric))).
function(string(char)) -->
    (@upper;@lower), paren(expr(string(char)))
  ; @convert, paren((expr(string(char)), @using, qualified_name))
  ; @translate, paren((expr(string(char)), @using, qualified_name))
  ; @trim, paren((? (?(@leading;@trailing;@both), ?expr(string(char)), @from), expr(string(char)))).

function(numeric) --> @position, paren((expr(string(char)), @in, expr(string(char))))
                   ;  @extract, paren((extract_field, @from, extract_source))
                   ;  (@char;@character;@octet;@bit), @length, paren(expr(string(_))).

extract_field --> non_second_datetime_field; @second; @timezone, (@hour;@minute).
extract_source --> expr(datetime); expr(interval).

% ------------------- IDENTIFIERS -------------------
table_name --> qualified_name ; @module, dot, identifier.
column_name --> identifier.
column_reference --> ? (qualifier, dot), column_name.
statement_name --> identifier.
qualifier --> table_name ; identifier.
qualified_name --> ? (schema_name, dot), identifier.
schema_name --> ? (catalog_name, dot), identifier.
catalog_name --> identifier.
identifier --> t(identifier(_)).

% --------------------- LITERALS ---------------------
literal(numeric)  --> t(unsigned).
literal(signed)   --> ?sign, t(unsigned).
literal(datetime) --> @date, qt(date) ; @time, qt(time) ; @timestamp, qt(date_time_tz)
                    ; @current, @date ; @current, @time, paren(unsigned_int); @current, @timestamp, paren(unsigned_int).
literal(interval) --> @interval, ?sign, qt(year_month;day_time), interval_qualifier.
literal(string(T)) --> t(string(T)).

qt(Phrase) --> string(char,Phrase).
unsigned_int --> t(unsigned).

day_time   --> days, ? (" ",hours, ? (":",minutes, ? (":",seconds))).
year_month --> years; years, "-", months.
date_time_tz --> date, " ", time, ?time_zone_interval.
date    --> years, "-", months, "-", days.
time    --> hours, ":", minutes, ":", seconds, ? (".", +digit).
months  --> digits.
years   --> digits.
days    --> digits.
hours   --> digits.
minutes --> digits.
seconds --> digits.
time_zone_interval --> ("+";"-"), hours, ":", minutes.

% -- HOST LANGUAGE INTERFACE -------
simple_val --> param_name ; embedded_variable_name ; literal(_).
general_val --> with_indicator(param_name)
            ;   with_indicator(embedded_variable_name)
            ;   @user ; @current, @user; @session, @user; @system, @user; @value
            ;   o('?').

with_indicator(Phrase) --> Phrase, ? (@indicator, Phrase).
param_name --> colon, identifier.
embedded_variable_name --> colon, host_identifier.
domain_name     --> identifier.
host_identifier --> identifier.


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

o(Op)    --> [t(d,operator,String)], {atom_string(Op,String)}.
@Word    --> [t(n,identifier(Word),_)].
string(T,Phrase) --> [t(_,string(T),String)], {freeze(String,phrase_string(q(Phrase),String))}.
t(Class) --> [t(_,Class,_)].

% ============== TOKENISATION ===========

tokens(Pred,[t(D1,T1,S1)|Ts]) --> call(Pred,D1,T1,S1), tokens(Pred,D1,Ts).
tokens(Pred,D1,[t(D2,T2,S2)|Ts]) --> inter_token(D1,D2), call(Pred,D2,T2,S2), !, tokens(Pred,D2,Ts).
tokens(_,_,[]) --> ?separator.

inter_token(d,_) --> [].
inter_token(n,d) --> [].
inter_token(_,_) --> separator.

separator --> +(any(" \n\t\r");comment).
comment --> "--", *(notany("\n\r")), any("\n\r").

scheme(n,identifier(I),_) --> {nonvar(I)}, !, fmt('~s',[I]).
scheme(D,T,S) --> {nonvar(S)}, !, {string_codes(S,C)}, list(C)//token(D,T).
scheme(n,C,_) --> fmt("[~@]",[(write_canonical(C),fail;true)]).

parse(D,T,S) --> token(D,T)//list(C), {string_codes(S,C)}.

token(n,charset)       --> "_", ? (regular_ident, "."), regular_ident.
token(D,identifier(I)) --> (  {nonvar(I)}
                           -> {atom_codes(I,Codes)}, identifier(generate,D,Codes)
                           ;  identifier(parse,D,Codes), {atom_codes(I,Codes)}
                           ).
token(d,string(char))  --> string_of(char//notany("'");"''").
token(n,string(char))  --> ("n";"N"), string_of(char//notany("'");"''").
token(n,string(hex))   --> ("x";"X"), string_of(digit;any("abcdefABCDEF")).
token(n,string(bit))   --> ("b";"B"), string_of("0";"1").
token(n,unsigned)      --> exact_number, ?exponent, not(".").
token(d,operator)      --> "<="; ">="; "<>"; "||"; ".."
                         ; ">", not("="); "<", not(">="); ".", not("."); "-", not("-")
                         ; any(",+*/&?%:;=()[]").

identifier(generate,n,Codes) --> seqmap(lower,Codes)//regular_ident.
identifier(parse,n,Codes) --> regular_ident//seqmap(lower,Codes).
identifier(_,d,Codes) --> "\"", esc(esc_qq,Codes), "\"".

% true when Lower is the lower case version of the next character in the sequence.
lower(Lower) --> [Either], {to_lower(Either,Lower)}.

to_lower(Upper,Lower) :- code_type(Lower,lower(Upper)).
to_lower(Char,Char) :- code_type(Char,graph), \+code_type(Char,upper).

% handles escaped sequences inside double quotes
esc_qq([0'"|Cs],Cs) --> "\"\"".
esc_qq([C|Cs],Cs) --> [C]//char//notany("\"").

exact_number --> digits, ? (".", ?digits); ".", digits.
exponent --> ("e";"E"), ?("+";"-"), digits.
regular_ident --> letter, **(letter;"_";digit).
string_of(Class) --> ++(separator,q(*(Class))).
q(Phrase) --> "'", Phrase, "'", not("'").

digits --> ++(digit).
digit  --> any("0123456789").
letter --> [X], {code_type(X,alpha)}.
char   --> [X], {code_type(X,graph); code_type(X,space)}.

not(_) --> end.
not(Cs), [X] --> [X]//notany(Cs).
end([],[]).


% --------------------------- NOT PROPERLY IMPLEMENTED ------------------------
data_type(numeric) --> @integer; @real.
data_type(string(char)) --> @text; @varchar.
data_type(datetime) --> @datetime; @date; @time.

% delayed length checker, useful for generating from grammars.
max_length([],0) :- !.
max_length(L,N) :- freeze(L,(L=[];L=[_|T],succ(M,N),max_length(T,M))).

