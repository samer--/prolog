/* This started out as Hákun Skarðhamar's postgres.pl from the swi2pg packege.
** Hardly anything is left of the original, but I did use it for a while until
** I eventually got round to rewriting it.
** 
** Samer Abdallah,
** C4DM, QMUL 2010.
**
** ---(Rest of original header)----
** 
** Copyright (C) Hákun Skarðhamar, 2001
**
** This program is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free Software
** Foundation; either version 2 of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
** PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.
*/
	  
:- module(pldb, [
		db_connection/2
	,	db_connect/2
	,	db_disconnect/1
	,	db_current_table/3
	,	db_transaction/2

	,	db_create_table/3
   ,  db_drop_table/2
	,	db_update/3
	,	db_delete/3
	,	db_insert/2
	,	db_select/1
	,	db_select/2
	,	db_select/3

	,	op(990,xfx,:=)
	]).

:- op(200,fy,@).

:- multifile dbh_table/2,
             dbh_table_column/4,
             dbh_query_affected/3,
             dbh_query_row/3,
             dbh_query_all/3,
             dbh_connect/2,
             dbh_disconnect/1.

:- multifile typed_value//2, type_class/2.


/** <module> ODBC database access

This module provides a Prolog predicate-like level of access to 
ODBC data sources.

==
typed(A) ---> A : type_name.

head(A) ---> univ( [ table_name | list(A) ])
           ; proj( proj, list(A)).

proj ---> table_name ^ list(column_name).

column_spec == typed(column_name).

table_name  == atom.
column_name == atom.
type_name   == atom.
==

Select and update language specification:
==
selector ---> pattern ; var := tester .

updater ---> selector ; (selector -> value).

result ---> type_name : value.

pattern ---> var              % leads to no condition
           ; null(_)          % leads to equality condition
           ; ground_term      % leads to equality condition
           ; partial_term     % leads to regular expression
           .

tester ---> (tester,tester)
          ; (tester;tester)
          ; \+(tester)
          ; null
          ; =(value)
          ; \=(value)
          ; >=(value)
          ; =<(value)
          ; <(value)
          ; >(value)
          ; like(atom)
          ; in(list(value))
          .

typed_condition ---> condition : type_name.
==

SQL statement type.
==
sql_statement ---> atom(atom)
                 ; phrase(phrase).
==

@author Samer Abdallah

@tbd
	debug odbc aggregates and findall
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(snobol)).
:- use_module(library(apply_macros)).
:- use_module(library(quasi_quotations)).
:- use_module(library(dcg/sql)).
:- use_module(library(pldb_facts)).

:- quasi_quotation_syntax(sql).
:- dynamic db_state/3.

:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(back_quotes, codes).


% CONNECTIONS ---------------------------------------------------------------- 

%% db_connection(-Con,-Spec) is nondet.
%  Enumerate current connections.
db_connection(Con,Spec) :- get_state(connection,Con,Spec).

%% db_connect(+Spec, -Con) is semidet.
%
%  Open connection using ODBC connection string DBString. Connection handle
%  will be Con. If a connection is already open with this Con, then this
%  will throw an exception.  Close it with db_disconnect/1.
%
%  @error Throws error(pldb:already_open(Spec,Con)) if connection is already open.
db_connect(Spec,Con) :- 
   must_be(nonvar,Spec),
   (  get_state(connection,Con1,Spec) 
   -> print_message(warning,error(pldb:already_open(Spec,Con1)))
   ;  dbh_connect(Spec,Con),
      put_state(connection,Con,Spec)
   ).

%% db_disconnect(+Con) is det.
%
%  Disconnection from active connection.
%
%  @error Throws error(pldb:not_open(Con)) if connection is not open.
db_disconnect(Con) :- 
   must_be(nonvar,Con),
	(	get_state(connection,Con,_)
	-> del_state(_,Con,_), 
		dbh_disconnect(Con)
	;	print_message(warning,error(pldb:not_open(Con)))
	).

%% db_current_table( +Con, -Table, -Spec:list(column_spec)) is nondet.
%
%  True when Table is a table in the database on connection Con with 
%  columns and types specified by Spec.
db_current_table(Con,Table,Spec) :-
	dbh_table(Con,Table),
	findall(Attr:Type,dbh_table_column(Con,Table,Attr,Type),Spec).


% - Manage state ---------------------------------------------------------- 

get_state(K1,K2,V) :- db_state(K1,K2,V).
del_state(K1,K2,V) :- retractall(db_state(K1,K2,V)).
put_state(K1,K2,V) :- 
   (  db_state(K1,K2,V) -> throw(error(pldb:state_already_set(K1,K2,V)))
   ;  asserta(db_state(K1,K2,V))
   ).

dbxs(Con,String) :- dbh_query_affected(Con,String,_).
dbx(Con,Phrase) :- sql_generate(Phrase,String), !, dbh_query_affected(Con,String,_).
dbx_row(Con,Phrase,Row) :- sql_generate(Phrase,String), !, dbh_query_row(Con,String,Row).
dbx_affected(Con,Phrase,N) :- sql_generate(Phrase,String), !, dbh_query_affected(Con,String,N).

% ================= CONVENIENCE WRAPPERS FOR SQL STATEMENTS ==============

% TRANSACTIONS --------------------------------------------------------------- 

%% db_transaction(+Con, :Goal) is nondet.
%
%  Wrap procedure in a transaction. If the goal fails or throws an exception,
%  the transaction is rolled back. Otherwise, it is commited.

db_transaction(Con,Goal) :-
   must_be(nonvar,Con),
	setup_call_catcher_cleanup( begin_trans(Con), once(Goal), Status,
                               (  Status=exit -> end_trans(Con,"COMMIT")
                               ;  print_message(warning,pldb:rollback(Status,Goal)), 
                                  end_trans(Con,"ROLLBACK")
                               )).

begin_trans(Con)   :- put_state(trans,Con,open), dbxs(Con,"BEGIN").
end_trans(Con,Act) :- del_state(trans,Con,_),   dbxs(Con,Act).


% ----------------- CREATE/DROP TABLES -------------------------------

%% db_create_table(+Con, +TableName:atom, +Columns:list(column_spec)) is semidet.
%
% Create a table with given name and column types.
db_create_table(Con,Table,Columns) :- 
   maplist(column_spec_to_table_element,Columns,Elements),
   dbx(Con,statement(create_table(Table,Elements,nothing))).

% convert simple column spec to full table element spec
column_spec_to_table_element(Name:Type,column(Name,Type,nothing,[],nothing)).

db_drop_table(Con,Table) :-
   dbx(Con,statement(drop_table(Table))),
   del_state(table(Table),Con,_).

% ----------------- INSERTING  ----------------------------

%% db_insert( +Con, +Head:head(value)) is semidet.
%
% Add a tuple represented by the term Tuple to a database. The name 
% if the table is head functor of Tuple and the N arguments correspond
% to the first N columns of the table. 

db_insert(Con,Head) :-
	analyse_head(Con,Head,Table,Args,Cols),
   maplist(insert_val,Cols,Args,Names,Vals),
	dbx(Con, statement(insert(Table,query(just(Names),values([row(Vals)]))))).

insert_val(Name:Type, Arg, Name, Val) :- typed_val(Type,Arg,Val).

% -------------------- SELECT -------------------------------- 

%% db_select( +Con, +Head:head(selector), +Ordering:list(order)) is nondet.
%% db_select( +Con, +Head:head(selector)) is nondet.
%% db_select( +Head:head(selector)) is nondet.
%
%  Database query of table TableName where functor(Head,TableName,N).
%  Arguments of Head term encode columns of table. If ordering is
%  specified, Ordering is a list of terms asc(K) or desc(K), which
%  means order by the Kth argument of Head.
%  ==
%  order ---> asc(natural) ; desc(natural).
%  ==

db_select(Head) :- db_select(_,Head).
db_select(Con,Head) :- db_select(Con,Head,nothing).
db_select(Con,Head,OrderBy) :-
	dbx_row( Con, compose_select(Con,Head,OrderBy,Arglst), Row),
   Row=..[row|Vals],
	maplist(decode,Vals,Arglst).

decode(Y,X:T) :- type_decode(T,Y,X).


% --------------- COMPOSE SELECT STATEMENT ----------------------------

%% compose_select( +Con, +Head, +Ord:ordering, -Args:list(result))// is det.
compose_select(Con,Head,Ord,Results) -->
	{	analyse_head(Con,Head,Tab,Args,Columns),
		build_select(Args,Columns,SelArgs,Where,Results),
	   maplist(build_ord(Columns),OrderList,OLS)
	},
	statement(ordered_select( select(nothing,SelArgs,just([Tab]),just(Where),nothing,nothing),Ord)).

build_ord(Columns,asc(K),ord(Name,asc)) :- nth1(K,Columns,Name:_).
build_ord(Columns,desc(K),ord(Name,desc)) :- nth1(K,Columns,Name:_).

%% build_select( +Action:list(selector), +Cols:list(column_spec), -Sels:list(selection), -Args:list(typed_condition), -Types:list(result)) is det.
build_select(Actions,Cols,SelArgs1,Where,Args1) :-
	seqmap(sel, Actions, Cols, Where-(SelArgs-Args), []-([]-[])),
	(	SelArgs=[] -> SelArgs1=[0], Args1=[_:int4]
	;	SelArgs1=SelArgs, Args1=Args
   ).


%% sel( +X:selector, +Col:column_spec)// is det.
%  Handle one selector and generate match conditions and selection arguments  as necessary. 
sel(V,N:T) --> \< match(V,N:T), \> if( sel_out(V,V1), out(N) <\> out(V1:T)).

sel_out(V,V) :- var(V), !.
sel_out(V:=_,V) :- !.
sel_out(V,V) :- V\=null(_), \+ground(V).


% ----------------- UPDATE/DELETE   ----------------------------

%% db_update(+Con, +Head:head(updater),-N:nonneg) is det.
%
%  Update rows of a table (not in a transaction).
db_update(Con,Head,N) :-
	analyse_head(Con,Head,Tab,Actions,Cols),
	seqmap(upd,Actions,Cols,Set-Where,[]-[]),
   and_list(Where, Cond),
   dbx_affected(Con,statement(update(Tab,Set,Cond)),N).

%% upd( +X:updater, +Col:column_spec)// is det.
% works on paired state DCG, (Sets,Wheres) 
upd(X,_)      --> {var(X)},!.
upd(P->V,N:T) --> !, \> match(P,N:T), {typed_val(T,V,X)}, \< out(N=X).
upd(P,N:T)    --> \> match(P,N:T).


%% db_delete(+Con,+Head:head(selector),-N:natural) is semidet.
%% db_delete(+Con,+Head:head(selector)) is semidet.
%
% Delete rows from table identified by Head using given connection.
% N is the number of rows deleted.
db_delete(Con,Head) :- db_delete(Con,Head,_).
db_delete(Con,Head,N) :-
	analyse_head(Con,Head,Tab,Action,Cols),
	seqmap(match,Action,Cols,Where,[]),
   and_list(Where, Cond),
	dbx_affected( Con, statement(delete(Tab,Cond)), N). 

and_list([X|XX],and(X,YY)) :- and_list(XX,YY).
and_list([X],X).


% --------- Generation of matching conditions -----------

%% match( +X:selector, +Col:column_spec)// is det.
match(V,     _)   --> {var(V)}, !.
match(V:=C,  N:T) --> !, {must_be(var,V),mk_cond(col(T,N),C,C1)}, out(C1).
% match(V,     N:T) --> ( {ground(V);V=null(_)} -> out((N:T)=V)
%                       ; {phrase_string(term_pattern(V),Pattern)}, out(like(N,Pattern):T)
%                       ).

%% mk_cond( +Name:column_name, +C1:tester, -C2:condition) is det.
% Translate unary algebra of conditions on column into boolean expression.
mk_cond(S, (C1,C2), and(C11,C21)) :- mk_cond(S,C1,C11), mk_cond(S,C2,C21).
mk_cond(S, (C1;C2), or(C11,C21)) :- mk_cond(S,C1,C11), mk_cond(S,C2,C21).
mk_cond(S, \+(C),  not(C1))    :- mk_cond(S,C,C1).
mk_cond(S, =(X),   cmp(=,S,Y)) :- typed_val(_,X,Y).
mk_cond(S, \=(X),  cmp('<>',S,Y)) :- typed_val(_,X,Y).
mk_cond(S, <(X),   cmp(<,S,Y)) :- typed_val(_,X,Y).
mk_cond(S, >(X),   cmp(>,S,Y)) :- typed_val(_,X,Y).
mk_cond(S, =<(X),  cmp((<=),S,Y)) :- typed_val(_,X,Y).
mk_cond(S, >=(X),  cmp((>=),S,Y)) :- typed_val(_,X,Y).
mk_cond(S, null,   null(S) is true) :- typed_val(_,X,Y).
mk_cond(S, like(X),like(S,Y) is true) :- typed_val(_,X,Y).
mk_cond(S, in(X),  in(S,Y) is true) :- typed_val(_,X,Y).

% ================== Support predicates =========================

%% analyse_head( +Con, +Head:head(A), -Table:table_name, -Actions:list(A), -Cols:list(column_spec)) is det.
%  Decompose a Head term into table name, list of per-column arguments, and
%  a matching list of column names and types.
analyse_head(Con,proj(Projection,Actions),Table,Actions,ProjColumns) :- !,
	Projection = Table^ProjNames,
	table_columns(Con,Table,TabCols),
	maplist(name_colspec(TabCols),ProjNames,ProjColumns),
   same_length(Actions,ProjColumns).

analyse_head(Con,Head,Table,Actions,Cols) :-
	Head =.. [Table|Actions],
	table_columns(Con,Table,Cols),
   same_length(Actions,Cols).

name_colspec(TabCols,Name,Name:Type) :- 
   memberchk(Name:Type,TabCols).

% memoised table columns and types
table_columns(Con,Tab,Typelist) :- 
   get_state(table(Tab),Con,Typelist),!.
table_columns(Con,Tab,Typelist) :-
   db_current_table(Con,Tab,Typelist),
	put_state(table(Tab),Con,Typelist).

same_length(L1,L2) :- length(L1,N), length(L2,N).

% ======================== Quasi-quatation ============================

sql(Content,Vars,Dict,Phrase) :-
   include(qq_vars(Vars),Dict,QQDict),
   phrase_from_quasi_quotation(sql_quasi(QQDict,Phrase),Content).

sql_quasi(Dict,Stuff) -->
   arb//trans(S1,S2),
   (  prolog_var_name(Name), {memberchk(Name=Subs, Dict)}
   -> {append_dl(Codes,S2,S1), Stuff=(Codes,sql(Subs),More)},
      sql_quasi(Dict,More)
   ;  \+[_]
   -> {append_dl(Codes,S2,S1), Stuff=Codes}
   ).

append_dl([],Tail,List) :- Tail==List, !.
append_dl([X|XX],Tail,[X|LL]) :- append_dl(XX,Tail,LL).

qq_vars(Vars,_=Var) :- member(V,Vars), V==Var, !.

% ------------------------ DCG rules ----------------------------------------
% expr(T,V)       --> freeze(T,typed_value(T,V)).

typed_value(T,V) --> {number(V), type_class(T,floating)},!, fmt('~16g',[V]).
typed_value(T,V) --> {number(V), type_class(T,numeric)},!, at(V).
typed_value(T,V) --> {atomic(V), type_class(T,textual)},!, quote(at(V)).
typed_value(T,B) --> {T=boolean, !, bool_bool(B,BB)}, at(BB). % !!! FIXME

typed_val(T,X,-Y) :- type_class(T,numeric), number(X), X<0, !, typed_val(T,X,Y).
typed_val(_,null(_),null) :- !. 
typed_val(T,X,unsigned(Num)) :- type_class(T,numeric), !, numeric_val(T,X,Num).
typed_val(T,X,string(char,Codes)) :- type_class(T,textual), !, string_codes(X,Codes).

numeric_val(T,X,int(X)) :- type_class(T,integral), !.
numeric_val(T,X,token(numeric,n,dcg_basics:float(X))) :- type_class(T,floating), !.

type_decode(_,null(_),null(_)) :- !.
type_decode(_,'$null$',null(_)) :- !.
type_decode(bool,t,true) :- !.
type_decode(bool,f,false) :- !.
type_decode(datetime,T,T) :- !.
type_decode(timestamptz,T,T) :- !.
type_decode(F,X,P) :- type_class(F,numeric),!, atom_to_term(X,P,_).

bool_bool(true,t).
bool_bool(false,f).

type_class(char(_),   textual).
type_class(varchar(_),textual).
type_class(varchar,   textual).
type_class(text,      textual).

type_class(float4,   floating).
type_class(float8,   floating).
type_class(duration, floating).

type_class(integer,  integral).
type_class(int2,     integral).
type_class(int4,     integral).
type_class(int8,     integral).

type_class(decimal(_,_), numeric).
type_class(T, numeric) :- type_class(T, integral).
type_class(T, numeric) :- type_class(T, floating).

prolog:message(error(pldb:Term)) --> db_message(Term).
prolog:message(pldb:Term) --> db_message(Term).

db_message(nonground(V))          --> ['Value to write (~w) must be ground.'-[V]].
db_message(nothing_selected)      --> ['SQL Query produces no output.'-[]].
db_message(columns_mismatch)      --> ['Query does not match table columns.'-[]].
db_message(pattern_must_be_var)   --> ['Pattern must be variable when condition is specified.'-[]].
db_message(not_open(C))           --> ['Database connection ~w is not open..'-[C]].
db_message(already_open(S,C))     --> ['Database connection ~w is already open on ~w.'-[S,C]].
db_message(rollback(Status,Goal)) --> ['Transaction rollback due to ~w in goal ~q.'-[Status,Goal]].

