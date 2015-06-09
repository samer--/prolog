:- module(pldb_odbc,[]).

:- use_module(library(odbc)).
:- use_module(library(pldb_core)).

pldb:dbh_connect(odbc(DSN,Opts),odbc(Con))       :- odbc_connect(DSN,Con,Opts).
pldb:dbh_disconnect(odbc(Con))                   :- odbc_disconnect(Con).
pldb:dbh_table(odbc(Con),Table)                  :- odbc_current_table(Con,Table).
pldb:dbh_query_row(odbc(Con),Statement,Row)      :- odbc_query(Con,Statement,Row).
pldb:dbh_query_all(odbc(Con),Statement,Results)  :- odbc_query(Con,Statement,Results,[findall(Row,Row)]).
pldb:dbh_query_affected(odbc(Con),Statement,N)   :- odbc_query(Con,Statement,affected(N)).
% pldb:dbh_table_column(odbc(Con),Table,Col,Type)  :- odbc_table_column(Con,Table,Col,type(Type)).
pldb:dbh_table_column(odbc(Con),Table,Col,Type)  :- 
   odbc_get_connection(Con,dbms_name(DBMS)),
   odbc:table_column(Con,Table,Col,Info),
   arg(6,Info,TypeName),
   (  dbms_sql_type(DBMS,TypeName,Info,Type) -> true
   ;  odbc:sql_type(TypeName,Info,Type)
   ).

dbms_sql_type('SQLite',     TypeName,_,    Type)       :- downcase_atom(TypeName,X), atom_to_term(X,Type,[]).
dbms_sql_type('PostgreSQL', bpchar,  Info, char(N))    :- odbc:column_facet(precision(N),Info).
dbms_sql_type(_,            varchar, Info, varchar(N)) :- odbc:column_facet(precision(N),Info).
dbms_sql_type(_,            char,    Info, char(N))    :- odbc:column_facet(precision(N),Info).

