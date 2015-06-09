:- module(pldb_sqlite,[]).

:- use_module(library(prosqlite)).
:- use_module(pldb).

pldb:dbh_connect(sqlite(File,Opts),sqlite(Con))    :- sqlite_connect(File,Con,[ext('')|Opts]).
pldb:dbh_disconnect(sqlite(Con))                   :- sqlite_disconnect(Con).
pldb:dbh_table(sqlite(Con),Table)                  :- sqlite_current_table(Con,Table).
pldb:dbh_table_column(sqlite(Con),Table,Col,Type)  :- sqlite_table_column(Con,Table,Col,data_type(N)), downcase_atom(N,Type).
pldb:dbh_query_affected(sqlite(Con),Statement,N)   :- sqlite_query(Con,Statement,row(N)).
pldb:dbh_query_row(sqlite(Con),Statement,Row)      :- sqlite_query(Con,Statement,Row).
pldb:dbh_query_all(sqlite(Con),Statement,Rows)     :- findall(Row, sqlite_query(Con,Statement,Row), Rows).
