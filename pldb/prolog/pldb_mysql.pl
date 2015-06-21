:- module(pldb_mysql,[]).
:- multifile pldb:type_class/2.

type_class(tinyint,    numeric).
type_class(smallint,   numeric).
type_class(mediumint,  numeric).
type_class(int,        numeric).
type_class(bigint,     numeric).
type_class('integer unsigned', numeric).
type_class('tinyint unsigned', numeric).
