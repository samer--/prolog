:- module(pldb_postgres,[]).

:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- multifile pldb:typed_value, pldb:type_decode/3.

pldb:typed_value(T,V) --> {is_geometric(T)}, !, quote(geo(T,V)).

pldb:type_decode(F,X,P) :- is_geometric(F), !, atom_codes(X,C), phrase(geo(F,P),C,[]). 

geo( point,   point(A,B))         --> paren((float(A), comma, float(B))).
geo( lseg,    linesegment(P1,P2)) --> paren(( geo(point,P1), comma, geo(point,P2))).  
geo( lseg,    line(P1,P2))        --> sqbr(((geo(point,P1), comma, geo(point,P2)))).
geo( line,    line(P1,P2))        --> sqbr(((geo(point,P1), comma, geo(point,P2)))).
geo( box,     box(P1,P2))         --> geo(point,P1), comma, geo(point,P2).
geo( circle,  circle(P,R))        --> "<", geo(point,P), comma, float(R), ">". 
geo( path,    path(closed,PX))    --> paren(seqmap_with_sep(comma, geo(point), PX)).
geo( path,    path(open,PX))      --> sqbr(seqmap_with_sep(comma, geo(point), PX)).
geo( polygon, polygon(PX))        --> paren(seqmap_with_sep(comma, geo(point), PX)).

is_geometric(point).
is_geometric(lseg).
is_geometric(path).
is_geometric(box).
is_geometric(circle).
is_geometric(polygon).
is_geometric(line).

