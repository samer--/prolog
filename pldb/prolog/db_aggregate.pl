:- module(db_aggregate, [db_aggregate/5]).

:- use_module(library(pldb_core)).

%% db_aggregate( +Con, +Head:head(selector), +Spec:list(aggr), +GVars:list(var), -Res:list(_)) is nondet.
%
%  Aggregation operations over tables. This predicate is intended to work somewhat
%  like SWI Prolog's aggregation library (aggregate.pl) but using SQL Select statements.
%    
%  ---+++ Examples
%
%  Given a database connection db and a table called 'census' with columns 'country', 
%  'town' and 'population',  we can compute the following:
%  The total urban population:
%  ==
%  db_aggregate(db,census(_,_,P),[],[sum(P)],[TotalPopn]).
%  ==
%  Urban population of China:
%  ==
%  db_aggregate(db,census(china,_,P),[],[sum(P)],[ChinaPopn]).
%  ==
%  Number of towns and urban population of each country 
%  (goes through each country on backtracking):
%  ==
%  db_aggregate(db,census(C,T,P),[C],[count(T),sum(P)],[NumTowns,NationalPopn]).
%  ==
%  Average population of cities with more than 100,000 people:
%  ==
%  db_aggregate(db,census(_,_,P:= >(100,000)),[],[avg(P)],[AvgCity]).
%  ==
%
%  The arguments are as follows:
%
%     @param Head A term with the table name as the head functor and selector terms as arguments.
%        This defines the 'FROM <tablename> WHERE <conditions>' part of the SQL query,
%        just as in db_select/2. Variables in this term will be unified with values from
%        the table during processing.
%
%     @param GVars These are the variables which are to be considered 'free' in the whole
%        operation, that is, they will be unified with particular values on exit,
%        and different values can be obtained on backtracking. Translates to the
%        'GROUP BY' clause of the SQL statement.
%
%     @param Spec A list of aggregation expressions in terms of variables in the query
%        defining term Head. Each expression must be a valid SQL expression
%        when the variables are unified with the corresponding column names.
%
%     @param Res A list of values, one for each aggregate expression in Spec. 

db_aggregate(Con,Head,Globals,Spec,Output) :- 
	phrase_string( compose_aggregate(Con,Head,Spec,Globals,Output,Result), SQL),
	dbh_query_row(Con,SQL,Row), Row=..[row|Vals],
	maplist(decode,Vals,Result).


%% compose_aggregate( +Con, +Head, +Spec, +Globals, @Out:list(var), -Args:list(result))// is det.
compose_aggregate(Con,Head,Spec,Globals,Output,AggResults) -->
	{	analyse_head(Con,Head,Tab,Args,Columns),
		build_select(Args,Columns,SelArgs,Where,Results),
		copy_term(Globals+Results+Spec,Globals1+Results1+Spec1),

		maplist(fst,Results1,SelArgs), % should bind variables in Globals1 to column names
		maplist(aux2(Columns),Globals,Globals1,GResults), % result catchers for global vars
		maplist(aux3,Output,Spec1,AResults), % result catchers for aggregates
		append(Globals1,Spec1,ToSelect), % list of things to SELECT is globals++aggregates
		append(GResults,AResults,AggResults) % list of result catchers
	},
	sql(select(Tab,ToSelect,Where)),
	group_by(Globals1).

% used above in compose_aggregate//6
aux2(Cols, X, Name,X:Type) :- member(Name:Type,Cols).
aux3(X, count(_), X:int4) :- !.
aux3(X, _, X:numeric).
fst(X:_,X).
