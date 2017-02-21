
% weird failure driven EM!
:- meta_predicate em(+,0,+,-), em(+,0,+,+,-).

em(Graph, Goal, P0, P2) :-
   graph_stats(Graph, Goal, P, LP, Stats),
   nb_setval(emp, P0),
   repeat,
   nb_getval(emp,P),
   stats_to_params(Stats,P2),
   format('Log prob = ~g\n',[LP]),
   nb_setval(emp,P2).

% broken aggregation based graph inversion
invert_graph(IGraph, InvGraph) :-
   bagof(Q-QCs, bagof(QC, IGraph^q_context(IGraph,Q,QC), QCs), InvGraph).

q_context(InsideG, QBetaQ, qc(Pe,P)) :-
   member(soln(P,_,Expls), InsideG),
   member(Expl-Pe, Expls), 
   member(QBetaQ, Expl).

