:- module(callgraph, 
	[	module_dotpdf/2
   ,  module_dot/2
	]).

/** <module> Visualisation of inter-predicate call graphs

   ---++ Usage
   This module allows you to produce a call graph of a module, where
   nodes (boxes) represent predicates in the module and an edge between
   two predicates indicates that one (the source end) calls the other
   (the pointy end).

   By default, node styles are used to indicate whether or not the predicate is
   exported (bold outline), dynamic (italic label, filled), multifile (box with diagonals
   in the corners). 
   For dynamic predicates, dashed edges are used to represent operations which
   mutate (assert to or retract from) the predicate.

   Items in the recorded database are also represented, as they consitute mutable
   state much like dynamic predicates. Recorded items are labelled as Key:Functor/Arity
   and drawn in a filled octagonal box. Dashed edges represents writing to the
   recorded database and ordinary solid edges represent reading.

   Basic method of usage is:
      1. Load the module you want to graph.
      2. Load this module.
      3. Generate and write the module call graph to a PDF document:
         ==
         ?- module_dotpdf(ModuleName,[]).
         ==

   See module_dot/2 for options that affect graph content and style, and
   module_dotpdf/2 for options that affect rendering.

   ---++ Implementation notes

   NB. This is very preliminary. The intereface is not necessarily stable.
   The dot DCG implementation is a bit embarassing but it does the job for now.

   Three parts to this:
   1. Using prolog_walk_code/1 to compile a database of inter-predicate calls
      into dynamic predicate edge/3.
   2. Possible transformations of graph (eg pruning).
   3. Collect information about desired modules/predicates into a Dot graph
      structure as defined by dotdct.pl

   ---+++ Types used internally
   
   *  pred_head
      Predicate specifier as Modue:Head where Head is a term with same 
      name and arity of the predicate concerned.
   *  module
      Module name as an atom.
   *  record_head
      Term like Key:Term, reference to recorded database terms with
      Term and given Key.
   *  node
      Node name as Atom:Name/Arity.
   ==
   module == atom.
   functor   ---> atom/natural.
   pred_head ---> module:term.
   node      ---> atom:functor.
   record_head ---> atom:term.
   edge_spec ---> dynamic(module,pred_head,list(edge_type))
                ; recorded(edge_type,record_head).
   edge_type ---> calls ; mutates ; reads ; writes.
   ==
*/


:- use_module('library/dcgu').
:- use_module('library/dot').

% ------------ Building the call graph in the Prolog database -----------------------

:- dynamic edge/3.
%% edge( -T:edge_type, -Source:node, -Target:node) is nondet.
%  Dynamic predicate for storing edges discoverd by code analysis.

retract_graph :- retractall(edge(_,_,_)).

%% assert_graph(+Modules:list(atom)) is det.
%  Analyses modules (using prolog_walk_code/1) asserting information about the
%  call graph to a set of private dynamic predicates. 
assert_graph(Mods) :-
   retract_graph,
   forall( member(Mod,Mods),
      prolog_walk_code([ trace_reference(_), module(Mod), on_trace(trace_call(Mods)), source(false) ])
   ),
	predicate_property(edge(_,_,_), number_of_clauses(N)),
	format('Got ~D edges.~n', [N]).


%% trace_call(+Mods:list(module), +Goal:pred_head, +Caller:pred_head, +Context) is det.
%  Callback predicate for prolog_walk_code/1.
trace_call(Mods, M1:H1, M2:H2, _) :-
   memberchk(M1,Mods), memberchk(M2,Mods), 
   head_node(M2:H2,Caller),
   (classify(M1:H1,Class) -> true; Class=normal(M1:H1)),
   assert_edges(Class,Caller,Mods).
trace_call(_,Goal,Caller,_) :-
   debug(callgraph,'Ignoring ~q ---> ~q.',[Caller,Goal]).


%% classify( +G:pred_head, -E:edge_spec) is semidet.
%  Goal Classification predicate. 
classify(M:assert(C),       dynamic(M,C,[mutates])).
classify(M:assertz(C),      dynamic(M,C,[mutates])).
classify(M:asserta(C),      dynamic(M,C,[mutates])).
classify(M:retractall(C),   dynamic(M,C,[mutates])).
classify(M:retract(C),      dynamic(M,C,[mutates,calls])).
classify(_:recorded(K,T,_), recorded(reads,K:T)).
classify(_:recorda(K,T,_),  recorded(writes,K:T)).
classify(_:recorda(K,T),    recorded(writes,K:T)).
classify(_:recordz(K,T,_),  recorded(writes,K:T)).
classify(_:recordz(K,T),    recorded(writes,K:T)).

%% assert_edges(+E:edge_spec, +Caller:node, +Mods:list(module)) is det.
%  Assert relevant edges (if any) for call from Caller to target specified by E.
assert_edges(recorded(T,Spec),Caller,_)   :- head_node(Spec,N), assert_edge(T,Caller,N).
assert_edges(dynamic(M,C,Ts), Caller, Ms) :- mod_clause_head(M,C,H), assert_types(Ts,H,Caller,Ms).
assert_edges(normal(Goal), Caller, Ms)    :- goal_pred_head(Goal,H), assert_types([calls],H,Caller,Ms).
assert_edges(_,_,_). % catch all if other clauses fail

%% mod_clause_head( +M:module, +C:clause, -H:pred_head) is det.
%  Combines a clause (as supplied to assert/retract) with its source
%  module to get the head (including module) of the dynamic predicate being
%  modified. Basically, it allows any module specified in C to to 
%  override M.
mod_clause_head(_, (M:H:-_), M:H) :- !.
mod_clause_head(_, (M:H), M:H) :- !.
mod_clause_head(M, (H:-_), M:H) :- !.
mod_clause_head(M, H, M:H).

%% assert_types(+Ts:list(edge_type), +Target:pred_head, +Caller:node, +Mods:list(module)) is semidet.
%  Assert edges of types in Ts from Caller to Target, but only if the 
%  home module of Target is a member of Mods.
assert_types(Types,M3:H,Caller,Mods) :-
   memberchk(M3,Mods),
   head_node(M3:H,Node),
   forall(member(T,Types), assert_edge(T,Caller,Node)).

%% goal_pred_head( +Goal:pred_head, -Pred:pred_head) is semidet.
% matches a goal to it's ultimate, non-built in source predicate.
% Fails if predicate is built in.
goal_pred_head(M1:H,M3:H) :-
   \+predicate_property(M1:H, built_in),
   (predicate_property(M1:H, imported_from(M3)) -> true; M3=M1).

%% assert_edge(+T:edge_type, +N1:node, +N2:node) is det.
% asserts edge if not already asserted.
assert_edge(T,N1,N2) :- edge(T,N1,N2) -> true; assertz(edge(T,N1,N2)).

%% head_node( +H:head, -P:node) is det.
%% head_node( -H:head, +P:node) is det.
%  convert predicate or record head from head term to functor/arity.
%  ==
%  head ---> atom:term.
%  ==
head_node(M:H,M:F/A) :- must_be(nonvar,M), (nonvar(H);ground(F/A)), functor(H,F,A).


% ----------------------- GraphML output ----------------------
% Leaving this out for the time being.

% module_graphml(Mod) :-
%    assert_graph([Mod]),
%    current_ugraph(Graph),
%    retract_graph,
%    format(atom(File),'~w.graphml',[Mod]),
%    graphml_write_ugraph(File, nomap, [], Graph).

% nomap(id,node(N),A) :- term_to_atom(N,A), writeln(nomap(id,node(N),A)).
% % nomap(id,edge(_,_),'').
%               % [key(node, color, string), key(edge,color,string)],
% % cmap(color, node(_), green).
% % cmap(color, edge(_), red).


% %% current_ugraph(-Graph:graphml) is det.
% %  Returns the current call graph as a GraphML document structure.
% current_ugraph(Graph) :-
%    findall(Pred, (calls_ir(Mod:Pred,_);calls_ir(_,Mod:Pred)), Preds),
%    sort(Preds,Preds1),
%    setof(Caller-Callees, (member(Caller,Preds1), callees(Mod,Caller,Callees)), Graph).

% callees(Mod,Caller,Callees) :- setof(Callee, calls_ir(Mod:Caller,Mod:Callee),Callees), !.
% callees(_,[]).



% ----------------------- Dot output ----------------------


%% module_dot(+ModuleName,Opts) is det.
%  Writes a call graph for named module as a dot file named "[ModuleName].dot". 
%  This predicate also accepts various options (some of the these types 
%  refer to the GraphViz attribute types):
%     * prune(Prune:bool) / false
%       If true, then graph subtrees are removed using prune_subtrees/0.
%     * recursive(bool) / false
%       If true, then looping edges are used to decorate predicates that call themselves
%       directly. Otherwise, such direct recursion is hidden.
%     * hide_list(list(pred_ind))  / []
%       A list of predicate (name/arity) to hide from the graph.
%     * arrowhead(arrow_type)  / vee
%       Dot arrowhead name as an atom, for all call edges.
%     * export_style(node_style)    / bold
%       Dot node style for exported predicates.
%     * dynamic_shape(node_shape)   / box
%       Dot node shape for dynamic predicates.
%     * dynamic_style(node_style)   / filled
%       Dot node style for dynamic predicates.
%     * multifile_shape(node_shape) / box
%       Dot node shape for multifile predicates.
%     * multifile_style(node_style) / diagonals
%       Dot node style for multifile predicates.
%     * recorded_shape(node_shape)  / octagon
%       Dot node shape for recorded facts.
%     * recorded_style(node_style)  / filled
%       Dot node style for recorded facts.
%     * mutate_style(line_style)    / dashed
%       Dot line style for edges representing mutation of a dynamic predicate.
%     * read_style(line_style)      / solid
%       Dot line style for edges representing readed of a recorded fact.
%     * write_style(line_style)     / dashed
%       Dot line style for edges representing writing of a recored fact.
%     * font(S:string)               
%       Font family (as a list of codes) for all labels. How these names are
%       interpreted depends on your host operating system. On Mac OS X, I find 
%       I am able to use any font available in the "Font Book" application with
%       the name written exactly (including spaces) as in the "Font" column.
%       Default is "Times".
%
% Types for Dot attributes:
% see http://graphviz.org/Documentation.php for more details on
%  * arrow_type: http://graphviz.org/content/attrs#karrowType
%  * node_shape: http://graphviz.org/content/node-shapes 
%  * node_style: http://graphviz.org/content/attrs#kstyle
%
% ==
% line_style ---> solid ; dashed ; dotted ; bold.
% arrow_type ---> normal ; vee ; empty ; box ; none ; dot ; ... . 
% node_shape ---> box ; ellipse ; circle ; diamond ; trapezium ; parallelogram 
%               ; house ; square ; pentagon ; hexagon ; septagon ; octagon ; ... .
% node_style ---> solid ; dashed ; dotted ; bold ; rounded 
%               ; diagonals ; filled ; striped ; wedged. 
% ==
module_dot(Mod,Opts) :-
   assert_graph([Mod]),
   (option(prune(true),Opts) -> prune_subtrees; true),
   current_dot(Mod,Opts,Graph),
   retract_graph,
   format(atom(File),'~w.dot',[Mod]),
   graph_dot(Graph,File).

%% module_dotpdf(+Mod,Opts) is det.
%  Writes a call graph for module Mod as a PDF file named "[Mod].pdf". 
%  As well as the options accepted by module_dot/2, this predicate also accepts:
%   * method(Method:graphviz_method) / unflatten
%     Determines which GraphViz programs are used to render the graph. The type 
%     graphviz_method is defined as:
%     ==
%     graphviz_method ---> dot ; neato; fdp ; sfdp ; circo ; twopi
%                        ; unflatten(list(unflatten_opt))
%                        ; unflatten.
%     unflatten_opt   ---> l(N:natural)   % -l<N>
%                        ; fl(N:natural)  % -f -l<N>
%                        ; c(natural).    % -c<N>
%     ==
%     The unflatten methods filter the graph through unflatten before passing
%     on to dot.
module_dotpdf(Mod,Opts) :-
   assert_graph([Mod]),
   option(method(Method),Opts,unflatten),
   (option(prune(true),Opts) -> prune_subtrees; true),
   current_dot(Mod,Opts,Graph),
   retract_graph,
   dotrun(Method,pdf,Graph,Mod).


%% current_dot(+Mod,+Opts,-DotGraph) is det.
%  Returns the currently asserted graph as a dot graph structure,
%  using the given options and restricting the graph to module Mod.
%  The options are documented under module_dot/2.
current_dot(Mod,Opts,digraph(Mod,Graph)) :-
   predopt(Opts,recorded,DBNodeAttr,[]),
   setof(with_opts(node(Pred),Attrs), node_decl(Opts,Mod,Pred,Attrs), Decls),
   esetof(with_opts(node(N),DBNodeAttr), db_node(N), DBNodes),
   writeln(DBNodes),
   module_graph(Mod,Opts,Decls,DBNodes,Graph,[]).

node_decl(Opts,Mod,Pred,Attrs) :-
   declarable_node(Opts,Mod,Pred),
   pred_attr(Opts,Mod:Pred,Attrs).

read_edge(Mod,_Opts,Pred,DBTerm) :- edge(reads,Mod:Pred, DBTerm).
write_edge(Mod,_Opts,Pred,DBTerm) :- edge(writes,Mod:Pred, DBTerm).

declarable_node(Opts,M,Pred) :-
   option(hide_list(HideList),Opts,[]),
   (  predicate_property(M:Head, dynamic)
   ;  predicate_property(M:Head, exported)
   ;  predicate_property(M:Head, multifile)
   ),
   \+predicate_property(M:Head, built_in),
   \+predicate_property(M:Head, imported_from(_)),
   head_node(M:Head,M:Pred),
   \+member(Pred, ['$mode'/2,'$pldoc'/4, '$pldoc_link'/2]),
   \+member(Pred,HideList).

visible_call(Mod,Opts,Caller,Callee) :-
   option(hide_list(L),Opts,[]),
   option(recursive(T),Opts,false),
   edge(calls,Mod:Caller,Mod:Callee),
   (T=false -> Caller\=Callee; true),
   \+member(Caller,L),
   \+member(Callee,L).

visible_mutation(Mod,Opts,P1,P2) :-
   option(hide_list(L),Opts,[]),
   edge(mutates,Mod:P1,Mod:P2),
   \+member(P1,L),
   \+member(P2,L).

module_graph(Mod,Opts,Decls,DBNodes) -->
   {  edgeopt(Opts,mutates,MAttr,[]),
      edgeopt(Opts,reads,RAttr,[]),
      edgeopt(Opts,writes,WAttr,[])
   },
   seqmap(global_opts(Opts),[graph,node,edge]), % global attributes
   list(Decls), list(DBNodes),
   findall(arrow(Caller,Callee), visible_call(Mod,Opts,Caller,Callee)),
   findall(with_opts(arrow(Mutator,Mutatee),MAttr), visible_mutation(Mod,Opts,Mutator,Mutatee)),
   findall(with_opts(arrow(Pred,DBTerm),RAttr),     read_edge(Mod,Opts,Pred,DBTerm)),
   findall(with_opts(arrow(Pred,DBTerm),WAttr),     write_edge(Mod,Opts,Pred,DBTerm)).

esetof(A,B,C) :- setof(A,B,C) *-> true; C=[].

list([]) --> [].
list([X|XS]) --> [X], list(XS).

db_node(N) :- edge(reads,_,N); edge(writes,_,N).

global_opts(_,graph) --> [].
global_opts(O,node) --> {font(normal,O,F)}, [node_opts([ shape=at(box), fontname=qq(F) ])].
global_opts(O,edge) --> {option(arrowhead(AH),O,vee)}, [edge_opts([ arrowhead=at(AH) ])].

predopt(O,exported) --> 
   {option(export_style(S),O,bold)}, 
   {font(bold,O,F)}, 
   [ style = qq(at(S)), fontname=qq(F) ].
predopt(O,dynamic) --> 
   {option(dynamic_shape(S),O,box)}, 
   {option(dynamic_style(St),O,filled)},
   {font(italic,O,F)}, 
   [ shape = at(S), fontname=qq(F), style = qq(at(St)) ].
predopt(O,multifile) --> 
   {option(multifile_shape(S),O,box)}, 
   {option(multifile_style(St),O,diagonals)},
   [ shape = at(S), style = qq(at(St)) ].
predopt(O,recorded) --> 
   {option(recorded_shape(S),O,octagon)}, 
   {option(recorded_style(St),O,filled)},
   [ shape = at(S), style = qq(at(St)) ].

edgeopt(O,mutates) --> {option(mutate_style(S),O,dashed)}, [ style = qq(at(S)) ].
edgeopt(O,writes) --> {option(write_style(S),O,dashed)}, [ style = qq(at(S)) ].
edgeopt(O,reads) --> {option(read_style(S),O,solid)}, [ style = qq(at(S)) ].

pred_attr(O,Pred,Attrs1) :-
   head_node(Goal,Pred),
   phrase( (  if( predicate_property(Goal,dynamic), predopt(O,dynamic)),
              if( predicate_property(Goal,multifile), predopt(O,multifile)),
              if( predicate_property(Goal,exported), predopt(O,exported))), 
           Attrs, []),
   Attrs = [_|_],
   compile_attrs(Attrs,[],Attrs1).

compile_attrs([],A,A).
compile_attrs([style=S|AX],AttrsSoFar,FinalAttrs) :- !,
   (  select(style=OS,AttrsSoFar,A1)
   -> combine_styles(S,OS,NS), A2=[style=NS|A1]
   ;  A2=[style=S|AttrsSoFar]
   ),
   compile_attrs(AX,A2,FinalAttrs).
compile_attrs([A|AX],A0,A2) :- compile_attrs(AX,[A|A0],A2).

combine_styles(qq(S1),qq(S2),qq((S1,",",S2))).

% compile_attrs1([],A,[]).
% compile_attrs1([A|AX],A0,[A|A1]) :- compile_attrs1(AX,[A|A0],A1).

font_family(O) --> {option(font(FF),O,"Times")}, seqmap(out,FF).
font(normal,O,F) :- phrase(font_family(O),F,[]).
font(italic,O,F) :- phrase((font_family(O)," Italic"),F,[]).
font(bold,O,F)   :- phrase((font_family(O)," Bold"),F,[]).

do_until(P) :-
   call(P,Flag),
   (  Flag=true -> true
   ;  do_until(P)
   ).

prunable(Node) :-
   setof( Parent, edge(calls,Parent,Node), [_]), % node has exactly one caller
   \+edge(_,Node,_), % edges out 
   head_node(G,Node),
   \+predicate_property(G,dynamic),
   \+predicate_property(G,multifile),
   \+predicate_property(G,exported).

%% prune_subtrees is det.
%  Operates on the currently asserted graph (see assert_graph/1). It searches
%  for any part of the call graph which is a pure tree, and removes all the nodes below
%  the root. Thus, any 'leaf' predicate which is only ever called by one 'parent' is
%  removed. This is step is repeated until there are no more leaf predicates. The idea
%  is that the child tree can be considered 'private' to its parent.
prune_subtrees :- do_until(prune_subtrees).

prune_subtrees(false) :-
   bagof(Node, prunable(Node), Nodes), !,
   forall(member(N,Nodes), (writeln(pruning:N), retractall(edge(calls,_,N)))).

prune_subtrees(true).
