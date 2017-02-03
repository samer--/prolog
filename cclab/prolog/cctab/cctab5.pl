:- module(cctab, [run_tabled/1, call_tabled/1, op(1150,fx,cctable)]).

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(library(ccstate), [run_nb_state/3, upd/1, set/1, get/1]).
:- use_module(library(lambda1)).

:- op(1150,fx,cctable).

system:term_expansion((:- cctable(Specs)), Clauses) :- 
   foldl_clist(expand_cctab, Specs, Clauses, []).

foldl_clist(P,(A,B)) --> !, call(P,A), foldl_clist(P,B).
foldl_clist(P,A) --> call(P,A).

expand_cctab(Name//Arity) --> !, {A2 is Arity+2}, expand_cctab(Name/A2).
expand_cctab(Name/Arity) --> 
   { functor(Head, Name, Arity), head_worker(Head, Worker)},
   [(:- discontiguous('$cctabled'/1)), '$cctabled'(Head), (Head :- call_tabled(Worker))]. 

prolog:rename_predicate(M:Head, M:Worker) :-
   '$flushed_predicate'(M:'$cctabled'(_)),
   call(M:'$cctabled'(Head)), !,
   head_worker(Head, Worker).

head_worker(Head, Worker) :-
   Head   =.. [H|As], atom_concat(H,'#',W),
   Worker =.. [W|As].

head_to_variant(Head, Variant) :-
   copy_term_nat(Head, Variant),
   numbervars(Variant, 0, _).

:- meta_predicate call_tabled(0).
call_tabled(Head) :- p_shift(tab, Head).

run_tab(Goal, Ans) :-
   p_reset(tab, Goal, Status),
   cont_tab(Status, Ans).

cont_tab(done, _).
cont_tab(susp(Head, Cont), Ans) :-
   term_variables(Head,Y), K= \Y^Ans^Cont,
   head_to_variant(Head, Variant),
   get(Tabs1),
   (  rb_update(Tabs1, Variant, tab(Solns,Status1), tab(Solns,Status2), Tabs2) 
   -> (  Status1=active(Ks)
      -> Status2=active([K|Ks]), set(Tabs2)
      ;  Status2=complete
      ),
      rb_in(YY, _, Solns), Y=YY,
      run_tab(Cont, Ans) 
   ;  rb_empty(Solns), 
      rb_insert_new(Tabs1, Variant, tab(Solns,active([])), Tabs2),
      set(Tabs2),
      run_tab(producer(Variant, \Y^Head, K, Ans), Ans)
   ).

producer(Variant, Generate, KP, Ans) :-
   call(Generate, Y1),
   upd(add_soln(Variant, Y1, active(Ks))),
   (K=KP; member(K,Ks)), 
   call(K,Y1,Ans).
producer(Variant, _, _, _) :- 
   debug(cctab, 'Table complete: ~p',[Variant]),
   upd(complete_table(Variant, _)).
% producer(Variant, _, _, Ans) :-
%    upd(complete_table(Variant, Solns)),
%    rb_in(YY, _, Solns), Y=YY,
%    call(KP,Y,Ans).

complete_table(Variant, Solns, Tabs1, Tabs2) :-
   rb_update(Tabs1, Variant, tab(Solns, _), tab(Solns, complete), Tabs2).

add_soln(Variant, Y1, Status, Tabs1, Tabs2) :-
   rb_update(Tabs1, Variant, tab(Solns1, Status), tab(Solns2, Status), Tabs2),
   rb_insert_new(Solns1, Y1, t, Solns2). 

:- meta_predicate run_tabled(0), run_tabled(0,-).
run_tabled(Goal) :- run_tabled(Goal,_).
run_tabled(Goal, FinalTables) :- 
   rb_empty(Tables),
   term_variables(Goal, Ans),
   run_nb_state(run_tab(Goal, Ans), Tables, FinalTables).


:- cctable fib/2.

fib(0,1).
fib(1,1).
fib(N,X) :-
   succ(M,N), fib(M,Y),
   succ(L,M), fib(L,Z),
   plus(Y,Z,X).

edge(a,b).
edge(a,c).
edge(b,d).
edge(c,d).
edge(d,e).
edge(d,f).
edge(f,g).
edge(N,M) :- number(N), (M is N+1; M is N-1).

:- cctable pathl//0, pathl1//0, pathr//0, pathr1//0.
pathl  --> edge; pathl, edge.
pathl1 --> pathl1, edge; edge.
pathr  --> edge; edge, pathr.
pathr1 --> edge, pathr1; edge.

path_a(Y) :- pathl(a,X), Y=a(X).
path1_a(Y) :- pathl1(a,X), Y=a(X).
