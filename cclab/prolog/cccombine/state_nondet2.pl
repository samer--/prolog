:- use_module(library(data/store)).
:- use_module(delimcc).

% ------- stateful computation reified as DCG ----------

get(S) :- p_shift(state,get(S)).
set(S) :- p_shift(state,set(S)).
upd(P) :- p_shift(state,P).

%% run_state(+P:pred, +S1:S, -S2:S) is det.
run_state(Goal) -->
   {p_reset(state, Goal, Status)},
   cont_state(Status).

cont_state(done) --> [].
cont_state(susp(P,Cont)) --> call(P), run_state(Cont).

get(S,S,S).
set(S,_,S).

run_ref(Goal) :-
   store_new(S),
   run_state(Goal, S, _).

ref_new(X,R) :- upd(store_add(X,R)).
ref_get(R,X) :- upd(store_get(R,X)).
ref_set(R,X) :- upd(store_set(R,X)).
ref_upd(R,X) :- upd(store_apply(R,X)).

memo(P,mem_call(P,R),mem_dump(R)) :- 
   empty_assoc(T),
   ref_new(T,R).

mem_call(P,R,X,Y) :-
   ref_get(R,Tab),
   (  get_assoc(X,Tab,Y) -> true
   ;  call(P,X,Y),
      ref_upd(R,add_assoc(X,Y))
   ).

add_assoc(K,V,T1,T2) :- put_assoc(K,T1,V,T2).

mem_dump(R,Pairs) :-
   ref_get(R,Tab),
   assoc_to_list(Tab,Pairs).

fib_inc(_,0,1) :- !.
fib_inc(_,1,1) :- !.
fib_inc(Fib,N,X) :-
   succ(M,N), call(Fib,M,Y),
   succ(L,M), call(Fib,L,Z),
   X is Y+Z.

test_fib(Fib,Goal) :-
   memo(fib_inc(Fib), Fib, Dump),
   call(Goal),
   call(Dump,List),
   writeln(List).
% ---------- nondeterminism reified as list -----------

choose(Xs,X) :- p_shift(nondet, Xs-X).

%% run_nondet(+P:pred(-A), -X1:list(A)) is det.
%% run_nondet(+P:pred(-A), X1:list(A), X2:list(A)) is det.
run_nondet(P,Xs) :- run_nondet(P,Xs,[]).
run_nondet(P) -->
   {p_reset(nondet, call(P, X), Status)},
   cont_nondet(Status, X).

cont_nondet(done, X) --> [X].
cont_nondet(susp(Ys-Y, Cont), X) --> foldl(expand1(Y,Cont,X), Ys).

expand1(Y0, Cont0, X0, Y) --> 
   {copy_term(t(Y0,Cont0,X0), t(Y,Cont,X))},
   run_nondet(run_cont(X, Cont)).

run_cont(X,Cont,X) :- call(Cont).

% ------------------------ test programs -----------------

test_state(X) :-
   get(S),
   X = s(S),
   set(s(X)).

test_nondet(X) :-
   choose([1,2,3], Y),
   choose([a-Y, b-Y], X).

test_both(X/Y) :-
   choose([a,b,c,d],X),
   upd(succ),
   get(Y).

test_store(r(X,Y,Z,W)-Tab) :-
   memo(trace(succ),Succ,Dump),
   choose([1,2,3],X),
   call(Succ,X,Y),
   choose([1,2,3],Z),
   call(Succ,Z,W),
   call(Dump,Tab).

cons(X,Y,[X|Y]).

trace(P,X,Y) :-
   call(P,X,Y),
   writeln(done:call(P,X,Y)).
% ----------- combining both effects ---------------------

%% run_nondet_state(+P:pred(-A), -X:list(A), +S1:S, -S2:S) is det.
run_nondet_state(P, S0, Xs-S1) :-
   run_state(run_nondet(P, Xs, []), S0, S1).

run_nondet_state(P, Xs, S0, S1) :-
   run_state(run_nondet(P, Xs, []), S0, S1).

%% run_state_nondet(+P:pred(-A), +S1:S, -R:list(pair(A,S))) is det.
run_state_nondet(P, S0, XSs) :-
   run_nondet(run_state_in_nondet(P, S0), XSs, []).

run_state_in_nondet(P, S0, X-S1) :-
   run_state(call(P,X), S0, S1).
 
:- run_nondet_state(test_state, 0,  XsS), writeln(XsS). 
:- run_nondet_state(test_nondet, 0, XsS), writeln(XsS).
:- run_state_nondet(test_state, 0, XSs), writeln(XSs). 
:- run_state_nondet(test_nondet, 0, XSs),writeln(XSs).
