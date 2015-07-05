:- module(algebra, [algebra//3, typed_algebra//5, typed_algebra2//5]).
:- use_module(library(settings)).

:- meta_predicate
      algebra(:,3,-,?,?),
      typed_algebra(+,4,4,-,-,?,?),
      typed_algebra2(+,4,4,-,-,?,?).

:- setting(max_left_recursion,nonneg,3,'Maximum depth for parsing left-recursive operators').

% simple version
% algebra(Mod:Ops,Base,S) --> {setting(max_left_recursion,M)}, algebra(Mod,M,M,Ops,Base,S).
% algebra(_,_,_,[],Base,S) --> call(Base,S).
% algebra(Mod,M,_,[_|Ops],Base,S) --> algebra(Mod,M,M,Ops,Base,S).
% algebra(Mod,M,M0,[O|Ops],Base,S) --> {O=Class-Op}, a_app(Class,Mod:Op,a(Mod,M0,[O|Ops],M,Base),S).
% a_app(pre(A1),Op,State,op(SO,S1))  --> call(Op,SO), a_arg(State,A1,S1).
% a_app(in(A1,A2),Op,State,op(SO,S1,S2)) --> a_arg(State,A1,S1), call(Op,SO), a_arg(State,A2,S2). 
% a_app(post(A1),Op,State,op(SO,S1)) --> a_arg(State,A1,S1), call(Op,SO).
% a_arg(a(Mod,M0,Ops,M,Base),A1,S1) --> {assoc(M,M0,Ops,A1,M1,Ops1)}, algebra(Mod,M,M1,Ops1,Base,S1).

% optimised for parsing, to avoid parsing the argument of an expression more than once
algebra(Mod:Ops,Base,S) --> {setting(max_left_recursion,M)}, a2(Mod,M,M,Ops,Base,S).

a2(_,_,_,[],Base,S) --> call(Base,S).
a2(Mod,M,M0,[O|Ops],Base,S) --> {O=Class-Op}, a2_app(Class,Mod:Op,a(Mod,M0,[O|Ops],M,Base),S).

a2_app(pre(A1),Op,State,op(SO,S1))  --> call(Op,SO), a2_arg(State,A1,S1).
a2_app(Class,Op,State,S) --> 
   {(nonvar(S) -> SS=S; true), a2_pre_check(SS,S1,A1,Class)}, 
   a2_arg(State,A1,S1), 
   a2_cont(A1,Class,Op,State,S1,S).

a2_cont(x,_,_,_,S,S) --> [].
a2_cont(A1,post(A1),Op,_,S1,op(SO,S1)) --> call(Op,SO).
a2_cont(A1,in(A1,A2),Op,State,S1,op(SO,S1,S2)) --> call(Op,SO), a2_arg(State,A2,S2). 
a2_arg(a(Mod,M0,Ops,M,Base),A1,S1) --> {assoc(M,M0,Ops,A1,M1,Ops1)}, a2(Mod,M,M1,Ops1,Base,S1).

% This restricts combinations first argument associativity and operators
% Helps to avoid parsing arg 1 as y if the current op class precludes it.
% Also unifies semantic representations of expression and first argument.
a2_pre_check(S,S,x,_).
a2_pre_check(op(_,S1),S1,A1,post(A1)).
a2_pre_check(op(_,S1,_),S1,A1,in(A1,_)).

assoc(M,_,[_|Ops],x,M,Ops).
assoc(_,M0,Ops,y,M1,Ops) :- succ(M1,M0).

% Typed algebra 
% using a different operator database format to enable semantic matching
% before parsing.
typed_algebra(L,DB,Base,Type,Sem) --> 
   {setting(max_left_recursion,M)},
   a8_top(L,M,M,DB,Base,Sem:Type).

a8_top(0,_,_,_,Base,Sem:Type) --> call(Base,Type,Sem).
a8_top(L,M0,M,DB,Base,X) --> {L>0}, a8_x(s(L,M0,M,DB,Base),X).

a8_x(St,X) --> a8_op(St,pre(A1),X), a8_arg(St,A1).
a8_x(St,X) --> a8_op(St,custom(algebra:a8_arg(St)),X).
a8_x(St,X) --> 
   {  X=S:_, nonvar(S) -> a8_sem(St,A1,X)
   ;  (A1=(x<_); once(a8_accept_y(St)), A1=(y<_))
   },
   a8_arg(St,A1), a8_cont(St,A1,X).

a8_cont(_,  x<X, X) --> [].
a8_cont(St, A1, X)  --> a8_op(St,post(A1),X).
a8_cont(St, A1, X)  --> a8_op(St,in(A1,A2),X), a8_arg(St,A2).

a8_sem(s(L,_,_,DB,_), A1, X) :- call(DB,L,Class,X,_), a8_arg1(Class,A1).
a8_sem(s(_,_,_,_,_), x<X, X).

a8_arg1(in(A1,_),A1).
a8_arg1(pre(A1),A1).
a8_arg1(post(A1),A1).

a8_arg(s(L,M0,M,DB,Base),A<X) --> {assoc9(M,L,M0,A,L1,M1)}, a8_top(L1,M1,M,DB,Base,X).
a8_op(s(L,_,_,Mod:DB,_),Class,X) --> {call(Mod:DB,L,Class,X,P)}, call_dcg(Mod:P).
a8_accept_y(s(L,_,_,DB,_)) :- (Class=in(y<_,_);Class=post(y<_)), call(DB,L,Class,_,_).

% Typed algebra, like typed_algebra//5, but slightly simpler, not as good for parsing.
typed_algebra2(L,DB,Base,Type,Sem) --> 
   {setting(max_left_recursion,M)},
   a7_top(L,M,M,DB,Base,Sem:Type).

a7_top(0,_,_,_,Base,Sem:Type) --> call(Base,Type,Sem).
a7_top(L,M0,M,DB,Base,X) --> 
   {L>0, (nonvar(X) -> member(C,[pre,op,down]); member(C,[pre,down,op]))}, 
   a7_x(C,s(L,M0,M,DB,Base),X).

a7_x(down,S,X) --> a7_arg(S,x<X).
a7_x(pre,S,X) --> {a7_op(S,pre(A1),X,P)}, call_dcg(P), a7_arg(S,A1).
a7_x(op,S,X) --> {a7_op(S,in(A1,A2),X,P)}, a7_arg(S,A1), call_dcg(P), a7_arg(S,A2).
a7_x(op,S,X) --> {a7_op(S,post(A1),X,P)}, a7_arg(S,A1), call_dcg(P).
a7_x(op,S,X) --> {a7_op(S,custom(algebra:a7_arg(S)),X,P)}, call_dcg(P).

a7_arg(s(L,M0,M,DB,Base),A<X) --> {assoc9(M,L,M0,A,L1,M1)}, a7_top(L1,M1,M,DB,Base,X).
a7_op(s(L,_,_,Mod:DB,_),Class,X,Mod:Parse) :- call(Mod:DB,L,Class,X,Parse).

assoc9(M,L,_,x,L1,M) :- succ(L1,L).
assoc9(_,L,M,y,L,M1) :- succ(M1,M).
