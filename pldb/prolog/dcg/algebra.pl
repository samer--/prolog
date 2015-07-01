:- module(algebra, [algebra//3, algebra2//3, algebra8//4, algebra9//5]).

:- setting(max_left_recursion,nonneg,3,'Maximum depth for parsing left-recursive operators').

algebra(Ops,Base,S) --> {setting(max_left_recursion,M)}, algebra(M,M,Ops,Base,S).
algebra(_,_,[],Base,S) --> call(Base,S).
algebra(M,_,[_|Ops],Base,S) --> algebra(M,M,Ops,Base,S).
algebra(M,M0,[O|Ops],Base,S) --> {O=Class-Op}, a_app(Class,Op,a(M0,[O|Ops],M,Base),S).
a_app(pre(A1),Op,State,op(SO,S1))  --> call(Op,SO), a_arg(State,A1,S1).
a_app(post(A1),Op,State,op(SO,S1)) --> a_arg(State,A1,S1), call(Op,SO).
a_app(in(A1,A2),Op,State,op(SO,S1,S2)) --> a_arg(State,A1,S1), call(Op,SO), a_arg(State,A2,S2). 
a_arg(a(M0,Ops,M,Base),A1,S1) --> {assoc(M,M0,Ops,A1,M1,Ops1)}, algebra(M,M1,Ops1,Base,S1).

algebra2(Ops,Base,S) --> {setting(max_left_recursion,M)}, algebra2(M,M,Ops,Base,S).
algebra2(_,_,[],Base,S) --> call(Base,S).
algebra2(M,M0,[O|Ops],Base,S) --> {O=Class-Op}, a2_app(Class,Op,a(M0,[O|Ops],M,Base),S).
a2_app(pre(A1),Op,State,op(SO,S1))  --> call(Op,SO), a2_arg(State,A1,S1).
a2_app(Class,Op,State,S) --> {class_assoc1(Class,A1)}, a2_arg(State,A1,S1), a2_cont(A1,Class,Op,State,S1,S).
a2_cont(x,_,_,_,S,S) --> [].
a2_cont(A1,post(A1),Op,_,S1,op(SO,S1)) --> call(Op,SO).
a2_cont(A1,in(A1,A2),Op,State,S1,op(SO,S1,S2)) --> call(Op,SO), a2_arg(State,A2,S2). 
a2_arg(a(M0,Ops,M,Base),A1,S1) --> {assoc(M,M0,Ops,A1,M1,Ops1)}, algebra2(M,M1,Ops1,Base,S1).

% this is to avoid parsing arg 1 as y if the current op class precludes it
class_assoc1(_,x).
class_assoc1(post(y),y).
class_assoc1(in(y,_),y).


assoc(M,_,[_|Ops],x,M,Ops).
assoc(_,M0,Ops,y,M1,Ops) :- succ(M1,M0).

% Untyped algebra
:- meta_predicate algebra8(+,5,3,-,?,?).
algebra8(L,DB,Base,Sem) --> 
   {setting(max_left_recursion,M)},
   a8_top(L,M,a(M,DB,Base),Sem).

a8_top(0,_,Alg,Sem) --> {Alg=a(_,_,Base)}, call(Base,Sem).
a8_top(L,M,Alg,Sem) --> {L>0}, a8_x(L-M,Alg,Sem).

% ideally, 3rd clause would not bother with A1=y if no operator will accept it.
a8_x(LM,Alg,op(SO,S1)) --> a8_op(Alg,LM,pre(A1),SO), a8_arg(LM,Alg,A1,S1).
a8_x(LM,Alg,Sem) --> a8_op(Alg,LM,custom(a8_arg(LM,Alg)),Sem).
a8_x(LM,Alg,Sem) --> a8_arg(LM,Alg,A1,S1), a8_cont(A1,LM,Alg,S1,Sem). 

a8_cont(x,_,_,S1,S1) --> [].
a8_cont(A1,LM,Alg,S1,op(SO,S1))  --> a8_op(Alg,LM,post(A1),SO).
a8_cont(A1,LM,Alg,S1,op(SO,S1,S2)) --> a8_op(Alg,LM,in(A1,A2),SO), a8_arg(LM,Alg,A2,S2).

a8_arg(LM,Alg,A,S) --> {Alg=a(M,_,_), assoc(M,LM,A,L1-M1)}, a8_top(L1,M1,Alg,S).
a8_op(a(_,DB,_),L-_,Class,SO) --> call(DB,L,Class,SO).

% Typed algebra
:- meta_predicate algebra9(+,6,4,-,-,?,?).
algebra9(L,DB,Base,Type,Sem) --> 
   {setting(max_left_recursion,M)},
   a9_top(L,M,a(M,DB,Base),Type,Sem).

a9_top(0,_,Alg,Type,Sem) --> {Alg=a(_,_,Base)}, call(Base,Type,Sem).
a9_top(L,M,Alg,Type,Sem) --> {L>0}, a9_x(L-M,Alg,Type,Sem).

% ideally, 3rd clause would not bother with A1=y if no operator will accept it.
a9_x(LM,Alg,Type,op(SO,S1)) --> a9_op(Alg,LM,pre(A1:T1),Type,SO), a9_arg(LM,Alg,A1,T1,S1).
a9_x(LM,Alg,Type,Sem) --> a9_op(Alg,LM,custom(a9_arg(LM,Alg)),Type,Sem).
a9_x(LM,Alg,Type,Sem) --> a9_arg(LM,Alg,A1,T1,S1), a9_cont(A1,LM,Alg,T1,S1,Type,Sem).

a9_cont(x,_,_,T1,S1,T1,S1) --> [].
a9_cont(A1,LM,Alg,T1,S1,TO,op(SO,S1))  --> a9_op(Alg,LM,post(A1:T1),TO,SO).
a9_cont(A1,LM,Alg,T1,S1,TO,op(SO,S1,S2)) --> a9_op(Alg,LM,in(A1:T1,A2:T2),TO,SO), a9_arg(LM,Alg,A2,T2,S2).

a9_arg(LM,Alg,A,T,S) --> {Alg=a(M,_,_), assoc(M,LM,A,L1-M1)}, a9_top(L1,M1,Alg,T,S).
a9_op(a(_,DB,_),L-_,Class,TO,SO) --> call(DB,L,Class,TO,SO).

assoc(M,L-_,x,L1-M) :- succ(L1,L).
assoc(_,L-M,y,L-M1) :- succ(M1,M).

