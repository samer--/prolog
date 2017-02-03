:- module(ccdetmem, [memo/3, memo/2]).

/** <module> Recursive but deterministic memoisation, functional style.
 
 This module provides memoisation as an effect. It must be used inside
 ccstate:run_ref/1, as this provides mutable references as an effect.
 The predicate to be memoised must be passed to memo/2 or memo/3, which
 will return a metacallable term.
 */

:- use_module(library(ccstate), [ref_new/2, ref_get/2, ref_app/2]).

:- meta_predicate memo(2,-,-).

%% memo(+P:pred(+A,-B), -Q:pred(+A,-B), -D:pred(list(pair(A,B)))) is det.
%% memo(+P:pred(+A,-B), -Q:pred(+A,-B)) is det.
memo(P, Q) :- memo(P,Q,_).
memo(P, ccdetmem:mem_call(P,R), ccdetmem:mem_dump(R)) :- 
   empty_assoc(T),
   ref_new(T,R).

%% mem_call(+P:pred(+A,-B), +R:ref(assoc(A,B)), +X:A, -Y:B) is det.
mem_call(P,R,X,Y) :-
   ref_get(R,Tab),
   (  get_assoc(X,Tab,Y) -> true
   ;  call(P,X,Y),
      ref_app(R,tab_add(X,Y))
   ).

%% mem_dump(+R:ref(assoc(A,B)), -Pairs:list(pair(A,B))) is det.
mem_dump(R,Pairs) :-
   ref_get(R,Tab),
   assoc_to_list(Tab,Pairs).

tab_add(K,V,T1,T2) :- put_assoc(K,T1,V,T2).
