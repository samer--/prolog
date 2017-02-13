## Some implementations of tabling

This directory contains implementations of tabling that are modelled on the
more functional style memoisation in ccmemo.pl, but instead of providing 
nondeterminism as a choose/2 control operator, which is completely parallel
to and incompatible with Prolog's normal nondeterminism, and then reifying 
the result as a search tree or other data structure, the implementations use
Prolog nondeterminism both inside the tabled execution context and to represent
the result to the outside context.

One consequences of this is that state changes (ie updates to the memo tables)
need to be protected from backtracking, in some sort of non-backtrackable state
keeper. Rather than going straight for unfettered use of the recorded database
or `nb_setval`/`nb_getval`, I've tried to 'sanitise' the non-backtrackable state as
a control effect implemented using delimited continuation, such that the statefulness
is confined to a limited context and looks pure and declarative from the outside.
This is done using `run_nb_state/3`, which provides access to the initial and final
states, and uses `nb_setval` etc under the covers.

The various implementations here vary along several dimensions, for example, the
data structure used to store found solutions for a tabled goal, the form in which
those solutions are represented, the way and time that the producer and consumer 
continuations are invoked, and the way term copying is handled, either explicitly
or using lambdas to capture the required semantics.

cctab  - Solutions as a list of predicate heads
			producer continuation called first before consumers
cctab2 - Solutions are lists of bound variables in rbtree, 
			build generator and producer continuation as lambdas
			producer continuation called first 
cctab3 - use `pr_reset/3` and unary predicates/lambdas, 
			solutions are list of bound variables, 
			generator and continuations as lambdas
cctab5 - cctab2 + mark completion in table,
			consumers of complete table don't register continuations
			use table updater predicates. 
         [BROKEN!]
cctab6 - cctab5 + collect all before sending to producer continuation
         [BROKEN! (see cctab5)]
cctab7 - cctab2 + probabilistic explanation graph. This is basically the inference
         part of PRISM (ie the generalisation of the inside-outside, forwards backwards, 
         sum-product and message-passing algorithms for probabilistic inference)
         in about 60 lines of code.
