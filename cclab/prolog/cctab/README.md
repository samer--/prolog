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
cctab8 - cctab2 modified to switch into tabling mode (context delimited by 'tab' prompt)
         on calling outermost tabled predicate (the 'leader'). Tables are marked 'complete' 
         on leaving tabling mode and subsequent lookups on complete tables need no delimited 
         context. This more or less implements the functionality of Desouter et al's
         library.
cctab9 - Variation on  cctab8 - more stuff in cctabled/1, less in shift-handler.

ccptab1 - Based on cctab7 (tabled probabilistic prolog) but giving up on production time
          probability labelling and instead going by goal terms, then doing a more
          thorough post processing of the tables to remove redundant variant solutions.
          After that, inside and outside probabilities can be computed more effiently.
          Also includes efficient lazy k-best explanation tree extraction.
ccptab2 - Based on ccptab2, but graphs are rbtrees instead of lists, and factor inside
 			 probabilities are handled differently during outside probability computation.
 			 Also, this was forked from ccptab1 before sampling execution was added to the
			 latter.
ccptab3 - Like ccptab1, but k-best processing stores a pair of integers in the done set, 
          instead of big terms. Also there are three implementations of the done set as
          the original rb_tree, an ord_set, or a plain list. It is not clear which will
          be the most efficient in realistic use cases.
ccptab4 - Like ccptab3, but with generalised semiring explanation analysis.



Multiple variant tables containing duplicate solutions.
Not using subsuming variants.
Not using subsumed variants to initialise subsuming variant table

Will each duplicate solution end up with the same explanations?
Sharing solutions between variants?
Look at subsumption lattice
What about variables in answers?
