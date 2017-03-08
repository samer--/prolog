This directory contains implementations which use beam search to explore
the results of memoisied probabilistic effects. This is in contrast
with cclazy which produces a lazy search tree, and cctab, which runs an
exhaustive search.

One approach to a beam or agenda based search would be to take the lazy
search tree and go through expanding nodes, adding them to the agenda a
'threads', and choosing the next thread to explore based on some priority
based policy.

Another is to skip the lazy tree and add new threads to the agenda as
soon as they are created - that's the plan here.

	ccbeam1 - priority queue with no memoisation
	ccbeam2 - with memoisation of binary predicates, no limit on number of threads
	ccbeam3 - memoisation of any arity goal
	ccbeam4 - using Prolog nondeterminism instead of dist/fail_
