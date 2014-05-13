:- use_module(library(callgraph)).

/* 
   Use these to test callgraph.
*/


%% test_pdf is det.
%  Writes callgraph.pdf in the current directry.
test_pdf :-
	module_dotpdf(callgraph,[method(unflatten([fl(4),c(4)]))]).

%% test_dot is det.
%  Writes callgraph.dot in the current directry.
test_dot :-
	module_dot(callgraph,[]).

test_multi :-
	modules_dotpdf([callgraph,dot],[method(unflatten([fl(4),c(4)]))],callgraph_dot).

%% test_xdot is det.
%  Writes callgraph.dot in the current directry and then
%  calls xdot to display it. Will bork if xdot not present.
%  Pack gvterm includes xdot, but you should probably get the
%  latest version from http://code.google.com/p/jrfonseca/wiki/XDot
test_xdot :-
	test_dot,
   shell('xdot callgraph.dot').
