:- module(hostname, [hostname/1]).
/** <module> Get hostname

 This module provides just one predicate to access the machine hostname.
 It first looks in the HOSTNAME environment variable. If this is unsuccessful,
 it call the system hostname command once and keeps the result for reuse.

 TODO: what happens if the calling hostname fails?
 */

% Possibly slight overkill to use memo and fileutils...
% :- use_module(library(fileutils)).
% :- use_module(library(memo)).
% :- volatile_memo(hostname(-atom)).
% hostname(H) :- 
% 	with_stream(S, open(pipe(hostname),read,S,[]), read_line_to_codes(S,C)),
% 	atom_codes(H,C).

user:term_expansion(hostname(_),hostname(H)) :-
   (  getenv('HOSTNAME',H) -> true
   ;  setup_call_cleanup(open(pipe(hostname),read,S),
                         read_line_to_codes(S,Codes),
                         close(S)), 
      atom_codes(H,Codes)
   ),
   print_message(informational, got_hostname(H)).

prolog:message(got_hostname(H)) --> ['Registering hostname "~w"'-[H]].

hostname(_).
