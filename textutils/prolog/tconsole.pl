:- module(tconsole, 
	[	open_output_console/1
	,	close_output_console/1
	,	ensure_output_console/0
	,	output_console/1
	,	con/1
	,	con_exec/2
	,	con_push/2
	,	con_pop/1
	]).

:- use_module(library(threadutil)).

:- dynamic output_console/1.

:- meta_predicate con_exec(+,0).
:- meta_predicate con_push(+,0).
:- meta_predicate con(0).
	
ensure_output_console :- output_console(_) -> true; open_output_console(_).

open_output_console(Q) :-
	thread_create(output_console_main,Q,[detached(false)]).

close_output_console(Q) :-
	output_console(Q), !,
	thread_send_message(Q,quit),
	thread_join(Q,_).

con(G) :- output_console(Q), !, con_exec(Q,G).
con_exec(Q,G) :- context_module(M), thread_send_message(Q,exec(M:G)).
con_push(Q,G) :- context_module(M), thread_send_message(Q,push(M:G)).
con_pop(Q) :- thread_send_message(Q,pop).

output_console_main :-
	attach_console,
	thread_self(Id),
	setup_call_cleanup(
      asserta(output_console(Id)),
		output_console_loop(true),
		retract(output_console(Id))).

output_console_loop(Pref) :-
	thread_get_message(Msg),
	(	Msg=quit -> true
	;	catch(
			(output_console_handle(Msg,Pref,Pref1) ->true;Pref1=Pref),
			Ex, (print_message(error,Ex), Pref1=Pref)
		),
		output_console_loop(Pref1)
	).

output_console_handle(exec(G),P,P) :- P, G.
output_console_handle(push(Q),P,(P,Q)).
output_console_handle(pop,(P,_),P).

remove_xterm_args :-
   retractall(thread_util:xterm_args(['-fg', '#000000'])),
   retractall(thread_util:xterm_args(['-bg', '#ffffdd'])),
   retractall(thread_util:xterm_args(['-sb', '-sl', 1000, '-rightbar'])).

:- initialization remove_xterm_args.

		
