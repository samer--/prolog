:- module(termutils, 
   [  with_status_line/1
   ,  put_cap/2
   ,  put_cap/1
   ,  status/2
   ,  msg/2
   ,  msg/1
   ,  heading/2
   ,  ask/3
   ]).
      
:- meta_predicate with_status_line(0).

put_cap(Cap) :- tty_get_capability(Cap,string,Atom), tty_put(Atom,1).
put_cap(Cap,Lines) :- tty_get_capability(Cap,string,Atom), tty_put(Atom,Lines).

user:goal_expansion(put_cap(Cap), tty_put(Atom,1)) :-
	tty_get_capability(Cap,string,Atom).
user:goal_expansion(put_cap(Cap,Lines), tty_put(Atom,Lines)) :-
	tty_get_capability(Cap,string,Atom).

with_status_line(Goal) :-
	stream_property(user_output,buffer(Buff)),
	tty_size(_,Width), W is Width-1,
	flag(line_len,_,W),
	setup_call_cleanup(
		set_stream(user_output,buffer(false)), (put_cap(cr), call(Goal), status("",[])),
		set_stream(user_output,buffer(Buff))).

msg(F) :- msg(F,[]).
msg(F,A) :- format(F,A), nl.
ask(F,A,Ch) :- format(F,A), flush_output, get_single_char(C), put_char(C), char_code(Ch,C), nl.
heading(F,A) :- ansi_format([bold],F,A), nl,nl.
status(F,A) :- 
	format(string(Msg),F,A), 
	flag(line_len,MaxLen,MaxLen),
	string_length(Msg,Len),
	(Len>MaxLen -> sub_string(Msg,0,MaxLen,_,Msg1); Msg=Msg1),
	write(Msg1), put_cap(ce), put_cap(cr).
