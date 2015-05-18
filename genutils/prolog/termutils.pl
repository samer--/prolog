/* 
	Copyright 2014-215 Samer Abdallah (UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

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
msg(F,A) :- format(user_output,F,A), nl.
ask(F,A,Ch) :- 
   format(user_output,F,A), flush_output(user_output), 
   get_single_char(C), put_char(user_output,C), 
   char_code(Ch,C), nl.
heading(F,A) :- 
   with_output_to(user_output, (ansi_format([bold],F,A), nl,nl)).
status(F,A) :- 
	format(string(Msg),F,A), 
	flag(line_len,MaxLen,MaxLen),
	string_length(Msg,Len),
	(Len>MaxLen -> sub_string(Msg,0,MaxLen,_,Msg1); Msg=Msg1),
	write(user_output,Msg1), put_cap(ce), put_cap(cr).
