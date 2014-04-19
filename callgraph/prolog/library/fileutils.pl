:- module(fileutils, [
		with_file/2,
		with_stream/3,
		read_lines/2,
		with_output_to_file/2,
		with_output_to_file/3,
		with_input_from_file/2,
		with_input_from_file/3,
		write_file_with/3,
		write_file_with/4,
		with_input_from/2,
		find_file/3,
		match_file/2,
		file_under/4
	]).

:- meta_predicate 
		with_output_to_file(?,0), 
		with_output_to_file(?,0,+), 
		with_input_from_file(?,0),
		with_input_from_file(?,0,+),
		write_file_with(?,?,0),
		write_file_with(?,?,0,?),
		with_input_from(+,0), 
		with_stream(0,?,0), 
		with_file(?,0). 


%% with_stream( :Opener, -Stream, :Goal) is semidet.
%
%  Base predicate for doing things with stream. Opener is a goal which must
%  prepare the stream, Stream is the variable which will hold the valid
%  stream handle, and Goal is called with the stream open. The stream is
%  guaranteed to be closed on exit. Stream will remain unbound on exit.
%  NB: the idea is that Opener and Goal share the Stream variable, eg:
%  ==
%  with_stream( open('out.txt',write,S), S, writeln(S,'Hello!')).
%  with_stream( open('in.txt',read,S), S, read(S,T)).
%  ==
with_stream(Opener,Stream,Goal) :- 
   copy_term(t(Opener,Stream,Goal),t(O,S,G)),
	setup_call_cleanup(O,G,close(S)).


%% with_file( :Opener, :Goal) is semidet.
%
%  Call Goal with an open file as specified by Opener, which can be
%    * open( +Filename, +Mode, @Stream)
%    * open( +Filename, +Mode, +Options, @Stream)
%  Opener is used to call open/3 or open/4. 
%
%  @deprecated Use with_stream/3 instead.
with_file(open(File,Mode,Stream),Goal) :-
	with_stream(open(File,Mode,Stream),Stream,Goal).

with_file(open(File,Mode,Stream,Options),Goal) :-
	with_stream(open(File,Mode,Stream,Options), Stream, Goal).

%% with_output_to_file( +File, :Goal) is semidet.
%% with_output_to_file( +File, :Goal, +Opts) is semidet.
%
%  Call Goal redirecting output to the file File, which is opened as with
%  open(File,write,Str) or open(File,write,Opts,Str).
%  However, if the option mode(Mode) is present, it is removed from the
%  list (leaving Opts1) and the file is opened as with open(File,Mode,Opts1,Str).
%  The default mode is write.
with_output_to_file(File,Goal) :- with_output_to_file(File,Goal,[]).
with_output_to_file(File,Goal,Opts) :- write_file_with(File,S,with_output_to(S,Goal),Opts).


%% with_input_from_file( +File, :Goal) is semidet.
%% with_input_from_file( +File, :Goal, +Opts) is semidet.
%
%  Call Goal redirecting output to the file File, which is opened as with
%  open(File,write,Str) or open(File,write,Opts,Str).
with_input_from_file(File,Goal) :- with_input_from_file(File,Goal,[]).
with_input_from_file(File,Goal,Opts) :- 
	with_stream( open(File,read,S,Opts), S, with_input_from(S,Goal)).

%% with_input_from( +Source, :Goal) is semidet.
%
%  Temporarily switch current input to object specified by Source while calling Goal as in once/1.
%  Source is a term like that supplied to with_output_to/2 and can be any of:
%  	* A stream handle or alias.
%  	* atom(+Atom) 
%  	* codes(+Codes)
%  	* chars(+Chars)
%  	* string(+String)

with_input_from(atom(A),G) :- !,
	setup_call_cleanup(
		atom_to_memory_file(A,MF),
		setup_call_cleanup(
			open_memory_file( MF, read, S),
			with_input_from(S,G),
			close(S)
		),
		free_memory_file(MF)
	).

with_input_from(codes(Codes),G) :- !, atom_codes(Atom,Codes), with_input_from(atom(Atom),G).
with_input_from(chars(Chars),G) :- !, atom_chars(Atom,Chars), with_input_from(atom(Atom),G).
with_input_from(string(Str),G)  :- !, string_to_atom(Str,Atom), with_input_from(atom(Atom),G).

with_input_from(S,G) :- is_stream(S), !,
	current_input(S0),
	setup_call_cleanup(set_input(S),once(G),set_input(S0)).


%% write_file_with( +File, @Stream, :Goal, +Options:list) is semidet.
%% write_file_with( +File, @Stream, :Goal) is semidet.
%
%  Call Goal after opening the named file and unifying Stream with a
%  valid stream. The file is guaranteed to be closed and Stream unbound
%  on exit. Any options are pased to open/4, except for mode(Mode),
%  which defaults to write and determines whether the file is opened
%  in write or append mode.

write_file_with(File,Stream,Goal) :- write_file_with(File,Stream,Goal,[]).
write_file_with(File,Stream,Goal,Options) :-
	select_option(mode(Mode),Options,Options1,write),
	must_be(oneof([write,append]),Mode),
	with_stream(
		open(File,Mode,Stream,Options1),
		Stream,
		Goal
	).


%% read_lines( +Stream, -Lines:list(list(integer))) is semidet.
%
%  Read all lines from Stream and return a list of lists of character codes.
read_lines(Stream,Lines) :-
	read_line_to_codes(Stream,Line),
	(	Line=end_of_file
	-> Lines=[]
	;	Lines=[Line|Lines1],
		read_lines(Stream,Lines1)).


%% match_file(+Spec,-File) is nondet.
%
%  Unify File with a filename that matches given spec. Yields
%  alternative matches on backtracking. Can give relative as
%  well as absolute paths.
match_file(Spec,File) :-
	expand_file_search_path(Spec,Path), 
	expand_file_name(Path,Files), 
	member(File,Files).


%% file_under( +Root, +Pattern, -File, -Path) is nondet.
%
%  Enumerate all files under directory root whose names match Pattern.
%  Root can be a unary term as understood by expand_file_search_path/2.
%  On exit, File is the fully qualified path to the file and path is
%  a list of directory names represented as atoms.
%  Returns absolute file paths only.

file_under(RootSpec,Pattern,File,Path) :- 
	expand_file_search_path(RootSpec,Root),
	file_under(Root,Pattern,File,Path,[]).

file_under(Root,Pattern,File) --> {file_in(Root,Pattern,File)}.
file_under(Root,Pattern,File) --> 
	{ directory_in(Root,Full,Rel) }, [Rel],
	file_under(Full,Pattern,File).

file_in(Root,Pattern,File) :-
	atomic_list_concat([Root,Pattern],'/',Spec),
	absolute_file_name(Spec,[expand(true),solutions(all)],File).

directory_in(Root,Dir,DirName) :-
	atom_concat(Root,'/*',Spec),
	absolute_file_name(Spec,[file_type(directory),expand(true),solutions(all)],Dir),
	file_base_name(Dir,DirName).


%% find_file( +FileSpec, +Extensions:list(atom), -File:atom) is nondet.
%
%  Looks for files matching FileSpec ending with one of the given extensions.
%  FileSpec is initially passed to expand_file_search_path/2 and so can be a unary term.
%  The resulting atom can include wildcards ('*', '?', '{..}'), environment 
%  variables ('$var') and an optional leading '~' which is equivalent to '$HOME'. 
%  (See expand_file_name/2). This predicate succeeds once for
%  each readable file matching FileSpec and ending with one of the extensions
%  in Extensions. NB. no dot is prepended to extensions: if you need '*.blah' then
%  put '.blah' in Extensions.
%  Returns ABSOLUTE file paths.

find_file(Spec,Exts,File) :-
	match_file(Spec,Path),
	match_extension(Path,Exts),
	absolute_file_name(Path,[access(read)],File).

match_extension(Path,Exts) :-
	downcase_atom(Path,PathLower), member(Ext,Exts), 
	atom_concat(_,Ext,PathLower).

