/* Part of fileutils
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)

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

:- module(fileutils, [
		read_lines/2,           % +Stream, +ListOfLists
		with_stream/3,          % @Stream, +Opener, +Goal DEPRECATED
		with_stream/2,          % +Opener, +Pred
		with_output_to_file/2,  % +File, +Goal
		with_output_to_file/3,  % +File, +Goal, +Opts
		with_input_from_file/2, % +File, +Goal
		with_input_from_file/3, % +File, +Goal, +Opts
		with_input_from/2,      % +Source, +Goal

      directory_entry/2,      % +Dir, -File
      expand_pattern/2,       % +Pattern, -File
      find_files/2,           % +FindSpec, -File
      file_under/4,           % +Root, +Pattern, -File, -RelPath
      file_under_dl/5,        % +Root, +Options, -File, ?DirsHead, ?DirsTail

      file_extension/2,
      extension_in/2,

      with_temp_dir/2,        % @Dir, +Goal
      in_temp_dir/1,          % +Goal
      file_modes/4            % +File, -UserClass, -Action, -Legal
	]).

/** <module> File reading, writing, and finding utilities

   This module provides a number of meta-predicates for directing the
   input or output streams of arbitrary goals from or to files or
   streams. It also provides some predicates for finding files in
   the file system, and for matching files on the basis of extension.

   ---++++ Types

   What follows is a half-baked scheme for assigning types to things like
   search paths and file names.

   First, a =|path|= is an atom that can be interpreted as a legal
   path in the file system, either relative or absolute.
   Next =|path(file)|= is a path that leads to a file
   and =|path(dir)|= is a path that leads to a directory.

   A =|pattern|= is an atom that can be understood by expand_file_name/2 and
   expanded to a legal path. Thus, it can include wildcards "*?[]{}", environment
   variables "$var" and "~", which is equivalent to "$HOME".
   Thus, the type of expand_file_name/2 is =|pred(+pattern, -path)|=.

   A =|spec(T)|= is a term that can be expanded by expand_file_search_path/2
   to produce an atom of type T. So, =|spec(pattern)|= expands to a pattern,
   and =|spec(path(file))|= expands to a file name. The type of
   expand_file_search_path/2 is =|pred(+spec(T), -T)|=.

   Then, for finding files, we have:
   ==
   findspec ---> in(spec(pattern), pattern)
               ; in(spec(pattern))
               ; under(spec(pattern), pattern)
               ; under(spec(pattern))
               ; like(spec(pattern)).
   ==



   @tbd

   - Consider extending with_output_to/2 and with_input_from/2 to understand
     file(Filename) and file(Filename,Opts) as sources/sinks.
   - Consider removing file_under/4 and providing another mechanism to
     obtain the path of a file as a list of directory names.
   - Consider other ways to represent recursive directory search, eg
     double-star notation: the pattern '/a/b/ ** /c/d/f' (without spaces).
     Note this allows matching on arbitrary segments of the directory
     path.
*/

:- meta_predicate
		with_output_to_file(+,0),
		with_output_to_file(+,0,+),
		with_input_from_file(+,0),
		with_input_from_file(+,0,+),
		with_input_from(+,0),
		with_stream(-,0,0),
		with_stream(1,1),
      with_temp_dir(-,0),
      in_temp_dir(0).

:- use_module(library(filesex)).

:- meta_predicate flip(2, ?, ?).
flip(P, X, Y) :- call(P, Y, X).

%% with_stream( +Opener:pred(-stream), +User:pred(+stream)) is semidet.
%
%  Base predicate for doing things with stream. Opener is a unary predicate
%  (ie callable with call/2)
%  which must prepare the stream. User is a unary predicate, called with
%  the open stream. The stream is  guaranteed to be closed on exit.  Eg,
%  using the lambda library to form anonymous predicates:
%  ==
%  with_stream(open('out.txt',write), \S^writeln(S,'Hello!')).
%  with_stream(open('in.txt',read), \S^read(S,T)).
%  ==
with_stream(Opener, User) :-
   setup_call_cleanup(call(Opener, S), call(User, S), close(S)).

%% with_stream( @Stream, :Opener, :Goal) is semidet.
%
%  DEPRECATED due to nasty term copying.
%  Base predicate for doing things with stream. Opener is a goal which must
%  prepare the stream, Stream is the variable which will hold the valid
%  stream handle, and Goal is called with the stream open. The stream is
%  guaranteed to be closed on exit. Stream will remain unbound on exit,
%  but any other variables in Opener or Goal will be left in the state
%  that Opener and Goal leave them in.
%  The idea is that Opener and Goal share the Stream variable, eg:
%  ==
%  with_stream( S, open('out.txt',write,S), writeln(S,'Hello!')).
%  with_stream( S, open('in.txt',read,S), read(S,T)).
%  ==
with_stream(Stream,Opener,Goal) :-
   replace_var(Stream,S,Opener-Goal,O-G),
   copy_term(t(FreeVars,Stream,Opener,Goal),t(FreeVars,S,O,G)),
	setup_call_cleanup(O,G,close(S)).

%% replace_var(Var1,Var2,Term1,Term2) is det.
%  Replace all instances of Var1 in Term1 with Var2, producing Term2.
%  Other variables are not copied, but shared between Term1 and Term2.
replace_var(V1,V2,T1,T2) :-
   term_variables(T1,Vars),
   remove_var(V1,Vars,Shared), !,
   copy_term(t(Shared,V1,T1),t(Shared,V2,T2)).

remove_var(V,[X|Xs],Xs) :- V==X.
remove_var(V,[X|Xs],[X|Ys]) :- remove_var(V,Xs,Ys).

% Alternative implementation built on explicit term walk.
% replace_var(V1,V2,T1,T2) :-
%    (  var(T1) -> (T1==V1 -> T2=V2; T2=T1)
%    ;  T1=..[F|A1], maplist(replace_var(V1,V2),A1,A2),
%       T2=..[F|A2] ).



%% with_output_to_file( +File, :Goal) is semidet.
%% with_output_to_file( +File, :Goal, +Opts) is semidet.
%
%  Call Goal redirecting output to the file File, which is opened as with
%  open(File,write,Str) or open(File,write,Opts,Str).
%  However, if the option mode(Mode) is present, it is removed from the
%  list (leaving Opts1) and the file is opened as with open(File,Mode,Opts1,Str).
%  The default mode is write.

:- predicate_options(with_output_to_file/3,3,
      [  mode(oneof([write,append]))
      ,  pass_to(system:open/4,4) % !!! explicit module to work-around bug in predicate options system (2016-06)
      ]).

with_output_to_file(File,Goal) :- with_output_to_file(File,Goal,[]).
with_output_to_file(File,Goal,Opts) :-
   maplist(check_predicate_option(with_output_to_file/3,3),Opts),
	select_option(mode(Mode),Opts,Opts1,write),
   with_stream(open(File,Mode,Opts1), flip(with_output_to, Goal)).

open_opts(Name, Mode, Opts, S) :- open(Name, Mode, S, Opts).


%% with_input_from_file( +File, :Goal) is semidet.
%% with_input_from_file( +File, :Goal, +Opts) is semidet.
%
%  Call Goal redirecting output to the file File, which is opened as with
%  open(File,write,Str) or open(File,write,Opts,Str).

:- predicate_options(with_input_from_file/3,3,[pass_to(open/4,4)]).

with_input_from_file(File,Goal) :- with_input_from_file(File,Goal,[]).
with_input_from_file(File,Goal,Opts) :-
	with_stream(open_opts(File,read,Opts), flip(with_input_from, Goal)).

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


%% read_lines( +Stream, -Lines:list(list(integer))) is semidet.
%
%  Read all lines from Stream and return a list of lists of character codes.
read_lines(Stream,Lines) :-
	read_line_to_codes(Stream,Line),
	(	Line=end_of_file
	-> Lines=[]
	;	Lines=[Line|Lines1],
		read_lines(Stream,Lines1)).


%% file_under( +Root:spec(pattern), +Pattern:pattern, -File:path(file), -RelPath:list(atom)) is nondet.
%
%  Enumerate all files under directory root whose names match Pattern.
%  Root can be a unary term as understood by expand_file_search_path/2.
%  On exit, File is the fully qualified path to the file and RelPath is
%  a list of directory names represented as atoms, relative to Root.
%
%  @deprecated Consider using find_files/3 and doing without RelPath.

file_under(DirSpec,Pattern,File,RelPath) :-
   expand_directory_absolute(DirSpec,Root),
	file_under(Root,Pattern,File,RelPath,[]).


%% file_under_dl(+Root:spec(dir),+Options:list, -File:atom, ?DirHead:list(atom), ?DirTail:list(atom)) is nondet.
%
%  Finds files under directory Dir, succeeding multiple times with AbsPath
%  bound to the absolute path (as an atom), and Parts bound to a list
%  of directory components ending with the file name. Options can include:
%     * abs(-AbsPath:path(file))
%     AbsPath is unified with the absolute path of each file found.
%     * filter(+Filter:pred(+atom,+path(file)))
%     Filter is called with the name of and absolute path of each directory
%     found. If it fails, that directory is not recursed into.
%
%  NB. this interface of this predicate is unstable and may change in future.

:- meta_predicate file_under_dl(+,:,-,?,?).
file_under_dl(Spec,Opts,File,Parts,PartsT) :-
   call_dcg( (
      meta_options(meta_opt),
      option_default_select(abs(AbsPath), _),
      option_default_select(filter(DFilt),true)
   ), Opts, _),
   absolute_file_name(Spec,Dir),
   file_under(Dir,DFilt,File,AbsPath,Parts,PartsT).

option_default_select(O,D,O1,O2):-select_option(O,O1,O2,D).
meta_opt(file_filter).
meta_opt(dir_filter).
true(_,_).

%% find_files( +FindSpec:findspec, -File:path(file)) is nondet.
%
%  General file finding predicate. FindSpec is one of:
%     * in(DirSpec:spec(pattern),Pattern:pattern)
%        Looks for names matching Pattern in all directories matching DirSpec.
%     * in(DirSpec:spec(pattern))
%        Equivalent to in(DirSpec,'*').
%     * under(DirSpec:spec(pattern),Pattern:pattern)
%        Looks for names matching Pattern recursively under all directories matching DirSpec,
%     * under(DirSpec:spec(pattern))
%        Equivalent to under(DirSpec,'*').
%     * like(FileSpec:spec(pattern))
%
%  DirSpec is an atom or file search path term as understood by expand_file_search_path/2.
%  It may contain wildcards as understood by expand_file_name/2, and is used to find
%  directories.
%  Pattern is a pattern for file names, without any directory components.
%  FileSpec is a files search path term that is used to find files.
%  File is unified with the absolute path of matching, readable files.

find_files(in(DirSpec),File) :- find_files(in(DirSpec,'*'),File).
find_files(under(DirSpec),File) :- find_files(under(DirSpec,'*'),File).
find_files(in(DirSpec,Pattern),File) :-
   expand_directory_absolute(DirSpec,Root),
   file_in(Root,Pattern,File).
find_files(under(DirSpec,Pattern),File) :-
   expand_directory_absolute(DirSpec,Root),
	file_under(Root,Pattern,File,_,[]).
find_files(like(Spec),AbsFile) :-
	expand_file_search_path(Spec,Pattern),
   expand_file(Pattern,File),
   absolute_file_name(File,AbsFile).


%% file_under(+Root:path(dir), +Pattern:pattern, -File:path(file))// is nondet.
%  DCG rule common to file_under/4 and find_files/2.
%  Finds file names matching Pattern in or under Root and matches
%  final argument pair with difference list containing the directory names
%  along the path from the the Root to the file.
%  File is an absolute path to the file in question.
:- public file_under/5.
file_under(Root,Pattern,File,P,P) :-
   file_in(Root,Pattern,File).
file_under(Root,Pattern,File,[DirName|P1],P2)  :-
   % directory_file_path(Root,'*',DirPatt),
   % expand_directory(DirPatt,Dir),
   % file_base_name(Dir,DirName),
	% file_under(Dir,Pattern,File,P1,P2).
   directory_entry(Root,DirName),
   directory_file_path(Root,DirName,Dir),
   exists_directory(Dir),
   file_under(Dir,Pattern,File,P1,P2).


%% file_under(+Root:path(dir), +Filter, -Name:atom, -Path:path(file))// is nondet.
%  Alternative implementation of file_under, with arbitrary filter
%  predicate on directories. Name is the is the name
%  component of the path to the file. Path is the path, of which Root is
%  always a prefix.
:- meta_predicate file_under(+,2,-,-,?,?).
file_under(Root,Filter,Name,Path,P1,P2) :-
   directory_entry(Root,Item),
   directory_file_path(Root,Item,ItemPath),
   file_under_x(Filter,Item,ItemPath,Name,Path,P1,P2).

file_under_x(_,Item,ItemPath,Item,ItemPath,P1,P1) :-
   exists_file(ItemPath).
file_under_x(Filter,Item,ItemPath,Name,Path,[Item|P1],P2) :-
   exists_directory(ItemPath),
   call(Filter,Item,ItemPath),
   file_under(ItemPath,Filter,Name,Path,P1,P2).


%% file_in(+Dir:path(dir), +Pattern:pattern, -File:path(file)) is nondet.
%  used by find_files/2 and file_under//3
%  Directory must be an atom containing an expanded path (no wildcards)
%  but can be relative or absolute.
file_in(Directory,Pattern,File) :-
   directory_file_path(Directory,Pattern,FullPattern),
   expand_file(FullPattern,File).

%% expand_absolute_directory( +Spec:spec(pattern), -Dir:path(dir)) is nondet.
% expands directory spec to absolute accessible directory paths.
expand_directory_absolute(Spec,Dir) :-
   expand_file_search_path(Spec,DirPatt),
   expand_directory(DirPatt,RelDir),
   absolute_file_name(RelDir,Dir).

expand_directory(Pattern,Dir) :- expand_pattern(Pattern,Dir), exists_directory(Dir).
expand_file(Pattern,File)     :- expand_pattern(Pattern,File), exists_file(File).


%% expand_pattern(+Pattern:pattern, -File:path) is nondet.
%
%  Expands Pattern and unifies File with names of matching, readable files, exactly
%  as expand_file_name/1, except that matches are produced one by one
%  on backtracking, instead of all together in a list. File which the current user
%  does not have permission to read are _not_ returned.
expand_pattern(Pattern,File) :-
   expand_file_name(Pattern,Files),
   member(File,Files),
   access_file(File,read).


%% directory_entry(+Dir:path(dir), -Entry:atom) is nondet.
%  Is true when Entry is a file or directory in the directory
%  Dir, not including the special entries '.' and '..'.
directory_entry(Dir,Entry) :-
   directory_files(Dir,Entries),
   member(Entry,Entries),
   Entry\='.', Entry\='..'.

%% file_extension(+File:path, -Ext:atom) is nondet.
%% file_extension(+File:path, +Ext:atom) is semidet.
%
%  True if Filename has an extensions of Ext, where Ext does not include
%  the dot. An extension is defined as any sequence of characters (including dots)
%  after a dot that is not the first character of the name part of a path.
%  Succeeds multiple times if File has multiple extensions,
%  eg, these are all true:
%  ==
%  file_extension('doc.ps.gz','ps.gz').
%  file_extension('doc.ps.gz','gz').
%  ==
%  The predicate is case sensitive and case presevering.
file_extension(Path,Ext) :-
   sub_atom(Path,BDot,1,_,'.'),    % find any dot
   succ(BBDot,BDot),              % must be something before dot
   \+sub_atom(Path,BBDot,1,_,'/'), % must not be /
   succ(BDot,Dot),                % look after dot
   sub_atom(Path,Dot,_,0,Ext),     % Ext=extension exluding dot
   \+sub_atom(Ext,_,_,_,'/').      % Ext cannot contain /


%% extension_in(+File:path, +Extensions:list(atom)) is semidet.
%
%  True if File has one of the extensions in the list Extensions.
%  Extensions are case insensitive. An extension is any sequence of
%  characters following a dot in the name part of a file name.
extension_in(Path,Exts) :-
   file_extension(Path,Ext),
   memberchk(Ext,Exts).


%% in_temp_dir(+Goal:callable) is semidet.
%
%  Calls Goal with the current
%  directory set to a newly created directory (using
%  with_temp_dir/2) which is deleted after the call is
%  finished. Goal is called as =|once(Goal)|= to ensure
%  that the working directory is restored to its original
%  value for any subsequent goals.
in_temp_dir(Goal) :-
   with_temp_dir(Dir,
      setup_call_cleanup(
         working_directory(Old,Dir), once(Goal),
         working_directory(_,Old))).


%% with_temp_dir(@Dir:path, +Goal:callable) is nondet.
%
%  Calls Goal with Dir bound to a new temporary directory.
%  Once Goal is finished, the directory and its contents are deleted.
with_temp_dir(Dir,Goal) :-
   tmp_file(swi,Dir),
   debug(fileutils(temp),"Will make dir ~w...",Dir),
   setup_call_cleanup(
      make_directory(Dir), Goal,
      delete_directory_and_contents(Dir)).

:- if(current_prolog_flag(unix,true)).

:- if(current_prolog_flag(apple,true)).
stat_args(File,['-f','%Sp',File]).
:- else.
stat_args(File,['-f','%A',File]).
:- endif.

%% file_modes(+File:path, +UserClass:oneof([owner,group,other]), +Action:oneof([read,write,execute]), -Legal:boolean) is det.
%% file_modes(+File:path, -UserClass:oneof([owner,group,other]), -Action:oneof([read,write,execute]), -Legal:boolean) is multi.
file_modes(File,UserClass,Action,Legal) :-
   stat_args(File,StatArgs),
   setup_call_cleanup(
      process_create('/usr/bin/stat',StatArgs,[stdout(pipe(Out))]),
      read_line_to_string(Out,Modes),
      close(Out)),
   user_action_index(UserClass,Action,I,TrueVal),
   sub_string(Modes,I,1,_,Val),
   (  Val=TrueVal -> Legal=true
   ;  Val="-"     -> Legal=false
   ;  throw(unexpected_mode_char(File,I,Val))
   ).

user_action_index(owner,read,1,"r").
user_action_index(owner,write,2,"w").
user_action_index(owner,execute,3,"x").
user_action_index(group,read,4,"r").
user_action_index(group,write,5,"w").
user_action_index(group,execute,6,"x").
user_action_index(other,read,7,"r").
user_action_index(other,write,8,"w").
user_action_index(other,execute,9,"x").

:- else.

file_modes(_,_,_,_) :- throw(not_implemented(file_modes/4)).

:- endif.
