:- module(trender, 
	[	render/2
	,	renderer/2
	,	words_codes/2
	,	sentence_codes/2
	,	voice/1, voice/2
	,	fmt/1, tick/3, osd/1, osd/2, say/2
	,	op(650,xfy,&&)
	,	render_line/2
   ,  set_speech_state/1
   ,  words_line/2
	]).

/** <module> framework for outputing text in interesting ways
 */
:- op(650,xfy,&&).
:- use_module(library(apply_macros)).
:- use_module(library(dcg_core),  [ nop//0 ]).
:- use_module(library(dcg_codes), [ escape//2, wr//1, sp//0 ]).
:- use_module(library(fileutils), [ with_output_to_file/3 ]).
:- use_module(library(listutils), [ rep/3 ]).
:- use_module(library(speech),    [ voice/1, voice/2 ]).
:- use_module(library(tconsole)).

:- module_transparent render/2, render_line/2.

:- multifile renderer/2.
	
:- initialization(assert(speech_state(on))).

%% set_speech_state(+State:oneof([on,off])) is det.
set_speech_state(State) :-
   must_be(State,one_of([on,off])),
   retractall(speech_state(_)),
   assert(speech_state(State)).

% ----------------- General renderer ----------------------

%% render( +Renderer:renderer, +Lines:list(line)) is semidet.
%
%  Render the given lines using the given renderer. A renderer
%  is defined as follows:
%  ==
%  renderer ---> renderer_macro         % defined using renderer/2
%              ; renderer >> renderer   % sequential composition
%              ; renderer && renderer   % concurrent composition
%              ; async(renderer)        % returns immediately
%              ; less(renderer)         % pipe output through less
%              ; con(renderer)          % send output to console
%              ; map(lrenderer)         % apply line renderer to each line
%              .
%
%  pred(line) :< lrenderer. % predicate to apply to line is an lrenderer
%  lrenderer ---> 
%               ; lrenderer >> lrenderer % sequential composition
%               ; lrenderer && lrenderer % concurrent composition
%               ; async(lrenderer)       % asynchronous, returns immediately
%               ; con(lrenderer)         % output to console
%               .
%
%  line ---> line(list(word))		% first word is capitalised
%          ; prose(list(word))   % plain formatting of words
%          ; title(list(word))  
%          ; nl
%          .
%  ==
%  Hence, renderers can be combined, For example, to simultaneously
%  display each line using the OSD and say, use:
%  ==
%  voice(Voice), render( say(Voice) && osd, Lines).
%  ==
%  See renderer/2 for more renderer macros.

render(_,[]) :- !.
render(C,Lines) :- renderer(C,Def), !, render(Def,Lines).
render(A>>B,Lines) :- render(A,Lines), render(B,Lines).
render(A&&B,Lines) :- concurrent(2,[render(A,Lines), render(B,Lines)],[]).
render(trap(A),Lines) :- catch(render(A,Lines), Ex, print_message(warning,Ex)).
render(async(A),Lines) :- thread_create(render(A,Lines), _,[detatched(true)]).
render(map(Action),[X|T]) :- render_line(Action,X), render(map(Action),T).
render(write_to(File,Opts,A),Lines) :- with_output_to_file( File, render(A,Lines), Opts).

render(con(M),Lines) :- con(render(M,Lines)).

render_line(A>>B,X) :- !, render_line(A,X), render_line(B,X).
render_line(A&&B,X) :- !, concurrent(2,[render_line(A,X), render_line(B,X)],[]).
render_line(trap(A),X) :- !, catch(render_line(A,X), Ex, print_message(warning,Ex)).
render_line(async(A),X) :- !, thread_create(render_line(A,X),_,[detached(true)]).
render_line(con(M),X) :- !, con(render_line(M,X)).
render_line(A,X)    :- call(A,X).



%% renderer( -M:renderer_macro, -R:renderer) is nondet.
%
%  Registry for renderer macros. This is a user extensible
%  predicate so that new macros can be registered by asserting
%  new clauses. Built in macros are:
%
%     | fmt | less(map(fmt)) | write to current output via less |
%     | con | con(map(fmt))  | write to console  |
%     | say(V) | map(say(V)) | say using voice V |
%     | osd(O) | map(osd(O)) | output to Xosd (on screen display) with options O |
%     | tick(D,M) | map(tick(D,M)) | write with ticker, delay D and method M |
%     | contick(D,M) | map(con(tick(D,M))) | write with ticker in console |
%     | contick(P)  | _ | equivalent to contick(0.06,pitched(5000,M)) |

renderer(less,less(map(fmt))).
renderer(less(A),write_to(pipe('less -F'),[buffer(line)],A)).
renderer(append(F),write_to(F,[mode(append)],fmt)).
renderer(fmt,map(fmt)).
renderer(con,con(map(fmt))).
renderer(say,say(_)).
renderer(say(V),map(say(V))).
renderer(osd(V),map(osd(V))).
renderer(tick(D,M),map(tick(D,M))).
renderer(contick(D,M),map(con(tick(D,M)))).
renderer(contick(M),map(con(tick(0.06,pitched(5000,M))))).

% ----------------- Format to stdout ----------------------

%% fmt( +Line:line) is det.
%
%  A line renderer that simply writes the formatted line to the current output.
%  Titles are formatted with an underline.
fmt(nl) :- nl.

fmt(title(L)) :- 
	sentence_codes(L,C), 
	length(C,LT), rep(LT,0'-,Dashes),
	format('\n\n   ~s\n   ~s\n\n',[C,Dashes]).

fmt(line(L)) :-
	sentence_codes(L,C), 
	format('   ~s\n',[C]).

fmt(prose(L)) :-
	words_codes(L,C), 
	format('   ~s\n',[C]).

fmt(prose(LW,L)) :-
	words_codes(L,C),
	format(atom(Cmd),'fmt ~w',[LW]),
	nl, with_output_to_file(pipe(Cmd),format('~s\n',[C]), []), nl.



% ----------------- Format to ticker ----------------------

%% ticker( +Delay:nonneg, +Method:tick_method, :Goal) is det.
%
%  Run Goal and send output to ticker with given inter-tick delay 
%  and the given tick method. See tick/3 for definition of tick_method.

ticker(D,M,Goal) :-
	with_output_to(codes(C),Goal),
	codes_ticker(D,M,C).

codes_ticker(_,_,[]).
codes_ticker(D,M,[C|CX]) :- 
	char_tick(C,M,D,D1), 
	put_code(C), 
	flush_output, 
	sleep(D1), 
	codes_ticker(D,M,CX).

char_tick(C,pitched(F,Pred),D,D1) :-
	code_delay(C,D,D1), 
	code_freq(C,F,F1),
	call(Pred,F1,D1).


code_delay(C,D,D1) :-
	(	code_type(C,alnum) -> D1=D
	;	code_type(C,space) -> D1 is 2*D
	;	code_type(C,quote) -> D1=D
	;	code_type(C,punct) -> D1 is 6*D
	).

code_freq(C,F,F1) :-
	(	code_type(C,newline) -> F1=2*F/3
	;	F1=F).


%% tick( +Delay:nonneg, +Method:tick_method, +Line:line) is det.
%
%  A renderer that formats the line and outputs it one character at
%  a time accompanied by a tick sound synthesised by Supercollider.
%  The delay between ordinary characers is Delay and Method describes
%  the parameters to be given to the synthesiser. The type =|tick_method|= is
%  ==
%  tick_method ---> pitched(+Freq:nonneg, +Pred:pred(+Freq:nonneg,+Dur:nonneg)).
%  ==
%  The second argument Pred must be a callable goal which will produce a sound
%  with nominal frequency Freq and duration Dur. It should return immediately.
tick(D,M,nl) :- nl, ticker(D,M,nl), sleep(D).
tick(D,M,title(L)) :- 
	sentence_codes(L,C), 
	write('\n\n   *** '), ticker(D,M,format('~s',[C])),
	write(' ***'),  ticker(D,M,format('\n\n')).

tick(D,M,line(L)) :-
	sentence_codes(L,C), 
	write('   '), ticker(D,M,format('~s\n',[C])).




% ------------------- Speech -------------------------------


say_words(Voice,Words) :-
	sentence_codes(Words,Codes),
	say_codes(Voice,Codes).

say_codes(Voice,Codes) :-
	speech:voice(Voice),
	(	speech_state(on)
	-> speech:say(Codes,[voice(Voice)])
	;  print_message(warning, speech_disabled)
   ).


%% say( ?Voice:atom, +Line:line) is det.
%
%  A renderer that uses the Mac OS X speech synthesiser command 'say'.
%  If the voice is not given, the first voice returned by voice/1 is used.
say(V,title(L)) :- say_words(V,L), sleep(1).
say(V,line(L))  :- say_words(V,L).
say(V,prose(L)) :- say_words(V,L).
say(V,prose(_,L)) :- say_words(V,L).
say(_,nl)       :- sleep(0.5).
say(V,phrase(P)):- phrase(P,L), say_codes(V,L). % module?



% ------------------- OSD  -------------------------------

osd_words(Options,L) :- 
	maplist(getopt(Options),
		[ 	wps(WPS)       / 4
		,	colour(Colour) / yellow
		,	vpos(VPos)     / middle
		,	hpos(HPos)     / center
		,	font(Font)     / aqui
		,	size(FontSize) / 22
		]),
	length(L,Words), Delay is ceil(Words/WPS),
	sentence_codes(L,Codes), 
	format(atom(FontStr),'-*-~w-medium-*-*-*-~w-*-*-*-*-*-*-*',[Font,FontSize]),
	format(string(Cmd)
		,'osd_cat -s 1 -d ~w -c ~w -f ~w -A ~w -p ~w <<< "~s"\n',
		[Delay,Colour,FontStr,HPos,VPos,Codes]),
	shell(Cmd).

getopt(OptsIn,Option/Default) :- option(Option,OptsIn,Default).

%% osd( +Options:list(osd_option), +Line:line) is det.
%% osd( +Line:line) is det.
%
%  A renderer that uses the X On Screen Display command osd_cat to
%  write the formatted output on an X11 display. Legal options (with
%  defaults):
%     * wps( N:nonneg) - [N=4]
%     Message display computed using N words per second.
%     * colour( C:atom) - [C=yellow]
%     Display colour.
%     * vpos( P:oneof([top,middle,bottom])) - [P=middle]
%     Vertical position.
%     * hpos( P:oneof([left,center,right])) - [P=center]
%     Horizontal position.
%     * font( F:atom) - [F=aqui]
%		An X11 font family.
%     * size( S:natural) - [S=22]
%     Font size

osd(X) :- osd([],X).
osd(_,nl) :- sleep(1).
osd(O,line(L)) :- osd_words(O,L).
osd(O,title(L)) :- maplist(upcase_atom,L,M), osd_words(O,M), sleep(1.5).

%% speak(Thing)// is det.
%  DCG for making Thing speakable. User extensible
%speak(int(N)) --> { number_codes(N,C) }, C.
%speak(wr(T))  --> { format(codes(L),'~w',[T]) }, L.
%speak(str(S)) --> { string_to_list(S,L) }, L.


%% words_codes( +Words:list(word), -Codes:list(code)) is semidet.
%
%  Transform a list words into a list of character codes.
%  Each word is an atom standing for either a word or a punctuation mark.
%  Underscores in words and translated into spaces.

words_codes(L,C2) :- words_codes(L,C,[]), maplist(us_to_space,C,C2).
words_codes(L,CX,T) :- atoms_codes(L,CX,T).


%% sentence_codes( +Sentence:list(word), -Codes:list(code)) is semidet.
%
%  Transform a list words into a list of character codes.
%  Each word is an atom standing for either a word or a punctuation mark.
%  Underscores in words and translated into spaces.

sentence_codes(L,C2) :- sentence_codes(L,C,[]), maplist(us_to_space,C,C2).
sentence_codes(L,[C1Upper|CX],T) :- 
	atoms_codes(L,[C1|CX],T),
	code_type(C1Upper,to_upper(C1)).

us_to_space(95,32) :- !.
us_to_space(A,A).

% punct( Symbol, SpaceBefore, SpaceAfter)
punct(',',nop,sp).
punct(';',nop,sp).
punct(':',nop,sp).
punct('-',nop,nop).
punct('--',nop,nop).
punct('.',nop,sp).
punct('!',nop,sp).
punct('?',nop,sp).
punct(')',nop, sp).
punct('(',sp,nop).
punct('\'',nop,nop).
punct('"',sp,sp).
punct('\'\'',nop,sp).
punct('``',sp,nop).
punct(',"',nop,nop).
punct('\'s',nop,sp).

atoms_codes([A]) --> !, wr(A).
atoms_codes([A,P]) --> 
	{punct(P,Before,_)}, !, 
	wr(A), Before, wr(P).

%FIXME
%atoms_codes([A,Q,P|AX]) --> {punct(Q,_,_), punct(P,_,_)}, !, atoms_codes([A,P|AX]).
atoms_codes([A,P|AX]) --> 
	{punct(P,Before,After)}, !, 
	wr(A), Before, wr(P), After, 
	atoms_codes(AX).  

atoms_codes([P|AX]) --> 
	{punct(P,_,After)}, !, 
	wr(P), After, atoms_codes(AX).

atoms_codes([A|AX]) -->
	({a_vowel(A,A1,AX)} -> wr(A1); wr(A)), 
	sp, atoms_codes(AX).

a_vowel(a,an,[W|_]) :- atom_codes(W,[W1|_]), member(W1,"aeiou").

%% words_line(Words:list(atom), Line:line) is det.
words_line(Words,line(Words)).
