:- module(docutils,[
		phrase_to_pager/1
	,	phrase_to_pager/2
	,	html_to_viewer/2
	,	html_to_pager/2
	,	roff_to_pager/1
	,	roff/3
	]).

:- use_module(library(fileutils)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library('http/html_write')).

:- meta_predicate
		phrase_to_pager(2)
	,	phrase_to_pager(+,2)
	,	html_to_pager(+,2)
	,	html_to_viewer(+,2)
	,	roff_to_pager(2)
	,	roff(:,?,?)
	.

phrase_to_pager(Phrase) :- phrase_to_pager('less -FX',Phrase).

phrase_to_pager(Pager,Phrase) :-
	phrase(Phrase,Codes,[]),
	with_output_to_file(pipe(Pager), format(Codes,[])).

html_to_pager(Viewer,Phrase) :-
	tty_size(_,TermWidth),
	FmtWidth is min(TermWidth,90),
	viewer_cmd_format(Viewer,FmtWidth,ViewerCmd),
	phrase(Phrase,Codes,[]),
	with_output_to_file(
		pipe(ViewerCmd),
		html_write:print_html(Codes)
	).

viewer_cmd_format(elinks/Pager,Width,Cmd) :- !, format(string(Cmd),'elinks -dump 1 -dump-width ~w | ~w',[Width,Pager]).
viewer_cmd_format(lynx/Pager,Width,Cmd) :- !, format(string(Cmd),'lynx -stdin -dump -width=~w | ~w',[Width,Pager]).
viewer_cmd_format(Browser,Width,Fmt) :- viewer_cmd_format(Browser/'less -FX',Width,Fmt).


html_to_viewer(Viewer,Phrase) :-
	tty_size(_,TermWidth),
	FmtWidth is min(TermWidth,90),
	(	Viewer=elinks,
		sformat(ElinksViewer,'elinks -force-html',[FmtWidth])
	;	Viewer=lynx,
		sformat(ElinksViewer,'lynx -stdin',[FmtWidth])
	),
	phrase(Phrase,Codes,[]),
	with_stream(Stream,
		open(pipe(ElinksViewer),write,Stream), 
		print_html(Stream,Codes)
	).

roff_to_pager(Phrase) :-
	tty_size(_,TermWidth), 
	FmtWidth is min(TermWidth,78),
	utils:rep(FmtWidth,'-',HRule),
	format(HRule), nl,
	format(string(Groff),'/usr/bin/groff -mdoc -Tascii -rLL=~wn | less -FX',[FmtWidth]),
	phrase(Phrase,Codes,[]),
	with_output_to_file(pipe(Groff), format(Codes,[])), nl.

roff(M:mm1(Nm,Args)) --> ".", roff(M:mm(Nm,Args)), cr.
roff(M:mm(Nm,Args)) --> at(Nm), sp, seq(M:Args,sp). 

roff(M:m(Nm)) --> roff(M:mm(Nm,[])).
roff(M:m(Nm,A1)) --> roff(M:mm(Nm,[A1])).
roff(M:m(Nm,A1,A2)) --> roff(M:mm(Nm,[A1,A2])).
roff(M:m(Nm,A1,A2,A3)) --> roff(M:mm(Nm,[A1,A2,A3])).
roff(M:m(Nm,A1,A2,A3,A4)) --> roff(M:mm(Nm,[A1,A2,A3,A4])).

roff(M:m1(Nm)) --> roff(M:mm1(Nm,[])).
roff(M:m1(Nm,A1)) --> roff(M:mm1(Nm,[A1])).
roff(M:m1(Nm,A1,A2)) --> roff(M:mm1(Nm,[A1,A2])).
roff(M:m1(Nm,A1,A2,A3)) --> roff(M:mm1(Nm,[A1,A2,A3])).
roff(M:m1(Nm,A1,A2,A3,A4)) --> roff(M:mm1(Nm,[A1,A2,A3,A4])).


