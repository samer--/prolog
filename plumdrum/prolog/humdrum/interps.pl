:- module(interps,[]).

/** <module> Hooks for parsing Humdrum spine interpretations

  This module defines the hook predicate humdrum:hum_interp_hook//1
  for recognising when a Humdrum spine contains interpretation data.

  It currently produces interpretation terms with the following type:
  ==
  interp ---> section(secname)
            ; section(atom,atom)
            ; explist(list(atom))
            ; explist(atom,list(atom))
            ; tb(natural)               % timebase
            ; metre(natural,natural)    % metre num and denom
            ; metro(float)              % metronome 
            ; metro(range(float))       % metronome range
            ; tempo(atom)               % tempo marking (verbal)
            ; staff(list(natural))      % staff number
            ; clef(atom)                % clef type name
            ; instr(atom)               % instrument name
            ; igroup(atom)              % instrument group 
            ; iclass(atom)              % instrument class
            ; keysig(list(pitch_class)) % key signature
            ; key(pitch_class,mode)     % key as major/minor
            ; trans                     % transposed?
            .

  range(A) ---> A--A.
  ==
  See humutils.pl for definition of pitch_class type.
*/

:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_macros)).
:- use_module(library(humdrum/humutils)).
:- op(550,xfx,--).

:- set_prolog_flag(double_quotes,codes).
% ------------- standard interpretations -------------


% tandem interpretations
% kern, solfg, pc, mint: M metre
% kern, solfg, degree, pc, mint : k[...] key signature
% kern, solfg, degree, pc, mint :<note>: key
% kern, solfg, pc: MM tempo
% pc: tb timebase
% > sectioning
% specC: *pure, *complex
% *H harmonic number

humdrum:hum_interp_hook(Term) --> interp(Term).



secname(A) --> atom_chars_ex(text,"",">[,",A).

% sectioning
interp(section(A))   --> ">", secname(A).
interp(section(B,A)) --> ">", secname(B), ">", secname(A).
interp(explist(A))   --> ">", sqbr(seqmap_with_sep(",",secname,A)).
interp(explist(B,A)) --> ">", secname(B), sqbr(seqmap_with_sep(",",secname,A)).

% timing
interp(tb(R))        --> "tb", nat(R). % time base
interp(metre(N,D))   --> "M", nat(N), "/", nat(D).
interp(metro(M))     --> "MM", float(M).
interp(metro(M1--M2))--> "MM", float(M1), "-", float(M2).
interp(tempo(W))     --> "MM", sqbr(charsex(alpha,[],[],WW)),
	{atom_codes(W,WW)}.


% other
interp(staff(S))  --> "staff", staff_numbers(S).
interp(clef(C))   --> "clef", atom_chars_ex(graph,[],[],C).
interp(instr(I))  --> "I",    atom_chars_ex(graph,"CGT",[],I).
interp(igroup(I)) --> "IG",   atom_chars_ex(graph,[],[],I).
interp(iclass(I)) --> "IC",   atom_chars_ex(graph,[],[],I).

interp(keysig(K)) --> "k", sqbr(seqmap(pitchclass(lower),K)).
interp(key(P,maj))--> pitchclass(upper,P), ":".
interp(key(P,min))--> pitchclass(lower,P), ":".
interp(trans)     --> "ITr".

staff_numbers(S) --> seqmap_with_sep("/",nat,S).

% read non-empty list of chars of type Type, first character not in Ex1, 
% remaining characters not in Ex, and convert to atom.
atom_chars_ex(Type,Ex1,Ex,A) --> 
	charsex(Type,Ex1,Ex,Codes), 
	{Codes\=[], atom_codes(A,Codes)}.

