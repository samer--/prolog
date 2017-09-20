:- module(speech, [ voice/1, voice/2
                  , say/1, say/2
                  , say_phrase/2
                  , with_output_to_say/2
                  ]).

/** <module> Mac OS X speech synthesiser services

   This module provides access to some of the services provided by the Mac OS X 
   say command. The text to speak can be provided as an argument to say/1 or say/2,
   or it can be generated dynamically by any goal by redirecting the current output
   stream to the input of the say process.

   This module also allows you to discover what voices are available on the system
   and whether the are of the new or old generation of voices.

   For much more information, see 
      https://developer.apple.com/library/mac/documentation/UserExperience/Conceptual/SpeechSynthesisProgrammingGuide/FineTuning/FineTuning.html#//apple_ref/doc/uid/TP40004365-CH5-SW4
 */

:- use_module(library(fileutils), [read_lines/2]).
:- use_module(library(dcg_core),  [list//1, (//)//2]).
:- use_module(library(dcg_codes), [at//1, ctype//1]).
:- use_module(library(snobol),    [arb//0, arbno//1]).
:- use_module(library(swipe),     [run/1, with_pipe_output/3, with_pipe_input/3]).
:- use_module(library(memo)).

:- volatile_memo voices(-voices:list(atom)).

voices(Voices) :-
   print_message(informational, consulting_system_voices),
   with_pipe_output(S,voices,read_lines(S,Lines)),
   maplist(voice_info,Lines,Voices).

voice_info(Line,Voice-Class) :-
   phrase(voice_line(VoiceCodes,Sample),Line),
   atom_codes(Voice,VoiceCodes),
   (new_style(Sample,_) -> Class=new; Class=old).

voice_line(VoiceCodes,Sample) -->
   voice_name//list(VoiceCodes), 
   spaces, lang, spaces, "# ", list(Sample).

voice_name --> alpha, arb, alpha.
lang --> alpha, alpha, ("_";"-"), arbno(alpha).
spaces --> " ", arbno(" ").
alpha --> ctype(alpha).
new_style --> "Hello, my name is ".

%% voice(-Style:one_of([old,new]), -Voice:atom) is nondet.
%% voice(-Voice:atom) is nondet.
%  True when Voice is a usable voice name.
%  This predicate uses the Mac OS X command say to discover what
%  voices are currently installed on the system. The style indicates
%  if this is one of the old generation speech synthesisers or the 
%  new generation, which sound better, but cannot sing or do phonemes.
voice(Style,Voice) :- voices(Voices), member(Voice-Style,Voices).
voice(Voice) :- voice(_,Voice).

swipe:def(say_string(String,Flags), sh(0>>0,"say~s ~s",[\Flags, @String])).
swipe:def(say_stdin(Flags),  sh($char >> 0,"say~s -f -",[\Flags])).
swipe:def(voices, sh(0 >> $char,"say -v ~s",[@'?'])).

%% say(+Text, +Options) is det.
%% say(+Text) is det.
%
%  Speaks the given Text, which can be of any type recognised by the
%  "~s" format specifier. Options can be:
%  * voice(V)
%     Uses the specified voice, as obtained from voice/1
%  * rate(R)
%     Speak at roughly R words-per-minute.
%  * progress
%     Write progress to stdout as synthesis proceeds.
%  * interactive
%     Write the sentence and highlight the current word as spoken.
say(Text) :- say(Text,[]).
say(Text,Options) :-
   phrase(foldl(say_option, Options),Flags),
   format(string(String),'~s',[Text]),
   run(say_string(String,Flags)).


%% say_phrase(+Phrase,+Options) is det.
%  Say results of given DCG phrase.
:- meta_predicate say_phrase(//,+).
say_phrase(Phrase,Opts) :-
   phrase(Phrase,Codes),
   say(Codes,Opts).

%% with_output_to_say(+Goal:callable, +Options) is det.
%
%  Calls the given goal with Prolog output diverted to the speech
%  synthesiser. Recognises the same options as say/2.
:- meta_predicate with_output_to_say(0,+).
with_output_to_say(Goal,Options) :-
   phrase(foldl(say_option, Options),Flags),
   with_pipe_input(S, say_stdin(Flags), with_output_to(S,Goal)).


% NB leading spaces!
say_option(voice(V)) --> {voice(V)}, " -v ", at(V).
say_option(rate(WPM)) --> " -r ", at(WPM). % words per minute
say_option(progress) --> " --progress".
say_option(interactive) --> " -i".
