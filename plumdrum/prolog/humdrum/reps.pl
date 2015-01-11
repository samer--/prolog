:- module(reps, [ representation/2 ]).

%% representation( +Rep:xinterp, -Desc:atom) is semidet.
%% representation( -Rep:xinterp, -Desc:atom) is nondet.
%
%  Known exclusive interpretations

	representation( bhatk,  'Bhatkhande notation').
	representation( cents,  'pitch in cents').
	representation( cbr,    'critcal band rand in ERBs').
	representation( cocho,  'cochlear coordinates in mm').
	representation( correl, 'statistical correlation').
	representation( date,   'absolute universal time').
	representation( dB,     'relative amplitude').
	representation( deg,    'relative scale degree').
	representation( degree, 'absolute scale degree').
	representation( diss,   'sensory dissonance').
	representation( dur,    'physical time duration').
	representation( embel,  'embelishment tones').
	representation( freq,   'frequency in Hz').
	representation( fret,   'fretted instrument tabulature').
	representation( harm,   'Western functional harmony').
	representation( hint,   'harmonic interval').
	representation( 'Hildegard', 'Hildegard manuscripts notation').
	representation( iv,     'interval vector').
	representation( 'IPA',  'International Phonetic Alphabet').
	representation( kern,   'Core pitch and duration').
	representation( melac,  'melodic accent').
	representation( metpos, 'metrical position').
	representation( mint,   'melodic interval').
	representation( 'MIDI', 'MIDI events tabulature').
	representation( nf,     'normal form for pitch sets').
	representation( ordo,   'sequential order').
	representation( pc,     'pitch-class').
	representation( pcset,  'Fortean pitch-class set').
	representation( pf,     'prime form representation').
	representation( pitch,  'ANSI pitch notation').
	representation( recip,  'Reciprocal duration').
	representation( simil,  'Damerau-Levenshtein edit distance').
	representation( semits, 'pitch in semitones from middle C').
	representation( solfg,  'tonic solfa syllable').
	representation( solfg,  'French solfège system').
	representation( specC,  'spectral centroid in Hz').
	representation( spect,  'discrete spectrum').
	representation( synco,  'degree of syncopation').
	representation( takt,   'beat-position').
	representation( text,   'vocal text').
	representation( time,   'relative time in seconds').
	representation( 'Tonh',  'German Tonhöhe pitch system').
	representation( 'URrhythm',  'Johnson-Laird beat prototype').
	representation( 'vox#', 'number of active voices').
	representation( 'Zeit', 'absolute universal time period').

