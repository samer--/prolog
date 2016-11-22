:- module(genmidi, [gm/1, gm/4, perc/2]).
/** <module> General MIDI instrument assignments 

Instruments are named by terms, some of which are of the form =|Class/N|=,
where =|N|= is an integer and =|Class|= is an atom representing a class of instruments,
for example =|piano, epiano, guitar, sax|= etc. These can be discovered using gm/1.
*/

%% gm(-I:instr, -P:byte, -MSB:byte, -LSB:byte) is nondet.
%% gm(-I:instr) is nondet.
%  True when I is a recognised instrument with General MIDI programme number P and 
%  bank most and least significant bytes MSB and LSB.
gm( Instr) :- gm(Instr,_,_,_).
gm( Instr, P0, 0, LSB) :- primo(Instr,P1,LSB), succ(P0,P1).
gm( kit/Type/1, P0, 0, 120) :- seco(Type,P1), succ(P0,P1).
gm( kit/Type/2, P0, 1, 0) :- seco(Type,P1), succ(P0,P1).

primo( piano/1,  1, 0). % acoustic grand
primo( piano/2,  1, 8).
primo( piano/3,  1, 16).

primo( piano/4,  2, 0). % bright acoustic grand
primo( piano/5,  2, 8).

primo( piano/6,  3, 0). % electric grand
primo( piano/7,  3, 8).

primo( piano/8,  4, 0). % honky-tonk
primo( piano/9,  4, 8).

primo( epiano/1,    5, 0).
primo( epiano/2,    5, 8).
primo( epiano/3,    5, 16).
primo( epiano/4,    5, 24).
primo( epiano/5,    6, 0).
primo( epiano/6,    6, 8).
primo( epiano/7,    6, 16).

primo( harpsichord/1,  7, 0).
primo( harpsichord/2,  7, 8).
primo( harpsichord/3,  7, 16).
primo( harpsichord/4,  7, 24).
primo( clavichord,     8, 0). % or clavinet
primo( celesta,        9, 0).

primo( glockenspiel,	10, 0).
primo( music_box,   	11, 0).
primo( vibraphone/1,	12, 0).
primo( vibraphone/2,	12, 8).
primo( marimba/1,	   13, 0).
primo( marimba/2,	   13, 8).
primo( xylophone,	   14, 0).

primo( bell/tubular, 15, 0).
primo( bell/church,  15, 8).
primo( bell/carillon, 15, 9).
primo( santur, 16, 0).

primo( organ/1, 17, 0).
primo( organ/2, 17, 8).
primo( organ/3, 17, 16).
primo( organ/4, 17, 32).
primo( organ/5, 18, 0).
primo( organ/6, 18, 8).
primo( organ/7, 18, 32).
primo( organ/8, 19, 0).
primo( organ/church/1, 20, 0).
primo( organ/church/2, 20, 8).
primo( organ/church/3, 20, 16).
primo( organ/reed, 21, 0).

primo( accordion,   22, 0).
primo( harmonica,   22, 0).
primo( bandeon,   22, 0).

primo( ukulele, 25, 8).
primo( guitar/nylon1, 25, 0).
primo( guitar/nylon2, 25, 16).
primo( guitar/nylon3, 25, 32).
primo( guitar/steel,   26, 0).
primo( mandolin,       26, 16).
primo( guitar/jazz,    27, 0).
primo( guitar/clean,   28, 0).
primo( guitar/muted,   29, 0).
primo( guitar/funk,    29, 8).
primo( guitar/overdrive, 30, 0).
primo( guitar/distortion, 31, 0).
primo( guitar/feedback, 31, 8).
primo( guitar/harmonics, 32, 0).
primo( guitar/feedback2, 32, 8).

primo( bass/acoustic, 33, 0).
primo( bass/fingered, 34, 0).
primo( bass/picked,   35, 0).
primo( bass/fretless, 36, 0).
primo( bass/slap1,    37, 0).
primo( bass/slap2,    38, 0).
primo( bass/synth1,   39, 0).
primo( bass/synth2,   39, 1).
primo( bass/synth3,   39, 8).
primo( bass/synth4,   40, 0).
primo( bass/synth5,   40, 8).
primo( bass/rubber,   40, 16).

primo( violin, 41, 0).
primo( viola, 42, 0).
primo( cello, 43, 0).
primo( contrabass, 44, 0).
primo( strings/tremolo,     45, 0).
primo( strings/pizzicato,     46, 0).

primo( harp,     47, 0).
primo( timpani,     47, 0).
primo( strings/1,     49, 0).
primo( strings/orch,  49, 8).
primo( strings/slow,  50, 0).
primo( strings/synth1, 51, 0).
primo( strings/synth2, 51, 8).
primo( strings/synth3, 52, 0).

primo( choir/aahs/1, 53, 0).
primo( choir/aahs/2, 53, 8).
primo( choir/oohs, 54, 0).

primo( trumpet, 57, 0).
primo( trombone/1, 58, 0).
primo( trombone/2, 58, 8).
primo( tuba, 59, 0).
primo( trumpet/muted, 60, 0).
primo( french_horn/1, 61, 0).
primo( french_horn/2, 61, 8).

primo( sax/soprano,  65, 0).
primo( sax/alto,     66, 0).
primo( sax/tenor,    67, 0).
primo( sax/baritone, 68, 0).
primo( oboe,        69, 0).
primo( english_horn, 70, 0).
primo( bassoon,    71, 0).
primo( clarinet,   72, 0).
primo( piccolo,      73, 0).
primo( flute,      74, 0).
primo( recorder,   75, 0).
primo( pan_pipe,   76, 0).
primo( shakuhachi,   78, 0).
primo( whistle,   79, 0).
primo( ocarina,   80, 0).

primo( square/1,    81, 0).
primo( square/2,    81, 1).
primo( sine,        81, 8).
primo( saw/1,       82, 0).
primo( saw/2,       82, 1).
primo( lead/calliope, 83, 0).
primo( lead/chiff,    84, 0).
primo( lead/charang,  85, 0).
primo( lead/voice,    86, 0).
primo( lead/fifths,   87, 0).
primo( lead/w_bass,   88, 0).

primo( pad/new_age,  89, 0).
primo( pad/warm,     90, 0).
primo( pad/polysynth,91, 0).
primo( pad/choir,    92, 0).
primo( pad/bowed,    93, 0).
primo( pad/metallic, 94, 0).
primo( pad/halo,     95, 0).
primo( pad/sweep,    96, 0).

primo( fx/rain,       97, 0).
primo( fx/soundtrack, 98, 0).
primo( fx/crystal,    99, 0).
primo( fx/atmosphere, 100, 0).
primo( fx/brightness, 101, 0).
primo( fx/goblins,    102, 0).
primo( fx/echoes,     103, 0).
primo( fx/sci_fi,     104, 0).

primo( sitar/1,     105, 0).
primo( sitar/2,     105, 1).
primo( banjo,       106, 0).
primo( shamisen,    107, 0).
primo( koto,        108, 0).
primo( kalimba,     109, 0).
primo( bagpipe,     110, 0).
primo( fiddle,      111, 0).
primo( shanai,      112, 0).


primo( tinkle_bell,   113, 0).
primo( agogo,   114, 0).
primo( steel_drums,   115, 0).
primo( woodblock,   116, 0).
primo( taiko,   117, 0).
primo( melodic_tom,   118, 0).
primo( synth_drum,   119, 0).
primo( reverse_cymbal,   120, 0).

% drum kits
seco( standard,  1).
seco( standard2, 2).
seco( room,      9).
seco( power,    17).
seco( elec,     25).
seco( tr808,    26).
seco( dance,    27).
seco( jazz,     33).
seco( brush,    41).
seco( orch, 	 49).
seco( sfx, 	    57).


%% perc(-NN:byte, -Name:atom) is nondet.
%  True when note number NN corresponds to named percussion sound. Applies
%  when using one of the =|kit/_|= instruments.
perc(35,'Acoustic Bass Drum').
perc(36,'Bass Drum 1').
perc(37,'Side Stick').
perc(38,'Acoustic Snare').
perc(39,'Hand Clap').
perc(40,'Electric Snare').
perc(41,'Low Floor Tom').
perc(42,'Closed Hi-Hat').
perc(43,'High Floor Tom').
perc(44,'Pedal Hi-Hat').
perc(45,'Low Tom').
perc(46,'Open Hi-Hat').
perc(47,'Low-Mid Tom').
perc(48,'High-Mid Tom').
perc(49,'Crash Cymbal 1').
perc(50,'High Tom').
perc(51,'Ride Cymbal 1').
perc(52,'Chinese Cymbal').
perc(53,'Ride Bell').
perc(54,'Tambourine').
perc(55,'Splash Cymbal').
perc(56,'Cowbell').
perc(57,'Crash Cymbal 2').
perc(58,'Vibraslap').
perc(59,'Ride Cymbal 2').
perc(60,'High Bongo').
perc(61,'Low Bongo').
perc(62,'Mute Hi Conga').
perc(63,'Open Hi Conga').
perc(64,'Low Conga').
perc(65,'High Timbale').
perc(66,'Low Timbale').
perc(67,'High Agogo').
perc(68,'Low Agogo').
perc(69,'Cabasa').
perc(70,'Maracas').
perc(71,'Short Whistle').
perc(72,'Long Whistle').
perc(73,'Short Guiro').
perc(74,'Long Guiro').
perc(75,'Claves').
perc(76,'Hi Woodblock').
perc(77,'Low Woodblock').
perc(78,'Mute Cuica').
perc(79,'Open Cuica').
perc(80,'Mute Triangle').
perc(81,'Open Triangle').
