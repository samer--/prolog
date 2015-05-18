/* Part of plumdrum
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

:- module(dynam, []). 

/** <module> dynam spine format for Humdrum 

   This module provides the humdrum hook predicate humdrum:hum_data_hook//2
   to enable parsing of the dynam and db spine type.

   The dynam interpretation provides the following data term type:
   ==
   dynam ---> st(loudness)  % marking, forte, piano etc
            ; dy(progress, process)
            ; ed(editorial)
            ; rest 
            ; accent
            ; subito
            ; sforz.

   loudness ---> mf  % mezzoforte
               ; mp  % mezzopiano
               ; f(N:natural) % N forte marks
               ; p(N:natural) % N piano marks.
   progress ---> begin; cont; end.
   process  ---> crescendo; diminuendo.
   editorial ---> explicit; published.
   ==

   The db interpretation simply produces floating point numbers
   encoding loudness in decibels.
   ==
   db == float.
   ==
*/

:- use_module(library(dcg_core)).
:- use_module(library(dcg_macros)).
:- use_module(library(humdrum)).
:- use_module(library(humdrum/humutils)).

:- set_prolog_flag(double_quotes,codes).

humdrum:hum_data_hook(dynam,Sigs) --> !, seqmap(dynam,Sigs).
humdrum:hum_data_hook(db,DB) --> !, float(DB).

dynam(st(f(N))) --> "f", !, rep_shared(M,"f"), {succ(M,N)}.
dynam(st(p(N))) --> "p", !, rep_shared(M,"p"), {succ(M,N)}.
dynam(st(mf))   --> "mf".
dynam(st(mp))   --> "mp".

dynam(rest)   --> "r".
dynam(accent) --> "v".
dynam(subito) --> "s".
dynam(sforz)  --> "z".

dynam(ed(explicit)) --> "X".
dynam(ed(published)) --> "x".

dynam(dy(begin,crescendo))  --> "<".
dynam(dy(cont,crescendo))   --> "(".
dynam(dy(end,crescendo))    --> "[".
dynam(dy(begin,diminuendo)) --> ">".
dynam(dy(cont,diminuendo))  --> ")".
dynam(dy(end,diminuendo))   --> "]".

