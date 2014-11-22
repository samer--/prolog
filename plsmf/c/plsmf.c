/*
 *  plsmf - Standard MIDI file reading and writing for SWI Prolog
 *
 *  Copyright (C) 2010 Samer Abdallah
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 */


#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <smf.h>

#include <stdio.h>
#include <string.h>

typedef struct smf_blob {
	smf_t *smf;
} smf_blob_t;

static PL_blob_t smf_blob;
static functor_t f_midi[4];


// --------------------------   Prolog boilerplate

install_t install();

foreign_t open_read( term_t filename, term_t smf); 
foreign_t is_smf( term_t conn); 
foreign_t get_description( term_t smf, term_t desc);
foreign_t get_duration( term_t smf, term_t dur);
foreign_t get_events( term_t smf, term_t events);
foreign_t get_events_between( term_t smf, term_t t1, term_t t2, term_t events);

int smf_release(atom_t a)
{
	PL_blob_t *type;
	size_t    len;
	void *p;

	p=PL_blob_data(a,&len,&type);
	if (p) {
		// printf("Releasing MIDI file %p.\n",p);
		smf_delete(((smf_blob_t *)p)->smf);
	}
	return TRUE;
}

install_t install() 
{ 
	PL_register_foreign("smf_read",  2, (void *)open_read, 0);
	PL_register_foreign("smf_description", 2, (void *)get_description, 0);
	PL_register_foreign("smf_duration", 2, (void *)get_duration, 0);
	PL_register_foreign("smf_events", 2, (void *)get_events, 0);
	PL_register_foreign("smf_events_between", 4, (void *)get_events_between, 0);
	PL_register_foreign("is_smf",  1, (void *)is_smf, 0);

	{
		atom_t a_midi = PL_new_atom("smf");
		int i;
		for (i=0; i<4; i++) f_midi[i] = PL_new_functor(a_midi,i+1);
	}

	smf_blob.magic = PL_BLOB_MAGIC;
	smf_blob.flags = PL_BLOB_UNIQUE;
	smf_blob.name = "plsmf_smf";
	smf_blob.acquire = 0; // rs_acquire;
	smf_blob.release = smf_release; 
	smf_blob.compare = 0; // rs_compare;
	smf_blob.write   = 0; // rs_write;
}

static int io_error(const char *file, const char *action)
{ 
	term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "smf_error", 2,
		        PL_CHARS, file,
		        PL_CHARS, action,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

	
// throws a Prolog exception to signal type error
static int type_error(term_t actual, const char *expected)
{ 
	term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "type_error", 2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

static int unify_smf(term_t smf,smf_blob_t *p) {
	return PL_unify_blob(smf, p, sizeof(smf_blob_t), &smf_blob); 
}

static int get_smf(term_t smf, smf_blob_t *p)
{ 
	PL_blob_t *type;
	size_t    len;
	smf_blob_t *p1;
  
	PL_get_blob(smf, (void **)&p1, &len, &type);
	if (type != &smf_blob) { return type_error(smf, "smf_blob"); }
	else { *p=*p1; return TRUE; }
} 

int chomp(const unsigned char status, unsigned short *ok, unsigned short *size) 
{
	// We are expecting that the next byte in the packet is a status byte.
	if ( !(status & 0x80) ) return 1; // abort this packet

	*ok=1; // default is to transmit message
	// Determine the number of bytes in the MIDI message.
	if      (status<0xC0) *size=3; 
	else if (status<0xE0) *size=2;
	else if (status<0xF0) *size=3;
	else {
		switch (status) {
			case 0xF0: return 1; // sys ex: abort packet
			case 0xF1: *size=3; *ok=0; break; // time code: ignore
			case 0xF2: *size=3; break;
			case 0xF3: *size=2; break;
			case 0xF8: *size=3; *ok=0; break; // timing tick: ignore
			case 0xFE: *size=1; *ok=0; break; // active sensing: ingore
			default:   *size=1; 
		}
	}
	return 0;
}


static int read_events_until(smf_t *smf, double t, term_t events)
{
	term_t midi=PL_new_term_ref();
	smf_event_t	*ev;

	events=PL_copy_term_ref(events);
	while ((ev=smf_get_next_event(smf)) != NULL) {
		unsigned short size;
		unsigned short transmit;

		if (smf_event_is_metadata(ev)) continue; 
		if (ev->time_seconds>t) break;
		if (chomp(ev->midi_buffer[0],&transmit,&size)) continue;

		if (transmit) {
			term_t data0=PL_new_term_refs(1+size);
			term_t head=PL_new_term_ref();
			term_t tail=PL_new_term_ref();
			int i;


			for (i=0; i<size; i++) PL_put_integer(data0+i+1,ev->midi_buffer[i]);
			PL_put_float(data0,ev->time_seconds);
			PL_cons_functor_v(midi,f_midi[size],data0);

			if (!PL_unify_list(events,head,tail)) PL_fail;
			if (!PL_unify(head,midi)) PL_fail;

			events=tail;
		}
	}
	return PL_unify_nil(events);
}


// ------- Foreign interface predicates

foreign_t open_read(term_t filename, term_t smf) 
{ 
	char 			*fn;
	smf_blob_t	smfb;

	if (PL_get_chars(filename, &fn, CVT_ATOM | CVT_STRING)) {
		printf("opening MIDI file '%s'...\n",fn);
		smfb.smf = smf_load(fn);
		if (smfb.smf) return unify_smf(smf,&smfb);
		else return io_error(fn,"read"); 
	} else return type_error(filename,"atom");
}

foreign_t get_description( term_t smf, term_t desc)
{
	smf_blob_t	s;
	
	if (get_smf(smf,&s)) {
		char 	*d=smf_decode(s.smf);
		if (d) return PL_unify_atom_chars(desc,d);
		else return PL_unify_atom_chars(desc,"<no description>");
	} return FALSE;
}

foreign_t get_duration( term_t smf, term_t dur)
{
	smf_blob_t	s;
	return get_smf(smf,&s) && PL_unify_float(dur,smf_get_length_seconds(s.smf));
}

foreign_t get_events( term_t smf, term_t events)
{
	smf_blob_t	s;

	if (!get_smf(smf,&s)) return FALSE;
	smf_rewind(s.smf);
	return read_events_until(s.smf,smf_get_length_seconds(s.smf),events);

}

foreign_t get_events_between( term_t smf, term_t start, term_t end, term_t events)
{
	smf_blob_t	s;
	double	t1, t2;
	if (!(get_smf(smf,&s) && PL_get_float(start,&t1) && PL_get_float(end,&t2))) 
		return FALSE;

	int rc=smf_seek_to_seconds(s.smf,t1);
	printf("seek, rc=%d.\n",rc);
	return read_events_until(s.smf,t2,events);
}

foreign_t is_smf(term_t conn) 
{ 
	PL_blob_t *type;
	return PL_is_blob(conn,&type) && type==&smf_blob;
}

