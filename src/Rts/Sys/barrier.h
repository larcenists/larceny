/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- write barrier interface.
 * See Rts/Sys/barrier.c and Rts/Sparc/barrier.s for more information.
 */

#ifndef INCLUDED_BARRIER_H
#define INCLUDED_BARRIER_H

#include "larceny-types.h"
#include "gclib.h"

/* Initialize the write barrier for a generational system. */

void wb_setup( gclib_desc_t *genv,  /* map from page to generation */
	       byte *pagebase,      /* fixed: address of lowest page */
	       int generations,     /* fixed: number of generations */
	       word *globals,       /* fixed: globals vector */
	       word **genssbtopv,   /* fixed: SSB top pointers */
	       word **genssblimv,   /* fixed: SSB lim pointers */
	       word **satbssbtopv,
	       word **satbssblimv,
	       int  np_young_gen,   /* -1 or generation # for NP young */
	       int  np_ssbidx       /* -1 or idx in vectors for magic remset */
	      );

/* Disable the write barrier. */
void wb_disable_barrier( word *globals );

/* If the descriptor tables change, notify the barrier */
void wb_re_setup( byte *pagebase, unsigned *genv );

/* Lowlevel support for barrier setup and shutdown. */

extern void wb_lowlevel_disable_barrier( word *globals );
extern void wb_lowlevel_enable_barrier( word *globals );

#endif
/* eof */
