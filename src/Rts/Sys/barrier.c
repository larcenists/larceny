/* Copyright 1998 Lars T Hansen.
 * 
 * $Id$
 *
 * Larceny run-time system -- write barrier for new collector
 *
 * Write barrier support code.
 *
 * wb_setup() sets up the write barrier in a generational system.
 * wb_setup0() sets initializes the module in a non-generational system.
 * wb_disable() disables the barrier (see file Rts/Sparc/barrier.s).
 * wb_re_setup() is used by the low-level allocator to inform the
 *    barrier about a new (reallocated) page table.  This is a hack.
 *
 * Also see Rts/Sparc/barrier.s.
 *
 * The code in this file is *NOT* reentrant.
 * The code in this file does not depend on word size.
 * The code in this file does not depend on header size.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "memmgr.h"
#include "barrier.h"

/* Having these variables global makes it impossible to have more than
 * one write barrier in the system, which makes it impossible to
 * have more than one GC.  We should fix this, but largely this WB
 * will go away with the new WB.
 */

static word **wb_genssbtopv;   /* [0..n] for SSB logs (0 *is* valid). */
static word **wb_genssblimv;   /* ditto */
static int wb_generations;     /* the value 'n' */
static word *wb_globals;       /* the globals array */

void wb_setup( gclib_desc_t *genv, /* maps page number to generation number */
	       byte *pagebase,     /* address of lowest page in arena: fixed */
	       int generations,    /* the value 'n': fixed */
               word *globals,      /* the globals vector */
	       word **genssbtopv,
	       word **genssblimv,
	       word **satbssbtopv,
	       word **satbssblimv,
	       int  np_young_gen,  /* -1 or generation # for NP young */
	       int  np_ssbidx      /* -1 or idx in vectors for magic remset */
             )
{
  wb_generations = generations;
  wb_globals = globals;
  wb_genssbtopv = genssbtopv;
  wb_genssblimv = genssblimv;

  assert( generations > 1 );

  globals[ G_SSBTOPV ] = (word)wb_genssbtopv;
  globals[ G_SSBLIMV ] = (word)wb_genssblimv;
  globals[ G_SATBTOPV ] = (word)satbssbtopv;
  globals[ G_SATBLIMV ] = (word)satbssblimv;
  globals[ G_GENV ] = (word)genv;
  globals[ G_PGBASE ] = (word)pagebase;
  globals[ G_NP_YOUNG_GEN ] = (word)np_young_gen;
  globals[ G_NP_YOUNG_GEN_SSBIDX ] = (word)np_ssbidx;
  wb_lowlevel_enable_barrier( globals );
}

void wb_disable_barrier( word *globals )
{
  wb_generations = 0;
  wb_lowlevel_disable_barrier( globals );
}

void wb_re_setup( byte *pagebase, unsigned *genv )
{
  if (wb_generations > 0) {
    wb_globals[ G_GENV ] = (word)genv;
    wb_globals[ G_PGBASE ] = (word)pagebase;
  }
}

/* eof */
