/* Rts/Sys/barrier.c
 * Larceny run-time system -- write barrier for new collector
 *
 * $Id: barrier.c,v 1.7 1997/05/23 13:42:46 lth Exp $
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
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "barrier.h"
#include "assert.h"


/* Having these variables global makes it impossible to have more than
 * one write barrier in the system, which makes it impossible to
 * have more than one GC.  We should fix this, but largely this WB
 * will go away with the new WB.
 */

static word **wb_ssbtopv;      /* [0..n] where 0 is invalid */
static word **wb_ssblimv;      /* ditto */
static int wb_generations;     /* the value 'n' */
static word *wb_globals;       /* the globals array */

void wb_setup( unsigned *genv,     /* maps page number to generation number */
	       unsigned pagebase,  /* address of lowest page in arena: fixed */
	       int generations,    /* the value 'n': fixed */
               word *globals,      /* the globals vector */
	       word **ssbtopv,
	       word **ssblimv,
	       int  np_young_gen,  /* -1 or generation # for NP young */
	       int  np_ssbidx      /* -1 or idx in vectors for magic remset */
             )
{
  wb_generations = generations;
  wb_globals = globals;
  wb_ssbtopv = ssbtopv;
  wb_ssblimv = ssblimv;

  assert( generations > 1 );

  globals[ G_SSBTOPV ] = (word)wb_ssbtopv;
  globals[ G_SSBLIMV ] = (word)wb_ssblimv;
  globals[ G_GENV ] = (word)genv;
  globals[ G_PGBASE ] = (word)pagebase;
  globals[ G_NP_YOUNG_GEN ] = (word)np_young_gen;
  globals[ G_NP_YOUNG_GEN_SSBIDX ] = (word)np_ssbidx;
}


void wb_setup0( void )
{
  wb_generations = 0;
}

void
wb_re_setup( unsigned *genv )
{
  if (wb_generations > 0)
    wb_globals[ G_GENV ] = (word)genv;
}


/**********************************************************************/
/* Obsolete code beyond this point. */


/* Synchronize the barrier tables with values from the remembered sets. */

void
wb_sync_ssbs( void )
{
#if 0
  int i;

  debug2msg( "   *** sync_ssbs" );

  /* ssblimv[] must be reset every time because remembered sets may have
   * been shuffled, esp. by the non-predictive collector. 
   */
  for ( i = 1 ; i < wb_generations ; i++ ) {
    wb_ssbtopv[i] = wb_remsets[i]->ssb_top;
    wb_ssblimv[i] = wb_remsets[i]->ssb_lim;
  }
#else
  panic( "wb_sync_ssbs" );
#endif
}


/* Synchronize the remembered sets with values from the barrier tables. */

void
wb_sync_remsets( void )
{
#if 0
  int i;

  debug2msg( "   *** sync_remsets" );

  for ( i = 1 ; i < wb_generations ; i++ )
    wb_remsets[i]->ssb_top = wb_ssbtopv[i];
#else
  panic( "wb_sync_remsets: obsolete" );
#endif
}


/* Compact the SSB. */

void
wb_compact( int gen )
{
#if 0
  assert( gen > 0 );
  wb_remsets[gen]->compact( wb_remsets[gen] );
#else
  panic_abort( "wb_compact: obsolete" );
#endif
}


/* Return the remset pointer tables -- useful for the non-predictive gc. */

void
wb_remset_ptrs( word ***top, word ***lim )
{
#if 0
  *top = wb_ssbtopv;
  *lim = wb_ssblimv;
#else
  panic( "wb_remset_ptrs: obsolete" );
#endif
}

/* eof */
