/* Rts/Sys/barrier.c
 * Larceny run-time system -- write barrier for new collector
 *
 * January 17, 1997
 *
 * The code in this file sets things up for the millicode write barrier: 
 * the values of pagebase, genv, ssbtopv, and ssblimv.  See the code 
 * for the millicode write barrier in Rts/Sparc/barrier.s for a deeper
 * understanding.
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

static remset_t **wb_remsets;  /* [0..n-1] where [0] has unspecified value */
static word **wb_ssbtopv;      /* ditto */
static word **wb_ssblimv;      /* ditto */
static int wb_generations;     /* the value 'n' */

void wb_setup( remset_t **remsets, /* one remset per generation, except [0] */
	       unsigned *genv,     /* maps page number to generation number */
	       unsigned pagebase,  /* address of lowest page in arena: fixed */
	       int generations,    /* the value 'n': fixed */
               word *globals       /* the globals vector */
             )
{
  wb_remsets = remsets;
  wb_generations = generations;

  assert( generations > 1 );

 again:
  wb_ssbtopv = (word**)malloc( generations*sizeof( word* ) );
  wb_ssblimv = (word**)malloc( generations*sizeof( word* ) );
  if (wb_ssbtopv == 0 || wb_ssblimv == 0) {
    if (wb_ssbtopv) free( wb_ssbtopv );
    if (wb_ssblimv) free( wb_ssblimv );
    memfail( MF_MALLOC, "barrier: can't allocate metadata." );
    goto again;
  }

  globals[ G_SSBTOPV ] = (word)wb_ssbtopv;
  globals[ G_SSBLIMV ] = (word)wb_ssblimv;
  globals[ G_GENV ] = (word)genv;
  globals[ G_PGBASE ] = (word)pagebase;
  wb_ssbtopv[0] = 0;
  wb_ssblimv[0] = 0;
  wb_sync_ssbs();
}


/* Synchronize the barrier tables with values from the remembered sets. */

void
wb_sync_ssbs( void )
{
  int i;

  /* ssblimv[] must be reset every time because remembered sets may have
   * been shuffled, esp. by the non-predictive collector. 
   */
  for ( i = 1 ; i < wb_generations ; i++ ) {
    wb_ssbtopv[i] = wb_remsets[i]->ssb_top;
    wb_ssblimv[i] = wb_remsets[i]->ssb_lim;
  }
}


/* Synchronize the remembered sets with values from the barrier tables. */

void
wb_sync_remsets( void )
{
  int i;

  for ( i = 1 ; i < wb_generations ; i++ )
    wb_remsets[i]->ssb_top = wb_ssbtopv[i];
}


/* Compact the SSB. */

void
wb_compact( int gen )
{
  assert( gen > 0 );
  wb_remsets[gen]->compact( wb_remsets[gen] );
}

/* eof */
