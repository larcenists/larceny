/* Rts/Sys/barrier.h
 * Larceny run-time system -- write barrier interface.
 *
 * $Id: barrier.h,v 1.5 1997/05/15 00:58:49 lth Exp lth $
 *
 * See Rts/Sys/barrier.c and Rts/Sparc/barrier.s for more information.
 */

#ifndef INCLUDED_BARRIER_H
#define INCLUDED_BARRIER_H

#ifndef INCLUDED_GC_INTERFACE_H
typedef struct young_heap young_heap_t;
typedef struct old_heap old_heap_t;
typedef struct remset remset_t;
typedef struct remset_stats remset_stats_t;
#endif

/* Initialize the write barrier for a generational system. */

void wb_setup( unsigned *genv,      /* map from page to generation */
	       unsigned pagebase,   /* fixed: address of lowest page */
	       int generations,     /* fixed: number of generations */
	       word *globals,       /* fixed: globals vector */
	       word **ssbtopv,      /* fixed: SSB top pointers */
	       word **ssblimv,      /* fixed: SSB lim pointers */
	       int  np_young_gen,
	       int  np_ssbidx
	      );


/* Initialize the write barrier for a non-generational system 
 * (also no static area.)
 */

void wb_setup0( void );

/* If the descriptor tables change, notify the barrier */

void wb_re_setup( unsigned *genv );


/* Syncronize the write barrier internal tables with values from the
 * remembered sets; the write barrier tables are in sync with the 
 * remembered sets only after this operation.
 */
void wb_sync_ssbs( void );


/* Synchronize the remembered set internal variables with values from 
 * the write barrier tables; the remembered set values are in sync with
 * the write barrier only after this operation.
 */
void wb_sync_remsets( void );


/* Compact the SSB (sequential store buffer) for the given generation. */

void wb_compact( int gen );


/* Return the SSB top and limit pointers for all remsets */

void wb_remset_ptrs( word ***top, word ***lim );


/* Disable the write barrier millicode code. (Rts/Sparc/barrier.s) */

void wb_disable( void );

#endif
/* eof */
