/* Rts/Sys/barrier.h
 * Larceny run-time system -- write barrier interface.
 *
 * January 13, 1997
 *
 * See Rts/Sys/barrier.c and Rts/Sparc/barrier.s for more information.
 */

/* Initialize the write barrier */

void wb_setup( remset_t **remsets,  /* one remset per generation */
	       unsigned *genv,      /* map from page to generation */
	       unsigned pagebase,   /* fixed: address of lowest page */
	       int generations,     /* fixed: number of generations */
	       word *globals );     /* fixed: globals vector */


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

/* eof */
