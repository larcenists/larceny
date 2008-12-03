/* Copyright 2007 Felix S Klock.
 *
 * $Id$
 * 
 * Sequential store buffer (SSB) interface.
 * 
 * A array of words is so simple that it almost does not deserve an
 * interface.  But Felix prefers to separate the SSB from the remset
 * (they used to be very tightly coupled together).
 * 
 */
#ifndef INCLUDED_SEQBUF_T_H
#define INCLUDED_SEQBUF_T_H

#include "config.h"
#include "larceny-types.h"

struct seqbuf {

  /* For the write barrier. */
  word **bot;		/* Location of pointer to start of SSB */
  word **top;		/* Location of pointer to next free word in SSB */
  word **lim;		/* Location of pointer past end of SSB */

  void *data;		/* Implementation's data */
};

typedef int (*seqbuf_processor)(gc_t *gc, word *bot, word *top, void *sp_data);

/* Constructs a sequential store buffer.
 * 
 * The bot_loc, top_loc, and lim_loc parameters are memory cells that
 * we use as a shared channel with the write barrier.
 * 
 * The sp parameter is the entry processor for the SSB; it is invoked
 * periodically and is expected to do something with all of the words
 * in the range [bot,top), since the SSB will be reset (making it
 * empty) after each invocation of the entry processor.
 */
seqbuf_t *
create_seqbuf( int num_entries, /* Number of entries in SSB */
	       word **bot_loc,  /* Location of pointer to start of SSB */
	       word **top_loc,  /* Location of pointer to next free of SSB*/
	       word **lim_loc,  /* Location of pointer past end of SSB */
	       seqbuf_processor sp,
	       void *sp_data );

/* Invokes entry_processor callback of the ssb, clears ssb, and
 * returns the entry processor's returned value.
 */
int process_seqbuf( gc_t *gc, seqbuf_t *ssb );

/* Is the ssb clear? */
bool seqbuf_clearp( seqbuf_t *ssb );

/* Switches in a new set of locations to link with the SSB log.
 */
void seqbuf_swap_in_ssb( seqbuf_t *ssb, 
                         word **bot_loc, word **top_loc, word **lim_loc );

/* Switches in a new sp_data to pass along to the seqbuf_processor. 
 * Returns previous data value. */
void* seqbuf_set_sp_data( seqbuf_t *ssb, void *sp_data );

#define SSB_ENQUEUE( gc, ssb, word )        \
  do { seqbuf_t *ssb_tmp = (ssb);           \
       **(ssb_tmp->top) = (word);           \
       (*ssb_tmp->top)++;		    \
       if (*ssb_tmp->top == *ssb_tmp->lim)  \
         process_seqbuf( (gc), ssb_tmp );   \
  } while (0);

#endif /* INCLUDED_SEQBUF_T_H */

/* eof */
