/* Copyright 2007 Felix S Klock.
 *
 * $Id$
 * 
 * Sequential store buffer implementation.
 * 
 */

#define GC_INTERNAL

#include "larceny.h"
#include "seqbuf_t.h"
#include "gclib.h"

typedef struct seqbuf_data seqbuf_data_t;

struct seqbuf_data {
  entry_processor ep;
  void* ep_data;
};

#define DATA(ssb)               ((seqbuf_data_t*)(ssb->data))

seqbuf_t *
create_seqbuf( int num_entries, /* Number of entries in SSB */
	       word **bot_loc,  /* Location of pointer to start of SSB */
	       word **top_loc,  /* Location of pointer to next free of SSB*/
	       word **lim_loc,  /* Location of pointer past end of SSB */
	       entry_processor processor,
	       void *ep_data )
{
  seqbuf_t *ssb;
  seqbuf_data_t *ssb_data;
  word *buf;
  
  ssb = (seqbuf_t*)must_malloc( sizeof( seqbuf_t ) );
  ssb_data = (seqbuf_data_t*)must_malloc( sizeof( seqbuf_data_t ) );

  ssb->bot = bot_loc;
  ssb->top = top_loc;
  ssb->lim = lim_loc;
  ssb->data = ssb_data;

  while (1) {
    buf = gclib_alloc_rts( num_entries * sizeof(word), MB_REMSET );
    if (buf != 0)
      break;
    memfail( MF_RTS, "Can't allocate sequential store buffer." );
  }
  *ssb->bot = *ssb->top = buf;
  *ssb->lim = buf + num_entries;

  DATA(ssb)->ep = processor;
  DATA(ssb)->ep_data = ep_data;

  return ssb;
}

int process_seqbuf( seqbuf_t *ssb ) 
{
  int retval;
  
  void *ep_data = DATA(ssb)->ep_data;
  retval = DATA(ssb)->ep( *ssb->bot, *ssb->top, ep_data );
  *ssb->top = *ssb->bot;
  return retval;
}
