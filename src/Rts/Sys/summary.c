/* Copyright 2008 Felix S Klock II.
 *
 * $Id$
 * 
 * Summary constructor implementations.
 * 
 * (These are somewhat trivial; it might be worthwhile to add some
 *  convenience methods, such as a couple example summary
 *  implementations and/or monolithic summary enumeration function.)
 */ 

#include "larceny.h"
#include "summary_t.h"

static bool my_next_chunk_wrapper( summary_t *this, word **start, word **lim )
{
  bool all_unseen_before; 
  /* (throwing away flag data) */
  return summary_next_chunk_with_flags( this, start, lim, &all_unseen_before );
}

void summary_init( summary_t *summary, 
                   int entries, 
                   bool (*next_chunk)( summary_t *this, 
                                       word **start,
                                       word **lim,
                                       bool *all_unseen_before ) )
{
  summary_init_dispose( summary, entries, next_chunk, NULL );
}

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *all_unseen_before ), 
                           void (*dispose)( summary_t *this ) )
{
  summary->entries = entries; 
  summary->next_chunk = my_next_chunk_wrapper;
  summary->next_chunk_with_flags = next_chunk;
  summary->dispose = dispose;
  summary->cursor1 = summary->cursor2 = 
    summary->cursor3 = summary->cursor4 = NULL;
  summary->icursor1 = summary->icursor2 = 
    summary->icursor3 = summary->icursor4 = 0;
}

void summary_enumerate( summary_t *summary,
                        void (*scanner)(word loc, void *data, unsigned *stats),
                        void *data )
{
  word *p, *q;
  unsigned word_count = 0;
  while( summary_next_chunk( summary, &p, &q ) ) {
    while (p < q) {
      if (*p != 0) {
        scanner( *p, data, &word_count );
      }
      p++;
    }
  }
}

/* eof */
