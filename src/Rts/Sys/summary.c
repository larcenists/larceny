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

#include "summary_t.h"

void summary_init( summary_t *summary, 
                   int entries, 
                   bool (*next_chunk)( summary_t *this, 
                                       word **start,
                                       word **lim,
                                       bool *duplicate_entries ) )
{
  summary_init_dispose( summary, entries, next_chunk, NULL );
}

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *duplicate_entries ), 
                           void (*dispose)( summary_t *this ) )
{
  summary->entries = entries; 
  summary->next_chunk = next_chunk;
  summary->dispose = dispose;
  summary->cursor1 = summary->cursor2 = 
    summary->cursor3 = summary->cursor4 = NULL;
  summary->icursor1 = summary->icursor2 = 
    summary->icursor3 = summary->icursor4 = 0;
}
