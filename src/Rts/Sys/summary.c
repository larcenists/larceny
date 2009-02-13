/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
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
  summary_init_dispose( summary, entries, next_chunk, NULL, NULL );
}

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *all_unseen_before ), 
                           void (*dispose)( summary_t *this ),
                           bool (*filter)( summary_t *this, word w ) )
{
  summary->entries = entries; 
  summary->composed_summary = FALSE;
  summary->next_chunk = my_next_chunk_wrapper;
  summary->next_chunk_with_flags = next_chunk;
  summary->dispose = dispose;
  summary->filter = filter;
  summary->cursor1 = summary->cursor2 = 
    summary->cursor3 = summary->cursor4 = NULL;
  summary->icursor1 = summary->icursor2 = 
    summary->icursor3 = summary->icursor4 = 0;
}

static
void summary_enumerate_composed( summary_t *summary,
                                 void (*scanner)(word loc, 
                                                 void *data, 
                                                 unsigned *stats),
                                 void *data )
{
  if (summary->cursor1 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor1, scanner, data );
  if (summary->cursor2 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor2, scanner, data );
  if (summary->cursor3 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor3, scanner, data );
  if (summary->cursor4 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor4, scanner, data );
}

static 
void summary_enumerate_dispatch( summary_t *summary,
                                 void (*scanner)(word loc, 
                                                 void *data, 
                                                 unsigned *stats),
                                 void *data )
{
  word *p, *q;
  unsigned word_count = 0;
  while( summary_next_chunk( summary, &p, &q ) ) {
    while (p < q) {
      if (*p != 0) {
        if (summary->filter != NULL && 
            ! summary->filter( summary, *p )) {
          p++;
          continue;
        }
        scanner( *p, data, &word_count );
      }
      p++;
    }
  }
}

void summary_enumerate( summary_t *summary,
                        void (*scanner)(word loc, void *data, unsigned *stats),
                        void *data )
{
  if (summary->composed_summary)
    summary_enumerate_composed( summary, scanner, data );
  else
    summary_enumerate_dispatch( summary, scanner, data );
}

static bool next_chunk_compose( summary_t *this,
                                word **start, word **lim,
                                bool *all_unseen_before )
{
 retry:
  if (this->cursor1 == NULL) {
    return FALSE;
  } else if (summary_next_chunk( (summary_t*)this->cursor1, start, lim )) {
    return TRUE;
  } else {
    this->cursor1 = this->cursor2;
    this->cursor2 = this->cursor3;
    this->cursor3 = this->cursor4;
    this->cursor4 = NULL;
    goto retry;
  }
}

void summary_compose( summary_t *fst, summary_t *snd, summary_t *thd, summary_t *fth, 
                      summary_t *recv )
{
  int entries = ((fst==NULL)?0:fst->entries) + ((snd==NULL)?0:snd->entries) + 
    ((thd==NULL)?0:thd->entries) + ((fth==NULL)?0:fth->entries);

  recv->entries = entries;
  recv->composed_summary = TRUE;
  recv->next_chunk = NULL;
  recv->next_chunk_with_flags = NULL;
  recv->dispose = NULL;
  recv->filter = NULL;

  recv->icursor1 = recv->icursor2 = recv->icursor3 = recv->icursor4 = 0;

  recv->cursor1 = fst;
  recv->cursor2 = snd;
  recv->cursor3 = thd;
  recv->cursor4 = fth;
}

/* eof */
