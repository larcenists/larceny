/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- heap statistics structure definition
 */

#ifndef INCLUDED_HEAP_STATS_T_H
#define INCLUED_HEAP_STATS_T_H

#include "remset_t.h"		/* For remset_stats_t */

/* Heap statistics are returned from the collector by calling the stats()
 * member function with a generation number.  The returned structure contains
 * data from two broad categories: snapshot state and accumulated data.
 * The snapshot state says something about the dynamic state of the system:
 * the amount of memory allocated.  The accumulated data are counters that
 * are _cleared_ internally every time statistics are obtained, so at each
 * call the difference of the counters with respect to the previous call
 * is obtained.
 *
 * Not all entries are meaningful for all heaps, and will be set to 0
 * if that is the case.  For example, only young heaps have stacks and
 * only old heaps can have remembered sets.
 *
 * The statistics are collected on a per-generation basis.
 */

struct heap_stats {
  /* Snapshot entries */
  unsigned live;             /* heap bytes live (not including stack cache) */
  unsigned stack;            /* stack bytes live, or 0 */
  unsigned semispace1;       /* bytes allocated to semispace1 */
  unsigned semispace2;       /* bytes allocated to semispace2, or 0 */
  unsigned target;           /* semispace target size (policy) */

  /* "Since last call" entries */
  unsigned copied_last_gc;   /* bytes copied during last GC */
  unsigned moved_last_gc;    /* bytes of large objects moved during last GC */

  unsigned stacks_created;
  unsigned frames_flushed;   /* stack frames flushed, or 0 */
  unsigned frames_restored;  /* Stack frames restored, or 0 */
  unsigned bytes_flushed;    /* bytes of stack flushed/copied, or 0 */

  remset_stats_t remset_data;

  /* Special entries for non-predictive collector */
  unsigned np_k;            /* Number of steps */
  unsigned np_j;            /* Number of steps in 'young' generation */
  unsigned np_young;        /* 1 iff NP young generation */
  unsigned np_old;          /* 1 iff NP old generation */

  /* "Since last call" entries */
  remset_stats_t np_remset_data;
};

#endif

/* eof */

