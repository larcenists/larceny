/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- heap statistics structure definition
 */

#ifndef INCLUDED_HEAP_STATS_T_H
#define INCLUED_HEAP_STATS_T_H

/* Heap statistics are returned from the collector by calling the stats()
 * member function with a generation number.  The returned structure contains
 * data from two broad categories: snapshot state and accumulated data.
 * The snapshot state says something about the dynamic state of the system:
 * the amount of memory allocated.  The accumulated data are counters that
 * are _cleared_ internally every time statistics is obtained, so at each
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
  unsigned live;            /* heap bytes live (not including stack cache) */
  unsigned stack;           /* stack bytes live, or 0 */
  unsigned semispace1;      /* bytes allocated to semispace1 */
  unsigned semispace2;      /* bytes allocated to semispace2, or 0 */
  unsigned target;          /* semispace target size (policy) */

  /* "Since last call" entries */
  unsigned copied_last_gc;  /* bytes copied during last GC */
  unsigned moved_last_gc;   /* bytes of large objects moved during last GC */
  unsigned frames_flushed;  /* stack frames flushed, or 0 */
  unsigned frames_restored; /* Stack frames restored, or 0 */
  unsigned bytes_flushed;   /* bytes of stack flushed/copied, or 0 */
  unsigned ssb_recorded;    /* SSB entries recorded */
  unsigned hash_recorded;   /* Hash table entries recorded */
  unsigned hash_removed;    /* Hash table entries removed */
  unsigned hash_scanned;    /* Hash table entries scanned */
  unsigned words_scanned;   /* Old object space words scanned */
  unsigned stacks_created;  /* Stacks created */

  /* Special entries for non-predictive collector */
  unsigned np_k;            /* Number of steps */
  unsigned np_j;            /* Number of steps in 'young' generation */
  unsigned np_young;        /* 1 iff NP young generation */
  unsigned np_old;          /* 1 iff NP old generation */

  /* "Since last call" entries */
  unsigned np_ssb_recorded;    /* SSB entries recorded */
  unsigned np_hash_recorded;   /* Hash table entries recorded */
  unsigned np_hash_removed;    /* Hash table entries removed */
  unsigned np_hash_scanned;    /* Hash table entries scanned */
  unsigned np_words_scanned;   /* Old object space words scanned */
};

#endif

/* eof */

