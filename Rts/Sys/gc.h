/* Rts/Sys/gc.h
 * Larceny run-time system -- garbage collector interface (public)
 *
 * $Id: gc.h,v 1.9 1997/02/27 16:40:26 lth Exp $
 *
 * The procedure create_gc() returns a new garbage collector that manages
 * some number of heap areas.  It is implemented in "memmgr.c".
 *
 * The structure gc_t encapsulates a garbage collector.
 *  - id is a string that identifies the collector.
 *  - initialize() performs post-creation initialization of the collector.
 *  - collect() initiates a garbage collection.
 *  - allocate() allocates memory from the garbage-collected heap.
 *  - data_load_area() returns a pointer to a load area for a heap image
 *    (both "actual" data and compiled code) and sets the heap limit for
 *    that area.
 *  - text_load_area() returns a pointer to a load area for compiled code
 *    and read-only strings _only_, and sets the heap limit for that area.
 *  - iflush() returns 1 if the icache should be flushed for the generation.
 */

#ifndef INCLUDED_GC_H
#define INCLUDED_GC_H

typedef struct gc gc_t;
typedef struct heap_stats heap_stats_t;
typedef struct old_param old_param_t;
typedef enum { GC_COLLECT, GC_PROMOTE } gc_type_t;

gc_t *
create_gc( unsigned esize_bytes,
	   unsigned ewatermark_percent,
	   unsigned ssize_bytes,
	   unsigned rhash,
	   unsigned ssb,
	   unsigned generations,
	   old_param_t *old_gen_info,
	   int np_gc,
	   unsigned np_steps,
	   unsigned np_stepsize_bytes,
	   word *globals,
	   int *actual_generations /* OUT */
	  );

struct gc { 
  char *id;
  int  (*initialize)( gc_t *gc );

  /* Memory management */
  void (*collect)( gc_t *gc, int gen, gc_type_t type, unsigned request_bytes);
  word *(*allocate)( gc_t *gc, unsigned request_bytes );

  /* Heap loading */
  word *(*data_load_area)( gc_t *gc, unsigned bytes_needed );
  word *(*text_load_area)( gc_t *gc, unsigned bytes_needed );

  /* Continuation management */
  word (*creg_get)( gc_t *gc );
  void (*creg_set)( gc_t *gc, word continuation );
  void (*stack_underflow)( gc_t *gc );

  /* Cache flushing -- returns 1 if cache needs flushing. */
  int  (*iflush)( gc_t *gc, int generation );

  /* Statistics */
  void (*stats)( gc_t *gc, int generation, heap_stats_t *stats );

  /* Remembered sets */
  int  (*compact_all_ssbs)( gc_t *gc );
  void (*clear_remset)( gc_t *gc, int generation );

  /* Support for simulated write barrier */
  int (*isremembered)( gc_t *gc, word w );

  /* PRIVATE */
  /* Internal to the collector implementation. */
  void (*promote_out_of)( gc_t *gc, int generation );
  void (*enumerate_roots)( gc_t *gc, void (*f)( word*, void *), void * );
  void (*enumerate_remsets_older_than)( gc_t *gc,
				        int generation,
				        int (*f)(word, void*, unsigned * ),
				        void *);
  void *data;
};


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

  /* "Since last call" entries */
  unsigned frames_flushed;  /* stack frames flushed, or 0 */
  unsigned frames_restored; /* Stack frames restored, or 0 */
  unsigned bytes_flushed;   /* bytes of stack flushed/copied, or 0 */
  unsigned ssb_recorded;    /* SSB entries recorded */
  unsigned hash_recorded;   /* Hash table entries recorded */
  unsigned hash_removed;    /* Hash table entries removed */
  unsigned hash_scanned;    /* Hash table entries scanned */
  unsigned words_scanned;   /* Old object space words scanned */
  unsigned stacks_created;  /* Stacks created */
};


struct old_param {
  unsigned size;         /* bytes */
  unsigned hiwatermark;  /* percent */
  unsigned lowatermark;  /* percent */
};

#endif

/* eof */
