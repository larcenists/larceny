/* Rts/Sys/gc.h
 * Larceny run-time system -- garbage collector interface (public)
 *
 * $Id: gc.h,v 1.14 1997/09/17 15:17:26 lth Exp $
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

#include "semispace.h"   /* Sigh.  gc_t needs it. */
#include "heapio.h"      /* Ditto */

/* Information about each heap */

typedef struct heap_info heap_info_t;
struct heap_info {
  int size_bytes;           /* requested heap size in bytes */
  int hi_mark;              /* expansion watermark, % of size */
  int lo_mark;              /* contraction watermark, % of size */
  int oflo_mark;            /* overflow (promotion) watermark, % of size */
};

/* A structure that is used to pass parameters to the collector */

typedef struct gc_param gc_param_t;
struct gc_param {
  int use_static_heap;        /* 1 if static heap is to be used */
  int use_np_heap;            /* 1 if non-predictive heap is to be used */
  int heaps;                  /* number of heaps to use (not counting static)*/

  int bdw_incremental;        /* 1 if incremental conservative GC */

  word *globals;              /* globals table used by collector */

  unsigned   disable_contraction;  /* 1 to disable contraction */
  unsigned   disable_nursery;      /* 1 to disable "promote-always" */

  /* For non-static heap and remembered-set information, a value of 0 
   * means "default" 
   */

  /* Information about non-static heaps */
  heap_info_t *heap_info;

  /* If the last heap is nonpredictive, then these values take precedence
   * over size_bytes, and oflo_mark is ignored.
   */
  unsigned np_steps;          /* number of steps */
  unsigned np_stepsize;       /* size of a step (bytes) */
  
  /* Remembered-set values (could be set-by-set; are global) */
  unsigned rhash;             /* # elements in each remset hash tbl */
  unsigned ssb;               /* # elements in each remset SSB */
  
  /* Static area information */
  unsigned static_size;       /* size of sspace (bytes) */
};

typedef struct gc gc_t;
typedef struct heap_stats heap_stats_t;
typedef struct old_param old_param_t;
typedef enum { GC_COLLECT, GC_PROMOTE } gc_type_t;

gc_t *
create_gc( gc_param_t *params, int *actual_generations /* OUT */ );
gc_t *
create_bdw_gc( gc_param_t *params, int *actual_generations /* OUT */ );

struct gc { 
  char *id;
  int  (*initialize)( gc_t *gc );

  /* Memory management */
  void (*collect)( gc_t *gc, int gen, gc_type_t type, unsigned request_bytes);
  word *(*allocate)( gc_t *gc, unsigned nbytes );
  word *(*allocate_nonmoving)( gc_t *gc, unsigned nbytes );

  /* Policy control */
  void (*set_policy)( gc_t *gc, int heap, int rator, unsigned rand );

  /* Heap loading */
  word *(*data_load_area)( gc_t *gc, unsigned bytes_needed );
  word *(*text_load_area)( gc_t *gc, unsigned bytes_needed );

  /* Continuation management */
  word (*creg_get)( gc_t *gc );
  void (*creg_set)( gc_t *gc, word continuation );
  void (*stack_underflow)( gc_t *gc );
  void (*stack_overflow)( gc_t *gc );

  /* Cache flushing -- returns 1 if cache needs flushing. */
  int  (*iflush)( gc_t *gc, int generation );

  /* Statistics */
  void (*stats)( gc_t *gc, int generation, heap_stats_t *stats );

  /* Remembered sets */
  int  (*compact_all_ssbs)( gc_t *gc );
  void (*clear_remset)( gc_t *gc, int generation );

  /* Support for simulated write barrier */
  int (*isremembered)( gc_t *gc, word w );
  
  /* Support for non-predictive collector */
  void (*compact_np_ssb)( gc_t *gc );
  void (*clear_np_remset)( gc_t *gc );
  void (*np_remset_ptrs)( gc_t *gc, word ***ssbtop, word ***ssblim );
  void (*set_np_collection_flag)( gc_t *gc );
  void (*np_merge_and_clear_remset)( gc_t *gc, int gen );

  /* Support for static heap. */
  void (*reorganize_static)( gc_t *gc, semispace_t **data, semispace_t **text);
  int  (*load_heap)( gc_t *gc, heapio_t *h );

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
  unsigned target;          /* semispace target size (policy) */

  /* "Since last call" entries */
  unsigned copied_last_gc;  /* bytes copied during last GC */
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
#if NP_EXTRA_REMSET
  /* "Since last call" entries */
  unsigned np_ssb_recorded;    /* SSB entries recorded */
  unsigned np_hash_recorded;   /* Hash table entries recorded */
  unsigned np_hash_removed;    /* Hash table entries removed */
  unsigned np_hash_scanned;    /* Hash table entries scanned */
  unsigned np_words_scanned;   /* Old object space words scanned */
#endif
};


struct old_param {
  unsigned size;         /* bytes */
  unsigned hiwatermark;  /* percent */
  unsigned lowatermark;  /* percent */
};


/* In "Rts/Sys/util.c" */
extern word copy_object( gc_t *gc, word obj );


#endif

/* eof */
