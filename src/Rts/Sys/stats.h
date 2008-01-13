/* Copyright 1998, 1999 Lars T Hansen
 * 
 * $Id$
 *
 * Stats module -- a moderately fancy adding machine.
 */

#include "config.h"
#include "larceny-types.h"
#if HAVE_HRTIME_T
# include <sys/time.h>
#endif

struct gclib_stats {
  /* Snapshot */
  int heap_allocated;		/* words currently allocated to heap areas */
  int heap_allocated_max;	/* max of heap_allocated over time */
  int heap_limit;		/* 0 or the current heap limit */
  int remset_allocated;		/* words allocated to remembered sets */
  int remset_allocated_max;	/* max of remset_allocated over time */
  int rts_allocated;		/* words allocated to run-time systems */
  int rts_allocated_max;	/* max of rts_allocated over time */
  int heap_fragmentation;	/* words of fragmentation in heap areas */
  int heap_fragmentation_max;	/* max of heap_fragmentation over time */
  int mem_allocated;		/* words of allocation: heap+rts+remset+frag */
  int mem_allocated_max;	/* max of mem_allocated over time */

  int max_remset_scan;
  int max_remset_scan_cpu;
  int total_remset_scan;
  int total_remset_scan_cpu;
  int remset_scan_count;
  int max_entries_remset_scan;
  int total_entries_remset_scan;
};

struct gc_stats {
  /* For accumulation */
  int allocated;		/* Words allocated */
  int reclaimed;		/* Words reclaimed */
  int objects_copied;		/* By copying collector */
  int words_copied;		/* ditto */
  int objects_moved;		/* By large object manager */
  int words_moved;		/* ditto */

  /* NP (ROF) collector */
  word np_k;			/* snapshot: total steps */
  word np_j;			/* snapshot: young steps */

  /* DOF collector */
  int resets;			/* accumulation: number of resets */
  int repeats;			/* accumulation: number of repeats */

  /* Full mark/sweep collections */
  int full_collections;		/* Global mark/sweep collections in DOF gc */
  int full_objects_marked;	/* By mark/sweep collector */
  int full_words_marked;	/* ditto */
  int full_pointers_traced;	/* ditto */
  int full_ms_collection;	/* ditto */
  int full_ms_collection_cpu;	/* ditto */

  int max_ms_collection;        /* Max milliseconds collecting in an area */
  int max_ms_collection_cpu;    /* ditto, CPU time */
};

struct gen_stats {
  /* Snapshot */
  int target;			/* Heap words target size */
  int allocated;		/* Heap words allocated */
  int used;			/* Heap words in use, excluding stack cache */

  /* For accumulation */
  int ms_promotion;		/* Milliseconds doing promotion into area */
  int ms_promotion_cpu;		/* Ditto, CPU time */
  int ms_collection;		/* Milliseconds doing collection in area */
  int ms_collection_cpu;	/* Ditto, CPU time */
  int promotions;		/* Promotions into area */
  int collections;		/* Copying collections in area */
};

struct remset_stats {
  /* Snapshot */
  int allocated;		/* Total words allocated (SSB+table+pool) */
  int used;			/* Words in use */
  int live;			/* Words in use minus currently removed */

  /* For accumulation */
  int ssb_recorded;		/* SSB entries recorded */
  int recorded;			/* Entries recorded */
  int objs_scanned;		/* Entries scanned */
  int words_scanned;		/* Words of objects scanned for pointers */
  int removed;			/* Entries removed */
  int cleared;			/* Number of times set was cleared */
  int scanned;			/* Number of times set was scanned */
  int compacted;		/* Number of times set was compacted */
};

struct stack_stats {
  /* For accumulation */
  int stacks_created;
  int frames_flushed;		/* stack frames flushed */
  int words_flushed;		/* bytes of stack flushed/copied */
  int frames_restored;		/* Stack frames restored */
};

#if defined(SIMULATE_NEW_BARRIER)
struct swb_stats {
  /* For accumulation */
  int total_assignments;	/* Total entries to barrier code */
  int array_assignments;	/* Total assignments to arrays */
  int lhs_young_or_remembered;	/* Filtered because lhs was young/in set */
  int rhs_constant;		/* Filtered because rhs was immediate */
  int cross_gen_check;		/* Filtered because not an intergen ptr */
  int transactions;		/* Transactions that reached SSB */
};
#endif

/* Ad-hoc instrumentation structure */
struct gc_event_stats {
  /* GC_HIRES_TIMERS */
  /* Generic */
  /* hrtime_t is usually a long long. */
  hrtime_t gctime;
  hrtime_t promtime;
  hrtime_t free_unused;
  hrtime_t root_scan_gc;
  hrtime_t root_scan_prom;
  hrtime_t los_sweep_gc;
  hrtime_t los_sweep_prom;
  hrtime_t remset_scan_gc;
  hrtime_t remset_scan_prom;
  hrtime_t tospace_scan_gc;
  hrtime_t tospace_scan_prom;
  /* DOF GC */
  hrtime_t reset_after_gc;
  hrtime_t decrement_after_gc;
  hrtime_t dof_remset_scan;
  hrtime_t sweep_shadow;
  hrtime_t msgc_mark;
  hrtime_t sweep_dof_sets;
  hrtime_t sweep_remset;
  hrtime_t sweep_los;
  hrtime_t assimilate_prom;
  hrtime_t assimilate_gc;

  /* GC_EVENT_COUNTERS */
  int copied_by_gc;             /* One way of counting */
  int moved_by_gc;
  int copied_by_prom;           /* One way of counting */
  int moved_by_prom;
  int words_forwarded;          /* Another way of counting */
  int ptrs_forwarded;
  int gc_barrier_hit;		/* DOF/ROF GC */
  int remset_large_objs_scanned;
  int remset_large_obj_words_scanned;
};

typedef int stats_id_t;		
  /* General purpose identifer type that does not allow one to
     query about the type of what it identifies.
     */


/* C-side interface -- in stats.c */

void stats_init( gc_t *gc );
  /* Initialize the instrumentation package.  
     */

stats_id_t stats_new_generation( int major_id, int minor_id );
  /* Add a generation to the set of generations being maintained and
     associate the identifiers major_id and minor_id with it.
     */

stats_id_t stats_new_remembered_set( int major_id, int minor_id );
  /* Add a remembered set to the set of generations being maintained and
     associate the identifiers major_id and minor_id with it.
     */

stats_id_t stats_start_timer( stats_timer_t t );
  /* Start a new timer of the specified type and return its identifier.
     */

int stats_stop_timer( stats_id_t timer );
  /* Stop the timer and return the accumulated time in milliseconds.
     */

void stats_add_gclib_stats( gclib_stats_t *stats );
  /* Add the gclib info the statistics variables.
     */

void stats_add_gc_stats( gc_stats_t *stats );
  /* Add the gc info to the statistics variables.
     */

void stats_add_gen_stats( stats_id_t generation, gen_stats_t *stats );
  /* Add the generation info to the statistics variables.
     */

void stats_add_remset_stats( stats_id_t remset, remset_stats_t *stats );
  /* Add the remembered set info to the statistics variables.
     */

void stats_add_stack_stats( stack_stats_t *stats );
  /* Add the stack info to the statistics variables.
     */

#if defined(SIMULATE_NEW_BARRIER)
void stats_add_swb_stats( swb_stats_t *stats );
  /* Add the simulated write barrier info to the statistics variables.
     */
#endif

void stats_set_gc_event_stats( gc_event_stats_t *stats );
  /* Set the GC event statistics to the values contained in stats,
     for all nonzero elements in stats. (Note, does _not_ add them.)
     */

/* Scheme-side interface -- in stats.c */

int stats_parameter( int key );
  /* Key = 0: return number of generations
     Key = 1: return number of remembered sets
     */

word stats_fillvector( word w_buffer );
  /* "w_buffer" is a tagged pointer to a Scheme vector of at least 
     STAT_VSIZE elements.  The elements at indices STAT_GENERATIONS
     and STAT_REMSETS must be vectors long enough to hold info
     about generations and remembered sets, respectively.

     STAT_VSIZE, STAT_GENERATIONS, and STAT_REMSETS are all defined
     in globals.cfg.

     Returns w_buffer.
     */


/* Logging -- in dumpstats.c */

bool stats_opendump( const char *filename );
  /* Start logging of GC statistics on the file named by "filename".
     One record will be written following every collection.  It is an 
     error if a GC log is already in progress.

     Returns TRUE if the operation succeeded, FALSE if not.
     */

void stats_closedump( void );
  /* Stop the gc logging and close the file.  It is an error if no GC
     log is in progress.
     */

void stats_dumpstate( void );
  /* If logging is not on, do nothing; otherwise, dump a GC log record
     with the current state of the system counters to the dump file.
     */

void stats_dumpstate_stdout( void );
  /* Whether logging is on or not, dump a GC log record with the current
     state of the system counters to standard output.  May not make sense
     on all platforms; mostly useful for profiling tasks that need to
     avoid heap allocation.
     */

/* eof */
