/* Rts/Sys/gc.h
 * Larceny -- public garbage collector interface
 *
 * $Id: gc.h,v 1.14 1997/09/17 15:17:26 lth Exp $
 *
 * The procedure create_gc() returns a new garbage collector that manages
 * some number of heap areas.
 */

#ifndef INCLUDED_GC_H
#define INCLUDED_GC_H

#include "larceny-types.h"

#define KILOBYTE                     1024
#define MEGABYTE                     (KILOBYTE*KILOBYTE)

#define MAX_HEAPS                    128               /* why not? */
#define DEFAULT_AREAS                3
#define DEFAULT_NURSERY_SIZE         MEGABYTE
#define DEFAULT_STOPCOPY_SIZE        (2*MEGABYTE)
#define DEFAULT_EPHEMERAL_INCREMENT  MEGABYTE
#define DEFAULT_DYNAMIC_INCREMENT    MEGABYTE

#define DEFAULT_STEPS                8
#define DEFAULT_STEPSIZE             (256*KILOBYTE)
#define DEFAULT_LOAD_FACTOR          3.0

struct nursery_info {		/* Generational gc nursery */
  int size_bytes;		/* size of area in bytes, > 0 */
};

struct sc_info {		/* Any two-space copying area */
  int size_bytes;		/* size of area in bytes, > 0 */
  double load_factor;		/* inverse load factor (dynamic generation) */
  int dynamic_max;		/* Upper bound on dynamic area size */
};

struct np_info {		/* Non-predictive dynamic area */
  int steps;			/* number of steps, > 0 */
  int stepsize;			/* size of a step in bytes, > 0 */
  int size_bytes;               /* total size */
  double load_factor;		/* inverse load factor */
  int dynamic_max;		/* Upper bound on dynamic area size */
};

struct gc_param {		/* Parameter structure passed to create_gc() */
  /* Overall flags to select the mode */
  bool is_conservative_system;
  bool is_generational_system;
  bool is_stopcopy_system;
  bool use_static_area;		       /* In the nonconservative systems */
  bool use_non_predictive_collector;   /* In the generational system */
  bool use_incremental_bdw_collector;  /* In the conservative system */

  /* Common parameters */
  word *globals;              /* globals table used by collector */

  /* Generational precise system */
  nursery_info_t nursery_info;
  int ephemeral_area_count;	/* Number of ephemeral areas */
  sc_info_t *ephemeral_info;	/* an array of these */
  sc_info_t dynamic_sc_info;
  np_info_t dynamic_np_info;

  /* Stop-and-copy precise system */
  sc_info_t sc_info;

  /* Remembered-set values (could be set-by-set; are global) */
  unsigned rhash;             /* # elements in each remset hash tbl */
  unsigned ssb;               /* # elements in each remset SSB */
};


/* In memmgr.c */
gc_t *create_gc( gc_param_t *params, int *generations_created );
int gc_compute_dynamic_size( int D, int S, int Q, double L, int limit );


/* In util.c */
extern word copy_object( gc_t *gc, word obj );


/* In sro.c */
extern word sro( gc_t *gc, word *globals, int p_tag, int h_tag, int limit );

#endif

/* eof */
