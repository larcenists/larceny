/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Public garbage collector interface
 */

#ifndef INCLUDED_GC_H
#define INCLUDED_GC_H

#include "larceny-types.h"

#define KILOBYTE                     1024
#define MEGABYTE                     (KILOBYTE*KILOBYTE)

#define MAX_HEAPS                    128               /* why not? */
#define MAX_GENERATIONS              128               /* a better name */
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
  int dynamic_min;              /* 0 or lower bound on expandable area */
  int dynamic_max;		/* 0 or upper bound on expandable area */
};

struct np_info {		/* Non-predictive dynamic area */
  int    steps;			/* number of steps, > 0 */
  int    stepsize;		/* size of a step in bytes, > 0 */
  int    size_bytes;            /* total size */
  int    dynamic_min;           /* 0 or lower bound on expandable area */
  int    dynamic_max;		/* 0 or upper bound on expandable area */
  int    extra_remset_limit;    /* 0 .. INT_MAX */
  double load_factor;		/* Inverse load factor */
  double luck;			/* 0.0 .. 1.0 (ought to have been 0 .. 6) */
  double phase_detection;	/* -1.0 or 0.0 .. 1.0 */
};

struct bdw_info {
  int divisor;			/* Allocation divisor */
  double load_factor;		/* inverse load factor */
  double expansion_factor;	/* inverse expansion factor */
  int dynamic_min;		/* 0 or lower bound on collected area */
  int dynamic_max;		/* 0 or upper bound on collected area */
};

struct dof_info {               /* Deferred-oldest-first intermediate area */
  int size_bytes;               /* Size of area in bytes, > 0 */
  int steps;			/* Number of steps, > 0 */
  double load_factor;		/* Inverse load factor */
  int dynamic_min;		/* 0 or lower bound on collected area */
  int dynamic_max;		/* 0 or upper bound on collected area */
};

struct gc_param {		/* Parameter structure passed to create_gc() */
  /* Overall flags to select the mode */
  bool is_conservative_system;
  bool is_generational_system;
  bool is_stopcopy_system;
  bool use_static_area;		       /* In the nonconservative systems */
  bool use_non_predictive_collector;   /* In the generational system */
  bool use_dof_collector;              /* In the generational system */
  bool use_incremental_bdw_collector;  /* In the conservative system */
  bool dont_shrink_heap;	       /* In the nonconservative systems */

  /* Common parameters */
  word *globals;              /* globals table used by collector */

  /* Generational precise system */
  nursery_info_t nursery_info;
  int            ephemeral_area_count;	/* Number of ephemeral areas */
  sc_info_t      *ephemeral_info;	/* an array of these */
  dof_info_t     dynamic_dof_info;
  sc_info_t      dynamic_sc_info;
  np_info_t      dynamic_np_info;

  /* Stop-and-copy precise system */
  sc_info_t sc_info;

  /* Conservative system */
  bdw_info_t bdw_info;

  /* Remembered-set values (could be set-by-set; are global) */
  unsigned rhash;             /* # elements in each remset hash tbl */
  unsigned ssb;               /* # elements in each remset SSB */
};

/* In memmgr.c */
gc_t *create_gc( gc_param_t *params, /* OUT */ int *actual_generations );
gc_t *create_bdw_gc( gc_param_t *params, /* OUT */ int *actual_generations );

#endif

/* eof */
