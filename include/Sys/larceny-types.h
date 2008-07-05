/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- Opaque type declarations.
 */

#ifndef INCLUDED_LARCENY_TYPES
#define INCLUDED_LARCENY_TYPES

#include "config.h"

/* Scalar data types. */
typedef unsigned int word;
typedef int s_word;
typedef unsigned char byte;
#if defined __bool_true_false_are_defined /* C99 <stdbool.h> */
#define TRUE true
#define FALSE false
#else
#if defined TRUE
# undef FALSE
# define FALSE 0
# undef TRUE
# define TRUE 1
# define bool int
#else
typedef enum { FALSE, TRUE } bool;
#endif
#endif
typedef enum { GCTYPE_COLLECT, 
	         /* A collection in an area that may also promote in
                    data from other areas. */
               GCTYPE_PROMOTE,
                 /* A promotion into an area where the target area is
                    not itself being collected, only scanned. */
	       GCTYPE_EVACUATE,
                 /* A promotion of objects out of the argument area.
                    The target area is left unspecified, to be decided
                    internally by the collector. */
               GCTYPE_FULL,
	         /* A collection that traces every live object in the
                    system.  Used for special purposes only, like the 
                    global marking collections in the DOF collector.  
		    */
              }  gc_type_t;
typedef enum { TIMER_ELAPSED, /* Measure elapsed time */
	       TIMER_CPU,     /* Measure process time (eg user+system) */
             } stats_timer_t;

/* gc_t is elaborated in gc_t.h */
typedef struct gc gc_t;

/* young_heap_t is elaborated in young_heap_t.h */
typedef struct young_heap young_heap_t;

/* old_heap_t is elaborated in old_heap_t.h */
typedef struct old_heap old_heap_t;
typedef enum { OHTYPE_EPHEMERAL, OHTYPE_DYNAMIC, OHTYPE_REGIONAL } oh_type_t;

/* static_heap_t is elaborated in static_heap_t.h */
typedef struct static_heap static_heap_t;

/* remset_t is elaborated in remset_t.h */
typedef struct remset remset_t;

/* summ_matrix_t is elaborated in summ_matrix_t.h */
typedef struct summ_matrix summ_matrix_t;

/* seqbuf_t is elaborated in seqbuf_t.h */
typedef struct seqbuf seqbuf_t;

/* semispace_t and ss_chunk_t are elaborated in semispace_t.h */
typedef struct semispace semispace_t;
typedef struct ss_chunk ss_chunk_t;

/* heapio_t and metadata_block_t are elaborated in heapio.h */
typedef struct heapio_t heapio_t;
typedef struct metadata_block_t metadata_block_t;

/* stats.h */
typedef struct gclib_stats gclib_stats_t;
typedef struct gc_stats gc_stats_t;
typedef struct gen_stats gen_stats_t;
typedef struct remset_stats remset_stats_t;
typedef struct stack_stats stack_stats_t;
#if defined(SIMULATE_NEW_BARRIER)
typedef struct swb_stats swb_stats_t;
#endif
typedef struct gc_event_stats gc_event_stats_t;

/* Currently these are in gc.h but should perhaps move? */
typedef struct np_info np_info_t;
typedef struct sc_info sc_info_t;
typedef struct bdw_info bdw_info_t;
typedef struct dof_info dof_info_t;
typedef struct nursery_info nursery_info_t;
typedef struct gc_param gc_param_t;
typedef struct old_param old_param_t;

/* los_t.h */
typedef struct los los_t;

/* Types for compatibility functions defined in util.c */
#if !defined(HAVE_HRTIME_T)
typedef int hrtime_t;
#endif

#endif /* INCLUDED_LARCENY_TYPES */
