/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- Opaque type declarations.
 */

#ifndef INCLUDED_LARCENY_TYPES
#define INCLUDED_LARCENY_TYPES

/* Scalar data types. */
typedef unsigned word;
typedef int s_word;
typedef unsigned char byte;
typedef enum { FALSE, TRUE } bool;
typedef enum { GCTYPE_COLLECT, 
	         /* A collection in an area that may also promote in
                    data from other areas. */
               GCTYPE_PROMOTE,
                 /* A promotion into an area where the target area is
                    not itself being collected, only scanned. */
               GCTYPE_FULL,
	         /* A collection that traces every live object in the
                    system.  Used for special purposes only, like the 
                    global marking collections in the DOF collector.  
		    */
              }  gc_type_t;

/* gc_t is elaborated in gc_t.h */
typedef struct gc gc_t;

/* young_heap_t is elaborated in young_heap_t.h */
typedef struct young_heap young_heap_t;

/* old_heap_t is elaborated in old_heap_t.h */
typedef struct old_heap old_heap_t;

/* static_heap_t is elaborated in static_heap_t.h */
typedef struct static_heap static_heap_t;

/* remset_t and remset_stats_t are elaborated in remset_t.h */
typedef struct remset remset_t;
typedef struct remset_stats remset_stats_t;

/* semispace_t and ss_chunk_t are elaborated in semispace_t.h */
typedef struct semispace semispace_t;
typedef struct ss_chunk ss_chunk_t;

/* heapio_t and metadata_block_t are elaborated in heapio.h */
typedef struct heapio_t heapio_t;
typedef struct metadata_block_t metadata_block_t;

/* Currently these are in gc.h but should perhaps move? */
typedef struct np_info np_info_t;
typedef struct sc_info sc_info_t;
typedef struct bdw_info bdw_info_t;
typedef struct dof_info dof_info_t;
typedef struct nursery_info nursery_info_t;
typedef struct gc_param gc_param_t;
typedef struct old_param old_param_t;

/* heap_stats_t.h */
typedef struct heap_stats heap_stats_t;
typedef struct dof_stats dof_stats_t;

/* los_t.h */
typedef struct los los_t;

#endif /* INCLUDED_LARCENY_TYPES */
