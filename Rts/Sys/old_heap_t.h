/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- old heap data structure.
 *
 * An `old_heap_t' represents an "old" heap -- an older generation in a
 * generational garbage collector.  Although objects can be allocated
 * directly in an old heap, most objects are promoted into old heaps from
 * younger heaps.
 */

#ifndef INCLUDED_OLD_HEAP_T_H
#define INCLUDED_OLD_HEAP_T_H

#include "larceny-types.h"

struct old_heap {
  gc_t *collector;
    /* The garbage collector that controls this heap.
       */

  char *id;
    /* A human-comprehensible string identifying the heap and its strategy;
       used by the startup banner.
       */

  word code;
    /* A numeric code identifying the heap, used by the heap dumper/loader.
       */

  int maximum;
     /* The maximum storage that can be allocated in this generation.
	This field is valid following a call to before_collection().
	*/

  int allocated;
     /* The amount of storage currently allocated in this generation.
	This field is valid following a call to before_collection().
	*/

  void *data;
     /* Data private to the heap implementation.
	*/

  int  (*initialize)( old_heap_t *heap );
    /* A method that finishes initialization, after all heaps have
       been allocated.
       */
  
  void (*collect)( old_heap_t *heap, gc_type_t request );
     /* A method that requests a garbage collection in the heap.
	Request is the type of GC requested.
	*/

  void (*collect_with_selective_fromspace)( old_heap_t *heap, int *fromspaces );
    /* Takes an array of generation numbers to consider as part of fromspace
       during the collection, and promotes these into oldspace (or collects 
       oldspace, promoting them in the process).  
     
       There is a write barrier during GC: any promoted object that points to
       any area not in fromspace must be added to the dynamic area's remembered
       set.
       */

  void (*before_collection)( old_heap_t *heap );
     /* A method that is called before any garbage collection.
	*/

  void (*after_collection)( old_heap_t *heap );
     /* A method that is called after any garbage collection.
	*/

  void (*stats)( old_heap_t *heap, int generation, heap_stats_t *stats );
     /* A method that fills in the stats structure for the heap.

	When called at the beginning of a collection, the before_collection()
	method will not yet have been called, and when called at the end of
	a collection, the after_collection() method will have been called
	first.

	Invariant: generation denotes a generation managed by the heap.
	*/

  word *(*data_load_area)( old_heap_t *heap, int nbytes );
     /* A method that allocates a block of at least `nbytes'
	consecutive bytes, suitable for loading data into.  A pointer
	to the first word of the block is returned.  No garbage 
	collection will be triggered.

	nbytes > 0
	*/

  int  (*load_prepare)( old_heap_t *heap, metadata_block_t *m, 
		        heapio_t *h, word **lo, word **hi );
     /* UNDOCUMENTED: Used by the future heap dumper.
        */

  int  (*load_data)( old_heap_t *heap, metadata_block_t *m, heapio_t *h );
     /* UNDOCUMENTED: Used by the future heap dumper.
        */

  void (*set_policy)( old_heap_t *heap, int op, int value );
     /* A method that gives external agents control over GC policy.
        */
};

/* The initialize and set_policy arguments may be NULL. */

old_heap_t *create_old_heap_t(
  char *id,
  word code,
  int  (*initialize)( old_heap_t *heap ), 
  void (*collect)( old_heap_t *heap, gc_type_t request ),
  void (*collect_with_selective_fromspace)( old_heap_t *heap, int *fromspaces ),
  void (*before_collection)( old_heap_t *heap ),
  void (*after_collection)( old_heap_t *heap ),
  void (*stats)( old_heap_t *heap, int generation, heap_stats_t *stats ),
  word *(*data_load_area)( old_heap_t *heap, int nbytes ),
  int  (*load_prepare)( old_heap_t *heap, metadata_block_t *m, 
		        heapio_t *h, word **lo, word **hi ),
  int  (*load_data)( old_heap_t *heap, metadata_block_t *m, heapio_t *h ),
  void (*set_policy)( old_heap_t *heap, int op, int value ),
  void *data
);

#define oh_initialize( oh )        ((oh)->initialize( oh ))
#define oh_collect( oh,r )         ((oh)->collect( oh,r ))
#define oh_collect_with_selective_fromspace( oh, fs ) \
  ((oh)->collect_with_selective_fromspace( oh, fs ))
#define oh_before_collection( oh ) ((oh)->before_collection( oh ))
#define oh_after_collection( oh )  ((oh)->after_collection( oh ))
#define oh_stats( oh, gen, s )     ((oh)->stats( oh, gen, s ))
#define oh_data_load_area( oh, n ) ((oh)->data_load_area( oh, n ))
#define oh_set_policy( oh, x, y )  ((oh)->set_policy( oh, x, y ))

#endif  /* INCLUDED_OLD_HEAP_T_H */

/* eof */
