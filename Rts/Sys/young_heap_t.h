/* Rts/Sys/young_heap_t.h
 * Larceny run-time system -- young heap data structure.
 * 
 * $Id: young_heap_t.h,v 1.1.1.1 1998/11/19 21:51:48 lth Exp $
 *
 * A `young_heap_t' represents a "young" heap -- the single heap in
 * a stop-and-copy system, or the youngest generation in a generational
 * system.  In a generational system, all objects are allocated in the
 * youngest heap, and the youngest heap is also in charge of managing
 * the memory used for the stack cache.
 */

#ifndef INCLUDED_YOUNG_HEAP_T_H
#define INCLUDED_YOUNG_HEAP_T_H

#include "larceny-types.h"

struct young_heap {
  gc_t *collector;
     /* The garbage collector that owns this heap.
	*/

  char *id;
     /* A human-comprehensible string identifying the heap and its strategy,
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
     /* Heap-specific private data.
	*/

  int (*initialize)( young_heap_t *heap );
     /* A method that finishes initialization, after all heaps have
	been allocated.  It returns 0 if the initialization failed, 
	1 otherwise.
	*/

  word *(*allocate)( young_heap_t *heap, int nbytes, int no_gc );
     /* A method that allocates `nbytes' bytes from the heap and returns
	a pointer to the first words.  If `no_gc' == 1, then it is
	guaranteed that allocation will not cause a garbage collection;
	hence, no objects will move during the call.

	nbytes > 0
	*/

  void (*collect)( young_heap_t *heap, int nbytes );
     /* A method that requests a garbage collection in the heap, without
	guaranteeing that one is performed, but in any event guaranteeing
	that at least `nbytes' bytes are free for allocation following
	the call, at the location of the current heap pointer.

	nbytes >= 0
	*/

  void (*before_collection)( young_heap_t *heap );
     /* A method that will be called at the start of any garbage collection.
	*/

  void (*after_collection)( young_heap_t *heap );
     /* A method that will be called at the end of any garbage collection.
	*/

  void (*set_policy)( young_heap_t *heap, int operator, int operand );
     /* A method that gives external agents control over GC policy.

	operator: needs to be defined more specifically
	operand: depends on the operator
        */

  int (*free_space)( young_heap_t *heap );
     /* A method that returns the number of bytes free for allocation
	in the heap.  The return value may be negative.
	*/

  void (*stats)( young_heap_t *heap, heap_stats_t *stats );
     /* A method that fills in the stats structure for the heap.
	
	When called at the beginning of a collection, the before_collection()
	method will not yet have been called, and when called at the end of
	a collection, the after_collection() method will have been called
	first.
	*/

  word *(*data_load_area)( young_heap_t *heap, int nbytes );
     /* A method that allocates a block of at least `nbytes'
	consecutive bytes, suitable for loading data into.  A pointer
	to the first word of the block is returned.  No garbage 
	collection will be triggered.

	nbytes > 0
	*/

  int (*load_prepare)( young_heap_t *heap, metadata_block_t *m, 
			    heapio_t *h, word **lo, word **hi );
     /* UNDOCUMENTED: Used by the future heap dumper.
        */

  int (*load_data)( young_heap_t *heap, metadata_block_t *m, heapio_t *h);
     /* UNDOCUMENTED: Used by the future heap dumper.
        */

  word (*creg_get)( young_heap_t *heap );
     /* Captures the current continuation and returns it as a tagged pointer.
	*/

  void (*creg_set)( young_heap_t *heap, word k );
     /* Discards the current continuation and installs the continuation
	k (a tagged pointer) as the current continuation.
	*/

  void (*stack_underflow)( young_heap_t *heap );
     /* Restores a stack frame into an empty stack cache.
	*/

  void (*stack_overflow)( young_heap_t *heap );
     /* Spills a full stack cache to the heap.
	*/
};


/* Allocate a young_heap_t object and initialize its fields with the
   parameters in the obvious way.  The following may be passed as NULL
   values; the fields are then assigned default no-op methods:
   initialize, set_policy, before_collection, after_collection
   */

young_heap_t *create_young_heap_t(
   char *id,
   word code,
   int  (*initialize)( young_heap_t *heap ),
   word *(*allocate)( young_heap_t *heap, int nbytes, int no_gc ),
   void (*collect)( young_heap_t *heap, int nbytes ),
   void (*before_collection)( young_heap_t *heap ),
   void (*after_collection)( young_heap_t *heap ),
   void (*set_policy)( young_heap_t *heap, int rator, int rand ),
   int (*free_space)( young_heap_t *heap ),
   void (*stats)( young_heap_t *heap, heap_stats_t *stats ),
   word *(*data_load_area)( young_heap_t *heap, int nbytes ),
   int  (*load_prepare)( young_heap_t *heap, metadata_block_t *m, 
			 heapio_t *h, word **lo, word **hi ),
   int  (*load_data)( young_heap_t *heap, metadata_block_t *m, heapio_t *h),
   word (*creg_get)( young_heap_t *heap ),
   void (*creg_set)( young_heap_t *heap, word k ),
   void (*stack_underflow)( young_heap_t *heap ),
   void (*stack_overflow)( young_heap_t *heap ),
   void *data
);

#define yh_initialize( h )         ((h)->initialize( (h) ))
#define yh_allocate( h, n, f )     ((h)->allocate( (h), (n), (f) ))
#define yh_collect( h, n )         ((h)->collect( (h), (n) ))
#define yh_before_collection( h )  ((h)->before_collection( (h) ))
#define yh_after_collection( h )   ((h)->after_collection( (h) ))
#define yh_set_policy( h, x, y )   ((h)->set_policy( h, x, y ))
#define yh_free_space( h )         ((h)->free_space( h ))
#define yh_stats( h, s )           ((h)->stats( h, s ))
#define yh_data_load_area( h, n )  ((h)->data_load_area( h, n ))
#define yh_creg_get( h )           ((h)->creg_get( h ))
#define yh_creg_set( h, k )        ((h)->creg_set( h, k ))
#define yh_stack_underflow( h )    ((h)->stack_underflow( h ))
#define yh_stack_overflow( h )     ((h)->stack_overflow( h ))

#endif   /* INCLUDED_YOUNG_HEAP_T_H */

/* eof */
