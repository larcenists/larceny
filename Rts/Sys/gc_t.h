/* Rts/Sys/gc_t.h
 * Larceny run-time system -- garbage collector data structure.
 *
 * $Id: gc_t.h,v 1.1.1.1 1998/11/19 21:51:45 lth Exp $
 */

#ifndef INCLUDED_GC_T_H
#define INCLUDED_GC_T_H

#include "larceny-types.h"

struct gc { 
  char *id;
    /* A human-readable string identifying the collector, its heaps,
       and its policies.
       */

  los_t *los;
    /* In precise collectors: A large-object-space data structure.
       */

  young_heap_t *young_area;
    /* In precise collectors: A pointer to the allocation area (a nursery
       or a stop-and-copy heap).
       */

  old_heap_t **ephemeral_area;
    /* In precise collectors: An array of pointers to ephemeral areas;
       the number of entries is held in ephemeral_area_count.  May be NULL.
       */

  old_heap_t *dynamic_area;
    /* In precise collectors: A pointer to a dynamic area, or NULL.
       */

  static_heap_t *static_area;
    /* In precise collectors: A pointer to a static area, or NULL.
       */

  remset_t **remset;
    /* In precise collectors: An array of pointers to remembered sets, 
       or NULL.  Entry 0 in the array is unused.
       */

  int ephemeral_area_count;
    /* The number of entries in the ephemeral_area table.
       */

  int remset_count;
    /* The number of entries in the remset table.
       */

  int np_remset;
    /* In a non-predictive collector, the index in the remset array of
       the extra non-predictive remembered set, otherwise -1.
       */

  void *data;
    /* Private data.
       */

  int (*initialize)( gc_t *gc );
    /* A method that is run after any other system initialization has
       taken place.  It runs the initialize() method on each heap
       controlled by the collector, and initializes the write barrier.
       */

  word *(*allocate)( gc_t *gc, int nbytes, bool no_gc, bool atomic );
    /* A method that allocates an object of size at least `nbytes'.  
       If `no_gc' == 1, then no garbage collection may be performed 
       during the allocation.  `nbytes' must not be larger than 
       LARGEST_OBJECT (defined in "larceny.h").
       Returns a pointer to the allocated object.
       */

  word *(*allocate_nonmoving)( gc_t *gc, int nbytes, bool atomic );
    /* A method that allocates a non-moving object of size at least `nbytes'.
       Returns a pointer to the allocated object.
       */

  void (*collect)( gc_t *gc, int gen, int bytes_needed );
    /* A method that requests that a garbage collection be performed in
       generation `gen', such that at least `bytes_needed' bytes can be
       allocated following the collection.
       */

  void (*set_policy)( gc_t *gc, int heap, int x, int y );

  word *(*data_load_area)( gc_t *gc, int nbytes );
    /* Return a pointer to a data area with the following properties:
       - it is contiguous
       - it will hold exactly as many bytes as requested 
       - it is uninitialized and may hold garbage
       - it is actually allocated - further calls to this function will not
         return a pointer to the area
       */

  word *(*text_load_area)( gc_t *gc, int nbytes );

  int  (*iflush)( gc_t *gc, int generation );
    /* A method that returns 1 if the instruction cache must be flushed
       after collecting the named generation.
       */

  word (*creg_get)( gc_t *gc );
  void (*creg_set)( gc_t *gc, word k );
  void (*stack_overflow)( gc_t *gc );
  void (*stack_underflow)( gc_t *gc );

  /* Statistics */
  void (*stats)( gc_t *gc, int generation, heap_stats_t *stats );

  /* Remembered sets */
  int  (*compact_all_ssbs)( gc_t *gc );

  /* Support for non-predictive collector */
  void (*compact_np_ssb)( gc_t *gc );
  void (*np_remset_ptrs)( gc_t *gc, word ***ssbtop, word ***ssblim );

  int  (*dump_heap)( gc_t *gc, const char *filename, bool compact );
    /* Method that dumps the heap image into the named file.  Compact
       the heap first iff compact is non-zero.

       Returns 0 on success, a negative error code (defined in heapio.h)
       on error.
       */

  int  (*load_heap)( gc_t *gc, heapio_t *h );
    /* Method that loads the heap image from the file into the heap.
       The heap image in the file must be recognizable by the garbage
       collector; different collectors use different heap formats.

       If the heap image was not recognized or could not be loaded,
       0 is returned, otherwise 1.
       */


  /* PRIVATE */
  /* Internal to the collector implementation. */
  void (*enumerate_roots)( gc_t *gc, void (*f)( word*, void *), void * );
  void (*enumerate_remsets_older_than)( gc_t *gc, int generation,
				        int (*f)(word, void*, unsigned * ),
				        void *,
				        bool enumerate_np_remset );
};

gc_t *create_gc( gc_param_t *params, /* OUT */ int *actual_generations );
gc_t *create_bdw_gc( gc_param_t *params, /* OUT */ int *actual_generations );

/* Operations.  For prototypes, see the method specs above. */

#define gc_initialize( gc )           ((gc)->initialize( gc ))
#define gc_allocate( gc, n, nogc, a ) ((gc)->allocate( gc, n, nogc, a ))
#define gc_allocate_nonmoving( gc,n,a ) ((gc)->allocate_nonmoving( gc, n,a ))
#define gc_collect( gc,gen,n )        ((gc)->collect( gc,gen,n ))
#define gc_set_policy( gc,h,x,y )     ((gc)->set_policy( gc,h,x,y ))
#define gc_data_load_area( gc,n )     ((gc)->data_load_area( gc,n ))
#define gc_text_load_area( gc,n )     ((gc)->text_load_area( gc,n ))
#define gc_iflush( gc )               ((gc)->iflush( gc, -1 ))
#define gc_creg_get( gc )             ((gc)->creg_get( gc ))
#define gc_creg_set( gc,k )           ((gc)->creg_set( gc, k ))
#define gc_stack_overflow( gc )       ((gc)->stack_overflow( gc ))
#define gc_stack_underflow( gc )      ((gc)->stack_underflow( gc ))
#define gc_stats( gc,g,stats )        ((gc)->stats( gc,g,stats ))
#define gc_compact_all_ssbs( gc )     ((gc)->compact_all_ssbs( gc ))
#define gc_compact_np_ssb( gc )       ((gc)->compact_np_ssb( gc ))
#define gc_dump_heap( gc, fn, c )     ((gc)->dump_heap( gc, fn, c ))
#define gc_load_heap( gc, h )         ((gc)->load_heap( gc, h ))
#define gc_enumerate_roots( gc,s,d )  ((gc)->enumerate_roots( gc, s, d ))
#define gc_np_remset_ptrs( gc, t, l ) ((gc)->np_remset_ptrs( gc, t, l ))

#define gc_enumerate_remsets_older_than( gc, g, s, d, f ) \
  ((gc)->enumerate_remsets_older_than( gc, g, s, d, f ))

gc_t 
*create_gc_t(char *id,
	     void *data,
	     int  (*initialize)( gc_t *gc ),
	     word *(*allocate)( gc_t *gc, int nbytes, bool no_gc, bool atomic),
	     word *(*allocate_nonmoving)( gc_t *gc, int nbytes, bool atomic ),
	     void (*collect)( gc_t *gc, int gen, int bytes_needed ),
	     void (*set_policy)( gc_t *gc, int heap, int x, int y ),
	     word *(*data_load_area)( gc_t *gc, int nbytes ),
	     word *(*text_load_area)( gc_t *gc, int nbytes ),
	     int  (*iflush)( gc_t *gc, int generation ),
	     word (*creg_get)( gc_t *gc ),
	     void (*creg_set)( gc_t *gc, word k ),
	     void (*stack_overflow)( gc_t *gc ),
	     void (*stack_underflow)( gc_t *gc ),
	     void (*stats)( gc_t *gc, int generation, heap_stats_t *stats ),
	     int  (*compact_all_ssbs)( gc_t *gc ),
	     void (*compact_np_ssb)( gc_t *gc ),
	     void (*np_remset_ptrs)( gc_t *gc, word ***ssbtop, word ***ssblim),
	     int  (*load_heap)( gc_t *gc, heapio_t *h ),
	     int  (*dump_heap)( gc_t *gc, const char *filename, bool compact ),
	     void (*enumerate_roots)( gc_t *gc, void (*f)( word*, void *),
				     void * ),
	     void (*enumerate_remsets_older_than)
	        ( gc_t *gc, int generation,
		  int (*f)(word, void*, unsigned * ),
		  void *data,
		  bool enumerate_np_remset )
	     );

void gc_parameters( gc_t *gc, int op, int *ans );

#endif   /* INCLUDED_GC_T_H */

/* eof */
