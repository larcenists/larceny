/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- definition of static_heap_t ADT.
 */

#ifndef INCLUDED_STATIC_HEAP_T_H
#define INCLUDED_STATIC_HEAP_T_H

#include "larceny-types.h"

struct static_heap {
  gc_t *collector;
    /* The garbage collector that controls this static heap.
       */

  char *id;
     /* A human-comprehensible string identifying the heap and its strategy,
	used by the startup banner.
        */

  word code;
     /* A numeric code identifying the heap, used by the heap dumper/loader.
        */

  semispace_t *data_area;
    /* A data area or NULL.
       */

  semispace_t *text_area;
    /* A text area or NULL.
       */

  int allocated;
    /* Summary of current allocation.
       */

  void *data;
     /* Heap-specific private data.
	*/

  int  (*initialize)( static_heap_t *h );
     /* A method that finishes initialization, after all heaps have
	been allocated.  It returns 0 if the initialization failed, 
	1 otherwise.  May be NULL.
	*/

  word *(*allocate)( static_heap_t *h, int nbytes );
     /* A method that allocates `nbytes' bytes from the heap and returns
	a pointer to the first words.  The memory is allocated
	permanently -- it cannot be released.

	nbytes > 0
	*/

  void (*reorganize)( static_heap_t *h );
    /* A method that copies ALL live data into two parts, _text_ 
       (for non-pointer containing) and _data_ (pointer containing), 
       and then makes the static area consist of those two.

       Arguably this method does not belong in this interface.
       */

  void (*stats)( static_heap_t *h );
     /* Update the stats for the heap in the central repository. 
	*/

  word *(*data_load_area)( static_heap_t *h, int nbytes );
     /* A method that allocates a block of at least `nbytes'
	consecutive bytes, suitable for loading data into.  A pointer
	to the first word of the block is returned.  No garbage 
	collection will be triggered.

	nbytes > 0
	*/

  word *(*text_load_area)( static_heap_t *h, int nbytes );
     /* A method that allocates a block of at least `nbytes'
	consecutive bytes, suitable for loading non-pointer containing
	data into.  A pointer to the first word of the block is returned.  
	No garbage collection will be triggered.

	nbytes > 0
	*/

  int  (*load_prepare)( static_heap_t *heap, metadata_block_t *m, 
		        heapio_t *h, word **lo, word **hi );
     /* UNDOCUMENTED: Used by the future heap dumper.
        */

  int  (*load_data)( static_heap_t *heap, metadata_block_t *m, heapio_t *h );
     /* UNDOCUMENTED: Used by the future heap dumper.
        */
  bool (*is_address_mapped)( static_heap_t *heap, word *addr, bool noisy );
    /* Returns true iff 'addr' is an object in 'heap'
     */
};

/* Operations */

#define sh_initialize( h )            ((h)->initialize?(h)->initialize( h ):1)
#define sh_allocate( h, n )           ((h)->allocate( (h), (n) ))
#define sh_reorganize( h )            ((h)->reorganize( (h) ))
#define sh_stats( h )                 ((h)->stats( (h) ))
#define sh_data_load_area( h, n )     ((h)->data_load_area( (h),(n) ))
#define sh_text_load_area( h, n )     ((h)->text_load_area( (h),(n) ))
#define sh_get_data_areas( h, d, t )  ((h)->get_data_areas( (h),(d),(t) ))
#define sh_is_address_mapped( h,a,n ) ((h)->is_address_mapped( (h), (a), (n) ))

#endif

/* eof */
