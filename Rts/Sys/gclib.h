/* Rts/Sys/gclib.h
 * Larceny run-time system -- garbage collector library
 *
 * $Id: gclib.h,v 1.1.1.1 1998/11/19 21:51:45 lth Exp $
 *
 * This header defines the interface to the gc library, which provides 
 * the following services:
 *  - memory allocation
 *  - memory descriptor table management
 *  - inner loops of copying garbage collection
 *
 * The gc library is not reentrant, although it can be made so by creating
 * e.g. an "arena_t" ADT that encapsulates the global variables declared
 * below.
 */

#ifndef INCLUDED_GCLIB_H
#define INCLUDED_GCLIB_H

#include <sys/types.h>		/* For caddr_t */
#include "larceny-types.h"

/* Page number macros */

#define PAGESIZE           (4*1024)
#define PAGEMASK           (PAGESIZE-1)
#define PAGESHIFT          12

#define roundup_page( n )  (((word)(n)+PAGEMASK)&~PAGEMASK)
#define pageof( n )        (int)(((word)(n)-(word)gclib_pagebase)>>(PAGESHIFT))
#define pageof_pb( n, pb ) (int)(((word)(n)-(word)(pb)) >> (PAGESHIFT))


/* Descriptor table bits */

#define MB_ALLOCATED      1      /* Page is allocated: 1=yes, 0=don't know */
#define MB_HEAP_MEMORY    2      /* Memory belongs to Scheme heap */
#define MB_RTS_MEMORY     4      /* Memory belongs to RTS (non-heap) */
#define MB_FOREIGN        8      /* Memory does not belong to Larceny */
#define MB_REMSET         16     /* Memory belongs to remembered set */
#define MB_FREE           32     /* Memory is on Larceny free list */
#define MB_LARGE_OBJECT   64     /* Memory is allocated to a large object */
#define MB_FLONUMS        128    /* Memory is part of a flonum space */


/* The following values are used in the desc_g array for 
 * pages that are not owned by a heap.  By design, their values are
 * larger than any generation number.
 */

#define FOREIGN_PAGE       ((unsigned)-1)    /* Unknown owner */
#define UNALLOCATED_PAGE   ((unsigned)-2)    /* Larceny owns it */
#define RTS_OWNED_PAGE     ((unsigned)-3)    /* Larceny owns it */


/* Global variables */

extern unsigned *gclib_desc_g;           /* generation owner */
extern unsigned *gclib_desc_b;           /* attribute bits */
extern caddr_t   gclib_pagebase;         /* address of lowest page */


/* The following are defined in "alloc.c" */

void gclib_init( void );
  /* Initialize the low-level memory allocator.
     */

void *gclib_alloc_heap( int bytes, int gen_no );
  /* Allocate `bytes' bytes of heap memory with the given
     generation attribute, and return a pointer to the block.  
     The memory is allocated starting on a page boundary.
     */

void *gclib_alloc_rts( int bytes, unsigned attribute );
  /* Allocate `bytes' bytes of RTS memory with the given attribute, and
     return a pointer to the block.
     The memory is allocated starting on a page boundary.
     */

void gclib_free( void *addr, int bytes );
  /* Free the memory.  `Addr' must have been returned from one of the
     allocation functions, and `bytes' must reflect the actual allocated
     size, possibly after adjustment with gclib_shrink_block().  That is,
     you may not use this function to free partial blocks.
     */

void gclib_shrink_block( void *addr, int oldsize, int newsize );
  /* Shrink the block by reducing its size; the address of the block
     remains the same.  `Oldsize' must reflect the actual size of the
     block, possibly after prior adjustment with gclib_shrink_block().
     `Newsize' must be <= `Oldsize'.
     */

void gclib_memory_range( caddr_t *lowest, caddr_t *highest );
  /* Returns the limits of the address range known to the memory manager;
     any pointer p where lowest <= p < highest can be looked up in the
     descriptor tables.
     */

void gclib_set_generation( void *address, int nbytes, int generation );
  /* Set the generation number for all pages in the range implied by
     `address' and `nbytes' to `generation'.
     */

void gclib_add_attribute( void *address, int nbytes, unsigned attr );
  /* Add the attribute bits of `attr' to the attributes for all the
     pages in the range implied by `address' and `nbytes'.
     */

void gclib_stats( word *wheap, word *wremset, word *wrts, word *wmax_heap );
  /* Returns some statistics about the memory manager.
     */


/* The following are defined in "cheney.c" */

void gclib_stopcopy_promote_into( gc_t *gc, semispace_t *to );
  /* Promote objects younger than the 'tospace' into 'tospace', growing 
     'tospace' as necessary.
     */

void gclib_stopcopy_collect( gc_t *gc, semispace_t *tospace );
  /* Garbage collect the generation of 'tospace', copying all live objects
     and all live younger objects into 'tospace', growing 'tospace' as
     necessary.
     */

void gclib_stopcopy_collect_and_scan_static( gc_t *gc, semispace_t *to );
  /* Garbage collect the generation of 'tospace', copying all live objects
     and all live younger objects into 'tospace', growing 'tospace' as
     necessary.

     The static area, if it exists, is used as roots and scanned in
     its entirety.
     */

void gclib_stopcopy_promote_into_np( gc_t *gc,
				     semispace_t *old, semispace_t *young,
				     int old_remaining, int young_remaining );
  /* Non-predictive promotion:  Promote all objects that are younger than
     gen(old) into old and young, filling old first and then young, making
     sure to update the non-predictive extra remembered set if an object 
     is copied into young that contains a pointer into old.

     old_remaining is the amount of available data in the old area;
     young_remaining ditto for the young area.
     */

void gclib_stopcopy_collect_np( gc_t *gc, semispace_t *tospace );
  /* Copy objects younger than the 'tospace' into 'tospace', growing 
     'tospace' as necessary.  'Tospace' must be the non-predictive
     'young' area.
     */

void gclib_stopcopy_split_heap( gc_t *gc,
			        semispace_t *data, semispace_t *text );
  /* Copy all live data into the two static-area semispaces provided. 
     We are making the assumption that the semispaces have higher
     generation numbers than any other areas.
     */

#endif /* INCLUDED_GCLIB_H */

/* eof */
