/* Copyright 1998 Lars T Hansen.
 * 
 * $Id$
 *
 * Larceny run-time system -- garbage collector library
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
 *
 * FIXME: Should remove use of caddr_t altogether; using char* or byte*
 *        everywhere is ok by the ANSI standard.
 */

#ifndef INCLUDED_GCLIB_H
#define INCLUDED_GCLIB_H

#include "config.h"

/* The attribute GCLIB_LARGE_TABLE may be set in config.h.  This
   attribute is experimental.  If set, then
      gclib_desc_b is not defined;
      gclib_pagebase is not defined;
      gclib_desc_g element type is byte, and the high bit is the large
        object bit and the low 7 bits are the generation number; and
      the table for entire 4GB address range is preallocated.
*/

#ifndef ASSEMBLER
# if defined(UNIX)
#  include <sys/types.h>	/* For caddr_t */
# else
   typedef char *caddr_t;	/* Need to fix this */
# endif
# include "larceny-types.h"
#endif /* not ASSEMBLER */

/* Page number macros */

#define PAGESIZE           (4*1024)
#define PAGEMASK           (PAGESIZE-1)
#define PAGESHIFT          12

#define roundup_page( n )  (((word)(n)+PAGEMASK)&~PAGEMASK)
#define pageof_pb( n, pb ) ((int)(((word)(n)-(word)(pb)) >> (PAGESHIFT)))
#if GCLIB_LARGE_TABLE
# define pageof( n )       ((int)((word)(n) >> (PAGESHIFT)))
#else
# define pageof( n )     ((int)(((word)(n)-(word)gclib_pagebase)>>(PAGESHIFT)))
#endif

#if GCLIB_LARGE_TABLE
# define gen_of( ptr )      (gclib_desc_g[pageof(ptr)] & ~MB_MASK)
# define attr_of( ptr )     (gclib_desc_g[pageof(ptr)] & MB_MASK)
#else
# define gen_of( ptr )      (gclib_desc_g[pageof(ptr)])
# define attr_of( ptr )     (gclib_desc_b[pageof(ptr)])
#endif

/* Descriptor table bits */

#if GCLIB_LARGE_TABLE
# define MB_LARGE_OBJECT   128    /* Memory is allocated to a large object */
# define MB_REMSET         64     /* Memory belongs to remembered set */
# define MB_MASK           (128+64)
#else
# define MB_ALLOCATED      1      /* Page is allocated: 1=yes, 0=don't know */
# define MB_HEAP_MEMORY    2      /* Memory belongs to Scheme heap */
# define MB_RTS_MEMORY     4      /* Memory belongs to RTS (non-heap) */
# define MB_FOREIGN        8      /* Memory does not belong to Larceny */
# define MB_REMSET         16     /* Memory belongs to remembered set */
# define MB_FREE           32     /* Memory is on Larceny free list */
# define MB_LARGE_OBJECT   64     /* Memory is allocated to a large object */
# define MB_FLONUMS        128    /* Memory is part of a flonum space */
#endif

/* The following values are used in the desc_g array for 
 * pages that are not owned by a heap.  By design, their values are
 * larger than any generation number.
 */
#ifndef ASSEMBLER

#if GCLIB_LARGE_TABLE
typedef byte gclib_desc_t;
#else
typedef unsigned gclib_desc_t;
#endif


/* Global variables */

extern gclib_desc_t *gclib_desc_g;	/* generation owner */
#if !GCLIB_LARGE_TABLE
extern gclib_desc_t* gclib_desc_b;      /* attribute bits */
extern caddr_t       gclib_pagebase;    /* address of lowest page */
#endif

/* The following are defined in "alloc.c" */

void gclib_init( void );
  /* Initialize the low-level memory allocator.
     */

void gclib_set_heap_limit( int bytes );
  /* Set the maximum number of bytes that may be allocated to the
     heap to `bytes'.  If bytes==0, remove the limit.
     */

void *gclib_alloc_heap( int bytes, int gen_no );
  /* Allocate `bytes' bytes of heap memory with the given generation
     attribute, and return a pointer to the block.  The memory is
     allocated starting on a page boundary.  If a heap limit is in
     effect, and if the request cannot be satisfied without exceeding
     the limit, memfail() is called to signal that the limit is
     exceeded.
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

void gclib_stats( gclib_stats_t *stats );
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
     young_remaining ditto for the young area.  The collector uses these
     when promoting small objects (but not when promoting large objects).
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

void gclib_check_object( word obj );
  /* Obj must be a tagged pointer.  Check that the object it points to is
     consistent, and signal an error if not.
     */

void gclib_check_memory_validity( word *p, int n );
  /* P must point to a pointerfull object of length n words (to the header 
     if the object has a header).  The object is checked that it is 
     consistent: that every constituent word is formatted properly and, 
     if a pointer, points to a valid word.  Signals an error with 
     conditional_abort() if any invalid data are found.
     */

#endif /* not ASSEMBLER */

#endif /* INCLUDED_GCLIB_H */

/* eof */
