/* Rts/Sys/gclib.h
 * Larceny run-time system -- garbage collector library
 *
 * $Id: gclib.h,v 1.6 1997/02/24 01:01:34 lth Exp $
 *
 * This header defines the interface to the gc library, which provides 
 * the following services:
 *  - memory allocation
 *  - memory descriptor table management
 *  - inner loops of copying garbage collection
 *  - the "semispace_t" ADT
 *
 * The GC library consists of the source files cheney.c, semispace.c,
 * and unix-alloc.c.
 *
 * The gc library is not reentrant, although it can be made so by creating
 * e.g. an "arena_t" ADT that encapsulates the global variables declared
 * below.
 */

#ifndef INCLUDED_GCLIB_H
#define INCLUDED_GCLIB_H

#include <sys/types.h>
#include "gc.h"

/* Page number macros */

#define PAGESIZE           (4*1024)
#define PAGEMASK           (PAGESIZE-1)
#define PAGESHIFT          12

#define roundup_page( n )  (((word)(n)+PAGEMASK)&~PAGEMASK)
#define pageof( n )        (((word)(n)-(word)gclib_pagebase) >> (PAGESHIFT))


/* Descriptor table bits */

#define MB_ALLOCATED      1      /* Page is allocated: 1=yes, 0=don't know */
#define MB_HEAP_MEMORY    2      /* Memory belongs to Scheme heap */
#define MB_RTS_MEMORY     4      /* Memory belongs to RTS (non-heap) */
#define MB_FOREIGN        8      /* Memory does not belong to Larceny */
#define MB_REMSET         16     /* Memory belongs to remembered set */
#define MB_FREE           32     /* Memory is on Larceny free list */


/* The following values are used in the desc_h and desc_g arrays for 
 * pages that are not owned by a heap.  By design, their values are
 * larger than any generation or heap number.
 */

#define FOREIGN_PAGE       ((unsigned)-1)    /* Unknown owner */
#define UNALLOCATED_PAGE   ((unsigned)-2)    /* Larceny owns it */
#define RTS_OWNED_PAGE     ((unsigned)-3)    /* Larceny owns it */


/* The following values are used to control gclib operation for the
 * non-predictive collector.
 */

typedef enum { 
  ROOTS_ONLY,            /* only forward roots */
  SCAN_ONLY,             /* only scan */
  ROOTS_AND_SCAN         /* both */
} np_operation_t;


/* Global variables */

extern unsigned *gclib_desc_g;           /* generation owner */
extern unsigned *gclib_desc_b;           /* attribute bits */
extern caddr_t   gclib_pagebase;         /* address of lowest page */


/* Semispaces */

/* A semispace is a set of memory chunks of the same generation. The set
 * can grow and shrink as necessary; typically, it is grown by the gclib
 * when the semispace overflows during gc or promotion, and shrunk by
 * the heap under direction of policy.
 *
 * An old-heap does not have to use these semispaces but I suspect they are
 * generally useful, and therefore included in this interface.
 */

typedef struct semispace semispace_t;
typedef struct chunk chunk_t;

semispace_t *create_semispace( unsigned bytes, int heap_no, int gen_no );

struct chunk {
  unsigned bytes;      /* # of bytes allocated; 0 => pointers are garbage */
  word *bot;           /* Pointer to first word */
  word *top;           /* Pointer to next free word */
  word *lim;           /* Pointer past last free word */
};

struct semispace {
  int      heap_no;    /* Heap identifier */
  int      gen_no;     /* Generation identifier */
  unsigned allocated;  /* Total allocated bytes in semispace */
  unsigned used;       /* Total used bytes in semispace */
  int      current;    /* Index of current chunk (may be -1 briefly) */
  unsigned n;          /* Length of chunk array */
  chunk_t  *chunks;    /* Array of chunks */
};


/* Allocation */

void gclib_init( void );
void *gclib_alloc_heap( unsigned bytes, unsigned heap_no, unsigned gen_no );
void *gclib_alloc_rts( unsigned bytes, unsigned attribute );
void gclib_free( void *addr, unsigned bytes );


/* Copying services */

word *gclib_stopcopy_fast( gc_t *gc, word *oldlo, word *oldhi, word *dest );
void gclib_copy_younger_into( gc_t *gc, semispace_t *to );
void gclib_copy_younger_into3( gc_t *gc, semispace_t *to, np_operation_t op );
void gclib_stopcopy_slow( gc_t *gc, semispace_t *to );
void gclib_np_copy_younger_into( gc_t *gc, semispace_t *tospace,
				 np_operation_t op );


/* Page attribute manipulation */

void gclib_set_gen_no( semispace_t *ss, int gen_no );


/* Semispace manipulation */

void ss_expand( semispace_t *ss, unsigned bytes_needed );
void ss_reset( semispace_t *ss );
void ss_sync( semispace_t *ss );
void ss_free( semispace_t *ss );


/* Statistics */

void gclib_stats( word *wheap, word *wremset, word *wrts );

#endif /* INCLUDED_GCLIB_H */

/* eof */
