/* Rts/Sys/memmgr.h
 * Larceny run-time system -- internal interfaces for precise GC.
 * 
 * $Id$
 */

#ifndef INCLUDED_MEMMGR_H
#define INCLUDED_MEMMGR_H

#include "larceny-types.h"
#include "gclib.h"

/* GC parameters */

#define GC_LARGE_OBJECT_LIMIT    PAGESIZE
#define GC_CHUNK_SIZE            (256*1024)

/* Heap codes (for load/dump matching) */

#define HEAPCODE_STATIC_2SPACE   0     /* text & data areas */
#define HEAPCODE_YOUNG_2SPACE    1     /* normal two-space */
#define HEAPCODE_OLD_2SPACE      2     /* normal two-space */
#define HEAPCODE_OLD_2SPACE_NP   3     /* non-predictive two-space */
#define HEAPCODE_YOUNG_1SPACE    4     /* nursery */

/* Policy codes (for set_policy()).  See also Lib/gcctl.sch. */

#define GCCTL_J_FIXED            0
#define GCCTL_J_PERCENT          1
#define GCCTL_INCR_FIXED         2
#define GCCTL_INCR_PERCENT       3
#define GCCTL_DECR_FIXED         4
#define GCCTL_DECR_PERCENT       5
#define GCCTL_HIMARK             6
#define GCCTL_LOMARK             7
#define GCCTL_OFLOMARK           8
#define GCCTL_GROW               9

/* In memmgr.c */

int gc_compute_dynamic_size( int D, int S, int Q, double L, int limit );

/* In nursery.c */

young_heap_t *
create_nursery( int gen_no, gc_t *gc, nursery_info_t *info, word *globals );

/* In sc-heap.c */

young_heap_t *
create_sc_heap( int gen_no, gc_t *gc, sc_info_t *info, word *globals );
  /* Create a stop-and-copy expandable young area.
     */

semispace_t *
yhsc_data_area( young_heap_t *heap );
  /* Returns the current semispace structure for a stop-and-copy expandable
     young area.
     */

/* In old-heap.c */

old_heap_t *
create_sc_area( int gen_no, gc_t *gc, sc_info_t *info, bool ephemeral );

/* In np-sc-heap.c */

old_heap_t *
create_np_dynamic_area( int gen_no, int *gen_allocd, gc_t *gc,
		       np_info_t *info );

void
np_gc_parameters( old_heap_t *heap, int *k, int *j );

/* In static-heap.c */

static_heap_t *
create_static_area( int gen_no, gc_t *gc );


#endif /* INCLUDED_GC_INTERFACE_H */

/* eof */
