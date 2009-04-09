/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: $
 */

#ifndef INCLUDED_REGION_GROUP_T_H
#define INCLUDED_REGION_GROUP_T_H

#include "larceny-types.h"

extern char* region_group_name( region_group_t group );
extern int region_group_count( region_group_t group );

extern
region_group_t region_group_of( old_heap_t *heap );
/* Produces group for heap. */

extern
void region_group_push( old_heap_t *heap, 
                        region_group_t old_grp, region_group_t new_grp );
/* Reassigns heap to new_grp, removing it from its old group.  LIFO. */
extern
void region_group_enq( old_heap_t *heap, 
                       region_group_t old_grp, region_group_t new_grp );
/* Reassigns heap to new_grp, removing it from its old group.  FIFO. */

extern
void region_group_enq_all( region_group_t old_grp, region_group_t new_grp );
/* Moves all heaps in old_grp to new grp.  FIFO. */

extern
old_heap_t *region_group_first_heap( region_group_t grp );
/* Produces first heap in grp, or NULL if grp is empty.
 * See also region_group_next_heap below. */

extern 
old_heap_t *region_group_largest( region_group_t grp,
                                  bool (*oh_geq)
                                  (old_heap_t *oh1, 
                                   old_heap_t *oh2, 
                                   void *data),
                                  int sample_count, 
                                  void *data );
/* Produces largest heap of first N in region_group_wait_w_sum, 
 * where N = sample_count and heaps are compared by oh_geq, 
 * or NULL if grp is empty. */

extern
old_heap_t *region_group_next_heap( old_heap_t *heap );
/* Iterates through group; produces NULL when none are left.
 * See also region_group_first_heap above. */

#endif  /* INCLUDED_OLD_HEAP_T_H */

/* eof */
