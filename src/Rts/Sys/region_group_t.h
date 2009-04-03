#ifndef INCLUDED_REGION_GROUP_T_H
#define INCLUDED_REGION_GROUP_T_H

#include "larceny-types.h"

extern char* region_group_name( region_group_t group );

extern
region_group_t region_group_of( old_heap_t *heap );
/* Produces group for heap. */

extern
void region_group_switch( old_heap_t *heap, 
                          region_group_t old_grp, region_group_t new_grp );
/* Reassigns heap to new_grp, removing it from its old group. */

extern
void region_group_switch_all( region_group_t old_grp, region_group_t new_grp );
/* Moves all heaps in old_grp to new grp. */

extern
old_heap_t *region_group_first_heap( region_group_t grp );
/* Produces first heap in grp.
 * See also region_group_next_heap below. */

extern
old_heap_t *region_group_next_heap( old_heap_t *heap );
/* Iterates through group; produces NULL when none are left.
 * See also region_group_first_heap above. */

#endif  /* INCLUDED_OLD_HEAP_T_H */

/* eof */
