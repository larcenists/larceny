/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: $
 */

#define GC_INTERNAL

#include "larceny.h"
#include "region_group_t.h"
#include "old_heap_t.h"

static old_heap_t *group_to_heap[region_group_limit_elem];
/* Table mapping each element of region_group_t to NULL or a heap in
 * the group; use get_next_in_group method to find the others.  Note
 * that nonrrof is always mapped to NULL and heaps set to the
 * nonrrof group are not maintained in the table. */
static int group_to_count[region_group_limit_elem];
/* Table mapping each element of region_group_t to count of heaps
 * on the associated linked list.  Note that nonrrof is always 
 * mapped to 0. */

char *region_group_name( region_group_t grp )
{
  switch (grp) {
  case region_group_nonrrof:  return "n";
  case region_group_unfilled: return "u";
  case region_group_waiting:  return "w";
  case region_group_summzing: return "s";
  case region_group_filled:   return "f";
  case region_group_popular:  return "p";
  default: assert(0); return NULL;
  }
}

region_group_t region_group_of( old_heap_t *heap ) 
{
  return heap->group;
}
int region_group_count( region_group_t grp ) 
{
  return group_to_count[ grp ];
}
old_heap_t *region_group_first_heap( region_group_t grp ) 
{
  return group_to_heap[ grp ];
}
old_heap_t *region_group_next_heap( old_heap_t *heap ) 
{
  return heap->next_in_group;
}

static void unlink_group( old_heap_t *heap ) 
{
  old_heap_t *p, *n;
#if 0
  consolemsg( "unlink_group( heap=0x%08x [%d] )", 
              heap, oh_current_space(heap)->gen_no );
#endif
  p = heap->prev_in_group;
  n = heap->next_in_group;

  if (p != NULL) {
    assert( p->next_in_group == heap );
    p->next_in_group = n;
  }
  if (n != NULL) {
    assert( n->prev_in_group == heap );
    n->prev_in_group = p;
  }

  if (group_to_heap[heap->group] == heap)
    group_to_heap[heap->group] = n;
  heap->prev_in_group = NULL;
  heap->next_in_group = NULL;
}
static void link_group( old_heap_t *heap, region_group_t group )
{
  old_heap_t *prev, *next;
#if 0
  consolemsg( "link_group( heap=0x%08x [%d], new_grp=%s )", 
              heap, oh_current_space(heap)->gen_no, region_group_name(group) );
#endif
  next = group_to_heap[group];
  prev = NULL;
  assert( (next == NULL) || (next->prev_in_group == prev) );
  assert( (next == NULL) || (next->group == group) );

  heap->prev_in_group = prev;
  heap->next_in_group = next;
  if (next != NULL) {
    next->prev_in_group = heap;
  }
  group_to_heap[group] = heap;
  heap->group = group;
}
void region_group_switch( old_heap_t *heap, 
                          region_group_t old_grp, region_group_t new_grp ) 
{
  old_heap_t *new_next, *new_prev;
#if 0
  consolemsg( "switch_group( heap=0x%08x [%d], new_grp=%s )", 
              heap, oh_current_space(heap)->gen_no, region_group_name(new_grp) );
#endif
  assert( heap->group == old_grp );

  if (heap->group != region_group_nonrrof) {
    group_to_count[ heap->group ] -= 1;
  }

  unlink_group( heap );
  link_group( heap, new_grp );

  if (heap->group != region_group_nonrrof) {
    group_to_count[ heap->group ] += 1;
  }
}

void region_group_switch_all( region_group_t old_grp, region_group_t new_grp )
{
  old_heap_t *h, *n;
  h = group_to_heap[ old_grp ];
  while (h != NULL) {
    region_group_switch( h, old_grp, new_grp );
    h = group_to_heap[ old_grp ];
  }
}
