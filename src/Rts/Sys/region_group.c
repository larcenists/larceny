/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: $
 */

#define GC_INTERNAL

#include "larceny.h"
#include "region_group_t.h"
#include "old_heap_t.h"

static old_heap_t *group_to_first_heap[region_group_limit_elem];
/* Table mapping each element of region_group_t to NULL or first heap in
 * the group; use get_next_in_group method to find the others.  Note
 * that nonrrof is always mapped to NULL and heaps set to the
 * nonrrof group are not maintained in the table. */
#if 0
static old_heap_t *group_to_last_heap[region_group_limit_elem];
/* Table mapping each element of region_group_t to NULL or last heap in
 * the group.  (Present to enable O(1) enqueue impl.)
 * Note that nonrrof is always mapped to NULL and heaps set to the
 * nonrrof group are not maintained in the table. */
#endif
static int group_to_count[region_group_limit_elem];
/* Table mapping each element of region_group_t to count of heaps
 * on the associated linked list.  Note that nonrrof is always 
 * mapped to 0. */

char *region_group_name( region_group_t grp )
{
  switch (grp) {
  case region_group_nonrrof:    return "n";
  case region_group_unfilled:   return "u";
  case region_group_wait_w_sum: return "w";
  case region_group_wait_nosum: return "W";
  case region_group_summzing:   return "s";
  case region_group_filled:     return "f";

#if 0
  case region_group_popular:    return "p";
#endif
  case region_group_risingstar: return "p";
  case region_group_infamous:   return "q";
  case region_group_hasbeen:    return "h";
  case region_group_advertised: return "a";

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
  return group_to_first_heap[ grp ];
}
old_heap_t *region_group_largest( region_group_t grp,
                                  bool (*oh_geq)
                                  (old_heap_t *oh1, old_heap_t *oh2, void *data),
                                  int sample_count, 
                                  void *data )
{
  old_heap_t *oh, *oh_max;
  oh = group_to_first_heap[ grp ];
  oh_max = oh;
  while (oh != NULL && (sample_count > 0)) {
    if ( oh_geq( oh, oh_max, data ) ) {
      oh_max = oh;
    }
    oh = oh->next_in_group;
    sample_count -= 1;
  }
  return oh_max;
}
old_heap_t *region_group_next_heap( old_heap_t *heap ) 
{
  return heap->next_in_group;
}

static void unlink_group( old_heap_t *heap ) 
{
  old_heap_t *p, *n;
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

  if (group_to_first_heap[heap->group] == heap)
    group_to_first_heap[heap->group] = n;
  heap->prev_in_group = NULL;
  heap->next_in_group = NULL;
}
static void link_group_push( old_heap_t *heap, region_group_t group )
{
  old_heap_t *prev, *next;
  next = group_to_first_heap[group];
  prev = NULL;
  assert( (next == NULL) || (next->prev_in_group == prev) );
  assert( (next == NULL) || (next->group == group) );

  heap->prev_in_group = prev;
  heap->next_in_group = next;
  if (next != NULL) {
    next->prev_in_group = heap;
  }
  group_to_first_heap[group] = heap;
  heap->group = group;
}
static void link_group_enq( old_heap_t *heap, region_group_t group ) 
{
  old_heap_t *prev;
  prev = group_to_first_heap[group];
  if (prev == NULL) {
    group_to_first_heap[group] = heap;
    heap->prev_in_group = NULL;
    heap->next_in_group = NULL;
  } else {
    while (prev->next_in_group != NULL) {
      prev = prev->next_in_group;
    }
    prev->next_in_group = heap;
    heap->prev_in_group = prev;
    heap->next_in_group = NULL;
  }
  heap->group = group;
}

static void decr_count( old_heap_t *heap ) 
{
  if (heap->group != region_group_nonrrof) {
    group_to_count[ heap->group ] -= 1;
  }
}

static void incr_count( old_heap_t *heap ) 
{
  if (heap->group != region_group_nonrrof) {
    group_to_count[ heap->group ] += 1;
  }
}

void region_group_push( old_heap_t *heap, 
                        region_group_t old_grp, region_group_t new_grp ) 
{
  old_heap_t *new_next, *new_prev;
  assert( heap->group == old_grp );
  decr_count( heap );
  unlink_group( heap );
  link_group_push( heap, new_grp );
  incr_count( heap );
}

void region_group_enq( old_heap_t *heap, 
                       region_group_t old_grp, region_group_t new_grp ) 
{
  old_heap_t *new_next, *new_prev;
  assert( heap->group == old_grp );
  decr_count( heap );
  unlink_group( heap );
  link_group_enq( heap, new_grp );
  incr_count( heap );
}

void region_group_enq_all( region_group_t old_grp, region_group_t new_grp )
{
  old_heap_t *h, *n;
  h = group_to_first_heap[ old_grp ];
  while (h != NULL) {
    region_group_enq( h, old_grp, new_grp );
    h = group_to_first_heap[ old_grp ];
  }
}
