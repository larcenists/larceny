/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- generic operations on old heaps.
 *
 * An old heap is a heap that receives new objects by promotion from
 * younger heaps, not by direct allocation.
 */

#include "larceny.h"
#include "old_heap_t.h"

static int default_initialize( old_heap_t *h ) { return 1; }
static void default_set_policy( old_heap_t *h, int x, int y ) { }

old_heap_t *create_old_heap_t(
  char *id,
  word code,
  int  (*initialize)( old_heap_t *heap ),
  void (*collect)( old_heap_t *heap ),
  void (*before_collection)( old_heap_t *heap ),
  void (*after_collection)( old_heap_t *heap ),
  void (*stats)( old_heap_t *heap, int generation, heap_stats_t *stats ),
  word *(*data_load_area)( old_heap_t *heap, int nbytes ),
  int  (*load_prepare)( old_heap_t *heap, metadata_block_t *m, 
		        heapio_t *h, word **lo, word **hi ),
  int  (*load_data)( old_heap_t *heap, metadata_block_t *m, heapio_t *h ),
  void (*set_policy)( old_heap_t *heap, int op, int value ),
  void *data
)
{
  old_heap_t *heap;

  heap = (old_heap_t*)must_malloc( sizeof( old_heap_t ) );

  heap->collector = 0;
  heap->id = id;
  heap->code = code;
  heap->maximum = 0;
  heap->allocated = 0;

  heap->data = data;

  heap->initialize = (initialize ? initialize : default_initialize);
  heap->collect = collect;
  heap->before_collection = before_collection;
  heap->after_collection = after_collection;
  heap->stats = stats;
  heap->data_load_area = data_load_area;
  heap->load_prepare = load_prepare;
  heap->load_data = load_data;
  heap->set_policy = (set_policy ? set_policy : default_set_policy);

  return heap;
}

/* eof */
