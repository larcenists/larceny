/* Rts/Sys/static-heap.c
 * Larceny run-time system -- static heap.
 *
 * $Id: static-heap.c,v 1.3 1997/04/30 16:01:41 lth Exp $
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

typedef struct static_data static_data_t;

struct static_data {
  int      gen_no;
  int      heap_no;
  word     *bot;
  word     *top;
  word     *lim;
  unsigned size;         /* bytes allocated */
};

#define DATA(heap)   ((static_data_t*)(heap->data))

static int initialize( static_heap_t *heap );
static void stats( static_heap_t *heap, heap_stats_t *s );
static word *data_load_area( static_heap_t *heap, unsigned nbytes );

static_heap_t *
create_static_heap( int heap_no,          /* given */
		    int gen_no,           /* given */
		    unsigned size_bytes
		   )
{
  static_heap_t *heap;
  static_data_t *data;

 again:
  heap = (static_heap_t*)malloc( sizeof( static_heap_t ) );
  data = (static_data_t*)malloc( sizeof( static_data_t ) );

  if (heap == 0 || data == 0) {
    if (heap) free( heap );
    if (data) free( data );
    memfail( MF_MALLOC, "static-heap: could not allocate metadata." );
    goto again;
  }

  heap->id = "static";
  heap->data = data;
  heap->initialize = initialize;
  heap->stats = stats;
  heap->data_load_area = data_load_area;

  data->gen_no = gen_no;
  data->heap_no = heap_no;

  size_bytes = roundup_page( size_bytes );

 again2:
  data->bot = gclib_alloc_heap( size_bytes, heap_no, gen_no );
  if (data->bot == 0) {
    memfail( MF_HEAP, "Could not allocate static heap." );
    goto again2;
  }

  data->size = size_bytes;
  data->top = data->bot;
  data->lim = data->bot + size_bytes/sizeof(word);
  return heap;
}

static int initialize( static_heap_t *heap )
{
  /* Nothing to be done */
  return 1;
}

static void stats( static_heap_t *heap, heap_stats_t *s )
{
  s->semispace1 = s->live = DATA(heap)->size;
  s->semispace2 = 0;
}

static word *data_load_area( static_heap_t *heap, unsigned nbytes )
{
  static_data_t *data = DATA(heap);
  word *p;
  
  if ((data->lim - data->top)*sizeof(word) < nbytes)
    return 0;
  p = data->top;
  data->top += nbytes/sizeof(word);
  return p;
}

/* eof */
