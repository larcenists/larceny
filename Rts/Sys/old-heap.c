/* Rts/Sys/old-heap.c
 * Larceny run-time system -- stop-and-copy varsized heap.
 *
 * $Id: old-heap.c,v 1.5 1997/01/28 19:48:15 lth Exp $
 *
 * An old heap is a heap that receives new objects by promotion from
 * younger heaps, not by direct allocation.  Promotion from younger heaps
 * into an older heap is handled by the older heap itself: it scans
 * itself and all remembered sets of older heaps and copies younger 
 * objects into the heap in the process.
 * 
 * See comments in the file "memmgr.h" for further details.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"


typedef struct old_data old_data_t;

struct old_data {
  int  heap_no;
  int  gen_no;
  int  must_promote;           /* 1 if heap was full after last gc */
  int  must_collect;           /* 1 if heap was full after last promote */

  semispace_t *current_space;  /* Space to promote into */
  semispace_t *other_space;    /* Unused */

  unsigned hiwatermark;        /* Percent */
  unsigned lowatermark;        /* Percent */
};


#define DATA(x)  ((old_data_t*)((x)->data))


/* Interface */
static int initialize( old_heap_t *h );
static void promote( old_heap_t *h );
static void collect( old_heap_t *h );
static void stats( old_heap_t *h, int generation, heap_stats_t *s );
static void before_promotion( old_heap_t *h );
static void after_promotion( old_heap_t *h );
static word *data_load_area( old_heap_t *, unsigned );


old_heap_t *
create_old_heap( int *gen_no,             /* add at least 1 to this */
		 int heap_no,             /* this is given */
		 unsigned size_bytes,
		 unsigned thiwatermark,   /* in percent */
		 unsigned tlowatermark    /* in percent */
		)
{
  old_heap_t *heap;
  old_data_t *data;

 again:
  heap = (old_heap_t*)malloc( sizeof( old_heap_t ) );
  data = heap->data = (old_data_t*)malloc( sizeof( old_data_t ) );

  if (heap == 0 || data == 0) {
    if (heap) free( heap );
    if (data) free( data );
    memfail( MF_MALLOC, "old-heap: could not allocate heap metadata." );
    goto again;
  }

  heap->oldest = 0;
  heap->id = "sc/variable";

  data->gen_no = *gen_no;
  data->heap_no = heap_no;
  data->must_promote = 0;
  data->must_collect = 0;

  *gen_no = *gen_no + 1;

  if (size_bytes == 0) size_bytes = DEFAULT_TSIZE;
  size_bytes = roundup_page( size_bytes );

  if (thiwatermark <= 0 || thiwatermark > 100)
    thiwatermark = DEFAULT_THIWATERMARK;
  if (tlowatermark <= 0 || tlowatermark > 100)
    tlowatermark = DEFAULT_TLOWATERMARK;

  data->current_space =
    create_semispace( size_bytes, data->heap_no, data->gen_no );
  assert( data->current_space != 0 );

  data->other_space =
    create_semispace( size_bytes, data->heap_no, data->gen_no );
  assert( data->other_space != 0 );

  data->hiwatermark = thiwatermark;
  data->lowatermark = tlowatermark;

  heap->initialize = initialize;
  heap->before_promotion = before_promotion;
  heap->after_promotion = after_promotion;
  heap->collect = collect;
  heap->promote_from_younger = promote;
  heap->stats = stats;
  heap->data_load_area = data_load_area;

  return heap;
}


static int 
initialize( old_heap_t *heap )
{
  /* Nothing special to do here, it's all been taken care of. */
  return 1;
}


/* 
 * Promote all younger objects into this heap.
 * Precondition: before_promote() has been called for all younger heaps.
 */
static void promote( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  semispace_t *s;
  unsigned allocated;

  if (data->must_collect) {
    heap->collect( heap );
    return;
  }

  debugmsg( "[debug] old heap (%d,%d): promotion.", data->heap_no,
	    data->gen_no );
  stats_gc_type( data->gen_no, STATS_PROMOTE );

  s = data->current_space;
  allocated = s->allocated;
  gclib_copy_younger_into( heap->collector, s );
  ss_sync( s );

  /* If heap overflowed during promotion, schedule a collection */
  data->must_collect = (s->allocated > allocated);
}


/*
 * Collect this and all younger heaps.
 * Precondition: before_promote() has been called for all younger heaps.
 */
static void collect( old_heap_t *heap )
{
  semispace_t *from, *to;
  old_data_t *data = DATA(heap);

  if (data->must_promote && !heap->oldest) {
    heap->collector->promote_out_of( heap->collector, data->heap_no );
    return;
  }

  debugmsg( "[debug] old heap (%d,%d): collection.",
	   data->heap_no, data->gen_no );
  stats_gc_type( data->gen_no, STATS_COLLECT );

  from = data->current_space;
  to = data->other_space;

  ss_reset( to );                                   /* clear tospace */
  gclib_stopcopy_slow( heap->collector, from, to ); /* promote&copy */
  ss_sync( to );                                    /* calculate statistics */

  /* If live after gc is over high watermark, schedule promotion */
  if (to->used > to->allocated/100*data->hiwatermark)
    if (!heap->oldest)
      data->must_promote = 1;

  /* If live is gc is under low watermark, deallocate some memory */
  if (to->used < to->allocated/100*data->lowatermark) {
    /* FIXME */
  }

  data->must_collect = 0;
  data->current_space = to;
  data->other_space = from;
}


static void stats( old_heap_t *heap, int generation, heap_stats_t *stats )
{
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );
  stats->live = data->current_space->used;
  stats->semispace1 = data->current_space->allocated;
  stats->semispace2 = data->other_space->allocated;
}


static void before_promotion( old_heap_t *heap )
{
  debugmsg( "[debug] old-heap(%d,%d):before_promotion.", DATA(heap)->heap_no,
	    DATA(heap)->gen_no );
  /* Nothing, for now */
}


static void after_promotion( old_heap_t *heap )
{
  debugmsg( "[debug] old-heap(%d,%d):after_promotion.", DATA(heap)->heap_no,
	    DATA(heap)->gen_no );

  DATA(heap)->must_promote = 0;
  DATA(heap)->must_collect = 0;
  ss_reset( DATA(heap)->current_space );
}


static word *
data_load_area( old_heap_t *heap, unsigned nbytes )
{
  old_data_t *data = DATA(heap);
  chunk_t *c = &data->current_space->chunks[data->current_space->current];

  if ((c->lim - c->top)*sizeof(word) >= nbytes) {
    word *p = c->top;
    c->top += nbytes/sizeof(word);
    return p;
  }
  else
    return 0;
}


/* eof */
