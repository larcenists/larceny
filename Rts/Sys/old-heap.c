/* Rts/Sys/old-heap.c
 * Larceny run-time system -- stop-and-copy varsized old heap.
 *
 * $Id: old-heap.c,v 1.9 1997/05/15 00:58:49 lth Exp lth $
 *
 * An old heap is a heap that receives new objects by promotion from
 * younger heaps, not by direct allocation.  Promotion from younger heaps
 * into an older heap is handled by the older heap itself: it scans
 * itself and all remembered sets of older heaps and copies younger 
 * objects into the heap in the process.
 * 
 * Allocation.  Only one semispace is allocated initially, the other being
 * allocated only when a collection takes place, and then on-demand in
 * smallish chunks.  After a collection, the inactive space is released.
 * There are two benefits to this:
 * - First, it commonly uses less (virtual) memory than what would have
 *   been used with two permanently allocated spaces (although the worst
 *   cases are the same).
 * - Second, and far more subtly, the way overflows are handled in the 
 *   GC means that semispaces are not bounded by their selected limits,
 *   but may aquire extra space when needed.  This space would stick around
 *   in a semispace unless freed explicitly, and freeing the semispace
 *   accomplishes that as a side effect.
 *
 * See comments in the file "memmgr.h" for further details about the flow
 * of control among the garbage collector modules.
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
  int  must_promote;           /* 1 if heap overflowed after last gc */
  int  must_promote_immediately;  /* 1 if heap was overfull after last gc */
  int  must_collect;           /* 1 if heap overflowed after last promote */
  int  size_bytes;             /* our notion of target data size */

  semispace_t *current_space;  /* Space to promote into */

  unsigned hiwatermark;        /* Percent */
  unsigned lowatermark;        /* Percent */
  unsigned oflowatermark;      /* Percent */
};

#define DATA(x)  ((old_data_t*)((x)->data))


static old_heap_t *allocate_old_sc_heap( int gen_no, int heap_no );
static void post_promote_policy( old_heap_t *heap );
static void post_gc_policy( old_heap_t *heap );

old_heap_t *
create_old_heap( int *gen_no,             /* add at least 1 to this */
		 int heap_no,             /* this is given */
		 unsigned size_bytes,     /* size requested by user, or 0 */
		 unsigned thiwatermark,   /* in percent */
		 unsigned tlowatermark,   /* in percent */
                 unsigned toflowatermark  /* in percent */
		)
{
  old_heap_t *heap;
  old_data_t *data;

  assert( *gen_no > 0 );

  if (size_bytes == 0) size_bytes = DEFAULT_TSIZE;
  size_bytes = roundup_page( size_bytes );

  if (thiwatermark == 0 || thiwatermark > 100)
    thiwatermark = DEFAULT_THIWATERMARK;
  if (tlowatermark == 0 || tlowatermark > 100)
    tlowatermark = DEFAULT_TLOWATERMARK;
  if (toflowatermark == 0 || toflowatermark > 100)
    toflowatermark = DEFAULT_TOFLOWATERMARK;

  heap = allocate_old_sc_heap( *gen_no, heap_no );
  data = DATA(heap);

  *gen_no = *gen_no + 1;

  data->current_space =
    create_semispace( size_bytes, data->heap_no, data->gen_no );
  assert( data->current_space != 0 );

  data->hiwatermark = thiwatermark;
  data->lowatermark = tlowatermark;
  data->oflowatermark = toflowatermark;

  data->size_bytes = size_bytes;

  return heap;
}


static int initialize( old_heap_t *h );
static void promote( old_heap_t *h );
static void collect( old_heap_t *h );
static void stats( old_heap_t *h, int generation, heap_stats_t *s );
static void before_promotion( old_heap_t *h );
static void after_promotion( old_heap_t *h );
static word *data_load_area( old_heap_t *, unsigned );

static old_heap_t *
allocate_old_sc_heap( int gen_no, int heap_no )
{
  old_heap_t *heap;
  old_data_t *data;

  heap = (old_heap_t*)must_malloc( sizeof( old_heap_t ) );
  data = heap->data = (old_data_t*)must_malloc( sizeof( old_data_t ) );

  heap->oldest = 0;
  heap->id = "sc/variable";

  data->gen_no = gen_no;
  data->heap_no = heap_no;
  data->must_promote = 0;
  data->must_promote_immediately = 0;
  data->must_collect = 0;

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

  if (data->must_collect) {
    heap->collect( heap );
    return;
  }

  /* Experimental policy that allows overflowed heaps to be promoted
   * more quickly.  In some sense, this policy is an implementation
   * of premature tenuring, so we need to benchmark it carefully.
   *
   * Known so far:
   *   Makes GC time for 50dynamic grow by 50%.
   */
#if 0
  if (data->must_promote_immediately && !heap->oldest) {
    heap->collector->promote_out_of( heap->collector, data->heap_no );
    return;
  }
#endif

  supremely_annoyingmsg( "Promoting into generation %d.", data->gen_no );
  debugmsg( "[debug] old heap (%d,%d): promotion.", data->heap_no,
	    data->gen_no );
  stats_gc_type( data->gen_no, STATS_PROMOTE );

  s = data->current_space;
  gclib_copy_younger_into( heap->collector, s );

  post_promote_policy( heap );
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

  annoyingmsg( "Garbage collecting generation %d.", data->gen_no );
  debugmsg( "[debug] old heap (%d,%d): collection.", data->heap_no,
	    data->gen_no );
  stats_gc_type( data->gen_no, STATS_COLLECT );

  from = data->current_space;
  to = create_semispace( OLDSPACE_EXPAND_BYTES, data->heap_no, data->gen_no );
  
  gclib_stopcopy_slow( heap->collector, to );       /* promote&copy */

  data->current_space = to;
  ss_free( from );

  post_gc_policy( heap );
}


static void
post_promote_policy( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  semispace_t *s = data->current_space;

  ss_sync( s );

  data->must_collect = 0;

  /* If heap overflowed during promotion, schedule a collection */
  if (s->used > data->size_bytes) {
    data->must_collect = 1;
    if (data->must_collect)
      supremely_annoyingmsg( "Generation %d decided to collect next time "
			    "(used=%d).", data->gen_no, s->used );
  }
}


static void
post_gc_policy( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  semispace_t *to = data->current_space;

  data->must_promote = 0;
  data->must_promote_immediately = 0;
  data->must_collect = 0;

  ss_sync( to );

  /* Expand/contract/promote checking */
  if (!heap->oldest) {
    /* In non-oldest heaps, promote if overflowed */
    if (to->used > data->size_bytes) {
      data->must_promote_immediately = 1;
      supremely_annoyingmsg( "Generation %d decided to promote ASAP "
			     "(used=%d).", data->gen_no, to->used );
    }
    else if (to->used > data->size_bytes/100*data->oflowatermark) {
      data->must_promote = 1;
      supremely_annoyingmsg( "Generation %d decided to promote next time "
			     "(used=%d).", data->gen_no, to->used );
    }
  }
  else {
    /* In oldest heaps, expand by 20% if overflowed. */
    if (to->used > data->size_bytes/100*data->hiwatermark) {
      unsigned n = roundup_page(data->size_bytes/5);
      data->size_bytes += n;
      annoyingmsg( "Expanding generation %d by %u bytes; size=%u.", 
		   data->gen_no, n, data->size_bytes );
    }

    /* If live is gc is under low watermark, contract by 20%.  */
    if (to->used < data->size_bytes/100*data->lowatermark) {
      if (heap->oldest) {
	unsigned n = roundup_page( data->size_bytes/5 );
	data->size_bytes -= n;
	annoyingmsg( "Contracting generation %d by %u bytes; size=%u.",
		    data->gen_no, n, data->size_bytes );
      }
    }
  }
}


static void stats( old_heap_t *heap, int generation, heap_stats_t *stats )
{
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );
  stats->live = data->current_space->used;
  stats->semispace1 = data->current_space->allocated;
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
  DATA(heap)->must_promote_immediately = 0;
  DATA(heap)->must_collect = 0;
  ss_prune( DATA(heap)->current_space );
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
