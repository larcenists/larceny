/* Rts/Sys/old-heap.c
 * Larceny run-time system -- stop-and-copy varsized old heap.
 *
 * $Id: old-heap.c,v 1.13 1997/09/17 15:17:26 lth Exp $
 *
 * An old heap is a heap that receives new objects by promotion from
 * younger heaps, not by direct allocation.  Promotion from younger heaps
 * into an older heap is handled by the older heap itself: it scans
 * itself and all remembered sets of older heaps and copies younger 
 * objects into the heap in the process.
 * 
 * Allocation.  
 *
 * Only one semispace is allocated initially, the other being allocated
 * only when a collection takes place, and then on-demand in smallish chunks.
 * After a collection, the inactive space is released.
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


/* Don't change this :-) */

#define PROMOTE_PREMATURELY     0

typedef struct old_data old_data_t;

struct old_data {
  int  heap_no;
  int  gen_no;
  int  must_promote;           /* 1 if heap overflowed after last gc */
  int  must_promote_immediately;  /* 1 if heap was overfull after last gc */
  int  must_collect;           /* 1 if heap overflowed after last promote */
  int  size_bytes;             /* our notion of target data size */

  int  expansion_fixed;        /* 1 if expansion is fixed-size */
  unsigned expand_fixed;       /* amount to expand by (bytes) */
  unsigned expand_percent;     /* amount to expand by */

  semispace_t *current_space;  /* Space to promote into */

  unsigned hiwatermark;        /* Percent */
  unsigned lowatermark;        /* Percent */
  unsigned oflowatermark;      /* Percent */

  unsigned copied_last_gc;     /* bytes */
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

  annoyingmsg("Heap %d hi_mark=%u, lo_mark=%u, oflo_mark=%u.",
              *gen_no, thiwatermark, tlowatermark, toflowatermark);

  heap = allocate_old_sc_heap( *gen_no, heap_no );
  data = DATA(heap);

  *gen_no = *gen_no + 1;

  data->current_space =
    create_semispace( size_bytes, data->heap_no, data->gen_no );
  assert( data->current_space != 0 );

  data->hiwatermark = thiwatermark;
  data->lowatermark = tlowatermark;
  data->oflowatermark = toflowatermark;
  data->expansion_fixed = 0;
  data->expand_fixed = OLDSPACE_EXPAND_BYTES/1024;
  data->expand_percent = 50;

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
static void set_policy( old_heap_t *heap, int op, unsigned value );

static old_heap_t *
allocate_old_sc_heap( int gen_no, int heap_no )
{
  old_heap_t *heap;
  old_data_t *data;

  heap = (old_heap_t*)must_malloc( sizeof( old_heap_t ) );
  data = heap->data = (old_data_t*)must_malloc( sizeof( old_data_t ) );

  heap->oldest = 0;
  heap->id = "sc/variable";
  heap->code = HEAPCODE_OLD_2SPACE;

  data->gen_no = gen_no;
  data->heap_no = heap_no;
  data->must_promote = 0;
  data->must_promote_immediately = 0;
  data->must_collect = 0;
  data->copied_last_gc = 0;

  heap->initialize = initialize;
  heap->before_promotion = before_promotion;
  heap->after_promotion = after_promotion;
  heap->collect = collect;
  heap->promote_from_younger = promote;
  heap->stats = stats;
  heap->data_load_area = data_load_area;
  heap->set_policy = set_policy;

  return heap;
}


static int 
initialize( old_heap_t *heap )
{
  /* Nothing special to do here, it's all been taken care of. */
  return 1;
}


static void
set_policy( old_heap_t *heap, int op, unsigned value )
{
  old_data_t *data = DATA(heap);

  switch (op) {
  case GCCTL_INCR_FIXED : /* incr-fixed.  Value is kilobytes. */
    data->expansion_fixed = 1;
    data->expand_fixed = value*1024;
    break;
  case GCCTL_INCR_PERCENT : /* incr-percent */
    data->expansion_fixed = 0;
    data->expand_percent = value;
    break;
  case GCCTL_HIMARK : /* himark -- expansion limit */
    if (value > 100) value=100;
    data->hiwatermark = value;
    break;
  case GCCTL_LOMARK : /* lomark -- contraction limit */
    if (value > 100) value=100;
    data->lowatermark = value;
    break;
  }
}


/* 
 * Promote all younger objects into this heap.
 * Precondition: before_promote() has been called for all younger heaps.
 */
static void promote( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  semispace_t *s;
  unsigned used_before;

  if (data->must_promote) {
    if (!heap->oldest) {
      heap->collector->promote_out_of( heap->collector, data->heap_no );
      return;
    }
    else data->must_collect = 1;
  }

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
#if PROMOTE_PREMATURELY
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
  ss_sync( s );
  used_before = s->used;
  gclib_copy_younger_into( heap->collector, s );
  ss_sync( s );

  supremely_annoyingmsg("  Size after promotion: %u.", s->used);

  data->copied_last_gc = s->used - used_before;

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

#if !PROMOTE_PREMATURELY
  data->must_promote = data->must_promote || data->must_promote_immediately;
#endif
  if (data->must_promote && !heap->oldest) {
    heap->collector->promote_out_of( heap->collector, data->heap_no );
    return;
  }

  supremely_annoyingmsg( "Garbage collecting generation %d.", data->gen_no );
  debugmsg( "[debug] old heap (%d,%d): collection.", data->heap_no,
	    data->gen_no );
  stats_gc_type( data->gen_no, STATS_COLLECT );

  from = data->current_space;
  to = create_semispace( OLDSPACE_EXPAND_BYTES, data->heap_no, data->gen_no );
  
  gclib_stopcopy_slow( heap->collector, to );       /* promote&copy */

  data->current_space = to;
  ss_free( from );
  ss_sync( to );

  data->copied_last_gc = to->used;
  supremely_annoyingmsg( "  live after gc: %u", to->used );

  post_gc_policy( heap );
}


static void
post_promote_policy( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  semispace_t *s = data->current_space;
  unsigned oflosize;

  ss_sync( s );
  oflosize = (data->size_bytes/100) * data->oflowatermark;
  data->must_collect = 0;

  /* If heap overflowed during promotion, schedule a collection */
  if (s->used > oflosize) {
    if (data->copied_last_gc > oflosize) {
      data->must_promote = 1;
      supremely_annoyingmsg( "Generation %d decided to promote next time "
                             "(used=%d).", data->gen_no, s->used );
    }
    else {
      data->must_collect = 1;
      supremely_annoyingmsg( "Generation %d decided to collect next time "
			    "(used=%d).", data->gen_no, s->used );
    }
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
    unsigned expanded = 0;

    /* In oldest heaps, expand if overflowed. */

    while (to->used > data->size_bytes/100*data->hiwatermark) {
      if (data->expansion_fixed) {
	data->size_bytes += data->expand_fixed;
	expanded += data->expand_fixed;
      }
      else {
	unsigned n = roundup_page(data->size_bytes/100*data->expand_percent);
	data->size_bytes += n;
	expanded += n;
      }
    }

    if (expanded)
      annoyingmsg( "Expanding generation %d by %u bytes; size=%u.", 
		   data->gen_no, expanded, data->size_bytes );

    /* If live is gc is under low watermark, contract by 33%.  */
    /* FIXME: should be under user control too. */

    if (!expanded && to->used < data->size_bytes/100*data->lowatermark) {
      if (heap->oldest) {
	unsigned n = roundup_page( data->size_bytes/3 );
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
  stats->copied_last_gc = data->copied_last_gc;
  data->copied_last_gc = 0;
  stats->live = data->current_space->used;
  stats->semispace1 = data->current_space->allocated;
  stats->target = data->size_bytes;
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
  DATA(heap)->copied_last_gc = 0;
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
