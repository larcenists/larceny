/* Rts/Sys/np-sc-heap.c
 * Larceny run-time system -- non-predictive copying collector.
 *
 * $Id: np-sc-heap.c,v 1.12 1997/09/17 15:17:26 lth Exp lth $
 *
 * The collector divides the heap into two generations.  Each generation
 * is represented by one semispace_t data type: one for the old generation,
 * and one for the young.  From the point of view of the gc, the 'young'
 * generation is older (it has the higher generation number).
 *
 * Allocation in the heap happens exclusively by promotion into the 'young'
 * generation, and collection in the 'old' generation is triggered if the
 * promotion overflows the 'young' generation.
 *
 * The heap creation procedure takes the size of the heap and the size of
 * a step as its parameters.  The initial number of steps is the heap-size
 * divided by the step-size, suitably fudged.
 *
 * Steps are added as the heap becomes fuller; see the "Policy" section
 * in internal_collect(), below.
 *
 * The collection algorithm is described in comments around the procedures
 * internal_promote() and internal_collect().
 *
 * The code in this file is reentrant.
 */


#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

typedef struct npsc_data npsc_data_t;
typedef struct gen gen_t;

struct gen {
  unsigned    steps;          /* number of steps in this generation */
  semispace_t *ss;            /* generation's data, or 0 */
};

struct npsc_data {
  int heap_no;                /* heap number of this heap */
  int gen_no;                 /* generation number of youngest generation */

  int k;                      /* k = current number of steps */
  int j;                      /* j = dividing point */
  int new_k;                  /* desired number of steps when new */

  unsigned stepsize;          /* size of a step in bytes */

  /* Policy */

  unsigned hi_mark;           /* expansion watermark in percent */
  unsigned lo_mark;           /* contraction watermark */
  unsigned oflo_mark;         /* promotion watermark (unused) */

  int must_collect;           /* 1 if we must collect */
  int j_percent;              /* percent: for calculating j */
  int pin_j;                  /* 1 if j should be pinned (modulo impossible) */
  int pin_value;              /* value to pin at */
  int expansion_fixed;        /* 1 if expansion amount is fixed; 0 if not */
  int expand_fixed;           /* # of steps to expand by */
  int expand_percent;         /* percentage to expand by */

  semispace_t *young;         /* young generation */
  semispace_t *old;           /* old generation */

  unsigned copied_last_gc_old;  /* bytes */
  unsigned copied_last_gc_young;  /* bytes */
};

#define DATA(x)         ((npsc_data_t*)((x)->data))
#define ceildiv(a,b)    ((a)%(b) == 0 ? (a)/(b) : (a)/(b)+1)


static void internal_promote( old_heap_t *heap, int force_collection );
static void internal_collect( old_heap_t *heap );
static old_heap_t *allocate_heap( unsigned gen_no, unsigned heap_no );
static void np_expansion_policy( old_heap_t *heap );


old_heap_t *
create_old_np_sc_heap( int *gen_no,          /* add at least 1 to this */
		       int heap_no,          /* this is given */
		       unsigned step_size,   /* size of a step (bytes) */
		       unsigned steps,       /* number of steps */
		       unsigned size_bytes,  /* initial heap size (total) */
		       unsigned hi_mark,     /* expansion watermark (%) */
		       unsigned lo_mark,     /* contraction watermark (%) */
		       unsigned oflo_mark    /* promotion watermark (%) */
		      )
{
  old_heap_t *heap;
  npsc_data_t *data;


  /* Parameter validation */

  assert( *gen_no > 0 );   /* This can't be the youngest heap */

  if (hi_mark == 0 || hi_mark > 100)
    hi_mark = DEFAULT_NP_HIWATERMARK;
  if (lo_mark == 0 || lo_mark > 100)
    lo_mark = DEFAULT_NP_LOWATERMARK;
  if (oflo_mark == 0 || oflo_mark > 100)
    oflo_mark = DEFAULT_NP_OFLOWATERMARK;

  annoyingmsg("Non-predictive hi_mark=%u, lo_mark=%u, oflo_mark=%u.",
              hi_mark, lo_mark, oflo_mark);

  /* Allocation */

  heap = allocate_heap( *gen_no, heap_no );
  data = DATA(heap);

  *gen_no = *gen_no + 2;

  if (steps == 0 && step_size == 0) {
    if (size_bytes == 0) {
      steps = DEFAULT_STEPS;
      step_size = DEFAULT_STEPSIZE;
      size_bytes = step_size * steps;
    }
    else {
      step_size = DEFAULT_STEPSIZE;
      steps = ceildiv( size_bytes, step_size );
    }
  }
  else if (steps != 0 && step_size == 0) {
    if (size_bytes == 0) {
      step_size = DEFAULT_STEPSIZE;
      size_bytes = step_size * steps;
    }
    else
      step_size = size_bytes / steps;
  }
  else if (steps == 0 && step_size != 0) {
    if (size_bytes == 0) {
      steps = DEFAULT_STEPS;
      size_bytes = step_size * steps;
    }
    else
      steps = size_bytes / step_size;
  }
  else {
    size_bytes = step_size * steps;
  }

  size_bytes = roundup_page( size_bytes );

  data->stepsize = step_size;
  data->new_k = steps;
  data->k = steps;

  /* This is an OK initial 'j' if the heap is empty.  If the heap
   * is used to load the heap image into, then data_load_area(), below,
   * computes a more appropriate 'j'.
   */
  data->j = ceildiv( steps, 3 );

  /* Defaults */
  data->pin_j = 0;
  data->pin_value = 1;
  data->j_percent = 50;
  data->expansion_fixed = 0;
  data->expand_fixed = 1;
  data->expand_percent = 50;

  data->old = create_semispace( size_bytes, data->heap_no, data->gen_no);
  data->young = 0;
  data->hi_mark = hi_mark;
  data->lo_mark = lo_mark;
  data->oflo_mark = oflo_mark;


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
allocate_heap( unsigned gen_no, unsigned heap_no )
{
  old_heap_t *heap;
  npsc_data_t *data;

  heap = (old_heap_t*)must_malloc( sizeof( old_heap_t ) );
  data = heap->data = (npsc_data_t*)must_malloc( sizeof( npsc_data_t ) );

  data->gen_no = gen_no;
  data->heap_no = heap_no;
  data->copied_last_gc_old = 0;
  data->copied_last_gc_young = 0;
  data->must_collect = 0;

  heap->oldest = 0;
  heap->id = "npsc/2/variable";
  heap->code = HEAPCODE_OLD_2SPACE_NP;

  heap->data = data;

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
  npsc_data_t *data = DATA(heap);

  switch (op) {
  case GCCTL_J_FIXED : /* j-fixed */
    data->pin_j = 1;
    data->pin_value = value;
    if (data->j > value) data->j = value;  /* Hack. */
    break;
  case GCCTL_J_PERCENT : /* j-percent */
    data->pin_j = 0;
    data->j_percent = value;
    break;
  case GCCTL_INCR_FIXED : /* incr-fixed */
    data->expansion_fixed = 1;
    data->expand_fixed = value;
    break;
  case GCCTL_INCR_PERCENT : /* incr-percent */
    data->expansion_fixed = 0;
    data->expand_percent = value;
    break;
  case GCCTL_LOMARK : /* lomark -- contraction limit */
    if (value > 100) value = 100;
    data->lo_mark = value;
    break;
  }
}

/* 
 * Promote all younger objects into this heap.
 * Precondition: before_promote() has been called for all younger heaps.
 */
static void
promote( old_heap_t *heap )
{
  internal_promote( heap, 0 );
}


/*
 * Collect this and all younger heaps.
 * Precondition: before_promote() has been called for all younger heaps.
 */
static void
collect( old_heap_t *heap )
{
  internal_promote( heap, 1 );
}


/* Promote data into the non-predictive collected heap.
 *
 * Data from generations younger than data->gen_no are promoted into
 * the 'old' or the 'young' spaces depending on policy decisions.
 *
 * Currently the policy decisions are primitive: if the 'old' space
 * is not full, promote into it, otherwise, promote into the 'young' space.
 * If the young space is full, call the collector.
 *
 * If a space overflows during promotion, the overflow data is kept with
 * the space.
 *
 * A somewhat better policy is to get some hints from the collector about
 * how much data we can expect to be promoted in, and decide the semispace
 * based on the estimate.  For example, if there is 1 step of free space in
 * 'old' but the estimate for live data is (considerably) larger than that
 * (more than 1 step, say), then promote into 'young' instead.  It is then
 * possible to move some data from 'young' to 'old' after promotion to fill
 * the available slot -- increasing j, as it were.  This movement does not
 * require copying.
 *
 * A reasonable estimate of data promoted in would be the fraction of the
 * next younger heap that was live after the last GC in that heap times the
 * amount of heap memory in use in all younger generations.
 */

static void
internal_promote( old_heap_t *heap, int force_collection )
{
  npsc_data_t *data = DATA(heap);
  unsigned used_before;

  if (data->old == 0)
    data->old =
      create_semispace( OLDSPACE_EXPAND_BYTES, data->heap_no, data->gen_no );

  if (data->young == 0)
    data->young =
      create_semispace( OLDSPACE_EXPAND_BYTES, data->heap_no, data->gen_no+1 );

  if (force_collection || data->must_collect) {
    internal_collect( heap );
    return;
  }

  ss_sync( data->old );
  if (data->old->used < (data->k - data->j)*data->stepsize) {
    debugmsg( "[debug] NPSC: promoting into 'old'." );
    stats_gc_type( data->gen_no, STATS_PROMOTE );

    used_before = data->old->used;
    gclib_copy_younger_into( heap->collector, data->old );
    ss_sync( data->old );
    data->copied_last_gc_old = data->old->used - used_before;

    if (data->old->used > (data->k - data->j)*data->stepsize)
      annoyingmsg( "Non-predictive 'old' area overflowed by %u bytes.",
		  data->old->used - (data->k - data->j)*data->stepsize );

/* disabled this since we have the new remembered set */
#if 0 && NP_EXTRA_REMSET
    heap->collector->np_merge_and_clear_remset( heap->collector,
					        data->gen_no+1 );
#endif
  }
  else if (data->j > 0) {
    debugmsg( "[debug] NPSC: promoting into 'young'." );
    stats_gc_type( data->gen_no+1, STATS_PROMOTE );

    ss_sync( data->young );
    used_before = data->young->used;

    gclib_np_copy_younger_into( heap->collector, data->young, ROOTS_AND_SCAN );
    ss_sync( data->young );
    data->copied_last_gc_young = data->young->used - used_before;

    if (data->young->used > data->j*data->stepsize)
      annoyingmsg( "Non-predictive 'young' area overflowed by %u bytes.",
		  data->young->used - data->j*data->stepsize );

/* disabled this since we have the new remembered set */
#if 0 && NP_EXTRA_REMSET
    heap->collector->np_merge_and_clear_remset( heap->collector,
					        data->gen_no+1 );
#endif

    if (data->young->used >= data->j*data->stepsize)
      data->must_collect = 1;
  }
  else 
    internal_collect( heap );
}


/* Collect the non-predictive 'old' generation.
 *
 * Data in 'old' and younger generations are copied into 'young', using
 * the normal remembered sets and the extra non-predictive set as roots.
 *
 * After the collection, new values of k (#steps) and j (split between old
 * and new) are chosen.
 *
 * FIXME: policy code is crude. 
 */

static void
internal_collect( old_heap_t *heap )
{
  npsc_data_t *d = DATA(heap);
  unsigned old_before, young_before, old_after;

  annoyingmsg( "GC in the non-predictive heap: k=%d, j=%d.", d->k, d->j );
  debugmsg( "[debug] NPSC: collecting. k=%d, j=%d", d->k, d->j );

  stats_gc_type( d->gen_no, STATS_COLLECT );

  /* Setting this flag causes the np_remset to be scanned. */
  heap->collector->set_np_collection_flag( heap->collector );

  ss_sync( d->young );
  ss_sync( d->old );
  young_before = d->young->used;
  old_before = d->old->used;
  gclib_copy_younger_into( heap->collector, d->young );

  /* Manipulate the semispaces: young becomes old, old is deallocated */
  gclib_set_gen_no( d->young, d->gen_no );
  ss_sync( d->young );
  old_after = d->young->used;
  ss_free( d->old );
  d->old = d->young;
  d->young = 0;
  d->copied_last_gc_old = old_after - young_before;

  supremely_annoyingmsg( "  old before: %u, young before: %u, old after: %d"
			 "\n  copied: %u",
			 old_before, young_before, old_after,
			 d->copied_last_gc_old );
			 
  /* Clear remembered sets not cleared by the collector infrastructure. */
  heap->collector->clear_remset( heap->collector, d->gen_no+1 );
#if NP_EXTRA_REMSET
  heap->collector->clear_np_remset( heap->collector );
#endif

  np_expansion_policy( heap );

  annoyingmsg( "Finished non-predictive GC; k=%d, j=%d.", d->k, d->j );
}


/* Expansion policy: The heap expansion and contraction policy is based
 * on high and low watermarks.  If, after a collection, the heap memory
 * in use for the NP heap exceeds the high watermark, then the heap
 * is expanded in increments of 20% of its current size until the
 * memory in use does not exceed the watermark.  (That's what the loop
 * is for: I'm sure there's a closed formula to determine it in one
 * step, but the loop is easy.)
 *
 * Ditto, if, after a collection, the heap memory in use for the NP
 * heap is below the low watermark, then the heap is contracted.  The
 * contraction is by up to 20% of the current size of the heap; however,
 * a contraction may not reduce the ratio of used space to free space
 * below the high watermark.
 *
 * Observe that expansion is more aggressive than contraction.  
 *
 * The policy is experimental; tuning is probably necessary.
 */

static void np_expansion_policy( old_heap_t *heap )
{
  npsc_data_t *d = DATA(heap);
  int live_steps;
  int expanded = 0;

  live_steps = ceildiv( d->old->used, d->stepsize );

  /* Expansion code */
  while (1) {
    int newsteps;
    int must_expand = 0;
    unsigned hi_target = (d->k * d->stepsize)/100*d->hi_mark;

    if (d->old->used > hi_target) {
      supremely_annoyingmsg( "Expanding NP heap because it's over watermark:\n"
			     "  used=%u, target=%u",
			     d->old->used, hi_target );
      must_expand = 1;
    }
    if (d->k - live_steps < 1) {
      supremely_annoyingmsg( "Expanding NP heap because it's full:\n"
			     "  k=%d, live_steps=%d",
			     d->k, live_steps );
      must_expand = 1;
    }

    if (!must_expand) break;

    expanded = 1;
    if (d->expansion_fixed)
      newsteps = d->expand_fixed;
    else {
      newsteps = max( 1, ceildiv( d->k*d->expand_percent, 100 ) );
      /* newsteps = ceildiv( d->k, 5 ); */
    }
    d->k += newsteps;
    annoyingmsg( "Expanding NP heap by %d steps; k=%d.", newsteps, d->k );
  }

  /* Contraction code */
#if 0
  if (!expanded) {
    unsigned lo_target = (d->k * d->stepsize)/100*d->lo_mark;

    if (d->old->used < lo_target) {
      unsigned hi_target;
      unsigned minus_steps;

      minus_steps = ceildiv( d->k, 5 );
      while (1) {
	hi_target = ((d->k - minus_steps) * d->stepsize)/100*d->hi_mark;
	if (d->old->used <= hi_target) break;
	minus_steps--;
      }
      if (minus_steps > 0) {
	d->k -= minus_steps;
	annoyingmsg( "Contracting NP heap by %d steps; k=%d.", minus_steps,
		     d->k );
      }
    }
  }
#endif

  assert( d->k >= live_steps );

  if (d->pin_j)
    d->j = min( d->k - live_steps, d->pin_value );
  else {
    /* OLD: d->j = ceildiv( d->k - live_steps, 2 ); */
    d->j = ceildiv( (d->k - live_steps)*d->j_percent, 100 );
  }

  d->must_collect = 0;
}


static void
stats( old_heap_t *heap, int generation, heap_stats_t *stats )
{
  npsc_data_t *data = DATA(heap);
  semispace_t *space;

  if (generation == data->gen_no) {
    space = data->old;
    stats->np_old = 1;
    stats->copied_last_gc = data->copied_last_gc_old;
    data->copied_last_gc_old = 0;
    stats->target = data->stepsize * (data->k-data->j);
  }
  else {
    space = data->young;
    stats->np_young = 1;
    stats->copied_last_gc = data->copied_last_gc_young;
    data->copied_last_gc_young = 0;
    stats->target = data->stepsize * data->j;
  }

  if (space != 0) {
    ss_sync( space );
    stats->live = space->used;
    stats->semispace1 = space->allocated;
    stats->semispace2 = 0;
  }

  /* These are the same for both generations; stats module must make sense
   * of it.
   */
  stats->np_j = data->j;
  stats->np_k = data->k;
}


static void
before_promotion( old_heap_t *heap )
{
  debugmsg( "[debug] npsc heap(%d,%d):before_promotion.", DATA(heap)->heap_no,
	    DATA(heap)->gen_no );
  /* Nothing, for now */
}


/* This code has never been tested -- the NP heap has always been oldest. */
static void
after_promotion( old_heap_t *heap )
{
  npsc_data_t *d = DATA(heap);

  debugmsg("[debug] npsc heap(%d,%d):after_promotion.", d->heap_no, d->gen_no);
  
  if (d->young) {
    ss_free( d->young );
    d->young = 0;
  }
  if (d->old) {
    ss_free( d->old );
    d->old = 0;
  }
  d->k = d->new_k;
  d->j = 0;
  d->must_collect = 0;
  d->copied_last_gc_old = 0;
  d->copied_last_gc_young = 0;
}


static word *
data_load_area( old_heap_t *heap, unsigned nbytes )
{
  npsc_data_t *data = DATA(heap);
  chunk_t *c = &data->old->chunks[data->old->current];

  if ((c->lim - c->top)*sizeof(word) >= nbytes) {
    word *p = c->top;
    c->top += nbytes/sizeof(word);
    data->j = ((c->lim - c->top)/2 / data->stepsize);  /* may be 0 */
    return p;
  }
  else
    return 0;
}

/* eof */
