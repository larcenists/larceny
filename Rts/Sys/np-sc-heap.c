/* Rts/Sys/np-sc-heap.c
 * Larceny run-time system -- copying non-predictive collector.
 *
 * $Id: np-sc-heap.c,v 1.7 1997/03/02 15:56:44 lth Exp $
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


#define DEFAULT_STEPS    4    /* default number of steps */


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

  int must_collect;

  semispace_t *young;         /* young generation */
  semispace_t *old;           /* old generation */
};


#define DATA(x)         ((npsc_data_t*)((x)->data))
#define ceildiv(a,b)    ((a)%(b) == 0 ? (a)/(b) : (a)/(b)+1)


/* Interface */
static int initialize( old_heap_t *h );
static void promote( old_heap_t *h );
static void collect( old_heap_t *h );
static void stats( old_heap_t *h, int generation, heap_stats_t *s );
static void before_promotion( old_heap_t *h );
static void after_promotion( old_heap_t *h );
static word *data_load_area( old_heap_t *, unsigned );


/* Private */
static void internal_promote( old_heap_t *heap, int force_collection );
static void internal_collect( old_heap_t *heap );


old_heap_t *
create_old_np_sc_heap( int *gen_no,          /* add at least 1 to this */
		       int heap_no,          /* this is given */
		       unsigned size_bytes,  /* initial heap size (total) */
		       unsigned step_size    /* size of a step (bytes) */
		      )
{
  old_heap_t *heap;
  npsc_data_t *data;
  int k;

  assert( *gen_no > 0 );

 again:
  heap = (old_heap_t*)malloc( sizeof( old_heap_t ) );
  data = heap->data = (npsc_data_t*)malloc( sizeof( npsc_data_t ) );

  if (heap == 0 || data == 0) {
    if (heap) free( heap );
    if (data) free( data );
    memfail( MF_MALLOC, "old-heap: could not allocate heap metadata." );
    goto again;
  }

  heap->oldest = 0;
  heap->id = "npsc/2/variable";
  heap->data = data;

  data->gen_no = *gen_no;
  data->heap_no = heap_no;

  *gen_no = *gen_no + 2;

  if (size_bytes == 0) size_bytes = DEFAULT_TSIZE;
  size_bytes = roundup_page( size_bytes );
  if (step_size == 0) {
    k = DEFAULT_STEPS;
    step_size = roundup_page( size_bytes / DEFAULT_STEPS );
  }
  else {
    k = ceildiv(size_bytes,step_size);
    step_size = roundup_page( step_size );
  }
  size_bytes = step_size * k;

  data->stepsize = step_size;
  data->new_k = k;
  data->k = k;
  /* This is an OK initial 'j' if the heap is empty.  If the heap
   * is used to load the heap image into, then data_load_area(), below,
   * computes a more appropriate 'j'.
   */
  data->j = ceildiv( k, 3 );

  data->old = create_semispace( size_bytes, data->heap_no, data->gen_no);
  data->young = 0;
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
 * FIXME!
 * Copy data that lives in generations younger than d->gen_no into
 * d->young.ss, making sure to create entries in the remembered set for
 * d->gen_no+1 if pointers into d->gen_no are created; that is, if the
 * non-predictive 'young' generation receives an object that points into
 * the 'old' generation (which is in fact a younger generation from the
 * gc's point of view).
 *
 * The procedure gclib_np_copy_younger does exactly this.
 *
 * If the youngspace filled up during promotion, perform a collection
 * of oldspace right away and swap the generations (see internal_collect()).
 */

static void
internal_promote( old_heap_t *heap, int force_collection )
{
  npsc_data_t *data = DATA(heap);

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

  stats_gc_type( data->gen_no, STATS_PROMOTE );
  
  /* Fast case: promote into 'old' */

  if (data->old->used < (data->k - data->j)*data->stepsize) {
    debugmsg( "[debug] NPSC: promoting into 'old'." );
    gclib_copy_younger_into( heap->collector, data->old );
    ss_sync( data->old );
    if (data->old->used > (data->k - data->j)*data->stepsize)
      annoyingmsg( "Non-predictive 'old' area overflowed by %u bytes.",
		  data->old->used - (data->k - data->j)*data->stepsize );
    return;   /* FIXME: this is wrong if force_collection == 1 */
  }

  /* Slower case: promote into 'young' */

  debugmsg( "[debug] NPSC: promoting into 'young'." );
  gclib_np_copy_younger_into( heap->collector, data->young, ROOTS_AND_SCAN );
  ss_sync( data->young );
  if (data->young->used > data->j*data->stepsize)
    annoyingmsg( "Non-predictive 'young' area overflowed by %u bytes.",
		 data->young->used - data->j*data->stepsize );

  if (data->young->used >= data->j*data->stepsize)
    data->must_collect = 1;
}


/* Collect the non-predictive 'old' generation.
 *
 * FIXME!
 * Now: collect 'old' into 'young', ignoring the remset, then choose a new
 * j so that all live data is in 'old'.

 * from 'young' to 'old'
 * 'old' 

 *
 * What goes on here is that we have
 *
 *   'oldspace' at d->gen_no
 *   'youngspace' at d->gen_no+1
 *   'newspace' at d->gen_no+1
 *
 * There is no live data in any younger generation, because all data
 * in younger generations has been promoted into this heap.  It is known,
 * however, that there exists at least one younger generation than d->gen_no.
 *
 * We want to copy live data from oldspace into newspace and make
 * youngspace the new oldspace and newspace the new youngspace.  We can
 * then free the old oldspace.
 * 
 * For reasons of efficiency, we wish to build up a new remembered set while
 * we are copying and scanning.  This remembered set will remember all those 
 * objects in what is the new newspace that point into what is now youngspace
 * (the future oldspace).
 *
 * We do this in three steps.
 *
 *  (1) Using the normal copying procedure, trace all roots and remembered
 *      sets and forward all immediately reachable objects into the newspace.
 *  (2) Clear the remset at d->gen_no+1 (it's dead), and change the 
 *      generation numbers:
 *         oldspace gets d->gen_no-1   (younger than the heap!)
 *         youngspace gets d->gen_no
 *  (3) Using the non-predictive scanning procedure, scan newspace.  This
 *      will copy all objects in the oldspace into newspace while leaving
 *      youngspace untouched; the remembered set at d->gen_no+1 will in
 *      the process be built up to remember all objects with pointers
 *      into youngspace.
 *
 * At this point, youngspace _is_ the new oldspace, newspace _is_ the new
 * youngspace, and oldspace is garbage.
 */

static void
internal_collect( old_heap_t *heap )
{
  npsc_data_t *d = DATA(heap);
  int live;

  annoyingmsg( "GC in the non-predictive heap: k=%d, j=%d.", d->k, d->j );

  debugmsg( "[debug] NPSC: collecting. k=%d, j=%d", d->k, d->j );
  stats_gc_type( d->gen_no, STATS_COLLECT );

  gclib_copy_younger_into( heap->collector, d->young );
  gclib_set_gen_no( d->young, d->gen_no );
  ss_sync( d->young );

  ss_free( d->old );

  heap->collector->clear_remset( heap->collector, d->gen_no+1 );

  d->old = d->young;
  d->young = 0;

  /* Policy: if, after gc, the heap is nearly full, then add k/5 steps
   * to the heap to avoid thrashing.
   *
   * Then pick j as the midpoint in the free area.
   *
   * We can also shrink the heap by noticing that if there is a lot of
   * space free, then reduce the number of steps.
   * FIXME.
   *
   * Need watermarks.  FIXME.
   */

  live = ceildiv( d->old->used, d->stepsize );
  while (d->k - live < 1) {
    int newsteps = ceildiv( d->k, 5 );
    d->k += newsteps;
    annoyingmsg( "Expanding the heap by %d steps; k=%d.", newsteps, d->k );
  }

  d->j = ceildiv( d->k - live, 2 );
  annoyingmsg( "Finished non-predictive GC; k=%d, j=%d.", d->k, d->j );

  d->must_collect = 0;
}


static void
stats( old_heap_t *heap, int generation, heap_stats_t *stats )
{
  /* Should probably also report the number of steps. */
#if 0
  FIXME
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );
  stats->live = data->current_space->used;
  stats->semispace1 = data->current_space->allocated;
  stats->semispace2 = data->other_space->allocated;
#else
  stats->live = 0;
  stats->semispace1 = 0;
  stats->semispace2 = 0;
#endif
}


static void
before_promotion( old_heap_t *heap )
{
  debugmsg( "[debug] npsc heap(%d,%d):before_promotion.", DATA(heap)->heap_no,
	    DATA(heap)->gen_no );
  /* Nothing, for now */
}


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
