/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- two-generation non-predictive copying dynamic area.
 *
 * The collector sets up the 'old' areas as generation data->gen_no, and
 * the 'young' area as generation data->gen_no+1, to let the existing 
 * write barrier and scanning machinery work.
 */

#define GC_INTERNAL

#include <stdlib.h>

#include "larceny.h"
#include "memmgr.h"
#include "gc.h"
#include "gclib.h"
#include "los_t.h"
#include "gc_t.h"
#include "young_heap_t.h"
#include "old_heap_t.h"
#include "semispace_t.h"
#include "stats.h"
#include "remset_t.h"
#include "static_heap_t.h"

enum action { PROMOTE_TO_OLD, PROMOTE_TO_BOTH, PROMOTE_TO_YOUNG, COLLECT };

#define PHASE_WINDOW 3		/* how much do we take into account? */
#define PHASE_BUFSIZ (2*PHASE_WINDOW) /* history, too */

typedef struct {
  int remset_size;		/* remset size */
  int j;			/* value of j for that size */
} phase_t;

typedef struct npsc_data npsc_data_t;

struct npsc_data {
  stats_id_t self_young;	/* Identity of 'young' generation */
  stats_id_t self_old;		/* Identity of 'old' generation */
  int        gen_no;		/* Generation number of 'old' generation */

  /* Parameters. */
  int size_bytes;             /* Original size */
  int k;                      /* k = current number of steps; k > 0 */
  int j;                      /* j = dividing point; 0 <= j < k */
  int stepsize;               /* bytes */

  /* Policy: j is calculated either as a percentage of free steps, or it's 
     pinned at some value (if possible).  For example, if pin_value == 1
     and there is at least one empty step, then j is set to 1.  
     j_percent and j_pin can't both be -1 at the same time.
     */
  int j_percent;              /* -1 or percentage for calculating j */
  int j_pin;                  /* -1 or value at which to pin j */
  double load_factor;
  double luck;		      /* 0.0 .. 1.0 */
  int remset_limit;	      /* 0 .. INT_MAX */
  int lower_limit;            /* 0 or lower limit on the non-predictive area */
  int upper_limit;	      /* 0 or upper limit on the non-predictive area */
  semispace_t *old;	      /* 'old' generation */
  semispace_t *young;	      /* 'young' generation */

  double  phase_detection;    /* -1.0 or 0.0 .. 1.0 */
  phase_t phase_buf[ PHASE_BUFSIZ ];  /* phase detection data */
  int     phase_idx;	      /* next index in phase_buf */

  gen_stats_t gen_stats_old;
  gen_stats_t gen_stats_young;
  gc_stats_t  gc_stats;
};

#define DATA(x)             ((npsc_data_t*)((x)->data))

static old_heap_t *allocate_heap( int gen_no, gc_t *gc );
static void perform_promote_to_old( old_heap_t *heap );
static void perform_promote_to_both( old_heap_t *heap );
static void perform_collect( old_heap_t *heap );
static enum action decision( old_heap_t *heap );
static int used_young( old_heap_t *heap );
static int used_old( old_heap_t *heap );
static int compute_dynamic_size( old_heap_t *heap, int D, int Q );

old_heap_t *
create_np_dynamic_area( int gen_no, int *gen_allocd, gc_t *gc, np_info_t *info)
{
  old_heap_t *heap;
  npsc_data_t *data;
  int target_size;

  heap = allocate_heap( gen_no, gc );
  data = DATA(heap);

  *gen_allocd = 2;

  /* We have that size_bytes = stepsize*steps */
  data->size_bytes = roundup_page( info->size_bytes );
  data->stepsize = info->stepsize;

  data->j_pin = -1;
  data->j_percent = 50;
  data->load_factor = info->load_factor;
  data->lower_limit = info->dynamic_min;
  data->upper_limit = info->dynamic_max;
  data->remset_limit = info->extra_remset_limit;
  data->luck = info->luck;
  data->phase_detection = info->phase_detection;
  
  /* Assume size/L live (steady state) for initial k */
  target_size = 
    compute_dynamic_size( heap,
			  data->size_bytes / data->load_factor,
			  0 );
  data->k = ceildiv( target_size, info->stepsize );

  /* This is an OK initial j if the heap is empty.  If the heap is used
     to load the heap image into, then data_load_area(), below,
     computes a more appropriate j.
     */
  data->j = ceildiv( data->k, 3 );

  annoyingmsg( "NP collector: k=%d j=%d", data->k, data->j );

  data->old = create_semispace( GC_CHUNK_SIZE, gen_no );
  data->young = create_semispace( GC_CHUNK_SIZE, gen_no+1 );

  heap->maximum = data->stepsize * data->k;
  heap->allocated = 0;

  return heap;
}

void np_gc_parameters( old_heap_t *heap, int *k, int *j )
{
  *k = DATA(heap)->k;
  *j = DATA(heap)->j;
}

/* Update gc stats
   Update gen stats
*/
static void collect( old_heap_t *heap, gc_type_t request )
{
  npsc_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  los_t *los = gc->los;
  int old_before_gc, old_los_before_gc, young_before_gc, young_los_before_gc;
  int type, t1, t2;
  stats_id_t timer1, timer2;

  annoyingmsg( "" );
  annoyingmsg( "Non-predictive dynamic area: garbage collection. " );
  annoyingmsg( "  Live old: %d   Live young: %d  k: %d  j: %d",
	       used_old( heap ), used_young( heap ), data->k, data->j );

  timer1 = stats_start_timer( TIMER_ELAPSED );
  timer2 = stats_start_timer( TIMER_CPU );

  ss_sync( data->old );
  ss_sync( data->young );
  old_before_gc = data->old->used;
  old_los_before_gc = los_bytes_used( gc->los, data->gen_no );
  young_before_gc = data->young->used;
  young_los_before_gc = los_bytes_used( los, data->gen_no+1 );

  type = decision(heap);
  if (request == GCTYPE_COLLECT)
    type = COLLECT;

  switch (type) {
    case PROMOTE_TO_OLD   : perform_promote_to_old( heap ); break;
    case PROMOTE_TO_BOTH  : perform_promote_to_both( heap ); break;
    case PROMOTE_TO_YOUNG : perform_promote_to_both( heap ); break;
    case COLLECT          : perform_collect( heap );  break;
    default               : panic_abort( "Impossible." );
  }

  ss_sync( data->young );
  ss_sync( data->old );

  if (type == COLLECT) {
    data->gc_stats.words_copied += 
      bytes2words( data->old->used - young_before_gc );
    data->gc_stats.words_moved +=
      bytes2words( los_bytes_used( los, data->gen_no ) - young_los_before_gc );
  }
  else {
    data->gc_stats.words_copied += 
      bytes2words( data->young->used - young_before_gc );
    data->gc_stats.words_moved += 
      bytes2words( los_bytes_used( los, data->gen_no+1 )-young_los_before_gc );
  }

  switch( type ) {
  case PROMOTE_TO_OLD :
    data->gen_stats_old.promotions++;
    data->gen_stats_old.ms_promotion += stats_stop_timer( timer1 ); 
    data->gen_stats_old.ms_promotion_cpu += stats_stop_timer( timer2 ); 
    break;
  case PROMOTE_TO_BOTH :
    /* Split the cost -- it probably comes out even in the end and
       it's less obviously wrong than assigning all time to one or the
       other.
       */
    t1 = stats_stop_timer( timer1 ); 
    t2 = stats_stop_timer( timer2 ); 
    data->gen_stats_old.promotions++; /* Only count one  */
    data->gen_stats_old.ms_promotion += t1 / 2;
    data->gen_stats_old.ms_promotion_cpu += t2 / 2;
    data->gen_stats_young.ms_promotion += t1 / 2;
    data->gen_stats_young.ms_promotion_cpu += t2 / 2;
    break;
  case PROMOTE_TO_YOUNG :
    data->gen_stats_young.promotions++;
    data->gen_stats_young.ms_promotion += stats_stop_timer( timer1 ); 
    data->gen_stats_young.ms_promotion_cpu += stats_stop_timer( timer2 ); 
    break;
  case COLLECT : 
    data->gen_stats_old.collections++;
    data->gen_stats_old.ms_collection += stats_stop_timer( timer1 ); 
    data->gen_stats_old.ms_collection_cpu += stats_stop_timer( timer2 ); 
    break;
  }

  annoyingmsg( "Non-predictive dynamic area: collection finished, k=%d j=%d",
	      data->k, data->j );
}

/* Cautious strategy: 
     Let X be the amount of memory allocated in all ephmeral generations.
     Let No be the amount of memory available in the 'old' generation.
     Let Ny be the amount of memory available in the 'young' generation.
     if X <= No then
       promote from ephemeral generations to 'old'
     else if No is small then
       promote from ephemeral generations to 'young'
     else if X <= No+Ny then
       promote from ephemeral generations to 'old' and 'young'
     else
       collect
   */
static enum action decision( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  int X, No, Ny, i;

  /* Gather data */

  X = gc->young_area->allocated;
  for ( i = 0 ; i < gc->ephemeral_area_count ; i++ )
    X += gc->ephemeral_area[i]->allocated;

  No = data->stepsize * (data->k - data->j) - used_old( heap );
  Ny = data->stepsize * data->j - used_young( heap );

  if (X <= No)
    return PROMOTE_TO_OLD;
  else if (No < PAGESIZE)
   return PROMOTE_TO_YOUNG;
  else if (X <= No + Ny)
    return PROMOTE_TO_BOTH;
  else
    return COLLECT;
}

/* Rule: we have a window of PHASE_WINDOW, and a buffer of twice that
   size.  If the older window exhibits growth, and the younger window
   exhibits stability, then we adjust j.  
*/
static int run_phase_detector( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int i, j, k, prev, young, young_before;

  if (data->phase_detection < 0.0) return 0;

  /* Determine if old window exhibits growth */
  /* Old window is window at phase_idx. */
  prev = j = data->phase_idx;
  i = 1;
  do {
    j = (j+1) % PHASE_BUFSIZ;
    if (data->phase_buf[prev].remset_size > data->phase_buf[j].remset_size)
      return 0;			/* shrank */
    k = data->phase_buf[prev].remset_size * (1.0+data->phase_detection);
    if (data->phase_buf[j].remset_size <= k)
      return 0;			/* grown too little */
    prev = j;
    i = i+1;
  } while (i < PHASE_WINDOW);

  /* Determine if young window exhibits stability. */
  /* Young window is window at (phase_idx+PHASE_WINDOW)%PHASE_BUFSIZ. */
     
  young = (data->phase_idx+PHASE_WINDOW) % PHASE_BUFSIZ;

  if (data->phase_buf[j].remset_size > data->phase_buf[young].remset_size)
    return 0;			/* shrank */

  prev = j = young;
  i = 1;
  do {
    j = (j+1) % PHASE_BUFSIZ;
    if (data->phase_buf[prev].remset_size > data->phase_buf[j].remset_size)
      return 0;			/* shrank */
    k = data->phase_buf[prev].remset_size * (1.0+data->phase_detection);
    if (data->phase_buf[j].remset_size > k)
      return 0;			/* grown too much */
    prev = j;
    i = i+1;
  } while (i < PHASE_WINDOW);

  if (data->phase_buf[young].j >= data->j) 
    return 0;

  young_before = used_young( heap );

  annoyingmsg( "Phase detection decided to adjust j "
	       "(old k=%d, old j=%d, used_old=%d, used_young=%d).",
	       data->k, data->j, used_old(heap), young_before );
  annoyingmsg( "Buffer contents (oldest first):" );
  for ( i=0, j=data->phase_idx; i < PHASE_BUFSIZ ; i++, j=(j+1)%PHASE_BUFSIZ )
    annoyingmsg( "  remset_size=%d   j=%d",
		 data->phase_buf[j].remset_size,
		 data->phase_buf[j].j );
  return 1;
}

static void update_phase_data( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int i;

  i = data->phase_idx;
  data->phase_buf[i].remset_size = 
    heap->collector->remset[ heap->collector->np_remset ]->live;
  data->phase_buf[i].j = 
    (data->j*data->stepsize - used_young( heap )) / data->stepsize;
  data->phase_idx = (data->phase_idx+1) % PHASE_BUFSIZ;
}

/* If the extra remset is fuller than allowed by the limit, then
   adjust j and clear the remset.
   */
static int check_for_remset_overflow( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  double size, used, entries, occ;

  size = data->j * data->stepsize;
  used = used_young( heap );
  entries = heap->collector->remset[ heap->collector->np_remset ]->live;

  /* `occ' is the inverse of the heap occupancy.
     If occ is large (heap nearly empty), then do not try to adjust.
     */
  occ = size/(used+1);
  if (occ > 20.0)		/* < 5% full */
    return 0;
  else if (occ*entries > data->remset_limit) {
    annoyingmsg( "NP remembered set overflow: j will be adjusted.\n"
		 "  size=%g used=%g entries=%g occ=%g",
		 size, used, entries, occ );
    return 1;
  }
  else
    return 0;
}

static void adjust_j( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int i, young_before = used_young( heap );
  los_t* los = heap->collector->los;

  /* Adjust data->j. */
  /* Expensive solution:
     Must shuffle some memory blocks and adjust attributes.
     Must scan the NP remset and remove pointers not into the correct area.
     Must scan the young remset and remove pointers not into the correct area.
  */
  /* Cheap solution: shuffle _all_ young into old.  This can be bad if
     the size of young has increased a lot since remset stability, but
     is otherwise a close approximation.
  */

  /* Cheap solution for now */

  /* Move data */
  /* The move changes current, so step _down_! */
  for ( i=data->young->current ; i >= 0 ; i-- )
    ss_move_block_to_semispace( data->young, i, data->old );
  los_append_and_clear_list( los, 
			     los->object_lists[ data->gen_no+1 ], 
			     data->gen_no);

  /* Nuke young space, recreate */
  ss_free( data->young );
  data->young = 
    create_semispace( GC_CHUNK_SIZE, data->gen_no+1 );

  /* Clear or move remset contents */
  rs_assimilate( heap->collector->remset[ data->gen_no ], 
		 heap->collector->remset[ data->gen_no+1 ] );
  rs_clear( heap->collector->remset[ heap->collector->np_remset ] );
  rs_clear( heap->collector->remset[ data->gen_no+1 ] );

  /* Adjust j */
  data->j = (data->j*data->stepsize - young_before) / data->stepsize;
  assert( data->j >= 0 );

  annoyingmsg( "Phase-adjusted value of j=%d", data->j );
}


static void perform_promote_to_old( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  annoyingmsg( "  Promoting into old area." );

  gclib_stopcopy_promote_into( heap->collector, data->old );
  rs_clear( heap->collector->remset[ data->gen_no ] );
}

static void perform_promote_to_both( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int x, young_available, old_available;

  annoyingmsg( "  Promoting to both old and young." );

  if (run_phase_detector( heap ))
    adjust_j( heap );

  young_available = data->j*data->stepsize - used_young( heap );
  old_available = (data->k-data->j)*data->stepsize - used_old( heap );

  gclib_stopcopy_promote_into_np( heap->collector,
				  data->old,
				  data->young,
				  old_available,
				  young_available);

  rs_clear( heap->collector->remset[ data->gen_no ] );
  rs_assimilate( heap->collector->remset[ heap->collector->np_remset ],
		 heap->collector->remset[ data->gen_no+1 ] );
  rs_clear( heap->collector->remset[ data->gen_no+1 ] );

  /* This adjustment is only required when a large object has been promoted
     into the 'old' space and overflowed it.

     Here it's ok to just adjust j without shuffling any data from one
     area to the other, because we're just accomodating data that's in 
     the old area in any case.
     */
  x = used_old( heap );
  while ((data->k - data->j)*data->stepsize < x)
    data->j--;

  if (data->j < 0) {
    data->k = ceildiv( x, data->stepsize );
    data->j = 0;
    annoyingmsg( "Extending NP area to accomodate large object overflow: "
		 "k=%d, j=%d.",
		 data->k, data->j );
  }

  if (check_for_remset_overflow( heap ))
    adjust_j( heap );
  update_phase_data( heap );
}

static void perform_collect( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  los_t *los = heap->collector->los;
  gc_t *gc = heap->collector;
  int free_steps, target_size, young_before, young_los_before, luck_steps;

  if (run_phase_detector( heap ))
    adjust_j( heap );

  ss_sync( data->young );
  young_before = data->young->used;
  young_los_before = los_bytes_used( los, data->gen_no+1 );

  annoyingmsg( "  Full garbage collection." );

  gclib_stopcopy_collect_np( gc, data->young );
  rs_clear( gc->remset[ data->gen_no ] );
  rs_clear( gc->remset[ data->gen_no+1 ] );
  rs_clear( gc->remset[ gc->np_remset ] );

  /* Manipulate the semispaces: young becomes old, old is deallocated */
  ss_free( data->old );
  data->old = data->young;

  ss_set_gen_no( data->old, data->gen_no );
  assert( los_bytes_used( los, data->gen_no ) == 0 );
  los_append_and_clear_list( los, 
			     los->object_lists[ data->gen_no+1 ], 
			     data->gen_no);

  data->young = create_semispace( GC_CHUNK_SIZE, data->gen_no+1);

  /* Compute new k and j, and other policy parameters */
  /* Young is empty */
  /* What should the new k be?
   *
   * At the time of the next gc, whatever's in 1..j will not be copied,
   * so no space need be set aside for it.  
   *
   * However, when the new heap size is computed, only the load factor
   * L is taken into account, so space is reserved for the live storage
   * from 1..j.  The amount of space so reserved is j*stepsize (because
   * we assume that all of 1..j is live).  If j is not close to 0, this can
   * be a considerable amount of space.  Also, assuming that 1..j is live,
   * then as 1..j approaches heapsize/L, live data in j+1..k will approach 0,
   * requiring copyspace that also approaches 0, whereas reserved copyspace
   * approaches heapsize/L.
   *
   * We can use the space in two ways:
   *  - Keep it as part of copyspace, effectively reducing L.  Since
   *    efficiency is improved as L increases, this is an undesirable
   *    solution, but it's easy.
   *  - Split the space among the steps j+1..k and copyspace in a
   *    proportion based on L; i.e., increase k.  This uses memory
   *    appropriately, but is harder to implement because the choices
   *    of j, k, and heap size are interdependent.
   *
   * Below, I use the former method, for simplicity, but note the use
   * of the luck parameter to adjust k, below.
   */
  ss_sync( data->old );
  target_size =
    compute_dynamic_size( heap,
			  data->old->used,
			  los_bytes_used( los, data->gen_no ) );
  data->k = ceildiv( target_size, data->stepsize );

  free_steps = (data->k * data->stepsize - used_old( heap )) / data->stepsize;
  if (free_steps < 0) {
    /* Soft overflow in a fixed heap. */
    /* It might be more reasonable to give up at this point. */
    free_steps = 0;
  }

  if (data->j_percent >= 0)
    data->j = (free_steps * data->j_percent) / 100;
  else if (free_steps >= data->j_pin)
    data->j = data->j_pin;
  else {
    data->j = free_steps / 2;
    supremely_annoyingmsg( "  Could not pin j at %d; chose %d instead",
			   data->j_pin, data->j );
  }

  /* I know what you're thinking, punk. You're thinking, did he fire six 
     shots or only five?  Well in all the excitement I've forgotten myself.
     So you have to ask yourself, do I feel lucky?  Well, do you, punk?

     (Thanks to Arthur for the quote.)
     */
  luck_steps = (int)(data->j*data->luck);
  data->k += luck_steps;

  annoyingmsg( "  Adjusting parameters: k=%d j=%d, luck=%d", 
	       data->k, data->j, luck_steps );
  assert( data->j >= 0 );
  assert( data->k >= 0 );

  update_phase_data( heap );
}

static void stats( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int live_los;

  live_los = los_bytes_used( heap->collector->los, data->gen_no );
  data->gen_stats_old.target = 
    bytes2words( data->stepsize * (data->k-data->j) );
  data->gen_stats_old.allocated = 
    bytes2words( data->old->allocated + live_los );
  data->gen_stats_old.used = bytes2words( used_old( heap ) + live_los );
  stats_add_gen_stats( data->self_old, &data->gen_stats_old );
  memset( &data->gen_stats_old, 0, sizeof( gen_stats_t ) );
  rs_stats( heap->collector->remset[ data->gen_no ] );

  live_los = los_bytes_used( heap->collector->los, data->gen_no+1 );
  data->gen_stats_young.target = bytes2words( data->stepsize * data->j );
  data->gen_stats_young.allocated = 
    bytes2words( data->young->allocated + live_los );
  data->gen_stats_young.used = bytes2words( used_young( heap ) + live_los );
  stats_add_gen_stats( data->self_young, &data->gen_stats_young );
  memset( &data->gen_stats_young, 0, sizeof( gen_stats_t ) );
  rs_stats( heap->collector->remset[ data->gen_no + 1] );
  rs_stats( heap->collector->remset[ heap->collector->np_remset ] );

  data->gc_stats.np_j = data->j;
  data->gc_stats.np_k = data->k;
  stats_add_gc_stats( &data->gc_stats );
  memset( &data->gc_stats, 0, sizeof( gc_stats_t ) );
}

static void before_collection( old_heap_t *heap )
{
  heap->allocated = used_old( heap ) + used_young( heap );
  heap->maximum = DATA(heap)->stepsize * DATA(heap)->k;
}

static void after_collection( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;

  heap->allocated = used_old( heap ) + used_young( heap );
  heap->maximum = data->stepsize * data->k;

  annoyingmsg( "  Generation %d (non-predictive old):  Size=%d, Live=%d, "
	       "Remset live=%d",
	       data->old->gen_no,
	       data->stepsize * (data->k - data->j), used_old( heap ),
	       gc->remset[ data->old->gen_no ]->live );
  annoyingmsg( "  Generation %d (non-predictive young):  Size=%d, Live=%d, "
	       "Remset live=%d",
	       data->young->gen_no,
	       data->stepsize * data->j, used_young( heap ),
	       gc->remset[ data->young->gen_no ]->live );
  annoyingmsg( "  Non-predictive parameters: k=%d, j=%d, Remset live=%d",
	       data->k, data->j, gc->remset[ gc->np_remset ]->live );
}

static void set_policy( old_heap_t *heap, int op, int value )
{
  npsc_data_t *data = DATA(heap);

  switch (op) {
  case GCCTL_J_FIXED : /* j-fixed */
    data->j_pin = value;
    if (data->j > value) data->j = value;  /* Hack. */
    break;
  case GCCTL_J_PERCENT : /* j-percent */
    data->j_pin = -1;
    data->j_percent = value;
    break;
  }
}

static word *data_load_area( old_heap_t *heap, int nbytes )
{
  npsc_data_t *data = DATA(heap);
  int n;

  assert( nbytes > 0 );
  assert( nbytes % BYTE_ALIGNMENT == 0 );

  n = ss_allocate_and_insert_block( data->old, nbytes );
  return data->old->chunks[ n ].bot;
}

static int used_young( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  ss_sync( data->young );
  return
   data->young->used + los_bytes_used( heap->collector->los, data->gen_no+1 );
}

static int used_old( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  ss_sync( data->old );
  return 
   data->old->used + los_bytes_used( heap->collector->los, data->gen_no );
}

/* Given the amount of live collectable small data (D) and large data (Q),
   return the total amount of memory that may be allocated (including
   that which is currently allocated) before the next collection.
   */
static int compute_dynamic_size( old_heap_t *heap, int D, int Q )
{
  static_heap_t *s = heap->collector->static_area;
  int S = (s ? s->allocated : 0);
  double L = DATA(heap)->load_factor;
  int upper_limit = DATA(heap)->upper_limit;
  int lower_limit = DATA(heap)->lower_limit;

  return gc_compute_dynamic_size( heap->collector,
				  D, S, Q, L, lower_limit, upper_limit );
}

static old_heap_t *allocate_heap( int gen_no, gc_t *gc )
{
  old_heap_t *heap;
  npsc_data_t *data;
  int i;

  data = (npsc_data_t*)must_malloc( sizeof( npsc_data_t ) );
  heap = create_old_heap_t( "npsc/2/variable",
			    HEAPCODE_OLD_2SPACE_NP,
			    0,               /* initialize */
			    collect,
			    before_collection,
			    after_collection,
			    stats,
			    data_load_area,
			    0,               /* FIXME: load_prepare */
			    0,               /* FIXME: load_data */
			    set_policy,
			    data
			   );
  heap->collector = gc;

  data->self_old = stats_new_generation( gen_no, 0 );
  data->self_young = stats_new_generation( gen_no+1, 0 );
  data->gen_no = gen_no;
  for ( i=0 ; i < PHASE_BUFSIZ ; i++ ) {
    data->phase_buf[i].remset_size = 0;
    data->phase_buf[i].j = 0;
  }
  data->phase_idx = 0;

  memset( &data->gen_stats_young, 0, sizeof( gen_stats_t ) );
  memset( &data->gen_stats_old, 0, sizeof( gen_stats_t ) );
  memset( &data->gc_stats, 0, sizeof( gc_stats_t ) );

  return heap;
}

/* eof */
