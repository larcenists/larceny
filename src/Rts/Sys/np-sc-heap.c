/* Copyright 1998 Lars T Hansen.         -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Two-generation non-predictive/renewal-older-first copying dynamic area.
 *
 * For general motivation and some theory, see
 *   William D Clinger and Lars T Hansen, "Generational garbage
 *   collection and the radioactive decay model," ACM PLDI 1997.
 *   See <ftp://ftp.ccs.neu.edu/pub/people/will/radioactive.ps.gz>.
 */

#define GC_INTERNAL

#define FLOAT_REDUCTION  0
  /* Define this to 1 to do a mark-and-remset-sweep before every ROF 
     collection and 2 to do ditto before every collection.

     Normally this should be 0!
     */

#define REMSET_TRACE     0
  /* Set this to 1 to dump a record of the remembered set sizes following
     every garbage collection.
     
     Normally this should be 0!
     */

#include <stdlib.h>

#include "larceny.h"

#if ROF_COLLECTOR

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
#include "remset_np_t.h"
#include "static_heap_t.h"

enum action { PROMOTE_TO_OLD, PROMOTE_TO_BOTH, PROMOTE_TO_YOUNG, COLLECT };

/* Phase detection mechanism: the window size is PHASE_WINDOW,
   and the buffer has space for two windows (most recent, and
   history).
   */
typedef struct {                /* Phase detection data */
  int remset_size;              /* remset size */
  int j;                        /* value of j for that size */
} phase_t;

#define PHASE_WINDOW   4
#define PHASE_BUFSIZ   (2*PHASE_WINDOW)

typedef struct npsc_data npsc_data_t;

struct npsc_data {
  /* The generations */
  int         gen_no;           /* Generation number of 'old' generation */
  stats_id_t  self_young;       /* Identity of 'young' generation */
  stats_id_t  self_old;         /* Identity of 'old' generation */
  semispace_t *old;             /* 'old' generation */
  semispace_t *young;           /* 'young' generation */

  /* Parameters. */
  int         k;                /* Current number of steps; k > 0 */
  int         j;                /* Number of young steps; 0 <= j < k */
  int         stepsize;         /* Step size in bytes */

  /* Policy */
  int         target_size;      /* Current max allocation */
  int         j_percent;        /* -1 or percentage for calculating j */
  int         j_pin;            /* -1 or value at which to pin j */
  double      load_factor;      /* Really L, the inverse load */
  double      luck;             /* 0.0..1.0 */
  int         remset_limit;     /* 0..INT_MAX, the NP remset limit (entries) */
  int         lower_limit;      /* 0 or lower limit on the area (bytes) */
  int         upper_limit;      /* 0 or upper limit on the area (bytes) */
  double      phase_detection;  /* -1.0 or 0.0..1.0, remset growth factor */
  int         phase_idx;        /* Next index in phase_buf */
  phase_t     phase_buf[ PHASE_BUFSIZ ];  /* Phase detection data */

  /* Stats */
  gen_stats_t gen_stats_old;    /* Stats structure for 'old' generation */
  gen_stats_t gen_stats_young;  /* Stats structure for 'young' generation */
  gc_stats_t  gc_stats;         /* Stats structure for the dynamic area */
#if GC_EVENT_COUNTERS
  gc_event_stats_t event_stats; /* Event counters */
#endif
};

#define DATA(x) ((npsc_data_t*)((x)->data))

static old_heap_t *allocate_heap( int gen_no, gc_t *gc );
static void perform_promote_to_old( old_heap_t *heap );
static void perform_promote_to_both( old_heap_t *heap );
static void perform_collect( old_heap_t *heap );
static enum action decision( old_heap_t *heap );
static int  used_young( old_heap_t *heap );
static int  used_old( old_heap_t *heap );
static int  compute_dynamic_size( old_heap_t *heap, int D, int Q );
static int  used_in_space( semispace_t *ss );
#if FLOAT_REDUCTION
static void full_collection( old_heap_t *heap );
#endif
#if REMSET_TRACE
static void remset_trace( old_heap_t *heap, char *type );
#endif

old_heap_t *
create_np_dynamic_area( int gen_no, int *gen_allocd, gc_t *gc, np_info_t *info)
{
  old_heap_t *heap;
  npsc_data_t *data;
  int size_bytes;

  *gen_allocd           = 2;

  heap                  = allocate_heap( gen_no, gc );
  data                  = DATA(heap);
  size_bytes            = roundup_page( info->size_bytes );

  data->stepsize        = info->stepsize;
  data->j_pin           = -1;
  data->j_percent       = 50;
  data->load_factor     = info->load_factor;
  data->lower_limit     = info->dynamic_min;
  data->upper_limit     = info->dynamic_max;
  data->remset_limit    = info->extra_remset_limit;
  data->luck            = info->luck;
  data->phase_detection = info->phase_detection;
  
  /* Assume size/L live (steady state) for initial k */
  data->target_size = 
    compute_dynamic_size( heap,
                          size_bytes / data->load_factor,
                          0 );
  data->k = data->target_size / info->stepsize; /* Round down */

  /* k/3 is an OK initial j if the static area is separate. */
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

static void collect( old_heap_t *heap, gc_type_t request )
{
  npsc_data_t *data = DATA(heap);
  int type, t1, t2;
  stats_id_t timer1, timer2;

  annoyingmsg( "ROF dynamic area GC.  Live old=%d  Live young=%d  k=%d  j=%d",
               used_old( heap ), used_young( heap ), data->k, data->j );

  timer1 = stats_start_timer( TIMER_ELAPSED );
  timer2 = stats_start_timer( TIMER_CPU );

  if (request == GCTYPE_COLLECT)
    type = COLLECT;
  else
    type = decision(heap);

  switch( type ) {
  case PROMOTE_TO_OLD :
    perform_promote_to_old( heap );
    data->gen_stats_old.promotions++;
    data->gen_stats_old.ms_promotion += stats_stop_timer( timer1 ); 
    data->gen_stats_old.ms_promotion_cpu += stats_stop_timer( timer2 ); 
#if REMSET_TRACE
    remset_trace( heap, "rof-promote-old" );
#endif
    break;
  case PROMOTE_TO_BOTH :
    perform_promote_to_both( heap );
    /* Split the cost -- it probably comes out even in the end and
       it's less obviously wrong than assigning all time to one or the
       other.
       */
    t1 = stats_stop_timer( timer1 ); 
    t2 = stats_stop_timer( timer2 ); 
    data->gen_stats_old.promotions++; /* Only count one.  Could split it?  */
    data->gen_stats_old.ms_promotion += t1 / 2;
    data->gen_stats_old.ms_promotion_cpu += t2 / 2;
    data->gen_stats_young.ms_promotion += t1 / 2;
    data->gen_stats_young.ms_promotion_cpu += t2 / 2;
#if REMSET_TRACE
    remset_trace( heap, "rof-promote-both" );
#endif
    break;
  case PROMOTE_TO_YOUNG :
    perform_promote_to_both( heap ); /* sic! */
    data->gen_stats_young.promotions++;
    data->gen_stats_young.ms_promotion += stats_stop_timer( timer1 ); 
    data->gen_stats_young.ms_promotion_cpu += stats_stop_timer( timer2 ); 
#if REMSET_TRACE
    remset_trace( heap, "rof-promote-young" );
#endif
    break;
  case COLLECT : 
    perform_collect( heap );
    data->gen_stats_old.collections++;
    data->gen_stats_old.ms_collection += stats_stop_timer( timer1 ); 
    data->gen_stats_old.ms_collection_cpu += stats_stop_timer( timer2 ); 
#if REMSET_TRACE
    remset_trace( heap, "rof-collect-old" );
#endif
    break;
  }

  annoyingmsg( "ROF GC done.  Live old=%d  Live young=%d  k=%d  j=%d",
               used_old( heap ), used_young( heap ), data->k, data->j );
}

/* Cautious strategy: 
     Let X be the amount of memory allocated in ephemeral area.
     Let No be the amount of memory available in the 'old' generation.
     Let Ny be the amount of memory available in the 'young' generation.
     if X <= No then
       promote from ephemeral generations to 'old'
     else if No is small and X <= Ny then
       promote from ephemeral generations to 'young'
     else if X <= No+Ny then
       promote from ephemeral generations to 'old' and 'young'
     else
       collect

   Large objects are handled specially: if the volume of live objects
   in the dynamic area exceeds the amount of free space, then a collection
   is performed.  Thus small amounts of large data do not alter the
   collector's decisions, but large amounts of large data do.
   */
static enum action decision( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  int X, No, Ny, i, max_fragmentation, large_live;
  
  X = gc_allocated_to_areas( gc, gset_range( 0, data->gen_no ));

  /* Max fragmentation in the data promoted in depends on the amount being
     promoted, the maximum small object size, and the allocation area chunk 
     size.
     */
  max_fragmentation = 
    GC_LARGE_OBJECT_LIMIT*(ceildiv( X, GC_CHUNK_SIZE )-1) + GC_CHUNK_SIZE;
  X += max_fragmentation;
  
  No = data->stepsize * (data->k - data->j) - used_in_space( data->old );
  Ny = data->stepsize * data->j - used_in_space( data->young );
  large_live =   los_bytes_used( gc->los, data->gen_no ) 
               + los_bytes_used( gc->los, data->gen_no+1 );
    
  assert( No >= 0 );
  assert( Ny >= 0 );
  
  if (large_live > No + Ny)
    return COLLECT;
  else if (X <= No)
    return PROMOTE_TO_OLD;
  else if (No < PAGESIZE && X <= Ny)
   return PROMOTE_TO_YOUNG;
  else if (X <= No + Ny)
    return PROMOTE_TO_BOTH;
  else
    return COLLECT;
}

/* Rule: we have a window of size PHASE_WINDOW, and a buffer of twice
   that size.  Return TRUE if the older window exhibits growth and the
   younger window exhibits stability.
   */
static bool run_phase_detector( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int i, j, min_entries, prev, young;

  if (data->phase_detection < 0.0) 
    return FALSE;               /* disabled */

  /* Determine if old window exhibits growth */
  /* Old window is window at phase_idx. */
  prev = j = data->phase_idx;
  for ( i=0 ; i < PHASE_WINDOW-1 ; i++ ) {
    j = (j+1) % PHASE_BUFSIZ;
    if (data->phase_buf[prev].remset_size > data->phase_buf[j].remset_size)
      return FALSE;             /* shrank */
    min_entries = 
      data->phase_buf[prev].remset_size * (1.0+data->phase_detection);
    if (data->phase_buf[j].remset_size <= min_entries)
      return FALSE;             /* grown too little */
    prev = j;
  }

  /* Determine if young window exhibits stability. */
  /* Young window is window at (phase_idx+PHASE_WINDOW)%PHASE_BUFSIZ. */
  young = (data->phase_idx+PHASE_WINDOW) % PHASE_BUFSIZ;

  if (data->phase_buf[j].remset_size > data->phase_buf[young].remset_size)
    return FALSE;               /* shrank */

  prev = j = young;
  for ( i=0 ; i < PHASE_WINDOW-1 ; i++ ) {
    j = (j+1) % PHASE_BUFSIZ;
    if (data->phase_buf[prev].remset_size > data->phase_buf[j].remset_size)
      return FALSE;             /* shrank */
    min_entries = 
      data->phase_buf[prev].remset_size * (1.0+data->phase_detection);
    if (data->phase_buf[j].remset_size > min_entries)
      return FALSE;             /* grown too much */
    prev = j;
  }

  if (data->phase_buf[young].j >= data->j) 
    return FALSE;

  annoyingmsg( "Phase detector triggered. "
               "(old k=%d, old j=%d, used_old=%d, used_young=%d).",
               data->k, data->j, used_old(heap), used_young(heap) );
  annoyingmsg( "Buffer contents (oldest first):" );
  for ( i=0, j=data->phase_idx; i < PHASE_BUFSIZ ; i++, j=(j+1)%PHASE_BUFSIZ )
    annoyingmsg( "  remset_size=%6d   j=%2d",
                 data->phase_buf[j].remset_size,
                 data->phase_buf[j].j );
  return TRUE;
}

static void update_phase_data( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int i, effective_j;

  effective_j = 
    (data->j*data->stepsize - used_in_space( data->young )) / data->stepsize;
  assert( effective_j >= 0 );

  i = data->phase_idx;
  data->phase_buf[i].remset_size = 
    heap->collector->remset[ heap->collector->np_remset ]->live;
  data->phase_buf[i].j = effective_j;
  data->phase_idx = (data->phase_idx+1) % PHASE_BUFSIZ;
}

/* Return TRUE if the extra remset, scaled to the expected size when the
   generation is full, is fuller than allowed by the limit.  (The
   default limit is INT_MAX, ie roughly infinite.)  
   */
static bool check_for_remset_overflow( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  double size, used, entries, occ;

  used = used_in_space( data->young );
  size = data->j * data->stepsize;
  entries = heap->collector->remset[ heap->collector->np_remset ]->live;

  /* `occ' is the inverse of the heap occupancy.
     If occ is large (heap nearly empty), then do not try to adjust.
     */
  occ = size/(used+1);
  if (occ > 20.0)               /* < 5% full */
    return FALSE;
  else if (occ*entries > data->remset_limit) {
    annoyingmsg( "NP remembered set overflow: j will be adjusted.\n"
                 "  size=%g used=%g entries=%g occ=%g",
                 size, used, entries, occ );
    return TRUE;
  }
  else
    return FALSE;
}

static void adjust_j( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int i, n;
  los_t* los = heap->collector->los;

  /* Adjust data->j.

     Expensive solution:
     Must shuffle some memory blocks and adjust attributes.
     Must scan the NP remset and remove pointers not into the correct area.
     Must scan the young remset and remove pointers not into the correct area.

     Cheap solution: 
     Shuffle _all_ young into old.  This can be bad if the size of young
     has increased a lot since remset stability, because it will shuffle
     too much memory, but is otherwise a close approximation.  Wholesale
     shuffling can also result in increased fragmentation because the
     last block being shuffled is not full.
     */

  /* Use cheap solution for now! */

  /* Move data.  The move changes data->young->current, so step _down_! */
  n=data->young->current;
  for ( i=n ; i >= 0 ; i-- )
    ss_insert_block_in_semispace( data->young, i, data->old );
  los_append_and_clear_list( los, 
                             los->object_lists[ data->gen_no+1 ], 
                             data->gen_no);

  ss_free( data->young );
  data->young = 
    create_semispace( GC_CHUNK_SIZE, data->gen_no+1 );

  /* Clear or move remset contents. Can't clear all three sets 
     because adjust_j() can be called while there's still live 
     data in the ephemeral area.
     */
  rs_assimilate_and_clear( heap->collector->remset[ data->gen_no ], 
                           heap->collector->remset[ data->gen_no+1 ] );
  rs_clear( heap->collector->remset[ heap->collector->np_remset ] );

  data->j -= n+1;
  assert( data->j >= 0 );
  
  annoyingmsg( "Adjusted value of j=%d", data->j );
}

static void perform_promote_to_old( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int old_before_gc, old_los_before_gc;
  gc_t *gc = heap->collector;
  los_t *los = gc->los;
  
  annoyingmsg( "  Promoting into old area." );

#if FLOAT_REDUCTION == 2
  full_collection( heap );
#endif

  ss_sync( data->old );
  old_before_gc = data->old->used;
  old_los_before_gc = los_bytes_used( gc->los, data->gen_no );

  gclib_stopcopy_promote_into( heap->collector, data->old );
  rs_clear( heap->collector->remset[ data->gen_no ] );

  ss_sync( data->old );
  data->gc_stats.words_copied += 
        bytes2words( data->old->used - old_before_gc );
  data->gc_stats.words_moved += 
        bytes2words( los_bytes_used( los, data->gen_no )-old_los_before_gc );
#if GC_EVENT_COUNTERS
  data->event_stats.copied_by_prom += 
        bytes2words( data->old->used - old_before_gc );
  data->event_stats.moved_by_prom += 
        bytes2words( los_bytes_used( los, data->gen_no )-old_los_before_gc );
#endif
}

static void perform_promote_to_both( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int young_available, old_available;
  int old_before_gc, old_los_before_gc, young_before_gc, young_los_before_gc;
  gc_t *gc = heap->collector;
  los_t *los = gc->los;

  annoyingmsg( "  Promoting to both old and young." );

#if FLOAT_REDUCTION == 2
  full_collection( heap );
#endif

  if (run_phase_detector( heap ))
    adjust_j( heap );

  /* Must obtain this after any adjustment of j */
  ss_sync( data->old );
  ss_sync( data->young );
  old_before_gc = data->old->used;
  old_los_before_gc = los_bytes_used( gc->los, data->gen_no );
  young_before_gc = data->young->used;
  young_los_before_gc = los_bytes_used( los, data->gen_no+1 );

  young_available = data->j*data->stepsize - used_in_space( data->young );
  old_available = 
    (data->k - data->j)*data->stepsize - used_in_space( data->old );

  /* Precondition for promotion into old and young: free space is the
     sum of free space in the current chunk plus an integral number of
     chunks.  
     */
  assert( old_available % GC_CHUNK_SIZE ==
          words2bytes( data->old->chunks[data->old->current].lim -
                       data->old->chunks[data->old->current].top ) 
            % GC_CHUNK_SIZE );
  assert( young_available % GC_CHUNK_SIZE == 
          words2bytes( data->young->chunks[data->young->current].lim -
                       data->young->chunks[data->young->current].top ) 
            % GC_CHUNK_SIZE );
  
  gclib_stopcopy_promote_into_np( heap->collector,
                                  data->old,
                                  data->young,
                                  old_available,
                                  young_available );

  /* Postcondition for promotion into old and young: no overflow. */
  assert( used_in_space( data->old ) <= (data->k - data->j)*data->stepsize );
  assert( used_in_space( data->young ) <= data->j*data->stepsize );

  rs_clear( heap->collector->remset[ data->gen_no ] );
  rs_assimilate_and_clear( heap->collector->remset[heap->collector->np_remset],
                           heap->collector->remset[ data->gen_no+1 ] );

  /* Must update stats before changing j */
  ss_sync( data->old );
  ss_sync( data->young );
  data->gc_stats.words_copied += 
      bytes2words( data->old->used - old_before_gc )
    + bytes2words( data->young->used - young_before_gc );
  data->gc_stats.words_moved += 
      bytes2words(los_bytes_used( los, data->gen_no )-old_los_before_gc)
    + bytes2words(los_bytes_used( los, data->gen_no+1 )-young_los_before_gc);
#if GC_EVENT_COUNTERS
  data->event_stats.copied_by_prom += 
      bytes2words( data->old->used - old_before_gc )
    + bytes2words( data->young->used - young_before_gc );
  data->event_stats.moved_by_prom += 
      bytes2words(los_bytes_used( los, data->gen_no )-old_los_before_gc)
    + bytes2words(los_bytes_used( los, data->gen_no+1 )-young_los_before_gc);
#endif

  if (check_for_remset_overflow( heap ))
    adjust_j( heap );
  update_phase_data( heap );
}

static void perform_collect( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  los_t *los = heap->collector->los;
  gc_t *gc = heap->collector;
  int free_steps, luck_steps;
  int young_before_gc, young_los_before_gc;

  annoyingmsg( "  Full garbage collection." );

#if FLOAT_REDUCTION
  full_collection( heap );
#endif

  if (run_phase_detector( heap ))
    adjust_j( heap );

  /* Must obtain this after any adjustment of j */
  ss_sync( data->young );
  young_before_gc = data->young->used;
  young_los_before_gc = los_bytes_used( los, data->gen_no+1 );

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

  /* Compute new k and j, and other policy parameters.
     Young is empty.

     What should the new k be?
     
     Assuming that all of old is live, computing k is easy: it's the
     standard load-factor based computation, adjusted for the large 
     object volume.
     
     All of old is probably not live, though, so k can be adjusted; 
     see further down.
     */
  data->target_size =
    compute_dynamic_size( heap,
                          used_in_space( data->old ),
                          los_bytes_used( los, data->gen_no ) );
  data->k = 
    (data->target_size - los_bytes_used( los, data->gen_no )) / data->stepsize;
  if (data->k < 0) {
    /* Hard heap overflow in fixed-size heap. */
    panic_exit( "ROF collector: The heap is full." );
  }

  free_steps = 
    (data->k * data->stepsize - used_in_space( data->old )) / data->stepsize;
  if (free_steps < 0) {
    /* Soft heap overflow in a fixed-size heap. */
    free_steps = 0;
  }

  /* Policy: j is calculated either as a percentage of free steps, or it's 
     pinned at some value (if possible).  For example, if pin_value == 1
     and there is at least one empty step, then j is set to 1.  
     */
  assert( data->j_percent >= 0 || data->j_pin >= 0 );
  if (data->j_percent >= 0)
    data->j = (free_steps * data->j_percent) / 100;
  else if (free_steps >= data->j_pin)
    data->j = data->j_pin;
  else {
    data->j = free_steps / 2;
    supremely_annoyingmsg( "  Could not pin j at %d; chose %d instead",
                           data->j_pin, data->j );
  }

  /* Now adjust k.
     
        I know what you're thinking, punk. You're thinking, did he 
        fire six shots or only five?  Well in all the excitement 
        I've forgotten myself.  So you have to ask yourself, do I 
        feel lucky?  Well, do you, punk?

     At the time of the next GC, whatever's in 1..j will not be copied,
     and no space need be set aside for its collection.  So add f*j to k
     to shuffle memory set aside for the young generation's copyspace
     into the old generation, where it becomes available for allocation.

     By assumption, the amount of garbage in the system at the next GC
     is H/L.  If all of 1..j is live, then j steps can be added to k.  
     However, there will probably be some garbage in 1..j, so assuming 
     that all of 1..j is live probably underestimates the amount of live 
     storage in steps j+1..k.  Therefore, less than j steps should be 
     added to k, because some space needs to be set aside for the copies
     of the live objects that are in j+1..k rather than in 1..j.  The
     parameter f controls the fraction of j that's added to k.

     The default value of f is 0.0, which is an extremely conservative 
     approximation.  But a value closer to 1.0 increases the risk of
     underestimating the amount of live storage in steps j+1..k, thus
     increasing the chance of memory overflow during collection in a 
     fixed heap.  So, how lucky do you feel?
     */
  luck_steps = (int)(data->j*data->luck);
  data->k += luck_steps;

  ss_sync( data->old );
  data->gc_stats.words_copied += 
    bytes2words( data->old->used - young_before_gc );
  data->gc_stats.words_moved +=
    bytes2words( los_bytes_used( los, data->gen_no ) - young_los_before_gc );
#if GC_EVENT_COUNTERS
  data->event_stats.copied_by_gc += 
    bytes2words( data->old->used - young_before_gc );
  data->event_stats.moved_by_gc +=
    bytes2words( los_bytes_used( los, data->gen_no ) - young_los_before_gc );
#endif
  
  annoyingmsg( "  ROF GC: Adjusting parameters, k=%d j=%d, luck=%d", 
               data->k, data->j, luck_steps );
  assert( data->k > 0 );
  assert( 0 <= data->j && data->j < data->k );

  update_phase_data( heap );
}

static void stats( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int live_los;

  ss_sync( data->old );
  ss_sync( data->young );
  
  live_los = los_bytes_used( heap->collector->los, data->gen_no );
  data->gen_stats_old.target = 
    bytes2words( data->stepsize * (data->k - data->j) );
  data->gen_stats_old.allocated = 
    bytes2words( data->old->allocated + live_los );
  data->gen_stats_old.used = bytes2words( data->old->used + live_los );
  stats_add_gen_stats( data->self_old, &data->gen_stats_old );
  memset( &data->gen_stats_old, 0, sizeof( gen_stats_t ) );
  rs_stats( heap->collector->remset[ data->gen_no ] );

  live_los = los_bytes_used( heap->collector->los, data->gen_no+1 );
  data->gen_stats_young.target = bytes2words( data->stepsize * data->j );
  data->gen_stats_young.allocated = 
    bytes2words( data->young->allocated + live_los );
  data->gen_stats_young.used = bytes2words( data->young->used + live_los );
  stats_add_gen_stats( data->self_young, &data->gen_stats_young );
  memset( &data->gen_stats_young, 0, sizeof( gen_stats_t ) );
  rs_stats( heap->collector->remset[ data->gen_no + 1] );
  rs_stats( heap->collector->remset[ heap->collector->np_remset ] );

  data->gc_stats.np_j = data->j;
  data->gc_stats.np_k = data->k;
  stats_add_gc_stats( &data->gc_stats );
  memset( &data->gc_stats, 0, sizeof( gc_stats_t ) );

#if GC_EVENT_COUNTERS
  stats_set_gc_event_stats( & data->event_stats );
#endif
}

static void before_collection( old_heap_t *heap )
{
  heap->allocated = used_old( heap ) + used_young( heap );
  heap->maximum = DATA(heap)->target_size;
}

static void after_collection( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;

  heap->allocated = used_old( heap ) + used_young( heap );
  heap->maximum = data->target_size;

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
    data->j_percent = -1;
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

/* Returns the amount used in a form useful to the collector.  This
   value takes into account that space at the end of non-current chunks
   is not available, hence is 'used' as far as policy is concerned.
   */
static int used_in_space( semispace_t *ss )
{
  int n = 0, i;

  for ( i=0 ; i < ss->current ; i++ )
    n += ss->chunks[i].bytes;
  n += words2bytes(ss->chunks[ss->current].top - ss->chunks[ss->current].bot);
  return n;
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
                            0,               /* FIXME? set_gen_no */
                            0,               /* FIXME is_address_mapped */
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

#if REMSET_TRACE
#include <stdio.h>

static char *eish( int n )
{
  static char buf[10];
  int e = 0;
  double m = n;
  
  while (m >= 1000.0) {
    e += 3;
    m /= 1000.0;
  }
  if (e == 0)
    sprintf( buf, "%d", n );
  else
    sprintf( buf, "%3.1fe%d", m, e );
  return buf;
}

static void remset_trace( old_heap_t *heap, char *type )
{
  int i;

  printf( "\nREMSET-TRACE (%-8s (gen ", type );
  for ( i=1 ; i < heap->collector->remset_count ; i++ )
    printf( " %8s", eish( heap->collector->remset[i]->live ) );
  printf( "))\n");
  fflush( stdout );
}
#endif

#if FLOAT_REDUCTION
/* Pilfered from the DOF collector.  Mark all objects reachable from
   the roots and then sweep all the remembered sets, removing any
   unmarked objects.
   */
#include "msgc-core.h"

typedef struct {
  msgc_context_t *context;
  int removed;
} scan_datum_t;

static bool
fullgc_should_keep_p( word loc, void *data, unsigned *stats )
{
  if (msgc_object_marked_p( ((scan_datum_t*)data)->context, loc ))
    return TRUE;
  else {
    ((scan_datum_t*)data)->removed++;
    return FALSE;
  }
}

static int 
sweep_remembered_sets( remset_t **remsets, int first, int last, 
		       msgc_context_t *context )
{
  int i;
  scan_datum_t d;

  d.context = context;
  d.removed = 0;
  for ( i=first ; i <= last ; i++ )
    rs_enumerate( remsets[i], fullgc_should_keep_p, &d );
  return d.removed;
}

static
void full_collection( old_heap_t *heap )
{
  msgc_context_t *context;
  int marked=0, traced=0, removed=0, words_marked=0;
  
  consolemsg( ">>> Full collection starts." );

  context = msgc_begin( heap->collector );
  msgc_mark_objects_from_roots( context, &marked, &traced, &words_marked );

  removed += sweep_remembered_sets( heap->collector->remset,
                                    1,
                                    heap->collector->remset_count-1,
                                    context );
  msgc_end( context );

  consolemsg( ">>> Full collection ends.  Marked=%d traced=%d removed=%d",
              marked, traced, removed );
}
#endif /* FLOAT_REDUCTION */

#endif /* ROF_COLLECTOR */

/* eof */
