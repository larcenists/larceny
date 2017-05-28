/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- stop-and-copy varsized old heap.
 *
 * The code in this file implements stop-and-copy older areas both for
 * ephemeral and dynamic generations.  The only real difference are the 
 * garbage collection decision algorithms (described below).
 */

#define GC_INTERNAL

#define FLOAT_REDUCTION  0
  /* Define this to 1 to do a mark-and-remset-sweep before every ROF 
     collection and 2 to do ditto before every collection.

     Normally this should be 0!
     */

#include "larceny.h"
#include "memmgr.h"
#include "gc.h"
#include "gc_t.h"
#include "gset_t.h"
#include "young_heap_t.h"
#include "region_group_t.h"
#include "old_heap_t.h"
#include "static_heap_t.h"
#include "semispace_t.h"
#include "seqbuf_t.h"
#include "los_t.h"
#include "gclib.h"
#include "remset_t.h"
#include "stats.h"
#include "gc_mmu_log.h"
#include "uremset_t.h"

#define PROMOTE_WITHOUT_COLLECTING       0
#define PROMOTE_WHILE_COLLECTING         1
#define PROMOTE_WHOLESALE_THEN_PROMOTE   2
#define PASS_THE_BUCK                    3

typedef struct old_data old_data_t;

struct old_data {
  stats_id_t  self;
  int         gen_no;             /* Generation and heap number */
  semispace_t *current_space;     /* Space to promote into */
  oh_type_t   oh_type;            /* ephemeral/dynamic/regional */

  /* Strategy/Policy/Mechanism */
  int         size_bytes;         /* Initial size */
  int         lower_limit;        /* 0 or lower limit on expandable area */
  int         upper_limit;	  /* 0 or upper limit on expandable area */
  int         target_size;        /* Current size */
  double      load_factor;        /* That's the L from the formula above */
  bool        must_clear_area;    /* Clear area after collection */
  bool        must_clear_remset;  /* Clear remset after collection */

  int         promoted_last_gc;	  /* For policy use */

  gen_stats_t gen_stats;	  /* accumulates collections and time */
  gc_stats_t  gc_stats;		  /* accumulates words copied/moved */
  gc_event_stats_t event_stats;	  /* Instrumentation data */
};

#define DATA(x)  ((old_data_t*)((x)->data))


static old_heap_t *allocate_heap( int gen_no, gc_t *gc, oh_type_t oh_type );

static int  decision( old_heap_t *heap );
static void perform_collect( old_heap_t *heap );
static void perform_promote( old_heap_t *heap );
static void perform_promote_then_promote( old_heap_t *heap );
static int  compute_dynamic_size( old_heap_t *heap, int live, int los );
static int  used_space( old_heap_t *heap );
static int  used_space_amid_gc( old_heap_t *heap );
#if FLOAT_REDUCTION
static void full_collection( old_heap_t *heap );
#endif
static void start_timers( stats_id_t *timer1, stats_id_t *timer2 );
static void stop_timers( bool is_promotion,
			 old_heap_t *heap, 
			 int bytes_copied, int bytes_moved, 
			 stats_id_t *timer1, stats_id_t *timer2 );

semispace_t *
ohsc_data_area( old_heap_t *heap ) 
{
  return DATA(heap)->current_space;
}

old_heap_t *
clone_sc_area( old_heap_t *src_heap, int tgt_gen_no )
{
  old_heap_t *tgt_heap;
  old_data_t *tgt_data;
  old_data_t *src_data;
  oh_type_t oh_type;

  annoyingmsg( "old-heap: clone heap tgt_gno %d src_gno: %d", 
	       tgt_gen_no, DATA(src_heap)->gen_no );

  src_data = DATA(src_heap);
  oh_type = src_data->oh_type;

  tgt_heap = allocate_heap( tgt_gen_no, src_heap->collector, oh_type );
  tgt_data = DATA(tgt_heap);

  tgt_data->current_space = create_semispace( GC_CHUNK_SIZE, tgt_gen_no );
  tgt_data->size_bytes = src_data->size_bytes;
  tgt_heap->reallocate_whole_semispace = src_heap->reallocate_whole_semispace;
  memset( &tgt_data->gen_stats, 0, sizeof( gen_stats_t ) );
  memset( &tgt_data->gc_stats, 0, sizeof( gc_stats_t ) );
  
  switch (oh_type) {
  case OHTYPE_DYNAMIC:
    tgt_data->load_factor = src_data->load_factor;
    tgt_data->lower_limit = src_data->lower_limit;
    tgt_data->upper_limit = src_data->upper_limit;
    tgt_data->target_size = src_data->target_size;
    break;
  case OHTYPE_EPHEMERAL:
    tgt_data->target_size = src_data->target_size;
    break;
  case OHTYPE_REGIONAL:
    tgt_data->target_size = src_data->target_size;
    break;
  default:
    assert(0);
  }

  tgt_heap->maximum = tgt_data->target_size;
  tgt_heap->allocated = 0;

  return tgt_heap;
}

old_heap_t *
create_sc_area( int gen_no, gc_t *gc, sc_info_t *info, oh_type_t oh_type )
{
  old_heap_t *heap;
  old_data_t *data;

  assert( info->size_bytes > 0 );

  heap = allocate_heap( gen_no, gc, oh_type );
  data = DATA(heap);

  data->current_space = create_semispace( GC_CHUNK_SIZE, gen_no );
  data->size_bytes = roundup_page( info->size_bytes );

  switch (oh_type) {
  case OHTYPE_DYNAMIC:
    data->load_factor = info->load_factor;
    data->lower_limit = info->dynamic_min;
    data->upper_limit = info->dynamic_max;
    data->target_size =
      compute_dynamic_size( heap,
			    data->size_bytes/data->load_factor,
			    0 );
    break;
  case OHTYPE_EPHEMERAL:
    data->target_size = data->size_bytes;
    break;
  case OHTYPE_REGIONAL:
    data->target_size = data->size_bytes;
    break;
  default: 
    assert(0);
  }

  heap->maximum = data->target_size;
  heap->allocated = 0;

  return heap;
}

static void collect_regional_into( old_heap_t *heap, gc_type_t request, old_heap_t *heap_to ) 
{
  old_data_t *data_from = DATA(heap);
  old_data_t *data_to = DATA(heap_to);
  semispace_t *from, *to, *newspace;
  int rgn_idx = data_from->gen_no;
  int used_before, tospace_before, los_before;
  stats_id_t timer1, timer2;
  int bytes_copied, bytes_moved;

  annoyingmsg( "Regional area: garbage collection." );

  switch (request) {
  case GCTYPE_COLLECT: 
    /* Collect this region and the nursery.  Forwards objects into
     * this region until it is full; then switches to using
     * gc_find_space to decide where things go after that. 
     */

    annoyingmsg("collect_rgnl major collect of %d into %d", rgn_idx, data_to->gen_no );

    start_timers( &timer1, &timer2 );

    /* The regional collector will generally need to build up a new
     * remembered set for this heap during collection, so we have to
     * clear out the remset's soon-to-be-invalid state before starting
     * the collection loop.
     */
    { 
      assert( seqbuf_clearp( heap->collector->ssb[rgn_idx] ));
      
      urs_clear( heap->collector->the_remset, rgn_idx );
    }

    /* NOTE on the above: the standard collectors seem to do their
     * rs_clear calls in an _untimed_ portion of the control flow.
     * This means that the standard collectors may seem artificially
     * superior to the regional collector according to internal
     * measurements.  FIXME. */
    
    from = data_from->current_space;
    newspace = create_semispace( GC_CHUNK_SIZE, data_from->gen_no );
    data_from->current_space = newspace;
    if (data_from == data_to) {
      to = newspace;
    } else {
      to = data_to->current_space;
    }

    tospace_before = to->used;
    los_before = los_bytes_used( heap->collector->los, data_to->gen_no );

    gc_phase_shift( heap->collector, gc_log_phase_misc_memmgr, gc_log_phase_majorgc );
    gclib_stopcopy_collect_locs( heap->collector, 
                                 gset_singleton( rgn_idx ),
                                 to );
    gc_phase_shift( heap->collector, gc_log_phase_majorgc, gc_log_phase_misc_memmgr );

    gc_check_invariants_between_fwd_and_free( heap->collector, rgn_idx );

    /* can two below be re-ordered? */
    ss_free( from );
    ss_sync( to );

    data_from->gen_stats.collections++;
    bytes_copied = to->used - tospace_before;
    bytes_moved = 
      los_bytes_used( heap->collector->los, data_to->gen_no ) - los_before;

    /* XXX Felix does not trust the computation below 100%. */
    heap->bytes_live_last_major_gc = bytes_copied + bytes_moved;

    heap->words_from_nursery_last_major_gc = 
      heap->collector->words_from_nursery_last_gc;

    break;
  case GCTYPE_PROMOTE: 
    assert(data_from == data_to); /* (code below assumes heap == heap_to ) */
    assert(heap == heap_to);
    /* Promote the nursery into this region. */
    annoyingmsg("collect_rgnl minor collect into %d", rgn_idx);
    gc_signal_minor_collection( heap->collector );

    start_timers( &timer1, &timer2 );
    used_before = used_space( heap );
    to = data_to->current_space;
    ss_sync( to );
    tospace_before = to->used;
    los_before = los_bytes_used( heap->collector->los, data_to->gen_no );

    gc_phase_shift( heap->collector, gc_log_phase_misc_memmgr, gc_log_phase_minorgc );
    gclib_stopcopy_collect_locs( heap->collector, 
                                 gset_singleton( 0 ), 
                                 to );
    gc_phase_shift( heap->collector, gc_log_phase_minorgc, gc_log_phase_misc_memmgr );
    data_to->promoted_last_gc = used_space( heap ) - used_before;
    
    data_to->gen_stats.promotions++;
    bytes_copied = to->used - tospace_before;
    bytes_moved = 
      los_bytes_used(heap->collector->los, data_to->gen_no)-los_before;

    break;
  default:
    assert(0);
  }
  stop_timers( (request==GCTYPE_PROMOTE), heap, 
	       bytes_copied, bytes_moved, &timer1, &timer2 );
}

static void collect_dynamic( old_heap_t *heap, gc_type_t request )
{
  gc_t *gc = heap->collector;
  old_data_t *data = DATA(heap);
  int alloc, i;

  annoyingmsg( "Dynamic area: garbage collection." );

  /* Compute live data in the ephemeral areas.  ->allocated includes LOS.
     */
  alloc = gc_allocated_to_areas( gc, gset_range( 0, data->gen_no ));

  supremely_annoyingmsg( "  alloc=%d  size=%d  used=%d",
			 alloc, data->target_size, used_space( heap ) );

  data->must_clear_remset = 1;
  if (request == GCTYPE_PROMOTE && 
      alloc <= data->target_size - used_space( heap ))
    perform_promote( heap );
  else {
    perform_collect( heap );
    ss_sync( data->current_space );
    data->target_size = 
      compute_dynamic_size( heap,
			    data->current_space->used, 
			    los_bytes_used( gc->los, data->gen_no ) );
  }

  annoyingmsg( "Collection finished." );
}

static void collect_ephemeral( old_heap_t *heap, gc_type_t request )
{
  old_data_t *data = DATA(heap);
  int collected = 0, type;

  type = decision( heap );
  if (type != PASS_THE_BUCK) {
    collected = 1;
    annoyingmsg( "Ephemeral area: garbage collection." );
    if (request == GCTYPE_COLLECT)
      type = PROMOTE_WHILE_COLLECTING;
  }

  data->must_clear_remset = 1;
  switch( type ) {
  case PROMOTE_WITHOUT_COLLECTING :
    perform_promote( heap );
    break;
  case PROMOTE_WHILE_COLLECTING :
    perform_collect( heap );
    break;
  case PROMOTE_WHOLESALE_THEN_PROMOTE :
    perform_promote_then_promote( heap );
    break;
  case PASS_THE_BUCK :
    data->must_clear_area = 1;
    gc_collect( heap->collector, data->gen_no+1, 0, request );
    break;
  default :
    panic_exit( "Impossible" );
  }

  if (collected)
    annoyingmsg( "Collection finished." );
}

static int decision( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  int X, Y, Z, M, Xnext;
  int i, n, j;

#if 0
  /* n: number of ephemeral areas (including young_area) */
  n = heap->collector->ephemeral_area_count+1;
#endif

  /* i: _index_ in ephemeral_area[] of this heap */
  i = data->gen_no-1;
  FIXME_UNUSED_VARIABLE(i);

  /* Gather information */
  /* X: allocated in younger generations */
  /* Y: allocated in this and younger generations */
  /* M: sum of sizes of younger generations */
  /* Note that it's possible to have X > M because of large objects */
  
  X = gc_allocated_to_areas( gc, gset_range( 0, data->gen_no ));
  M = gc_maximum_allotted( gc, gset_range( 0, data->gen_no ));
  Y = X + gc_allocated_to_areas( gc, gset_singleton( data->gen_no ));

  /* Z: estimate of live data to promote */
  /* Xnext: Estimate of X next time */
  Z = data->promoted_last_gc;
  Xnext = (X+M) / 2;

  supremely_annoyingmsg( "old-heap: DECISION for generation %d\n"
			 "  M=%d, X=%d, Y=%d, Z=%d, Xnext=%d\n"
			 "  alloc=%d, max=%d",
			 data->gen_no, M, X, Y, Z, Xnext, 
			 heap->allocated, heap->maximum );

  /* Case 1: data will fit in this area. */
  if (Y <= heap->maximum) {
    if (heap->allocated + Z >= heap->maximum - Xnext)
      return PROMOTE_WHILE_COLLECTING;
    else
      return PROMOTE_WITHOUT_COLLECTING;
  }

#if 0
  /* Case 2: data will not fit, but can we do better than pass the buck? */
  if (data->gen_no+1 < n) {
    old_heap_t *next = gc->ephemeral_area[i+1];

    if (heap->allocated <= next->maximum - next->allocated) {
      /* Is there reason to believe that at the time of the next collection
	 in E{i-1}, the storage now in E0 through E{i-1} will contain a higher
	 fraction of garbage than Ei has now?
	 */
      /* For the time being, assume that is never the case. */
      /* FIXME */
    }
  }
#endif

  /* Case 3: not here. */
  return PASS_THE_BUCK;
}

static void start_timers( stats_id_t *timer1, stats_id_t *timer2 ) {
  *timer1 = stats_start_timer( TIMER_ELAPSED );
  *timer2 = stats_start_timer( TIMER_CPU );
}

static void stop_timers( bool is_promotion, 
			 old_heap_t *heap, 
			 int bytes_copied, int bytes_moved, 
			 stats_id_t *timer1, stats_id_t *timer2 ) {
  int ms;
  int ms_cpu;
  old_data_t *data = DATA(heap);

  /* Why isn't this `+=' ?  I think it is benign in this collector. */
  data->gc_stats.words_copied = bytes2words( bytes_copied );
  data->gc_stats.words_moved = bytes2words( bytes_moved );

  ms = stats_stop_timer( *timer1 );
  ms_cpu = stats_stop_timer( *timer2 );
  heap->collector->stat_last_ms_gc_cheney_pause = ms;
  heap->collector->stat_last_ms_gc_cheney_pause_cpu = ms_cpu;
  if (is_promotion) {
    data->gen_stats.ms_promotion += ms;
    data->gen_stats.ms_promotion_cpu += ms_cpu;
    heap->collector->stat_last_gc_pause_ismajor = 0;
  } else {
    data->gen_stats.ms_collection += ms;
    data->gen_stats.ms_collection_cpu += ms_cpu;
    heap->collector->stat_last_gc_pause_ismajor = 1;
  }
  data->gc_stats.max_ms_cheney_collection = 
    max( data->gc_stats.max_ms_cheney_collection, ms );
  data->gc_stats.max_ms_cheney_collection_cpu =
    max( data->gc_stats.max_ms_cheney_collection_cpu, ms_cpu );
#if GC_EVENT_COUNTERS
  data->event_stats.copied_by_gc += bytes2words( bytes_copied );
  data->event_stats.moved_by_gc  += bytes2words( bytes_moved );
#endif
}

static void perform_collect( old_heap_t *heap )
{
  semispace_t *from, *to;
  old_data_t *data = DATA(heap);
  stats_id_t timer1, timer2;

  annoyingmsg( "  Collecting generation %d.", data->gen_no );

#if FLOAT_REDUCTION
  full_collection( heap );
#endif
  
  start_timers( &timer1, &timer2 );

  from = data->current_space;
  to = create_semispace( GC_CHUNK_SIZE, data->gen_no );
  data->current_space = to;
  
  gc_phase_shift( heap->collector, gc_log_phase_misc_memmgr, gc_log_phase_majorgc );
  gclib_stopcopy_collect( heap->collector, to );
  gc_phase_shift( heap->collector, gc_log_phase_majorgc, gc_log_phase_misc_memmgr );

  ss_free( from );
  ss_sync( to );

  data->gen_stats.collections++;
  stop_timers( FALSE, heap, 
	       to->used, 
	       los_bytes_used( heap->collector->los, data->gen_no ),
	       &timer1, &timer2 );
}

static void perform_promote( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);
  int used_before, tospace_before, los_before;
  stats_id_t timer1, timer2;

  annoyingmsg( "  Promoting into generation %d.", data->gen_no );

#if FLOAT_REDUCTION
  full_collection( heap );
#else
  if (data->gen_no == 1)
    gc_signal_minor_collection( heap->collector );
#endif

  start_timers( &timer1, &timer2 );

  used_before = used_space( heap );
  ss_sync( data->current_space );
  tospace_before = data->current_space->used;
  los_before = los_bytes_used( heap->collector->los, data->gen_no );

  gc_phase_shift( heap->collector, gc_log_phase_misc_memmgr, gc_log_phase_minorgc );
  gclib_stopcopy_promote_into( heap->collector, data->current_space );
  gc_phase_shift( heap->collector, gc_log_phase_minorgc, gc_log_phase_misc_memmgr );

  data->promoted_last_gc = used_space( heap ) - used_before;

  data->gen_stats.promotions++;
  stop_timers( TRUE, heap, 
	       data->current_space->used - tospace_before,
	       los_bytes_used(heap->collector->los, data->gen_no)-los_before,
	       &timer1, &timer2 );
}

static void perform_promote_then_promote( old_heap_t *heap )
{
  panic_exit( "perform_promote_then_promote not implemented." );
  /* FIXME */
}

static void check_budget( old_heap_t *heap, char *where ) 
{
#if 0
  if (heap->allocated > heap->maximum) {
    int nonlos_allocated = DATA(heap)->current_space->used;
    int los_allocated = 
      los_bytes_used( heap->collector->los, DATA(heap)->gen_no );
    consolemsg("old-heap[%d] allocated=%d=%d+%d more than maximum=%d %s",
               DATA(heap)->gen_no, 
               heap->allocated, nonlos_allocated, los_allocated, 
               heap->maximum, "synchronize" );
  }
#endif
}

static void before_collection( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  data->must_clear_area = 0;
  data->must_clear_remset = 0;
  heap->maximum = data->target_size;
  heap->allocated = used_space( heap );
  check_budget( heap, "before_collection" );
}

static void after_collection( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  /* Clear area if data was promoted out */
  if (data->must_clear_area) {
    ss_free( data->current_space );
    data->current_space = 
      create_semispace( GC_CHUNK_SIZE, data->gen_no );
  }
  if (data->must_clear_remset) 
    /* FIXME: not accounted for in GC time measurement.  */
    urs_clear( heap->collector->the_remset, data->gen_no );

  heap->allocated = used_space( heap );
  check_budget( heap, "after_collection" );
  annoyingmsg( "  Generation %d: Size=%d, Live=%d, Remset live=%d.", 
	       data->gen_no, data->target_size, heap->allocated, 
	       urs_live_count( heap->collector->the_remset, data->gen_no ));
}

static void stats( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );

  data->gen_stats.target = bytes2words( data->target_size );
  data->gen_stats.allocated = 
    bytes2words(data->current_space->allocated + 
		los_bytes_used( heap->collector->los, data->gen_no ));
  data->gen_stats.used = bytes2words(used_space( heap ));

  stats_add_gen_stats( data->self, &data->gen_stats );
  stats_add_gc_stats( &data->gc_stats );
  stats_set_gc_event_stats( &data->event_stats );
  urs_checkpoint_stats( heap->collector->the_remset, data->gen_no );

  memset( &data->gen_stats, 0, sizeof( gen_stats_t ) );
  memset( &data->gc_stats, 0, sizeof( gc_stats_t ) );
}

static word *data_load_area( old_heap_t *heap, int nbytes )
{
  old_data_t *data = DATA( heap );
  int n;

  assert( nbytes > 0 );
  assert( nbytes % BYTE_ALIGNMENT == 0 );

  n = ss_allocate_and_insert_block( data->current_space, nbytes );
  return data->current_space->chunks[ n ].bot;
}

/* Internal */

static int static_used( old_heap_t *heap )
{
  static_heap_t *s = heap->collector->static_area;  /* may be NULL */

  /* Including the static area inflates the estimate of actual live
   * storage.  (That is not totally inappropriate, because the
   * stopcopy collector does incur the cost of scanning the static
   * area on every collection.)  We do *not* scan the text portion of
   * the static area, which takes up the majority of the static area
   * (at least on x86), and so we can avoid the over-inflation of the
   * live estimate by not including the text area's bytes.
   */
  return (s ? s->data_area->allocated : 0);
}

static int compute_dynamic_size( old_heap_t *heap, int D, int Q )
{
  int S = static_used( heap );
  double L = DATA(heap)->load_factor;
  int upper_limit = DATA(heap)->upper_limit;
  int lower_limit = DATA(heap)->lower_limit;

  return gc_compute_dynamic_size( heap->collector,
				  D, S, Q, L, lower_limit, upper_limit );
}


static int used_space( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );
  return data->current_space->used +
           los_bytes_used( heap->collector->los, data->gen_no );
}

static int used_space_amid_gc( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );
  return data->current_space->used +
           los_bytes_used_include_marklists( heap->collector->los, 
                                             data->gen_no );
}

static void set_gen_no( old_heap_t *heap, int gen_no ) 
{
  DATA(heap)->gen_no = gen_no;
  ss_set_gen_no( DATA(heap)->current_space, gen_no );
  /* XXX this method should be removed, since it is difficult
   * to determine whether it is (or should be) responsible 
   * for setting the gen_no of the LOS (which is a complicated
   * operation if the gen_no is not freshly chosen). 
   */
}

static semispace_t *current_space( old_heap_t *heap )
{
  return DATA(heap)->current_space;
}

static void assimilate( old_heap_t *heap, semispace_t *ss )
{
  ss_assimilate( DATA(heap)->current_space, ss );
}

static void *enumerate( old_heap_t *heap, 
			void *(*visitor)( word *addr, int tag, void *accum ),
			void *accum_init ) 
{
  void *accum = accum_init;
  accum = ss_enumerate( DATA(heap)->current_space, visitor, accum );

  { 
    los_list_t *list = 
      heap->collector->los->object_lists[ DATA(heap)->gen_no ];
    word *cursor = los_walk_list( list, NULL );
    while (cursor != NULL) {
      int tag;
      word w = *cursor;
      if (header(w) == BV_HDR) {
	accum = visitor( cursor, BVEC_TAG, accum );
      } else if (header(w) == VEC_HDR) {
	accum = visitor( cursor, VEC_TAG, accum );
      } else if (header(w) == header(PROC_HDR)) {
	accum = visitor( cursor, PROC_TAG, accum );
      }
      cursor = los_walk_list( list, cursor );
    }
  }
  
  return accum;
}

static bool is_address_mapped( old_heap_t *heap, word *addr, bool noisy )
{
  return ss_is_address_mapped( DATA(heap)->current_space, addr, noisy );
}

static void synchronize( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  heap->maximum = data->target_size;
  heap->allocated = used_space_amid_gc( heap );
  check_budget( heap, "synchronize" );
}

static void collect_into_self( old_heap_t *heap, gc_type_t request ) 
{
  oh_collect_into( heap, request, heap );
}

static old_heap_t *allocate_heap( int gen_no, gc_t *gc, oh_type_t oh_type )
{
  old_heap_t *heap;
  old_data_t *data;

  char *my_id;
  void (*my_collect)( old_heap_t *heap, gc_type_t request );
  void (*my_collect_into)( old_heap_t *heap, gc_type_t req, old_heap_t *to );

  my_collect = 0;
  my_collect_into = 0;

  switch (oh_type) {
  case OHTYPE_EPHEMERAL:
    my_id = "sc/fixed";
    my_collect = collect_ephemeral;
    break;
  case OHTYPE_DYNAMIC:
    my_id = "sc/variable";
    my_collect = collect_dynamic;
    break;
  case OHTYPE_REGIONAL:
    my_id = "sc/regional";
    my_collect = collect_into_self;
    my_collect_into = collect_regional_into;
    break;
  default:
    assert(0);
  }

  data = (old_data_t*)must_malloc( sizeof( old_data_t ) );
  heap = create_old_heap_t( my_id, 
			    HEAPCODE_OLD_2SPACE,
			    0,                    /* initialize */
			    my_collect, 
			    my_collect_into,
			    before_collection,
			    after_collection,
			    stats,
			    data_load_area,
			    0,                    /* FIXME: load_prepare */
			    0,                    /* FIXME: load_data */
			    0,	                  /* set_policy */
			    set_gen_no,
			    current_space,
			    assimilate, 
			    enumerate, 
			    is_address_mapped,
			    synchronize, 
			    data );
  heap->collector = gc;
  {
    static int total_gens = 0;
    data->self = stats_new_generation( gen_no, total_gens++ );
  }
  data->gen_no = gen_no;
  data->promoted_last_gc = 0;
  data->oh_type = oh_type;
  data->load_factor = 0.0;
  data->target_size = 0;
  data->must_clear_area = 0;
  data->must_clear_remset = 0;

  heap->group = region_group_nonrrof;
  heap->prev_in_group = NULL;
  heap->next_in_group = NULL;

  heap->incoming_words.summarizer = 0;
  heap->incoming_words.marker = 0;

  return heap;
}

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
  
  context = msgc_begin( heap->collector );
  msgc_mark_objects_from_roots( context, &marked, &traced, &words_marked );

  removed += sweep_remembered_sets( heap->collector->remset,
                                    1,
                                    heap->collector->remset_count-1,
                                    context );
  msgc_end( context );
}
#endif

/* eof */
