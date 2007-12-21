/* Copyright 1998 Lars T Hansen.
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
#include "young_heap_t.h"
#include "old_heap_t.h"
#include "static_heap_t.h"
#include "semispace_t.h"
#include "los_t.h"
#include "gclib.h"
#include "remset_t.h"
#include "stats.h"

#define PROMOTE_WITHOUT_COLLECTING       0
#define PROMOTE_WHILE_COLLECTING         1
#define PROMOTE_WHOLESALE_THEN_PROMOTE   2
#define PASS_THE_BUCK                    3

typedef struct old_data old_data_t;

struct old_data {
  stats_id_t  self;
  int         gen_no;             /* Generation and heap number */
  semispace_t *current_space;     /* Space to promote into */
  bool        is_ephemeral_area;  /* Otherwise, it's dynamic */

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

static old_heap_t *allocate_heap( int gen_no, gc_t *gc, bool ephem );

static int  decision( old_heap_t *heap );
static void perform_collect( old_heap_t *heap );
static void perform_promote( old_heap_t *heap );
static void perform_promote_then_promote( old_heap_t *heap );
static int  compute_dynamic_size( old_heap_t *heap, int live, int los );
static int  used_space( old_heap_t *heap );
#if FLOAT_REDUCTION
static void full_collection( old_heap_t *heap );
#endif

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
  bool ephemeral;

  annoyingmsg( "old-heap: clone heap tgt_gno %d src_gno: %d", 
	       tgt_gen_no, DATA(src_heap)->gen_no );

  src_data = DATA(src_heap);
  ephemeral = src_data->is_ephemeral_area;

  tgt_heap = allocate_heap( tgt_gen_no, src_heap->collector, ephemeral );
  tgt_data = DATA(tgt_heap);

  tgt_data->current_space = create_semispace( GC_CHUNK_SIZE, tgt_gen_no );
  tgt_data->size_bytes = src_data->size_bytes;
  
  if (!ephemeral) {
    tgt_data->load_factor = src_data->load_factor;
    tgt_data->lower_limit = src_data->lower_limit;
    tgt_data->upper_limit = src_data->upper_limit;
    tgt_data->target_size = src_data->target_size;
  } else {
    tgt_data->target_size = src_data->target_size;
  }

  tgt_heap->maximum = tgt_data->target_size;
  tgt_heap->allocated = 0;

  return tgt_heap;
}

old_heap_t *
create_sc_area( int gen_no, gc_t *gc, sc_info_t *info, bool ephemeral )
{
  old_heap_t *heap;
  old_data_t *data;

  assert( info->size_bytes > 0 );

  heap = allocate_heap( gen_no, gc, ephemeral );
  data = DATA(heap);

  data->current_space = create_semispace( GC_CHUNK_SIZE, gen_no );
  data->size_bytes = roundup_page( info->size_bytes );

  if (!ephemeral) {
    data->load_factor = info->load_factor;
    data->lower_limit = info->dynamic_min;
    data->upper_limit = info->dynamic_max;
    data->target_size =
      compute_dynamic_size( heap,
			    data->size_bytes/data->load_factor,
			    0 );
  }
  else
    data->target_size = data->size_bytes;

  heap->maximum = data->target_size;
  heap->allocated = 0;

  return heap;
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

static void stop_timers( old_data_t *data, 
			 int bytes_copied, int bytes_moved, 
			 stats_id_t *timer1, stats_id_t *timer2 ) {
  /* Why isn't this `+=' ?  I think it is benign in this collector. */
  data->gc_stats.words_copied = bytes2words( bytes_copied );
  data->gc_stats.words_moved = bytes2words( bytes_moved );

  data->gen_stats.ms_collection += stats_stop_timer( *timer1 );
  data->gen_stats.ms_collection_cpu += stats_stop_timer( *timer2 );
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
  
  gclib_stopcopy_collect( heap->collector, to );

  ss_free( from );
  ss_sync( to );

  data->gen_stats.collections++;
  stop_timers( data, 
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

  gclib_stopcopy_promote_into( heap->collector, data->current_space );

  data->promoted_last_gc = used_space( heap ) - used_before;

  data->gen_stats.promotions++;
  stop_timers( data, 
	       data->current_space->used - tospace_before,
	       los_bytes_used(heap->collector->los, data->gen_no)-los_before,
	       &timer1, &timer2 );
}

static void perform_promote_then_promote( old_heap_t *heap )
{
  panic_exit( "perform_promote_then_promote not implemented." );
  /* FIXME */
}

static void before_collection( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  data->must_clear_area = 0;
  data->must_clear_remset = 0;
  heap->maximum = data->target_size;
  heap->allocated = used_space( heap );
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
    rs_clear( heap->collector->remset[ data->gen_no ] );

  annoyingmsg( "  Generation %d: Size=%d, Live=%d, Remset live=%d.", 
	       data->gen_no, data->target_size, used_space( heap ),
	       heap->collector->remset[ data->gen_no ]->live );
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
  rs_stats( heap->collector->remset[ data->gen_no ] );

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


static int used_space( old_heap_t *heap )
{
  old_data_t *data = DATA(heap);

  ss_sync( data->current_space );
  return data->current_space->used +
           los_bytes_used( heap->collector->los, data->gen_no );
}

static void set_gen_no( old_heap_t *heap, int gen_no ) 
{
  DATA(heap)->gen_no = gen_no;
  ss_set_gen_no( DATA(heap)->current_space, gen_no );
}

static semispace_t *current_space( old_heap_t *heap )
{
  return DATA(heap)->current_space;
}

static bool is_address_mapped( old_heap_t *heap, word *addr, bool noisy )
{
  return ss_is_address_mapped( DATA(heap)->current_space, addr, noisy );
}

static old_heap_t *allocate_heap( int gen_no, gc_t *gc, bool ephem )
{
  old_heap_t *heap;
  old_data_t *data;

  data = (old_data_t*)must_malloc( sizeof( old_data_t ) );
  heap = create_old_heap_t( (ephem ? "sc/fixed" : "sc/variable" ),
			    HEAPCODE_OLD_2SPACE,
			    0,                    /* initialize */
			    (ephem ? collect_ephemeral : collect_dynamic),
			    before_collection,
			    after_collection,
			    stats,
			    data_load_area,
			    0,                    /* FIXME: load_prepare */
			    0,                    /* FIXME: load_data */
			    0,	                  /* set_policy */
			    set_gen_no,
			    current_space,
			    is_address_mapped,
			    data );
  heap->collector = gc;
  data->self = stats_new_generation( gen_no, 0 );
  data->gen_no = gen_no;
  data->promoted_last_gc = 0;
  data->is_ephemeral_area = ephem;
  data->load_factor = 0.0;
  data->target_size = 0;
  data->must_clear_area = 0;
  data->must_clear_remset = 0;

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
