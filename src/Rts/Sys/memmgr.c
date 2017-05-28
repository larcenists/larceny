/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny  -- precise garbage collector, top level.
 */

const char *larceny_gc_technology = "precise";

#define GC_INTERNAL

#include <string.h>
#include <stdio.h>     /* gc_mmu_log_print_data uses FILE* type */
#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "stats.h"
#include "memmgr.h"
#include "young_heap_t.h"
#include "old_heap_t.h"
#include "static_heap_t.h"
#include "locset_t.h"
#include "remset_t.h"
#include "los_t.h"
#include "semispace_t.h"
#include "gset_t.h"
#include "gclib.h"
#include "gc_mmu_log.h"
#include "heapio.h"
#include "barrier.h"
#include "stack.h"
#include "msgc-core.h"
#include "region_group_t.h"
#include "smircy.h"
#include "smircy_checking.h"
#include "summary_t.h"
#include "summ_matrix_t.h"
#include "seqbuf_t.h"
#include "uremset_t.h"
#include "uremset_array_t.h"
#include "uremset_debug_t.h"
#include "uremset_extbmp_t.h"
#include "math.h"

#include "memmgr_flt.h"
#include "memmgr_vfy.h"
#include "memmgr_internal.h"

static gc_t *alloc_gc_structure( word *globals, gc_param_t *info );
static word *load_text_or_data( gc_t *gc, int size_bytes, int load_text );
static int allocate_generational_system( gc_t *gc, gc_param_t *params );
static int allocate_stopcopy_system( gc_t *gc, gc_param_t *info );
static int allocate_regional_system( gc_t *gc, gc_param_t *info );
static void before_collection( gc_t *gc );
static void after_collection( gc_t *gc );
static void stats_following_gc( gc_t *gc );
static void before_incremental( gc_t *gc );
static void after_incremental( gc_t *gc );
static void stats_following_incremental( gc_t *gc );
#if defined(SIMULATE_NEW_BARRIER)
static int isremembered( gc_t *gc, word w );
#endif
static void compact_all_areas( gc_t *gc );
static void effect_heap_limits( gc_t *gc );
static int
dump_generational_system( gc_t *gc, const char *filename, bool compact );
static int
dump_stopcopy_system( gc_t *gc, const char *filename, bool compact );
const int rrof_first_region = 1;
static int calc_cN( gc_t *gc );
static int calc_mutator_activity_sumz( gc_t *gc );


/*  FIXME:  This hack suppresses some console messages used to debug
 *  Will's port of of the regional collector to Windows.
 */

void summary_vs_remset_msg( char *msg ) {
  debugmsg( msg );
}

gc_t *create_gc( gc_param_t *info, int *generations )
{
  gc_t *gc;

  /* mutually exclusive, collectively exhaustive */
  assert( (info->is_generational_system + 
           info->is_stopcopy_system + 
           info->is_regional_system) == 1 );

  gclib_init();
  gc = alloc_gc_structure( info->globals, info );

  /* Number of generations includes static heap, if any */
  if (info->is_generational_system) 
    *generations = allocate_generational_system( gc, info );
  else if (info->is_stopcopy_system)
    *generations = allocate_stopcopy_system( gc, info );
  else if (info->is_regional_system)
    *generations = allocate_regional_system( gc, info );
  else 
    assert(0);

  DATA(gc)->generations = *generations;
  DATA(gc)->generations_after_gc = DATA(gc)->generations;

  DATA(gc)->shrink_heap = !info->dont_shrink_heap;
  gc->los = create_los( *generations );

  effect_heap_limits( gc );
  return gc;
}

#define GATHER_MMU_DATA 1

void gc_phase_shift( gc_t *gc, gc_log_phase_t prev, gc_log_phase_t next )
{
#if GATHER_MMU_DATA
  if ( DATA(gc)->mmu_log != NULL ) {
    gc_mmu_log_phase_shift( DATA(gc)->mmu_log, prev, next );
  }
#endif
}

void gc_dump_mmu_data( gc_t *gc, FILE *f ) 
{
#if GATHER_MMU_DATA
  if ( DATA(gc)->mmu_log != NULL ) {
    gc_mmu_log_print_data( DATA(gc)->mmu_log, f );
  }
#endif
}

/* The size of the dynamic (expandable) area is computed based on live data.

   The size is computed as the size to which allocation can grow
   before GC is necessary.  D = live copied data, Q = live large 
   data, S = live static data, L is the inverse load factor, lower_limit
   is 0 or the lower limit on the dynamic area, upper_limit is 0 or
   the upper limit on the dynamic area.

   Let the total memory use be M = L*(D+S+Q).

   Fundamentally, size = M - live_at_next_gc, that is, the amount of 
   memory that can be used for currently live data and allocation, 
   assuming some value for the amount of live data at the next GC that
   must also be accomodated (since we have a copying GC).

   The question is, how do we estimate what will be live at the next GC?
   Ignoring 'limit' for the moment, we have at least three choices:

   * The obvious choice is D, since D is all the data that will be copied.

   * A refinement (maybe) is kD where k is some fudge factor to account
     for non-steady state; if k > 1, then growth is assumed, if k < 1,
     contraction is assumed.  k > 1 seems more useful.  I haven't tried this.

   * A different approach is to use (D+S+Q); that quantity is independent
     of where live data is located, which is nice because it makes the
     effects of changes to L more predictable.  However, it underestimates
     the size.  

   I'm currently using the latter quantity.

   Taking lower_limit into account: pin M as max( M, lower_limit ).

   Taking upper_limit into account: first pin M as min( M, upper_limit ).
   If M - current_live - live_at_next_gc is negative, then there is
   negative space available for allocation, which is absurd, hence the
   limit may be exceeded.  However, whether it actually will be exceeded
   depends on the actual amount of live data at next GC, so compute size 
   to be exactly large enough to allow 0 allocation; that way, by the time
   the next collection happens, enough data may have died to allow the 
   collection to take place without exceeding the limit.  

   This appears to work well in practice, although it does depend on
   the program actually staying within the limit most of the time and
   having spikes in live data that confuses the estimate.  That's OK,
   because the limit is really for use with benchmarking where live sizes
   can be estimated with some certainty.  Other programs should use the
   load factor for control.
   */
int gc_compute_dynamic_size( gc_t *gc, int D, int S, int Q, double L, 
                             int lower_limit, int upper_limit )
{
  int live = D+S+Q;
  int M = (int)(L*live);
  int est_live_next_gc = live;        /* == M/L */

  if (lower_limit && upper_limit == lower_limit) {
    /* Fixed heap size */
    return roundup_page( upper_limit - (int)(upper_limit/L) );
  }

  if (lower_limit) {
    /* Must adjust estimate of live at next GC to avoid inflating
       the allocation budget beyond reason, if not much is live at
       present.
       */
    M = umax( M, lower_limit );
    est_live_next_gc = umax( M/L, est_live_next_gc );
  }
  
  if (!DATA(gc)->shrink_heap) {
    gclib_stats_t stats;

    gclib_stats( &stats );
    M = umax( M, stats.heap_allocated_max ); /* use no less than before */
  }

  if (upper_limit > 0) {
    int newM = min( M, upper_limit );
    int avail = (newM - live - est_live_next_gc);
    if (avail < 0)
      M = newM + roundup_page( abs( avail ) );        /* _Minimal_ amount */
    else
      M = newM;
  }

  annoyingmsg( "New heap size by policy should be %d bytes", M );

  return roundup_page( M - est_live_next_gc );
}

void gc_parameters( gc_t *gc, int op, int *ans )
{
  gc_data_t *data = DATA(gc);

  assert( op >= 0 && op <= data->generations );

  if (op == 0) {
    ans[0] = data->is_partitioned_system;
    ans[1] = data->generations;
  }
  else {
    /* info about generation op-1
       ans[0] = type: 0=nursery, 1=two-space, 2=np-old, 3=np-young, 4=static
       ans[1] = size in bytes [the 'maximum' field]
       ans[2] = parameter if appropriate for type
                   if np, then k
                   otherwise, 0=fixed, 1=expandable
       ans[3] = parameter if appropriate for type
                   if np, then j
       */
    /* Can you recognize a mess when you see one? */
    op--;
    if (op == 0) {
      ans[1] = gc->young_area->maximum;
      if (data->is_partitioned_system) {
        /* Nursery */
        ans[0] = 0;
        ans[2] = 0;
      }
      else {
        /* Stopcopy area */
        ans[0] = 1;
        ans[2] = 1;
      }
    }
    else if (op-1 < DATA(gc)->ephemeral_area_count) {
      ans[0] = 1;
      ans[1] = DATA(gc)->ephemeral_area[op-1]->maximum;
      ans[2] = 0;
    }
    else if (op < data->static_generation &&
             DATA(gc)->dynamic_area &&
             data->use_np_collector) {
#if ROF_COLLECTOR
      int k, j;

      /* Non-predictive dynamic area */
      np_gc_parameters( DATA(gc)->dynamic_area, &k, &j );
      ans[2] = k;
      ans[3] = j;
      if (op-1 == DATA(gc)->ephemeral_area_count) {
        /* NP old */
        ans[0] = 2;
        ans[1] = (DATA(gc)->dynamic_area->maximum / k) * (k - j);
      }
      else {
        ans[0] = 3;
        ans[1] = (DATA(gc)->dynamic_area->maximum / k) * j;
      }
#endif
    }
    else if (op-1 == DATA(gc)->ephemeral_area_count
             && DATA(gc)->dynamic_area) {
      /* Dynamic area */
      ans[0] = 1;
      ans[1] = DATA(gc)->dynamic_area->maximum;
      ans[2] = 1;
    }
    else if (gc->static_area) {
      /* Static area */
      ans[0] = 4;
      ans[1] = gc->static_area->allocated; /* [sic] */
      ans[2] = 1;
    }
  }
}

static int initialize( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  int i;

  if (!yh_initialize( gc->young_area ))
    return 0;

  if (DATA(gc)->ephemeral_area)
    for ( i = 0 ; i < DATA(gc)->ephemeral_area_count  ; i++ )
      if (!oh_initialize( DATA(gc)->ephemeral_area[i] ))
        return 0;

  if (DATA(gc)->dynamic_area)
    if (!oh_initialize( DATA(gc)->dynamic_area ))
      return 0;

  if (gc->static_area)
    if (!sh_initialize( gc->static_area ))
      return 0;

  if (data->is_partitioned_system) {
    wb_setup( gclib_desc_g,
#if GCLIB_LARGE_TABLE
              (byte*)0,
#else
              (byte*)gclib_pagebase,
#endif
              data->generations,
              data->globals,
              data->ssb_top,
              data->ssb_lim, 
              &data->satb_ssb_top,
              &data->satb_ssb_lim,
              (data->use_np_collector ? data->generations-1 : -1 ),
              gc->np_remset
             );
  }
  else
    wb_disable_barrier( data->globals );

  annoyingmsg( "\nGC type: %s", gc->id );

  gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_mutator );

  return 1;
}
  
static word *allocate( gc_t *gc, int nbytes, bool no_gc, bool atomic )
{
  assert( nbytes > 0 );

  nbytes = roundup_balign( nbytes );
  if (nbytes > LARGEST_OBJECT)
    panic_exit( "Can't allocate an object of size %d bytes: max is %d bytes.",
                nbytes, LARGEST_OBJECT );

  if (DATA(gc)->ephemeral_area_count > 0 && 
      nbytes > DATA(gc)->ephemeral_area[ 0 ]->maximum ) {
    consolemsg("allocate: holy cow; nbytes=%d and max=%d",
               nbytes, DATA(gc)->ephemeral_area[0]->maximum );
  }

  return yh_allocate( gc->young_area, nbytes, no_gc );
}

static word *allocate_nonmoving( gc_t *gc, int nbytes, bool atomic )
{
  assert( nbytes > 0 );

  if (gc->static_area == 0)
    panic_exit( "Can't allocate nonmoving in systems with no static heap." );

  nbytes = roundup_balign( nbytes );
  if (nbytes > LARGEST_OBJECT)
    panic_exit( "Can't allocate an object of size %d bytes: max is %d bytes.",
                nbytes, LARGEST_OBJECT );

  return sh_allocate( gc->static_area, nbytes );
}

static void make_room( gc_t *gc ) 
{
  yh_make_room( gc->young_area );
}

static void collect_generational( gc_t *gc, 
                                  int gen, 
                                  int bytes_needed, 
                                  gc_type_t request )
{
  gclib_stats_t stats;
  gc_data_t *data = DATA(gc);

  assert( gen >= 0 );
  assert( gen > 0 || bytes_needed >= 0 );

  assert( data->in_gc >= 0 );

  if (data->in_gc++ == 0) {
    gc_signal_moving_collection( gc ); /* should delegate to collector */
    data->pause_timer_elapsed = stats_start_timer( TIMER_ELAPSED );
    data->pause_timer_cpu = stats_start_timer( TIMER_CPU );
    gc_phase_shift( gc, gc_log_phase_mutator, gc_log_phase_misc_memmgr );
    before_collection( gc );
  }
  
  if (request == GCTYPE_EVACUATE) {
    if (gen < DATA(gc)->ephemeral_area_count) {
      /* A evacuation out of gen=k is turned into a promotion into gen=k+1,
       * which has index=gen in the ephemeral_area array (but gen_no=gen+1 !)
       */
      oh_collect( DATA(gc)->ephemeral_area[ gen ], GCTYPE_PROMOTE );
    } else if (DATA(gc)->dynamic_area) {
      oh_collect( DATA(gc)->dynamic_area, GCTYPE_PROMOTE );
    } else {
      /* Both kinds of young heaps ignore the request parameter, 
       * so the third argument below isn't relevant. */
      yh_collect( gc->young_area, bytes_needed, GCTYPE_PROMOTE );
    }
  } else if (gen == 0) {
    yh_collect( gc->young_area, bytes_needed, request );
  } else if (gen-1 < DATA(gc)->ephemeral_area_count) {
    oh_collect( DATA(gc)->ephemeral_area[ gen-1 ], request );
  } else if (DATA(gc)->dynamic_area) {
    oh_collect( DATA(gc)->dynamic_area, request );
  } else {
    /* Both kinds of young heaps ignore the request parameter, 
     * so the third argument below isn't relevant. */
    yh_collect( gc->young_area, bytes_needed, request );
  }

  assert( data->in_gc > 0 );

  if (--data->in_gc == 0) {
    int pause_elapsed, pause_cpu;
    after_collection( gc );

    /* XXX where should the phase shift go?
     * 
     * The stats_following_gc invocation needs the pause time
     * measurement, so the question is whether we would prefer for the
     * max pause time to include time spent maintaining the MMU log,
     * or if we want the MMU log to include the time spent updating
     * the stats data.
     * 
     * (Felix does not think we can have it both ways.)
     */
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_mutator );
    DATA(gc)->last_pause_cpu = 
      stats_stop_timer( data->pause_timer_cpu );
    DATA(gc)->last_pause_elapsed = 
      stats_stop_timer( data->pause_timer_elapsed );

    stats_following_gc( gc );

    gclib_stats( &stats );
    annoyingmsg( "  Memory usage: heap %d, remset %d, RTS %d words",
                 stats.heap_allocated, stats.remset_allocated, 
                 stats.rts_allocated );
    annoyingmsg( "  Max heap usage: %d words", stats.heap_allocated_max );
  }
}

static int next_rgn( int rgn, int num_rgns ) {
  rgn++;
  if (rgn > num_rgns) 
    rgn = 1;
  return rgn;
}

/* The number represents how many cycles per expansion. (first guess is 1) */
#define WEIGH_PREV_ESTIMATE_LOADCALC 0
#define USE_ORACLE_TO_VERIFY_REMSETS 0
#define USE_ORACLE_TO_VERIFY_SUMMARIES 0
#define USE_ORACLE_TO_VERIFY_SMIRCY 0
#define USE_ORACLE_TO_VERIFY_FWDFREE 0
#define SMIRCY_RGN_STACK_IN_ROOTS 1
#define SYNC_REFINEMENT_RROF_CYCLE 0
#define DONT_USE_REFINEMENT_COUNTDOWN 1
#define PRINT_SNAPSHOT_INFO_TO_CONSOLE 0
#define PRINT_INCOMING_REFS_INFO_TO_CONSOLE 0
#define USE_ORACLE_TO_CHECK_SUMMARY_SANITY 0
#define USE_URS_WRAPPER_TO_CHECK_REMSET_SANITY 0
#define INCREMENTAL_REFINE_DURING_SUMZ 1

#if 0
/* (The essential idea illustrating the interface) */
#define GENERIC_VERIFICATION_POINT( gc, PRED, fcn ) \
  do { if (PRED) fcn(gc); } while (0)
#else
/* (The more general approach allowing one to skip early checks) */
#define GENERIC_VERIFICATION_POINT( gc, PRED, verify_fcn )       \
  do {                                                           \
    if ((PRED) && (DATA(gc)->oracle_countdown > 0)) {            \
      if (DATA(gc)->oracle_countdown == 1) {                     \
        verify_fcn( gc );                                        \
        DATA(gc)->oracle_pointsrun += 1;                         \
        if ((DATA(gc)->oracle_pointsrun % 10) == 9) {          \
          unsigned ms;                                           \
          ms = osdep_realclock();                                \
          consolemsg("oracle_pointsrun: %d clock: %d.%d secs",   \
                     DATA(gc)->oracle_pointsrun,                 \
                     (ms/1000),                                  \
                     ((ms%1000)/100));                           \
        }                                                        \
      } else {                                                   \
        DATA(gc)->oracle_countdown -= 1;                         \
      }                                                          \
    }                                                            \
  } while (0)
#endif

#define REMSET_VERIFICATION_POINT( gc )         \
  GENERIC_VERIFICATION_POINT( gc, USE_ORACLE_TO_VERIFY_REMSETS, verify_remsets_via_oracle)

#define NURS_SUMMARY_VERIFICATION_POINT( gc )   \
  GENERIC_VERIFICATION_POINT( gc, USE_ORACLE_TO_VERIFY_REMSETS, verify_nursery_summary_via_oracle )

#define SUMMMTX_VERIFICATION_POINT( gc )        \
  GENERIC_VERIFICATION_POINT( gc, USE_ORACLE_TO_VERIFY_SUMMARIES, verify_summaries_via_oracle )

#define SMIRCY_VERIFICATION_POINT( gc )         \
  GENERIC_VERIFICATION_POINT( gc, USE_ORACLE_TO_VERIFY_SMIRCY, verify_smircy_via_oracle )

#define FWDFREE_VERIFICATION_POINT( gc )         \
  GENERIC_VERIFICATION_POINT( gc, USE_ORACLE_TO_VERIFY_FWDFREE, verify_fwdfree_via_oracle )

#define quotient2( x, y ) (((x) == 0) ? 0 : (((x)+(y)-1)/(y)))

/* Felix used these defaults. */

/*
static const double default_popularity_factor = 8.0;
static const double default_infamy_factor = 10000.0;
static const double default_sumz_budget_inv = 2.0;
static const double default_sumz_coverage_inv = 3.0;
static const int    default_sumz_max_retries  = 1;
*/

/*
 * FIXME
 *
 * According to section 9.2 of Felix's thesis, these three sets
 * of parameters work pretty well:
 *
 * S  (popularity)               8  6     4
 * F1 (sumzcoverage)             2  1.75  2
 * F2 (sumzbudget)               2  2.75  3
 * F3 (sumzretries)              1  2     2
 *
 * but the second and third columns don't work.
 * They do work if sumzretries is 1 instead of 2.
 *
 */

#if PARAMETERS8221
static const double default_popularity_factor = 8.0;
static const double default_infamy_factor = 2.0;
static const double default_sumz_coverage_inv = 2.0;
static const double default_sumz_budget_inv = 2.0;
static const int    default_sumz_max_retries  = 1;
#elif PARAMETERS6121
static const double default_popularity_factor = 6.0;
static const double default_infamy_factor = 2.0;
static const double default_sumz_coverage_inv = 1.75;
static const double default_sumz_budget_inv = 2.75;
static const int    default_sumz_max_retries  = 1;
#else
static const double default_popularity_factor = 4.0;
static const double default_infamy_factor = 2.0;
static const double default_sumz_coverage_inv = 2.0;
static const double default_sumz_budget_inv = 3.0;
static const int    default_sumz_max_retries  = 1;
#endif

static void smircy_start( gc_t *gc ) 
{
  if (DATA(gc)->region_count < 2) {
    /* almost no reason to do mark refine on a single region.
     * (only "benefit" I see is refining static area's remset.) */
    return;
  }
  gc->smircy = smircy_begin_opt( gc, gc->gno_count, 
                                 DATA(gc)->rrof_alloc_mark_bmp_once );
  assert( DATA(gc)->globals[G_CONCURRENT_MARK] == 0 );
  DATA(gc)->globals[G_CONCURRENT_MARK] = 1;
  { int i; 
    for ( i = 0; i < DATA(gc)->ephemeral_area_count; i++ )
      DATA(gc)->ephemeral_area[i]->incoming_words.marker = 0; 
  }
  smircy_push_roots( gc->smircy );
  if (DATA(gc)->summaries != NULL) {
    sm_push_nursery_summary( DATA(gc)->summaries, gc->smircy );
  }

  DATA(gc)->since_developing_snapshot_began.words_promoted = 0;
  DATA(gc)->since_developing_snapshot_began.count_promotions = 0;
  DATA(gc)->rrof_mark_cycles_begun_in_this_full_cycle += 1;
}

static bool scan_refine_remset( word loc, void *data )
{
  smircy_context_t *context = (smircy_context_t*)data;
  if (smircy_object_marked_p( context, loc )) {
    return TRUE;
  } else {
    return FALSE;
  }
}

/* (Once the marking is concurrent, we should dynamically determine
 * the budget based on how much progress the concurrent marker has
 * made, to ensure that we keep up with the policy determined by the
 * refinement parameter.)
 */

static void refine_remsets_via_marksweep( gc_t *gc )
{
  /* use mark/sweep system to refine the remembered set. */
  urs_enumerate( gc->the_remset, FALSE, scan_refine_remset, gc->smircy );
}

static void zeroed_promotion_counts( gc_t* gc ) 
{
  DATA(gc)->since_finished_snapshot_began.words_promoted     = 0;
  DATA(gc)->since_developing_snapshot_began.words_promoted   = 0;
  DATA(gc)->since_cycle_began.words_promoted                 = 0;
  DATA(gc)->since_finished_snapshot_at_time_cycle_began_began
    .words_promoted = 0;

  DATA(gc)->since_finished_snapshot_began.count_promotions   = 0;
  DATA(gc)->since_developing_snapshot_began.count_promotions = 0;
  DATA(gc)->since_cycle_began.count_promotions               = 0;
  DATA(gc)->since_finished_snapshot_at_time_cycle_began_began
    .count_promotions = 0;
}

static void update_promotion_counts( gc_t *gc, int words_promoted )
{
  DATA(gc)->since_finished_snapshot_began.words_promoted   += words_promoted;
  DATA(gc)->since_developing_snapshot_began.words_promoted += words_promoted;
  DATA(gc)->since_cycle_began.words_promoted               += words_promoted;
  DATA(gc)->since_finished_snapshot_at_time_cycle_began_began
    .words_promoted += words_promoted;

  DATA(gc)->mutator_effort.words_promoted_this.full_cycle += words_promoted;
  DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle += words_promoted;
  DATA(gc)->mutator_effort.max_words_promoted_any.full_cycle =
    max( DATA(gc)->mutator_effort.max_words_promoted_any.full_cycle,
         DATA(gc)->mutator_effort.words_promoted_this.full_cycle );
  DATA(gc)->mutator_effort.max_words_promoted_any.sumz_cycle =
    max( DATA(gc)->mutator_effort.max_words_promoted_any.sumz_cycle,
         DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle );

  DATA(gc)->since_finished_snapshot_began.count_promotions   += 1;
  DATA(gc)->since_developing_snapshot_began.count_promotions += 1;
  DATA(gc)->since_cycle_began.count_promotions               += 1;
  DATA(gc)->since_finished_snapshot_at_time_cycle_began_began 
    .count_promotions += 1;
}

static void reset_countdown_to_next_refine( gc_t *gc )
{
  int marked, words_marked; 
  smircy_context_t *context;
  context = gc->smircy;

  if (DATA(gc)->rrof_refine_mark_period > 0) {
    DATA(gc)->rrof_refine_mark_countdown = 
      DATA(gc)->rrof_refine_mark_period;
  } else if (DATA(gc)->rrof_has_refine_factor) {
    double R = DATA(gc)->rrof_refinement_factor;
    int new_countdown;
    marked = smircy_objs_marked( context );
    words_marked = smircy_words_marked( context );
    /* FIXME: does this make any sense? */
    new_countdown = 
      (int)((R*(double)(sizeof(word)*marked*2))
            / ((double)gc->young_area->maximum));
    assert( new_countdown >= 0 );
    DATA(gc)->rrof_refine_mark_countdown += new_countdown;
    DATA(gc)->rrof_refine_mark_countdown = 
      max( DATA(gc)->rrof_refine_mark_countdown,
           DATA(gc)->region_count );
    DATA(gc)->rrof_last_live_estimate = sizeof(word)*words_marked;
    DATA(gc)->last_live_words = words_marked;
    DATA(gc)->max_live_words = 
      umax( DATA(gc)->max_live_words, words_marked );

    DATA(gc)->since_finished_snapshot_began.words_promoted =
      DATA(gc)->since_developing_snapshot_began.words_promoted;
    DATA(gc)->since_finished_snapshot_began.count_promotions =
      DATA(gc)->since_developing_snapshot_began.count_promotions;

#if 0
    if (0) annoyingmsg("revised mark countdown: %d", new_countdown );
#endif
  } else {
    assert(0);
  }
  
}

static int add_region_to_expand_heap( gc_t *gc, int maximum_allotted )
{
  semispace_t *ss = gc_fresh_space(gc);
  int eidx = ss->gen_no - 1;
  old_heap_t *fresh_heap = DATA(gc)->ephemeral_area[ eidx ];
  assert2( fresh_heap->bytes_live_last_major_gc == 0 );
  return maximum_allotted + fresh_heap->maximum;
}

static void initialize_summaries( gc_t *gc, bool about_to_major );

static int nonempty_region_count( gc_t *gc ) 
{
  int region_count;
  region_count = 
    (DATA(gc)->region_count 
     + 1 /* unfilled may contain one non-empty to-space region */
     - region_group_count( region_group_unfilled ));
  return region_count;
}

static void rrof_completed_major_collection( gc_t *gc ) 
{
  if (DATA(gc)->print_float_stats_each_major)
    print_float_stats( "major ", gc );
}

static void rrof_completed_minor_collection( gc_t *gc )
{
  if (DATA(gc)->print_float_stats_each_minor)
    print_float_stats( "minor ", gc );
}

static void rrof_calc_target_allocation( gc_t *gc, 
                                         long long *N_recv, 
                                         long long *N_old_recv,
                                         long long *P_old_recv,
                                         long long *A_this_recv, 
                                         long long *A_target_1_recv,
                                         long long *A_target_2_recv, 
                                         long long *A_target_recv );

int debug_counter = 0; /* FIXME */

static void rrof_completed_regional_cycle( gc_t *gc ) 
{
  long long allocation_target;

  DATA(gc)->rrof_cycle_count += 1;

  /* FIXME */
  annoyingmsg( "COMPLETED FULL CYCLE %d (%d)",
               DATA(gc)->rrof_cycle_count, debug_counter );

  if (DATA(gc)->print_float_stats_each_cycle)
    print_float_stats( "cycle ", gc );

  if (DATA(gc)->region_count < 2) {
    int live_words = gc_allocated_to_areas( gc, gset_singleton( 1 ));
    if (gc->static_area != NULL) {
      live_words += (gc->static_area->data_area->allocated / sizeof(word));
    }
    DATA(gc)->last_live_words = live_words;
    DATA(gc)->max_live_words = umax( DATA(gc)->max_live_words, live_words );
    zeroed_promotion_counts( gc );
  }

  DATA(gc)->since_cycle_began.words_promoted   = 0;
  DATA(gc)->since_cycle_began.count_promotions = 0;
  DATA(gc)->last_live_words_at_time_cycle_began = 
    DATA(gc)->last_live_words;
  DATA(gc)->max_live_words_at_time_cycle_began = 
    max(DATA(gc)->last_live_words_at_time_cycle_began,
        DATA(gc)->max_live_words_at_time_cycle_began);
  DATA(gc)->since_finished_snapshot_at_time_cycle_began_began =
    DATA(gc)->since_finished_snapshot_began;

  /* If we completed a mark cycle in the last full cycles, then sticking
     to minor collections is working so far; stick with it.  Otherwise
     allow smircy steps during major collections for (just) next full cycle.
  */
  DATA(gc)->rrof_smircy_step_on_minor_collections_alone = 
    ( ! ((DATA(gc)->region_count >= 2) &&
         (DATA(gc)->rrof_mark_cycles_run_in_this_full_cycle == 0)));
  /* FIXME */
  if ( ! (DATA(gc)->rrof_smircy_step_on_minor_collections_alone) )
    annoyingmsg( "ENABLING MARKING DURING MAJOR GC" );

  DATA(gc)->rrof_mark_cycles_run_in_this_full_cycle = 0;
  DATA(gc)->rrof_mark_cycles_begun_in_this_full_cycle = 0;

#if SYNC_REFINEMENT_RROF_CYCLE
  if (gc->smircy == NULL)
    smircy_start( gc );
#endif

  DATA(gc)->region_count = DATA(gc)->ephemeral_area_count;
  DATA(gc)->rrof_cycle_majors_total = nonempty_region_count( gc );
  DATA(gc)->rrof_cycle_majors_sofar = 0;

  rrof_calc_target_allocation( gc, NULL, NULL, NULL, NULL, 
                               NULL, NULL, &allocation_target );

  assert( allocation_target == (long long)((int)allocation_target) );
  DATA(gc)->allocation_target_for_cycle = (int) allocation_target;

  if ((DATA(gc)->summaries == NULL) && (DATA(gc)->region_count > 4)) {
    initialize_summaries( gc, FALSE );
  }

  DATA(gc)->mutator_effort.satb_ssb_entries_flushed_this.full_cycle = 0;
  DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.full_cycle = 0;
  DATA(gc)->mutator_effort.words_promoted_this.full_cycle = 0;
}

#if PRINT_INCOMING_REFS_INFO_TO_CONSOLE
static void print_incoming_summarizer( gc_t *gc, char *prefix ) 
{
    int gno, words, mega_words, mega_words_remainder;
    int MILLION = 1000000;
    fprintf( stdout, "%s incoming summarizer ", prefix );
    for ( gno = 1; gno < gc->gno_count; gno++ ) {
      if (gno == gc->static_area->data_area->gen_no)
        continue;
      words = gc_heap_for_gno( gc, gno )->incoming_words.summarizer;
      mega_words = words / MILLION;
      mega_words_remainder =  (words % MILLION) / (MILLION / 100);
      if (words == 0) {
        fprintf( stdout, "    0%s ", 
                 region_group_name( gc_region_group_for_gno( gc, gno )));
      } else {
        fprintf( stdout, "%2d.%02d%s ", mega_words, mega_words_remainder,
                 region_group_name( gc_region_group_for_gno( gc, gno )));
      }
    }
    fprintf( stdout, "\n" );
    fflush( stdout );
}

static void print_incoming_marker( gc_t * gc, char *prefix ) 
{
    int gno, words, mega_words, mega_words_remainder;
    int MILLION = 1000000;
    fprintf( stdout, "%s incoming marker     ", prefix );
    for ( gno = 1; gno < gc->gno_count; gno++ ) {
      if (gno == gc->static_area->data_area->gen_no)
        continue;
      words = gc_heap_for_gno( gc, gno )->incoming_words.marker;
      mega_words = words / MILLION;
      mega_words_remainder = (words % MILLION) / (MILLION / 100);
      if (words == 0) {
        fprintf( stdout, "    0%s ", 
                 region_group_name( gc_region_group_for_gno( gc, gno )));
      } else {
        fprintf( stdout, "%2d.%02d%s ", mega_words, mega_words_remainder,
                 region_group_name( gc_region_group_for_gno( gc, gno )));
      }
    }
    fprintf( stdout, "\n" );
    fflush( stdout );
}
#endif

static void rrof_completed_summarization_cycle( gc_t *gc ) 
{

#if PRINT_INCOMING_REFS_INFO_TO_CONSOLE
  { 
    print_incoming_summarizer( gc, "sumz" );
    print_incoming_marker(     gc, "    " );
  }
#endif
  DATA(gc)->mutator_effort.satb_ssb_entries_flushed_this.sumz_cycle = 0;
  DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle = 0;
  DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle = 0;
}

static void start_timers( stats_id_t *timer1, stats_id_t *timer2 )
{
  *timer1 = stats_start_timer( TIMER_ELAPSED );
  *timer2 = stats_start_timer( TIMER_CPU );
}

static void stop_sumrize_timers( gc_t *gc, 
                                 stats_id_t *timer1, stats_id_t *timer2 ) 
{
  int ms, ms_cpu;
  ms     = stats_stop_timer( *timer1 );
  ms_cpu = stats_stop_timer( *timer2 );
  
  DATA(gc)->stat_last_ms_remset_sumrize     = ms;
  DATA(gc)->stat_last_ms_remset_sumrize_cpu = ms_cpu;
}

static void stop_markm_timers( gc_t *gc, 
                                 stats_id_t *timer1, stats_id_t *timer2 )
{
  int ms, ms_cpu;
  ms     = stats_stop_timer( *timer1 );
  ms_cpu = stats_stop_timer( *timer2 );
  
  DATA(gc)->stat_last_ms_smircy_mark     = ms;
  DATA(gc)->stat_last_ms_smircy_mark_cpu = ms_cpu;
}

static void stop_refinem_timers( gc_t *gc, 
                                 stats_id_t *timer1, stats_id_t *timer2 )
{
  int ms, ms_cpu;
  ms     = stats_stop_timer( *timer1 );
  ms_cpu = stats_stop_timer( *timer2 );
  
  DATA(gc)->stat_last_ms_smircy_refine     = ms;
  DATA(gc)->stat_last_ms_smircy_refine_cpu = ms_cpu;
}

static void handle_secondary_space( gc_t *gc ) 
{
  if (DATA(gc)->secondary_space != NULL) {
    int gen_no = DATA(gc)->secondary_space->gen_no;
    ss_sync( DATA(gc)->secondary_space );
    oh_assimilate( DATA(gc)->ephemeral_area[ gen_no-1 ],
                   DATA(gc)->secondary_space );
    DATA(gc)->secondary_space = NULL;
  }
}

static void summarization_step( gc_t *gc, bool about_to_major )
{
  stats_id_t timer1, timer2;
  int word_countdown = -1, object_countdown = -1;
  int ne_rgn_count;
  int dA;
  bool completed_cycle;
  double mut_activity_sumz;
  double cN;
  double m_cN;

  REMSET_VERIFICATION_POINT(gc);
  SUMMMTX_VERIFICATION_POINT(gc);

  assert( DATA(gc)->summaries != NULL );
  ne_rgn_count = nonempty_region_count( gc );

  /*  FIXME
   *
   *  As a temporary workaround, the code below is commented out.
   *
   *  The mutator activity during any summarization cycle is limited to cN
   *  (as calculated by calc_cN).  The code below delays this accounting
   *  until the summarization process actually starts, which would be okay
   *  so long as some separate accounting prevents the ratio of filled to
   *  ready regions from increasing too much before summarization starts.
   *  Unfortunately, that accounting is not being done or is being done
   *  incorrectly.
   *  
   *  Letting the cN accounting determine initiation of summarization
   *  isn't really the right thing, but it will work until we fix the
   *  other accounting.
   */

#if 0
  if (sm_progress_would_no_op( DATA(gc)->summaries, ne_rgn_count )) {
    DATA(gc)->mutator_effort.satb_ssb_entries_flushed_this.sumz_cycle = 0;
    DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle = 0;
    DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle = 0;
    return;
  }
#endif

  start_timers( &timer1, &timer2 );

  dA = quotient2(DATA(gc)->allocation_target_for_cycle, 
                 (DATA(gc)->rrof_words_per_region_min 
                  * DATA(gc)->rrof_cycle_majors_total));

  mut_activity_sumz = calc_mutator_activity_sumz( gc );
  cN = calc_cN( gc );
  m_cN = mut_activity_sumz / cN;

  completed_cycle =
    sm_construction_progress( DATA(gc)->summaries, 
                              DATA(gc)->rrof_next_region, 
                              ne_rgn_count, 
                              about_to_major,
                              FALSE,
                              dA,
                              m_cN );

  stop_sumrize_timers( gc, &timer1, &timer2 );

#if 0
  /* FIXME */
  if (DATA(gc)->stat_last_ms_remset_sumrize_cpu > 200)
    consolemsg( "SUMMARIZATION PAUSE = %d ********** (%d) "
                 "%d %d %d %d %d%%",
                 DATA(gc)->stat_last_ms_remset_sumrize_cpu,
                 debug_counter,
                 DATA(gc)->rrof_next_region, ne_rgn_count, 
                 about_to_major, dA, (int) (100.0 * m_cN) );
#endif

  if (completed_cycle) {
    annoyingmsg( "COMPLETED SUMMARIZATION CYCLE" );  /* FIXME */
    rrof_completed_summarization_cycle( gc );
  }

  SUMMMTX_VERIFICATION_POINT(gc);
}

static void initiate_refinement_during_summarization( gc_t *gc )
{
  { 
    /* marking is done; the snapshot object graph will remain fixed
     * until we dispose of it (newly allocated objects will still be
     * added to it; this just avoids cost of SATB write barrier). */
    DATA(gc)->globals[G_CONCURRENT_MARK] = 0; 
  }

  sm_start_refinement( DATA(gc)->summaries );
}

static void initiate_refinement( gc_t *gc )
{
  if (DATA(gc)->summaries != NULL) {
    if (smircy_in_construction_stage_p( gc->smircy )) {
      if (DATA(gc)->print_float_stats_each_refine)
        print_float_stats( "prefin", gc );
      initiate_refinement_during_summarization( gc );
    } else {
      assert( smircy_in_refinement_stage_p( gc->smircy ));
      assert( DATA(gc)->globals[G_CONCURRENT_MARK] == 0 );
    }
  } else {
    if (DATA(gc)->print_float_stats_each_refine)
      print_float_stats( "prefin", gc );
    refine_remsets_via_marksweep( gc );
    reset_countdown_to_next_refine( gc );
  }
}

static void incremental_refinement_has_completed( gc_t *gc )
{
#if PRINT_INCOMING_REFS_INFO_TO_CONSOLE
  { 
    print_incoming_summarizer( gc, "    " );
    print_incoming_marker(     gc, "mark" );
  }
#endif

  reset_countdown_to_next_refine( gc ); /* XXX still necessary/meaningful? */

  if (DATA(gc)->rrof_mark_cycles_begun_in_this_full_cycle > 0)
    DATA(gc)->rrof_mark_cycles_run_in_this_full_cycle += 1;

  if (DATA(gc)->print_float_stats_each_refine && 
      ! DATA(gc)->print_float_stats_each_major)
    print_float_stats( "pstfin", gc );


  smircy_end( gc->smircy );
  gc->smircy = NULL;
  assert( DATA(gc)->globals[G_CONCURRENT_MARK] == 0 );
}

typedef enum { 
  smircy_step_dont_refine, smircy_step_can_refine, smircy_step_must_refine
} smircy_step_finish_mode_t;

static void smircy_step( gc_t *gc, smircy_step_finish_mode_t finish_mode ) 
{
  stats_id_t timer1, timer2;
  int marked_recv = 0, traced_recv = 0, words_marked_recv = 0;
  int misc_recv = 0;

  REMSET_VERIFICATION_POINT(gc);
  NURS_SUMMARY_VERIFICATION_POINT(gc);
  SMIRCY_VERIFICATION_POINT(gc);

  if (gc->smircy != NULL 
      && smircy_in_completed_stage_p( gc->smircy )) {
    incremental_refinement_has_completed( gc );
  }

#if ! SYNC_REFINEMENT_RROF_CYCLE
  if (DATA(gc)->region_count <= 2)
    return;
  start_timers( &timer1, &timer2 );
  if (gc->smircy == NULL) {
    smircy_start( gc );
  }
#else 
  if (gc->smircy == NULL) {
    return;
  }
  start_timers( &timer1, &timer2 );
#endif

  SMIRCY_VERIFICATION_POINT(gc);

  {
    int promoted = gc->words_from_nursery_last_gc;
    int bound = (((double)gc->young_area->maximum / sizeof(word))
                 / (DATA(gc)->rrof_refinement_factor));
#if 0
    bound /= (1 + DATA(gc)->rrof_mark_cycles_run_in_this_full_cycle);
#endif
#if 1
    /* FIXME: seems to help, but questionable */
    /* See also the help message in larceny.c */
    bound = 5 * bound;
#endif
    smircy_progress( gc->smircy, bound, bound, bound, SMIRCY_MISC_BOUND,
                     &marked_recv, &traced_recv, &words_marked_recv,
                     &misc_recv );
  }

  SMIRCY_VERIFICATION_POINT(gc);

  if (smircy_stack_empty_p( gc->smircy )) {
    int words_marked;
    words_marked = smircy_words_marked( gc->smircy );

    DATA(gc)->last_live_words = words_marked;
    DATA(gc)->max_live_words = 
      umax( DATA(gc)->max_live_words, words_marked );
    DATA(gc)->since_finished_snapshot_began =
      DATA(gc)->since_developing_snapshot_began;
  }

  stop_markm_timers( gc, &timer1, &timer2 );

  if ((finish_mode == smircy_step_must_refine) 
      || ((finish_mode == smircy_step_can_refine) 
          && smircy_stack_empty_p( gc->smircy ))) {
  start_timers( &timer1, &timer2 );
#if INCREMENTAL_REFINE_DURING_SUMZ
    initiate_refinement( gc );
#else
#error bring back  refine_metadata_via_marksweep(..)
#endif
  stop_refinem_timers( gc, &timer1, &timer2 );
  }


  REMSET_VERIFICATION_POINT(gc);
  NURS_SUMMARY_VERIFICATION_POINT(gc);
}

/* Called only once, at the completion of a full cycle. */

static void initialize_summaries( gc_t *gc, bool about_to_major ) 
{
  annoyingmsg("[FIXME] region_group_filled = %d", region_group_filled);
  annoyingmsg("[FIXME] region_group_count = %d",
              region_group_count( region_group_filled ));
  assert( region_group_count( region_group_filled ) >= 2 );
  {
    /* prime the pump */
    old_heap_t *oh;
    oh = region_group_first_heap( region_group_filled );
    region_group_enq( oh, region_group_filled, region_group_wait_nosum );
  }
  DATA(gc)->summaries =
    create_summ_matrix( gc, 1, DATA(gc)->region_count,
                        1.0 / DATA(gc)->rrof_sumz_params.coverage_inv,
                        1.0 / (DATA(gc)->rrof_sumz_params.budget_inv
                               * DATA(gc)->rrof_sumz_params.coverage_inv),
                        DATA(gc)->rrof_sumz_params.popularity_factor,
                        DATA(gc)->rrof_sumz_params.popularity_limit_words, 
                        about_to_major,
                        DATA(gc)->rrof_next_region,
                        DATA(gc)->rrof_sumz_params.coverage_inv,
                        DATA(gc)->rrof_sumz_params.budget_inv,
                        DATA(gc)->rrof_sumz_params.max_retries );

  DATA(gc)->mutator_effort.satb_ssb_entries_flushed_this.sumz_cycle = 0;
  DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle = 0;
  DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle = 0;
}

static void collect_rgnl_clear_summary( gc_t *gc, int rgn_next )
{
  if ( DATA(gc)->summaries != NULL ) { 
    sm_clear_summary( DATA(gc)->summaries, rgn_next, DATA(gc)->region_count );
  }
}

static void collect_rgnl_choose_next_region( gc_t *gc, int num_rgns, 
                                             bool about_to_major ) 
{
  int n;
  n = next_rgn(DATA(gc)->rrof_next_region,  num_rgns);
  DATA(gc)->rrof_next_region = n;
}

struct msgc_visit_check_summary_data {
  gc_t *gc;
  int rgn_next;
  locset_t *summary_as_ls;
};

static void* msgc_visit_check_summary( word obj, word src, int offset, 
                                       void *my_data )
{
  struct msgc_visit_check_summary_data *data;
  data = (struct msgc_visit_check_summary_data*)my_data;
  if (isptr(obj) && isptr(src)) {
    int obj_gno = gen_of(obj);
    int src_gno = gen_of(src);
    if ((obj_gno == data->rgn_next) && 
        (src_gno != obj_gno) &&
        (src_gno != 0)) {
      assert( ls_ismember_loc( data->summary_as_ls, 
                               make_loc( src, offset )));
    }
  }
  return my_data;
}

static void assert_summary_as_ls_complete( gc_t *gc, 
                                           int rgn_next, 
                                           locset_t *summary_as_ls )
{
  msgc_context_t *msgc;
  int ignm, ignt, ignw;
  struct msgc_visit_check_summary_data data;
  data.gc = gc;
  data.rgn_next = rgn_next;
  data.summary_as_ls = summary_as_ls;

  msgc = msgc_begin( gc );
  msgc_set_object_visitor( msgc, msgc_visit_check_summary, &data );
  msgc_mark_objects_from_roots( msgc, &ignw, &ignt, &ignw );
  msgc_end( msgc );
}
struct lsscan_summary_sound_data {
  msgc_context_t *msgc;
  int rgn_next;
  gc_t *gc;
};
static bool lsscan_summary_sound( loc_t loc, void *my_data )
{
  struct lsscan_summary_sound_data *data;
  msgc_context_t *msgc;
  word *slot;
  word val;
  bool bad_condition;
  data = (struct lsscan_summary_sound_data*)my_data;
  msgc = data->msgc;

  /* cannot generally assert this; removed filter from summ_matrix */
#if 0
  assert( gen_of(loc.obj) != data->rgn_next );
#endif

  slot = loc_to_slot(loc);
  val = *slot;
  bad_condition = (isptr(val) 
                   && (! msgc_object_marked_p( msgc, val ))
                   /* nursery objects get a pass b/c smircy cant filter them;
                    * its float, but some float is inevitable. */
                   && (gen_of(val) != 0));
  if (bad_condition) {
    word loc_obj = loc_to_obj(loc);
    consolemsg(  "UNSOUND SUMMARY 0x%08x (%d) -> 0x%08x (%d) "
                 "marked {obj->slot:N obj:%s, rs: %s, smircy obj->slot:%s obj:%s}", 
                 loc_obj, gen_of(loc_obj), val, gen_of(val), 
                 msgc_object_marked_p( msgc, loc_obj )?"Y":"N", 
                 urs_isremembered( data->gc->the_remset, loc_obj )?"Y":"N",
                 ((data->gc->smircy == NULL)?"n/a":
                  (smircy_object_marked_p( data->gc->smircy,     val )?"Y  ":"N  ")),
                 ((data->gc->smircy == NULL)?"n/a":
                  (smircy_object_marked_p( data->gc->smircy, loc_obj )?"Y  ":"N  "))
                 );
  }
  assert( ! bad_condition );
  return TRUE;
}

struct push_remset_entry_if_in_smircy_data {
  gc_t *gc;
  msgc_context_t *msgc;
};

static bool push_remset_entry_if_in_smircy( word obj, void *my_data )
{
  struct push_remset_entry_if_in_smircy_data *data =
    (struct push_remset_entry_if_in_smircy_data*)my_data;
  if (smircy_object_marked_p( data->gc->smircy, obj ))
    msgc_push_object( data->msgc, obj );
  return TRUE;
}

static void assert_summary_as_ls_sound( gc_t *gc,
                                        int rgn_next, locset_t *summary_as_ls )
{
  msgc_context_t *msgc;
  int ignm, ignt, ignw;

    /* note from Wed Jan 20 13:06:59 EST 2010
     * These are some notes on the use of (clone of) gc->smircy 
     * as msgc_context, and which mark traversal I should use.
     * They are out of date and perhaps useless now. */
    /* WANT to use msgc_mark_objects_from_roots_and_remsets as given
     * below (rather than msgc_mark_objects_from_nil) because smircy
     * is snapshot of the past that may not have objects that ARE in
     * the heap NOW. */
    /* note from Tue Jan 12 23:45:55 EST 2010
     * (reasoning immediately above sounds bogus to me; objects are
     *  marked when allocated, so why is it not safe to do
     *  msgc_mark_objects_from_nil?  A better reason is that the
     *  summaries hold imprecise data that the snapshot does not have;
     *  that is, the data in the remset is actually *older* than the
     *  snapshot, and that outdated-ness can propogate into the
     *  summary itself.  This reasoning sounds more plausible to me;
     *  note that it is indicating the exact *reverse* of the
     *  reasoning employed in the comment immediately above.)
     */

  if (gc->smircy != NULL && smircy_in_refinement_stage_p( gc->smircy )) {
    msgc = smircy_clone_begin( gc->smircy, FALSE );
    {
      int i;
      struct push_remset_entry_if_in_smircy_data data;
      data.gc = gc;
      data.msgc = msgc;
      for( i = 1; i < gc->gno_count; i++ ) {
        urs_enumerate_gno( gc->the_remset, TRUE, i, 
                           push_remset_entry_if_in_smircy, &data );
      }
    }
    msgc_mark_objects_from_roots( msgc, &ignw, &ignt, &ignw );
  } else {
    msgc = msgc_begin( gc );
    msgc_mark_objects_from_roots_and_remsets( msgc, &ignw, &ignt, &ignw );
  }
  { 
    struct lsscan_summary_sound_data data;
    data.msgc = msgc;
    data.rgn_next = rgn_next;
    data.gc = gc;
    ls_enumerate_locs( summary_as_ls, lsscan_summary_sound, (void*)&data );
  }
  msgc_end( msgc );
}
struct summaryscan_buildup_ls_data {
  locset_t *target_ls;
  int rgn_next;
};
static void summaryscan_buildup_ls( loc_t loc, void *my_data ) 
{ 
  word obj   = loc_to_obj(loc);
  int offset = loc_to_offset(loc);
  struct summaryscan_buildup_ls_data *data;
  locset_t *target_ls;
  static word last_obj = 0x0;
  static int last_offset = 0x0;
  FIXME_UNUSED_VARIABLE(last_offset);
  data = (struct summaryscan_buildup_ls_data*)my_data;
  target_ls = data->target_ls;

  /* cannot generally assert this; removed filter from summ_matrix */
#if 0
  assert( gen_of(obj) != data->rgn_next );
#endif

  ls_add_obj_offset( target_ls, obj, offset );
  last_obj = obj;
  last_offset = offset;
  FIXME_UNUSED_VARIABLE(last_obj);
  FIXME_UNUSED_VARIABLE(last_offset);
}

static void assert_summary_sanity( gc_t *gc, int rgn_next )
{
  static locset_t *summary_as_ls;
  if (summary_as_ls == NULL) {
    summary_as_ls = create_locset( 0, 0 );
  }
  ls_clear( summary_as_ls );
  { 
    struct summaryscan_buildup_ls_data data;
    data.target_ls = summary_as_ls;
    data.rgn_next = rgn_next;
    summary_enumerate_locs2( &DATA(gc)->summary, summaryscan_buildup_ls, &data );
    summary_dispose( &DATA(gc)->summary );
  }

  consolemsg( "using summary_as_ls for rgn_next=%d, live: %d",
              rgn_next, summary_as_ls->live );
  assert_summary_as_ls_complete( gc, rgn_next, summary_as_ls );
  assert_summary_as_ls_sound( gc, rgn_next, summary_as_ls );

  ls_init_summary( summary_as_ls, -1, &DATA(gc)->summary );
}

/* returns TRUE iff the collection took place.  If FALSE, reselect
 * rgn_to and rgn_next (aka goto collect_evacuate_nursery). */
static bool collect_rgnl_majorgc( gc_t *gc, 
                                  int rgn_to, int rgn_next, int num_rgns ) 
{
  int n;
  bool summarization_active, summarization_active_rgn_next;

  assert2( DATA(gc)->rrof_to_region == rgn_to );
  assert( *gc->ssb[rgn_to]->bot == *gc->ssb[rgn_to]->top );

  if (DATA(gc)->print_float_stats_each_major && 
      ! DATA(gc)->print_float_stats_each_minor)
    print_float_stats( "premaj", gc );

  REMSET_VERIFICATION_POINT(gc);
  NURS_SUMMARY_VERIFICATION_POINT(gc);

  summarization_active = (DATA(gc)->summaries != NULL);

  assert( (! summarization_active) || (rgn_next != rgn_to) );

  if (summarization_active) {
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
    summarization_step( gc, TRUE );
    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
  }

  REMSET_VERIFICATION_POINT(gc);
  NURS_SUMMARY_VERIFICATION_POINT(gc);

  summarization_active_rgn_next = 
    (summarization_active &&
     sm_is_rgn_summarized( DATA(gc)->summaries, rgn_next ));

  assert(! DATA(gc)->use_summary_instead_of_remsets );

  if ( ! summarization_active || 
       sm_majorgc_permitted( DATA(gc)->summaries, rgn_next )) {

    { 
      old_heap_t *heap;
      region_group_t grp;
      heap = DATA(gc)->ephemeral_area[ rgn_next-1 ];
      grp = heap->group;
      assert( (grp == region_group_wait_nosum)
              || (grp == region_group_wait_w_sum)
              || (heap->allocated == 0) /* XXX */
              || (grp == region_group_filled) /* XXX */);
    }

    REMSET_VERIFICATION_POINT(gc);
    NURS_SUMMARY_VERIFICATION_POINT(gc);
    SMIRCY_VERIFICATION_POINT(gc);

#if ! SMIRCY_RGN_STACK_IN_ROOTS
    assert2( rgn_next == DATA(gc)->rrof_next_region );
    if (gc->smircy != NULL) {
      gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_smircy );
      smircy_jit_process_stack_for_rgn( gc->smircy, rgn_next );
      smircy_drop_cleared_stack_entries( gc->smircy, rgn_next );
      gc_phase_shift( gc, gc_log_phase_smircy, gc_log_phase_misc_memmgr );
    }
#endif

    if (summarization_active_rgn_next) {
      gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
      sm_fold_in_nursery_and_init_summary( DATA(gc)->summaries,
                                           rgn_next, 
                                           &DATA(gc)->summary );
      gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
      if (0) consolemsg("folded nursery into summary, entries: %d", 
                 DATA(gc)->summary.entries);
    }
    assert(! DATA(gc)->use_summary_instead_of_remsets );

    SMIRCY_VERIFICATION_POINT(gc);
    SUMMMTX_VERIFICATION_POINT(gc);

    if (USE_ORACLE_TO_CHECK_SUMMARY_SANITY &&
        summarization_active_rgn_next) {
      assert_summary_sanity( gc, rgn_next );
    }

    if (USE_ORACLE_TO_VERIFY_SMIRCY && (gc->smircy != NULL)) {
      gc->smircy_completion = smircy_clone_begin( gc->smircy, FALSE );
      msgc_mark_objects_from_nil( gc->smircy_completion );
    }

    if (summarization_active) {
      gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
      sm_clear_contribution_to_summaries( DATA(gc)->summaries, rgn_next );
      gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
    }

    if (summarization_active_rgn_next) {
      DATA(gc)->use_summary_instead_of_remsets = TRUE;
      summary_vs_remset_msg( "FIXME: setting TRUE" );
    } else {
      DATA(gc)->enumerate_major_with_minor_remsets = TRUE;
    }

    assert2( DATA(gc)->rrof_to_region == rgn_to );
    {
      region_group_t grp = gc_region_group_for_gno( gc, rgn_to );
      assert( (grp == region_group_unfilled)
              || (grp == region_group_wait_nosum) /* XXX */
              || (grp == region_group_wait_w_sum) /* XXX */ );
    }

    DATA(gc)->ephemeral_area[ rgn_to-1 ]->was_target_during_gc = TRUE;
    oh_collect_into( DATA(gc)->ephemeral_area[ rgn_next-1 ], GCTYPE_COLLECT,
                     DATA(gc)->ephemeral_area[ rgn_to-1 ] );
    /* at this point, rrof_to_region and rgn_to have no guaranteed relationship */
    DATA(gc)->use_summary_instead_of_remsets = FALSE;
    summary_vs_remset_msg( "FIXME: setting FALSE" );
    DATA(gc)->enumerate_major_with_minor_remsets = FALSE;

    if (gc->smircy_completion != NULL) {
      smircy_clone_end( gc->smircy_completion );
      gc->smircy_completion = NULL;
    }

    REMSET_VERIFICATION_POINT(gc);

    if (summarization_active_rgn_next) {
      summary_dispose( &DATA(gc)->summary );
    }
    DATA(gc)->rrof_last_tospace = rgn_to;
    handle_secondary_space( gc );
    update_promotion_counts( gc, gc->words_from_nursery_last_gc );

    if (summarization_active_rgn_next) {
      int i;
      for (i = 0; i < DATA(gc)->ephemeral_area_count; i++ ) {
        if (DATA(gc)->ephemeral_area[i]->was_target_during_gc) {
          int target = i+1;
          oh_synchronize( DATA(gc)->ephemeral_area[i] );
          if (target == rgn_next) {
            /* copying in this case would be silly (we're just going
             * to delete it below).  We could avoid deleting it, but
             * its not clear that would be sound... */
            consolemsg( "  skip sm_copy_summary_to( summaries, rgn_next=%d, target=%d );",
                        rgn_next, target );
          } else {
            gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
            sm_copy_summary_to( DATA(gc)->summaries, rgn_next, target );
            sm_copy_summary_to( DATA(gc)->summaries, 0, target );
            gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
          }
        }
      }
    }

    if (summarization_active) {
      gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
      sm_clear_nursery_summary( DATA(gc)->summaries );
      gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
    }

    NURS_SUMMARY_VERIFICATION_POINT(gc); /* should be no-op, as it was just cleared */

    oh_synchronize( DATA(gc)->ephemeral_area[ rgn_next-1 ] );
    DATA(gc)->rrof_cycle_majors_sofar += 1;
    if (DATA(gc)->rrof_cycle_majors_sofar >= 
        DATA(gc)->rrof_cycle_majors_total)
      DATA(gc)->rrof_last_gc_rolled_cycle = TRUE;
    rrof_completed_major_collection( gc );

    /* At start, we have two regions that act as to/from semispaces
     * during major gc's.  If we still have only two regions at the
     * end of a major gc, then one will be empty and the other will
     * have a _precise_ measure of the amount of live storage.  We can
     * use that measure to guide the allocation policy.
     */
    if (DATA(gc)->region_count == 2 &&
        DATA(gc)->ephemeral_area_count == 2) {
      int to_idx = rgn_to - 1;
      int other_idx = (-to_idx)+1;
      assert( to_idx == 0 || to_idx == 1 );
      assert( other_idx == 0 || other_idx == 1 );
      assert( (other_idx + to_idx) == 1 );
      if (DATA(gc)->ephemeral_area[other_idx]->allocated == 0) {
        int live_words = 
          DATA(gc)->ephemeral_area[to_idx]->allocated / sizeof(word);
        if (gc->static_area != NULL) {
          live_words += (gc->static_area->data_area->allocated / sizeof(word));
        }
        DATA(gc)->last_live_words = live_words;
        DATA(gc)->max_live_words = 
          umax( DATA(gc)->max_live_words, live_words );
      }
    }

    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
    collect_rgnl_clear_summary( gc, rgn_next );
    {
      old_heap_t *heap;
      region_group_t grp;
      heap = gc_heap_for_gno(gc, rgn_next);
      grp = heap->group;
      assert( (grp == region_group_wait_nosum)
              || (grp == region_group_wait_w_sum)
              || (heap->allocated == 0) /* XXX */);
      region_group_enq( heap, grp, region_group_unfilled );
    }

    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
    collect_rgnl_choose_next_region( gc, num_rgns, FALSE );

    if (! DATA(gc)->rrof_smircy_step_on_minor_collections_alone) {
      SUMMMTX_VERIFICATION_POINT(gc);

      gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_smircy );
      smircy_step( gc, 
                   ((SYNC_REFINEMENT_RROF_CYCLE || 
                     DONT_USE_REFINEMENT_COUNTDOWN ||
                     (DATA(gc)->rrof_refine_mark_countdown > 0))
                    ? smircy_step_can_refine
                    : smircy_step_must_refine ));
      gc_phase_shift( gc, gc_log_phase_smircy, gc_log_phase_misc_memmgr );

      SUMMMTX_VERIFICATION_POINT(gc);
    }
    
  } else {

    annoyingmsg( "remset summary says region %d too popular to collect", 
                 rgn_next );
    DATA(gc)->ephemeral_area[ rgn_next-1 ]->bytes_live_last_major_gc = 
      DATA(gc)->ephemeral_area[ rgn_next-1 ]->allocated;
    
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
    collect_rgnl_clear_summary( gc, rgn_next );
    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
    collect_rgnl_choose_next_region( gc, num_rgns, TRUE );
    return FALSE;
  }
  
  return TRUE;
}

static bool data_definitely_stays_in_one_region( gc_t *gc, int to_idx ) 
{
  /* HACK.  At start, we have two regions that we treat as alternate
   * targets for promotions out of the nursery during minor gc's, and
   * the to/from semispaces during major gc's.  This means that we
   * often are just promoting data out of the nursery into one region
   * while the other region lies entirely empty and will *remain*
   * entirely empty even after the promotion is complete.
   *
   * When that arises, we do not need to spend any time in cheney loop
   * maintaining the remembered set.
   * 
   * (If incremental marking is not in progress, that means we do not
   * need to spend any time in the cheney loop maintaining *any* of
   * the regional meta-data; but toggling that overhead is already
   * handled within code of cheney itself.) 
   */

  int other_idx = (-to_idx)+1;
  int nursery_allocated = (gc->young_area->allocated +
                           los_bytes_used( gc->los, 0 ));
  int nursery_alloc_roundup;
  int fragmentation_allowance;
  int rgn_allocated;
  int rgn_avail;
  bool data_may_stay_in_one_region, data_stays_in_one_region;

  assert2( other_idx == 0 || other_idx == 1 );
  assert2( to_idx    == 0 || to_idx    == 1 );
  assert2( (other_idx + to_idx) == 1 );

  /* heuristic check; sync; final check with sync'd version */
  data_stays_in_one_region = FALSE;
  data_may_stay_in_one_region
    = ((DATA(gc)->ephemeral_area[ other_idx ]->allocated == 0)
       && 
       ((DATA(gc)->ephemeral_area[ to_idx ]->maximum -
         DATA(gc)->ephemeral_area[ to_idx ]->allocated)
        > nursery_allocated));
  if (data_may_stay_in_one_region) {
    int max_num_gc_chunks;
    oh_synchronize( DATA(gc)->ephemeral_area[ other_idx ] );
    oh_synchronize( DATA(gc)->ephemeral_area[ to_idx ] );

    rgn_allocated = (DATA(gc)->ephemeral_area[ to_idx ]->allocated 
                     + los_bytes_used( gc->los, to_idx+1 ));
    rgn_avail = (DATA(gc)->ephemeral_area[ to_idx ]->maximum
                 - rgn_allocated);
    max_num_gc_chunks = quotient2( rgn_avail, GC_CHUNK_SIZE )+1;
    fragmentation_allowance = max_num_gc_chunks * GC_LARGE_OBJECT_LIMIT;
    nursery_alloc_roundup = 
      (quotient2( nursery_allocated + fragmentation_allowance, 
                  GC_CHUNK_SIZE )) * GC_CHUNK_SIZE;
    data_stays_in_one_region
      =  ((DATA(gc)->ephemeral_area[ other_idx ]->allocated == 0)
          && 
          ((DATA(gc)->ephemeral_area[ to_idx ]->maximum - rgn_allocated)
           > nursery_alloc_roundup));
  }

  return data_stays_in_one_region;
}

static void collect_rgnl_minorgc( gc_t *gc, int rgn_to )
{
  bool summarization_active;

  summarization_active = (DATA(gc)->summaries != NULL);

  if (summarization_active) {
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
    summarization_step( gc, FALSE );
    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
  }

  SUMMMTX_VERIFICATION_POINT(gc);

  /* if there's room, minor collect the nursery into current region. */
  
  /* check that SSB is flushed. */
  assert( *gc->ssb[rgn_to]->bot == *gc->ssb[rgn_to]->top );
  
  DATA(gc)->rrof_currently_minor_gc = TRUE;

  if (summarization_active) {
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
    sm_init_summary_from_nursery_alone( DATA(gc)->summaries, &(DATA(gc)->summary));
    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
    DATA(gc)->use_summary_instead_of_remsets = TRUE;
    summary_vs_remset_msg( "FIXME: setting TRUE" );
  }
  {
    region_group_t grp;
    grp = gc_region_group_for_gno( gc, rgn_to );
    assert( (grp == region_group_unfilled)
            || (grp == region_group_wait_nosum) /* XXX */
            || (grp == region_group_wait_w_sum) /* XXX */ );
  }

#if 1
  if (DATA(gc)->region_count == 2) {
    int to_idx = rgn_to - 1;
    assert2( rgn_to == 1 || rgn_to == 2 );

    if (data_definitely_stays_in_one_region( gc, to_idx )) {
      gc->scan_update_remset = FALSE;
    }
  }
#endif

  DATA(gc)->ephemeral_area[ rgn_to-1 ]->was_target_during_gc = TRUE;
  oh_collect( DATA(gc)->ephemeral_area[ rgn_to-1 ], GCTYPE_PROMOTE );
  DATA(gc)->use_summary_instead_of_remsets = FALSE;
  summary_vs_remset_msg( "FIXME: setting FALSE" );
  gc->scan_update_remset = TRUE; /* undo hack above */

  urs_copy_minor_to_major( gc->the_remset );
  urs_clear_minor( gc->the_remset );

  if (summarization_active) {
    int i;
    /* is this loop *really* necessary?  Why does rgn_to even have a summary? */
    for (i = 0; i < DATA(gc)->ephemeral_area_count; i++ ) {
      if (DATA(gc)->ephemeral_area[i]->was_target_during_gc) {
        oh_synchronize( DATA(gc)->ephemeral_area[i] );
        gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
        sm_copy_summary_to( DATA(gc)->summaries, 0, rgn_to ); /* XXX */
        gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
      }
    }

    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_summarize );
    sm_clear_nursery_summary( DATA(gc)->summaries );
    summary_dispose( &(DATA(gc)->summary) );
    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_misc_memmgr );
  }

  DATA(gc)->rrof_last_tospace = rgn_to;
  
  handle_secondary_space( gc );
  update_promotion_counts( gc, gc->words_from_nursery_last_gc );

  DATA(gc)->total_heap_words_allocated += gc->words_from_nursery_last_gc;
  if (summarization_active) { /* ????   what??? */
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_smircy );
    smircy_step( gc, smircy_step_can_refine );
    gc_phase_shift( gc, gc_log_phase_smircy, gc_log_phase_misc_memmgr );
  }

  rrof_completed_minor_collection( gc );

  SUMMMTX_VERIFICATION_POINT(gc);
}

static bool rgn_has_summary_p( gc_t *gc, int rgn )
{
  if (DATA(gc)->summaries == NULL) {
    return FALSE;
  } else {
    return (sm_is_rgn_summarized( DATA(gc)->summaries, rgn ) ||
            sm_will_rgn_be_summarized_next( DATA(gc)->summaries, rgn ));
  }
}

static int find_appropriate_to( gc_t *gc ) 
{
  old_heap_t *first_unfilled;
  if ( gc_region_group_for_gno( gc, DATA(gc)->rrof_to_region )
       == region_group_unfilled ) {
    return DATA(gc)->rrof_to_region;
  }
  first_unfilled = region_group_first_heap( region_group_unfilled );
  if (first_unfilled == NULL) {
    assert( gc->scan_update_remset );
    add_region_to_expand_heap( gc, 0 );
    first_unfilled = region_group_first_heap( region_group_unfilled );
    assert( first_unfilled != NULL );
  }
  return oh_current_space( first_unfilled )->gen_no;
}

static bool oh_summ_geq( old_heap_t *oh1, old_heap_t *oh2, void *data ) 
{
  gc_t *gc = (gc_t*) data;
  int summ_sz1, summ_sz2;
  summ_sz1 = sm_summarized_live( DATA(gc)->summaries, 
                                 oh_current_space(oh1)->gen_no );
  summ_sz2 = sm_summarized_live( DATA(gc)->summaries, 
                                 oh_current_space(oh2)->gen_no );
  return summ_sz1 >= summ_sz2;
}

static bool oh_summ_leq( old_heap_t *oh1, old_heap_t *oh2, void *data ) 
{
  gc_t *gc = (gc_t*) data;
  int summ_sz1, summ_sz2;
  summ_sz1 = sm_summarized_live( DATA(gc)->summaries, 
                                 oh_current_space(oh1)->gen_no );
  summ_sz2 = sm_summarized_live( DATA(gc)->summaries, 
                                 oh_current_space(oh2)->gen_no );
  return summ_sz1 <= summ_sz2;
}

static bool oh_last( old_heap_t *oh1, old_heap_t *oh2, void *data ) 
{
  return TRUE;
}

static int find_appropriate_next( gc_t *gc )
{
  old_heap_t *first_waiting;
  first_waiting = region_group_first_heap( region_group_wait_nosum );
  if (first_waiting == NULL) {
    /* (this is actually the common case, but we want to get above out
     * of the way; otherwise it will just sit around) */

    if ( ! DATA(gc)->rrof_prefer_big_summ &&
         ! DATA(gc)->rrof_prefer_lil_summ &&
         ! DATA(gc)->rrof_prefer_lat_summ ) {
      first_waiting = region_group_first_heap( region_group_wait_w_sum );
    } else if (DATA(gc)->rrof_prefer_big_summ) {
      first_waiting = 
        region_group_largest( region_group_wait_w_sum, 
                              oh_summ_geq, 1000, gc );
    } else if (DATA(gc)->rrof_prefer_lil_summ ) {
      first_waiting = 
        region_group_largest( region_group_wait_w_sum, 
                              oh_summ_leq, 1000, gc );
    } else if (DATA(gc)->rrof_prefer_lat_summ ) {
      first_waiting = 
        region_group_largest( region_group_wait_w_sum, 
                              oh_last, 1000, gc );
    } else {
      assert(0);
    } 
  }
  if (first_waiting == NULL) {
    old_heap_t *first_filled;
    if (DATA(gc)->summaries != NULL) {
        char *(*n)( region_group_t grp );
        int   (*c)( region_group_t grp );
        n = &region_group_name;
        c = &region_group_count;

      consolemsg( "ran out of ready-and-waiting "
                  "summarized regions to collect!\n"
                  "Are parameter choices sane?"
                  " %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d", 
                  n( region_group_nonrrof    ), c( region_group_nonrrof ),
                  n( region_group_unfilled   ), c( region_group_unfilled ),
                  n( region_group_wait_w_sum ), c( region_group_wait_w_sum ),
                  n( region_group_wait_nosum ), c( region_group_wait_nosum ),
                  n( region_group_summzing   ), c( region_group_summzing ),
                  n( region_group_filled     ), c( region_group_filled ), 
                  n( region_group_risingstar ), c( region_group_risingstar ),
                  n( region_group_infamous   ), c( region_group_infamous ),
                  n( region_group_hasbeen    ), c( region_group_hasbeen ) );

    }
    assert( DATA(gc)->summaries == NULL );
    first_filled = region_group_first_heap( region_group_filled );
    assert( first_filled != NULL );
    return oh_current_space( first_filled )->gen_no;
  } else {
    return oh_current_space( first_waiting )->gen_no;
  }
}

static void collect_rgnl_annoy_re_inputs( gc_t *gc, int rgn, 
                                          int bytes_needed, gc_type_t request )
{
  char *type_str;
  switch (request) {
  case GCTYPE_PROMOTE:  type_str = "PROMOTE"; break;
  case GCTYPE_COLLECT:  type_str = "COLLECT"; break;
  case GCTYPE_EVACUATE: type_str = "EVACUATE"; break;
  case GCTYPE_FULL:     type_str = "FULL"; break;
  default: assert(0);
  }
  supremely_annoyingmsg("collect_rgnl(gc, %d, %d, %s)", 
                        rgn, bytes_needed, type_str );
}

static void rrof_calc_target_allocation( gc_t *gc, 
                                         long long *N_recv, 
                                         long long *N_old_recv,
                                         long long *P_old_recv,
                                         long long *A_this_recv, 
                                         long long *A_target_1_recv,
                                         long long *A_target_2_recv, 
                                         long long *A_target_recv ) 
{
  double L_hard = DATA(gc)->rrof_load_factor_hard;
  double L_soft = DATA(gc)->rrof_load_factor_soft;
  long long N_old  = DATA(gc)->last_live_words_at_time_cycle_began;
#if 0
  long long P_old  = DATA(gc)->max_live_words;    /* FIXME: incorrect! */
#endif
  long long A_this = DATA(gc)->since_cycle_began.words_promoted;
  double F_2    = DATA(gc)->rrof_sumz_params.budget_inv;
  int F_3    = DATA(gc)->rrof_sumz_params.max_retries;
  int N = /* FIXME should be incrementally calculated via collection delta */
    gc_allocated_to_areas( gc,
                           gset_range( 1, DATA(gc)->ephemeral_area_count ))
    / sizeof(word);

  /* FIXME
   *
   * The calculation of P_old above contains a subtle bug.
   * P_old is defined as the maximum over all past values of N_old.
   * The current value of DATA(gc)->max_live_words may be greater
   * than the correct value of P_old, and often is greater when the
   * heap is expanding.
   * Using the incorrect value of P_old leads to an overly large
   * value for A_target, which causes the heap to expand faster
   * than the available summary sets can support.
   *
   * A_target1 was also being calculated incorrectly, but has been
   * corrected below.
   */

  long long P_old = DATA(gc)->max_live_words_at_time_cycle_began;

  long long A_target_1
    = (long long) (0.5 * ((L_hard / (F_2 * F_3)) - 1.0) * P_old);
  long long A_target_2 = ((L_soft*P_old)-N_old);
  long long A_target   = max( 5*MEGABYTE/sizeof(word),
                              min( A_target_1, A_target_2 ));

  if (N_recv != NULL)           *N_recv           = N;
  if (N_old_recv != NULL)       *N_old_recv       = N_old;
  if (P_old_recv != NULL)       *P_old_recv       = P_old;
  if (A_this_recv != NULL)      *A_this_recv      = A_this;
  if (A_target_1_recv != NULL)  *A_target_1_recv  = A_target_1;
  if (A_target_2_recv != NULL)  *A_target_2_recv  = A_target_2;
  if (A_target_recv != NULL)    *A_target_recv    = A_target;
}

static void rrof_gc_policy( gc_t *gc, 
                            bool *will_says_should_major_recv, 
                            bool calculate_loudly )
{
  int majors_sofar = DATA(gc)->rrof_cycle_majors_sofar;
  int majors_total = DATA(gc)->rrof_cycle_majors_total;

  long long N, N_old, P_old, A_this; 
  long long A_target_1, A_target_2, A_target;
  bool will_says_should_major;

  rrof_calc_target_allocation( gc, &N, &N_old, &P_old, &A_this, 
                               &A_target_1, &A_target_2, &A_target );

  /* XXX need to figure out what to do about majors_sofar = 0 case
   * (when no regions are waiting for major gc) ... */

  /*  If the allocation per major gc in this major cycle exceeds
   *  the target allocation divided by the total number of major gcs
   *  expected in this major cycle, then it's time for another
   *  major gc.
   */

  will_says_should_major =
    (((long long)(majors_total) * A_this)
     > ((long long)(majors_sofar+1) * A_target));

  if (calculate_loudly) {
#define FMT "% 3lld"
    long long div = (1000 * 1000) / sizeof(word);    /* convert to MB */

    char *(*n)( region_group_t grp );
    int   (*c)( region_group_t grp );
    n = &region_group_name;
    c = &region_group_count;

    consolemsg( "majs{% 3d /% 3d} "
                "N:" FMT " Nold:" FMT " Pold:" FMT " A:" FMT " "
                "Atgt:" FMT "=max(5,min(" FMT "," FMT "))\n" 
                "    => %s "
                " %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d %s%3d", 
                majors_sofar, majors_total, 
                N/div, N_old/div, P_old/div, A_this/div, 
                A_target/div, A_target_1/div, A_target_2/div,
                will_says_should_major?"MAJOR":"minor",
                n( region_group_nonrrof    ), c( region_group_nonrrof ),
                n( region_group_unfilled   ), c( region_group_unfilled ),
                n( region_group_wait_w_sum ), c( region_group_wait_w_sum ),
                n( region_group_wait_nosum ), c( region_group_wait_nosum ),
                n( region_group_summzing   ), c( region_group_summzing ),
                n( region_group_filled     ), c( region_group_filled ), 
                n( region_group_risingstar ), c( region_group_risingstar ),
                n( region_group_infamous   ), c( region_group_infamous ),
                n( region_group_hasbeen    ), c( region_group_hasbeen ) );
  }

  assert( !will_says_should_major || 
          (region_group_first_heap( region_group_wait_w_sum ) != NULL ) ||
          (region_group_first_heap( region_group_wait_nosum ) != NULL ) ||
          (region_group_first_heap( region_group_filled ) != NULL ) );

  *will_says_should_major_recv = will_says_should_major;
}

static bool two_regions_one_filled_p( gc_t *gc )
{
  if ((DATA(gc)->region_count == 2) &&
      (DATA(gc)->ephemeral_area[0]->group == region_group_filled) && 
      (DATA(gc)->ephemeral_area[1]->group == region_group_unfilled)) {
    return TRUE;
  } else if ((DATA(gc)->region_count == 2) &&
             (DATA(gc)->ephemeral_area[1]->group == region_group_filled) && 
             (DATA(gc)->ephemeral_area[0]->group == region_group_unfilled)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

static void collect_rgnl_evacuate_nursery( gc_t *gc ) 
{
  /* only forward data out of the nursery, if possible */
  int rgn_to;
  int num_rgns = DATA(gc)->region_count;

  bool felix_says_should_major;
  bool verbose = FALSE;
  char *prefix = ("                                             "
                  "                                             ");

  if ((DATA(gc)->rrof_cycle_majors_sofar == 0)
      && two_regions_one_filled_p( gc )) {
    if (verbose) {
      consolemsg("%s    two_regions_one_filled => felix says: MAJOR", prefix);
    }
    felix_says_should_major = TRUE;
  } else if ((region_group_count( region_group_wait_nosum ) 
              + region_group_count( region_group_wait_w_sum )
              + region_group_count( region_group_filled ) /* XXX */)
             > 0) {
    rrof_gc_policy( gc, &felix_says_should_major, verbose );
  } else {
    felix_says_should_major = FALSE;
  }

  DATA(gc)->rrof_refine_mark_countdown -= 1;

 collect_evacuate_nursery:
  if (felix_says_should_major) {
    bool didit;
    int rgn_next; 
    rgn_to = find_appropriate_to( gc );
    rgn_next = find_appropriate_next( gc );
    assert2( (DATA(gc)->summaries == NULL) 
             || (rgn_to != rgn_next 
                 && ! rgn_has_summary_p( gc, rgn_to )));
    DATA(gc)->rrof_to_region = rgn_to;
    DATA(gc)->rrof_next_region = rgn_next;
    didit = collect_rgnl_majorgc( gc, rgn_to, rgn_next, num_rgns );
    if (! didit) 
      goto collect_evacuate_nursery;
  } else { /* ! felix_says_should_major */
    rgn_to = find_appropriate_to( gc );
    DATA(gc)->rrof_to_region = rgn_to;
    collect_rgnl_minorgc( gc, rgn_to );
  }
}

static void collect_rgnl( gc_t *gc, int rgn, int bytes_needed, gc_type_t request )
{
  gclib_stats_t stats;
  gc_data_t *data = DATA(gc);

  collect_rgnl_annoy_re_inputs( gc, rgn, bytes_needed, request );

  assert( rgn >= 0 );
  assert( rgn > 0 || bytes_needed >= 0 );
  assert( data->in_gc >= 0 );

  if (data->in_gc++ == 0) {
    gc_signal_moving_collection( gc );
    data->pause_timer_elapsed = stats_start_timer( TIMER_ELAPSED );
    data->pause_timer_cpu = stats_start_timer( TIMER_CPU );
    gc_phase_shift( gc, gc_log_phase_mutator, gc_log_phase_misc_memmgr );
    before_collection( gc );
  }
  
  switch (request) {
  case GCTYPE_COLLECT: /* collect nursery and rgn, promoting into rgn first. */
    if (rgn == 0) {
      /* (Both kinds of young heaps ignore request parameter.) */
      yh_collect( gc->young_area, bytes_needed, request );
    } else {
      /* explicit request for major collection of rgn. */
      oh_collect( DATA(gc)->ephemeral_area[ rgn - 1 ], request );
      DATA(gc)->rrof_last_tospace = rgn;
    }
    break;
  case GCTYPE_EVACUATE: /* collect nursery and rgn, promoting _anywhere_. */
    if (rgn == 0) {
      collect_rgnl_evacuate_nursery( gc );
    } else {
      /* explicit request for evacuation-style major collection of rgn. */
      oh_collect( DATA(gc)->ephemeral_area[ rgn - 1 ], request );
      DATA(gc)->rrof_last_tospace = rgn;
    }
    break;
  default: 
    assert(0); /* Regional collector only supports above collection types. */
  }

  assert( data->secondary_space == NULL );

  assert( data->in_gc > 0 );
  
  oh_synchronize( DATA(gc)->ephemeral_area[ DATA(gc)->rrof_next_region-1 ] );
  oh_synchronize( DATA(gc)->ephemeral_area[ DATA(gc)->rrof_to_region - 1 ] );

  if (data->rrof_last_gc_rolled_cycle)
    rrof_completed_regional_cycle( gc );

  if (--data->in_gc == 0) {
    after_collection( gc );

    /* XXX where should the phase shift go?
     * 
     * The stats_following_gc invocation needs the pause time
     * measurement, so the question is whether we would prefer for the
     * max pause time to include time spent maintaining the MMU log,
     * or if we want the MMU log to include the time spent updating
     * the stats data.
     * 
     * (Felix does not think we can have it both ways.)
     */
    gc_phase_shift( gc, gc_log_phase_misc_memmgr, gc_log_phase_mutator );
    DATA(gc)->last_pause_cpu = 
      stats_stop_timer( data->pause_timer_cpu );
    DATA(gc)->last_pause_elapsed =
      stats_stop_timer( data->pause_timer_elapsed );

    /* FIXME
     *
     * Increasing the mark budget by a factor of 10 made
     * summarization pauses go to 10+ seconds, but had no
     * effect on mark pauses.  It also improved throughput.
     *
     * Reducing the region size to 5M brought pause times
     * back to what they were before (about 700ms).
     *
     * Better accounting brought mark pauses under control.
     * The cheney and summarization pauses are now limiting.
     *
     */

    if (DATA(gc)->last_pause_cpu > 400)
      annoyingmsg( "PAUSE = %d ********** (%d) rgn = %d",
                   DATA(gc)->last_pause_cpu, debug_counter, rgn );
    debug_counter = debug_counter + 1;

    stats_following_gc( gc );
    gclib_stats( &stats );
    annoyingmsg( "  Memory usage: heap %d, remset %d, RTS %d words",
                 stats.heap_allocated, stats.remset_allocated, 
                 stats.rts_allocated );
    annoyingmsg( "  Max heap usage: %d words", stats.heap_allocated_max );
  }

}

/* These procedures may be called thousands of times per second. */

static void incremental_nop( gc_t *gc ) { /* do nothing */ }

static void incremental_rgnl_activity( gc_t *gc )
{
  /* Schedule some work here. */

  double mut_activity_sumz;
  double cN;
  double m_cN;
  int ne_rgn_count;
  bool about_to_major;
  int dA;
  bool completed_cycle;
  stats_id_t timer1, timer2;
  bool summarization_active;

  if (DATA(gc)->summaries == NULL)
    return;

  before_incremental( gc );

  /*  Marking.  */

  /*  Summarization.  */

  start_timers( &timer1, &timer2 );

  mut_activity_sumz = calc_mutator_activity_sumz( gc );
  cN = calc_cN( gc );
  m_cN = mut_activity_sumz / cN;

  ne_rgn_count = nonempty_region_count( gc );
  about_to_major = 0;
  dA = 1;  /* FIXME */

  if (m_cN >= 1.0)
    annoyingmsg( "m/cN = %d (%%)", (int) (100.0 * m_cN) );

  completed_cycle
    = sm_construction_progress( DATA(gc)->summaries, 
                                DATA(gc)->rrof_next_region, 
                                ne_rgn_count, 
                                about_to_major,
                                TRUE,
                                dA,
                                m_cN );
  stop_sumrize_timers( gc, &timer1, &timer2 );

#if 0
  /* FIXME */
  if (DATA(gc)->stat_last_ms_remset_sumrize_cpu > 100) {
    consolemsg( "SHORT SUMMARIZATION PAUSE = %d ********** (%d) "
                 "%d %d %d %d %d%%",
                 DATA(gc)->stat_last_ms_remset_sumrize_cpu,
                 debug_counter,
                 DATA(gc)->rrof_next_region, ne_rgn_count,
                 about_to_major, dA, (int) (100.0 * m_cN) );
  }
#endif

  after_incremental( gc );
  stats_following_incremental( gc );

  if (completed_cycle) {
    annoyingmsg( "COMPLETED SUMMARIZATION CYCLE (on short pause)" );
    rrof_completed_summarization_cycle( gc );
  }
}

/*  FIXME
 *
 *  To compensate for Larceny's highly variable rate of software
 *  timer interrupts, which lowers MMU at fine resolution, this
 *  function does nothing unless a certain amount of time has
 *  passed since it last did incremental work.
 */

#define INCREMENTAL_TIMING_DELAY 5

static unsigned time_of_last_work = 0;

static void incremental_rgnl( gc_t *gc )
{
  unsigned t0;

  if (DATA(gc)->summaries == NULL)
    return;

  if (((unsigned) osdep_realclock())
      > (time_of_last_work + INCREMENTAL_TIMING_DELAY)) {

    gc_phase_shift( gc, gc_log_phase_mutator, gc_log_phase_summarize );
    incremental_rgnl_activity( gc );
    gc_phase_shift( gc, gc_log_phase_summarize, gc_log_phase_mutator );

    time_of_last_work = osdep_realclock();
  }
}

static void check_remset_invs_rgnl( gc_t *gc, word src, word tgt ) 
{
  assert(isptr(tgt));
  supremely_annoyingmsg( "check_remset_invs_rgnl( gc, 0x%08x (%d), 0x%08x (%d) )", 
                         src, isptr(src)?gen_of(src):0, tgt, gen_of(tgt) );
  /* XXX Felix is not convinced this assertion is sound. */
  assert( ! isptr(src) ||
          gen_of(src) != gen_of(tgt) ||   // FSK: why are below || (opposed to &&)
          gen_of(src) != 0 ||
          gen_of(tgt) != DATA(gc)->static_generation ||
          urs_isremembered( gc->the_remset, src ) );
}
static void check_remset_invs( gc_t *gc, word src, word tgt ) 
{
  supremely_annoyingmsg( "check_remset_invs( gc, 0x%08x (%d), 0x%08x (%d) )", 
                         src, src?gen_of(src):0, tgt, gen_of(tgt) );
  /* XXX Felix is not convinced this assertion is sound. */
  assert( !src || 
          gen_of(src)  < gen_of(tgt) ||
          gen_of(src) != 0 ||
          gen_of(tgt) != DATA(gc)->static_generation ||
          urs_isremembered( gc->the_remset, src ));
}

void gc_signal_moving_collection( gc_t *gc )
{
  DATA(gc)->globals[ G_GC_CNT ] += fixnum(1);
  DATA(gc)->globals[ G_MAJORGC_CNT ] += fixnum(1);
  if (DATA(gc)->globals[ G_GC_CNT ] == 0)
    hardconsolemsg( "\n\nCongratulations!\n"
                    "You have survived 1,073,741,824 garbage collections!\n" );
}

void gc_signal_minor_collection( gc_t *gc ) {

  /* Undo the increment performed by gc_signal_moving_collection. */

  DATA(gc)->globals[ G_MAJORGC_CNT ] -= fixnum(1);

}

void gc_check_rise_to_infamy( gc_t *gc,
                              old_heap_t *heap, 
                              int incoming_words_estimate )
{
  int max_infamous = (DATA(gc)->region_count 
                      / (DATA(gc)->rrof_sumz_params.popularity_factor 
                         * DATA(gc)->rrof_sumz_params.infamy_factor ));
  int infamy_threshold = 
    (DATA(gc)->rrof_sumz_params.popularity_limit_words 
     * DATA(gc)->rrof_sumz_params.infamy_factor);
  if (region_group_count( region_group_infamous )+1 > max_infamous) {
    /* disallow #infamous regions overflow, no matter what */
    return;
  }
  if (incoming_words_estimate > infamy_threshold &&
      heap->group != region_group_infamous) {

    /* One might think that heap must be in one of the popular groups;
     * but that is NOT true because we can shift regions not under-going
     * summarization into the infamous set. 
     */
    region_group_enq( heap, heap->group, region_group_infamous );
  }
}

void gc_check_infamy_drop_to_hasbeen( gc_t *gc, 
                                      old_heap_t *heap, 
                                      int incoming_words_estimate )
{
  if (incoming_words_estimate 
      < DATA(gc)->rrof_sumz_params.popularity_limit_words) {
    region_group_enq( heap, region_group_infamous, region_group_hasbeen );
  }
}

static void before_collection( gc_t *gc )
{
  int e;

  gc->stat_last_ms_gc_cheney_pause = 0;
  gc->stat_last_ms_gc_cheney_pause_cpu = 0;
  gc->stat_last_gc_pause_ismajor = -1;

  DATA(gc)->stat_last_ms_remset_sumrize = -1;
  DATA(gc)->stat_last_ms_remset_sumrize_cpu = -1;
  DATA(gc)->stat_last_ms_smircy_mark = -1;
  DATA(gc)->stat_last_ms_smircy_mark_cpu = -1;
  DATA(gc)->stat_last_ms_smircy_refine = -1;
  DATA(gc)->stat_last_ms_smircy_refine_cpu = -1;

  osdep_pagefaults( &DATA(gc)->major_page_fault_count_at_gc_start,
                    &DATA(gc)->minor_page_fault_count_at_gc_start );

  /* assume it does not roll over until we discover otherwise */
  DATA(gc)->rrof_last_gc_rolled_cycle = FALSE;

  gc_compact_all_ssbs( gc );

  if (gc->satb_ssb != NULL) 
    process_seqbuf( gc, gc->satb_ssb );

  DATA(gc)->rrof_currently_minor_gc = FALSE;

  yh_before_collection( gc->young_area );
  for ( e=0 ; e < DATA(gc)->ephemeral_area_count ; e++ ) {
    oh_before_collection( DATA(gc)->ephemeral_area[ e ] );
    DATA(gc)->ephemeral_area[e]->was_target_during_gc = FALSE;
  }
  if (DATA(gc)->dynamic_area)
    oh_before_collection( DATA(gc)->dynamic_area );

  REMSET_VERIFICATION_POINT(gc);
  NURS_SUMMARY_VERIFICATION_POINT(gc);

  assert(! DATA(gc)->use_summary_instead_of_remsets );

  SUMMMTX_VERIFICATION_POINT(gc);
  SMIRCY_VERIFICATION_POINT(gc);

  if (DATA(gc)->summaries != NULL)
    sm_before_collection( DATA(gc)->summaries );
}

static void after_collection( gc_t *gc )
{
  int e;

  DATA(gc)->generations = DATA(gc)->generations_after_gc;

  DATA(gc)->mutator_effort.forcing_collector_to_progress = FALSE;

  if (DATA(gc)->summaries != NULL)
    sm_after_collection( DATA(gc)->summaries );

  SMIRCY_VERIFICATION_POINT(gc);
  if (DATA(gc)->remset_undirected) {
    /* why the guard?  See note below. */
    REMSET_VERIFICATION_POINT(gc);
    NURS_SUMMARY_VERIFICATION_POINT(gc); 
  }
  assert(! DATA(gc)->use_summary_instead_of_remsets );
  SUMMMTX_VERIFICATION_POINT(gc);

  yh_after_collection( gc->young_area );
  for ( e=0 ; e < DATA(gc)->ephemeral_area_count ; e++ )
    oh_after_collection( DATA(gc)->ephemeral_area[ e ] );
  if (DATA(gc)->dynamic_area)
    oh_after_collection( DATA(gc)->dynamic_area );
  if (! DATA(gc)->remset_undirected) {
    /* hack to work around delayed clearing of remset in generational gc;
     * the remset has dangling pointers in it until it is cleared by
     * the oh_after_collection invocation above. */
    REMSET_VERIFICATION_POINT(gc);
    NURS_SUMMARY_VERIFICATION_POINT( gc );
  }
}

/*  Called before incremental work that doesn't involve collection.
 *  Called only by regional collector.
 */

static void before_incremental( gc_t *gc )
{

  DATA(gc)->stat_last_ms_remset_sumrize = -1;
  DATA(gc)->stat_last_ms_remset_sumrize_cpu = -1;
  DATA(gc)->stat_last_ms_smircy_mark = -1;
  DATA(gc)->stat_last_ms_smircy_mark_cpu = -1;
  DATA(gc)->stat_last_ms_smircy_refine = -1;
  DATA(gc)->stat_last_ms_smircy_refine_cpu = -1;

  REMSET_VERIFICATION_POINT(gc);
  NURS_SUMMARY_VERIFICATION_POINT(gc);

  assert(! DATA(gc)->use_summary_instead_of_remsets );

  SUMMMTX_VERIFICATION_POINT(gc);
  SMIRCY_VERIFICATION_POINT(gc);
}

/*  Called after incremental work that didn't involve collection.
 *  Called only by regional collector.
 */

static void after_incremental( gc_t *gc )
{
  SMIRCY_VERIFICATION_POINT(gc);
  if (DATA(gc)->remset_undirected) {
    /* why the guard?  See note below. */
    REMSET_VERIFICATION_POINT(gc);
    NURS_SUMMARY_VERIFICATION_POINT(gc); 
  }
  assert(! DATA(gc)->use_summary_instead_of_remsets );
  SUMMMTX_VERIFICATION_POINT(gc);
  if (! DATA(gc)->remset_undirected) {
    /* hack to work around delayed clearing of remset in generational gc;
     * the remset has dangling pointers in it until it is cleared by
     * the oh_after_collection invocation above. */
    REMSET_VERIFICATION_POINT(gc);
    NURS_SUMMARY_VERIFICATION_POINT( gc );
  }
}

/* Called after after_incremental, only by the regional collector. */

static void stats_following_incremental( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  stack_stats_t stats_stack;
  gclib_stats_t stats_gclib;
  int i;

  memset( &stats_gclib, 0, sizeof( gclib_stats_t ) );
  gclib_stats( &stats_gclib );

  stats_gclib.last_ms_remset_sumrize     = 
    DATA(gc)->stat_last_ms_remset_sumrize;
  stats_gclib.last_ms_remset_sumrize_cpu = 
    DATA(gc)->stat_last_ms_remset_sumrize_cpu;
  stats_gclib.last_ms_smircy_mark     = 
    DATA(gc)->stat_last_ms_smircy_mark;
  stats_gclib.last_ms_smircy_mark_cpu = 
    DATA(gc)->stat_last_ms_smircy_mark_cpu;
  stats_gclib.last_ms_smircy_refine     = 
    DATA(gc)->stat_last_ms_smircy_refine;
  stats_gclib.last_ms_smircy_refine_cpu = 
    DATA(gc)->stat_last_ms_smircy_refine_cpu;

  stats_add_gclib_stats_incremental( &stats_gclib );
}

static void set_policy( gc_t *gc, int gen, int op, int value )
{
  if (gen == 0)
    yh_set_policy( gc->young_area, op, value );
  else if (gen-1 <= DATA(gc)->ephemeral_area_count)
    oh_set_policy( DATA(gc)->ephemeral_area[gen-1], op, value );
  else if (DATA(gc)->dynamic_area)
    oh_set_policy( DATA(gc)->dynamic_area, op, value );
}

struct apply_f_to_smircy_stack_entry_data {
  void (*f)( word *addr, void *scan_data );
  void *scan_data;
};

static void apply_f_to_smircy_stack_entry( word *w, void *data_orig ) 
{
  struct apply_f_to_smircy_stack_entry_data *data;
  data = (struct apply_f_to_smircy_stack_entry_data*)data_orig;
  data->f( w, data->scan_data );
}

static void 
enumerate_smircy_roots( gc_t *gc, void (*f)(word *addr, void *scan_data), void *scan_data )
{ 
#if SMIRCY_RGN_STACK_IN_ROOTS 
  if (gc->smircy != NULL && ! DATA(gc)->rrof_currently_minor_gc) { 
    struct apply_f_to_smircy_stack_entry_data smircy_data; 
    smircy_data.f = f; 
    smircy_data.scan_data = scan_data; 
    smircy_enumerate_stack_of_rgn( gc->smircy, 
                                   DATA(gc)->rrof_next_region, 
                                   apply_f_to_smircy_stack_entry, 
                                   &smircy_data ); 
    smircy_drop_cleared_stack_entries( gc->smircy, DATA(gc)->rrof_next_region );
  }
#endif 
} 

static void
enumerate_roots( gc_t *gc, void (*f)(word *addr, void *scan_data), void *scan_data )
{
  int i;
  gc_data_t *data = DATA(gc);
  word *globals = data->globals;

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    f( &globals[ i ], scan_data );
  for ( i = 0 ; i < data->nhandles ; i++ )
    if (data->handles[i] != 0)
      f( &data->handles[i], scan_data );
}

/* WARNING: this only enumerates elements of the remsets tracking
 * mutator activity, not the major_remsets. 
 * 
 * If you want information from the major remsets, you need to
 * propagate it via the remset summary.
 */
static void
enumerate_remsets_complement( gc_t *gc,
                              gset_t gset,
                              bool (*f)(word obj, void *data),
                              void *fdata )
{
  int i;
  int ecount;

  if (!DATA(gc)->is_partitioned_system) return;

  assert( ! DATA(gc)->use_summary_instead_of_remsets );

  /* Felix is pretty sure that this method is intended only
   * for use by clients who are always attempting to enumerate
   * the elements of the static area's remembered set,
   * and therefore the static area should not be in gset. 
   */
  assert2( ! gset_memberp( DATA(gc)->static_generation, gset ));

  /* The ecount corresponds to the largest region index in the
   * ephemeral_area array.  (ephemeral_area_count field may be
   * incremented by f, so we have to checkpoint its value here.) */
  ecount = DATA(gc)->ephemeral_area_count;

  /* Add elements to regions outside collection set. 
   *
   * I might need to extend this interface in some way so that we
   * don't waste time adding elements to remset[gno] for 
   * gno <= generation
   */
  /* FSK: Is this loop still necessary?  Where are elems added between
   * before_collection's invoke of compact_all_ssbs and here? */
  for ( i=0 ; i <= ecount; i++ ) {
    process_seqbuf( gc, gc->ssb[i] );
  }

  if (DATA(gc)->enumerate_major_with_minor_remsets) {
    urs_enumerate_complement( gc->the_remset, TRUE, gset, f, fdata );
  } else {
    urs_enumerate_minor_complement( gc->the_remset, TRUE, gset, f, fdata );
  }
}

struct apply_f_to_remset_obj_entry_data {
  void (*f)( loc_t loc, void *scan_data );
  void *scan_data;
};

static bool apply_f_to_remset_obj_entry( word obj, void *data_orig )
{
  word *w;
  struct apply_f_to_remset_obj_entry_data *data;
  void *scan_data;
  void (*f)( loc_t loc, void *scan_data );

  data = (struct apply_f_to_remset_obj_entry_data*)data_orig;

  scan_data = data->scan_data;
  f         = data->f;
  w = ptrof(obj);
  if (tagof(obj) == PAIR_TAG) {
    f( make_loc(obj, 0), scan_data );
    f( make_loc(obj, sizeof(word)), scan_data );
  } else {
    word words; 
    int offset;
    assert2( tagof(obj) == VEC_TAG || tagof(obj) == PROC_TAG );
    words = sizefield( *w ) / 4; /* XXX sizeof(word) for generality? */
    offset = 0;
    while (words--) {
      offset += sizeof(word);
      f( make_loc(obj, offset), scan_data );
    }
  }

  return TRUE;
}

static void enumerate_remembered_locations( gc_t *gc, gset_t genset, 
                                            void (*f)(loc_t loc, 
                                                      void *scan_data), 
                                            void *scan_data,
                                            bool (*g)(word obj, void *data),
                                            void *g_scan_data )
{
  if ( DATA(gc)->use_summary_instead_of_remsets) {
    summary_enumerate_locs2( &DATA(gc)->summary, f, scan_data );
  } else {
#if 0
    struct apply_f_to_remset_obj_entry_data remsets_data;
    remsets_data.f = f;
    remsets_data.scan_data = scan_data;
    gc_enumerate_remsets_complement( gc, genset, 
                                     apply_f_to_remset_obj_entry, 
                                     (void*) &remsets_data );
#else
    gc_enumerate_remsets_complement( gc, genset, g, g_scan_data );
#endif
  }
}

static void enumerate_hdr_address_ranges( gc_t *gc, 
                                          int gno, 
                                          void (*f)( word *s,word *l,void *d ),
                                          void *d ) 
{
  unsigned t0, t1; /* FIXME */
  int iterations = 0; /* FIXME */
  if (gno-1 < DATA(gc)->ephemeral_area_count) {
    semispace_t *ss;
    ss = oh_current_space( DATA(gc)->ephemeral_area[gno-1] );
    t0 = osdep_realclock(); /* FIXME */
    ss_enumerate_hdr_ranges( ss, f, d );
    t1 = osdep_realclock(); /* FIXME */
    if ((t1 - t0) > 200)
      consolemsg( "===1 enumerate_hdr_address_ranges %d",
                  t1 - t0 ); /* FIXME */

    {
      word *p;
      p = NULL;
      t0 = osdep_realclock(); /* FIXME */
      do {
        iterations++;
        p = los_walk_list( gc->los->object_lists[gno], p );
        if (p != NULL) 
          f( p, p+1, d );
      } while (p != NULL);
      t1 = osdep_realclock(); /* FIXME */
      if ((t1 - t0) > 200)
        consolemsg( "===2 enumerate_hdr_address_ranges %d (%d)",
                    t1 - t0, iterations ); /* FIXME */
    }
  }
  {
    if (gc->static_area != NULL && 
        (gno == DATA(gc)->static_generation)) {
      t0 = osdep_realclock(); /* FIXME */
      ss_enumerate_hdr_ranges( gc->static_area->data_area, f, d );
      t1 = osdep_realclock(); /* FIXME */
      if ((t1 - t0) > 200)
        consolemsg( "===3 enumerate_hdr_address_ranges %d",
                    t1 - t0 ); /* FIXME */
    }
  }
}

word *data_load_area( gc_t *gc, int size_bytes )
{
  return load_text_or_data( gc, size_bytes, 0 );
}

static word *text_load_area( gc_t *gc, int size_bytes )
{
  return load_text_or_data( gc, size_bytes, 1 );
}

static word *load_text_or_data( gc_t *gc, int nbytes, int load_text )
{
  word *rtn;

  assert( nbytes > 0 );
  assert( nbytes % BYTE_ALIGNMENT == 0 );
  
  if (gc->static_area)
    DATA(gc)->nonexpandable_size += nbytes;
  effect_heap_limits( gc );

  if (gc->static_area && load_text)
    return sh_text_load_area( gc->static_area, nbytes );
  else if (gc->static_area)
    return sh_data_load_area( gc->static_area, nbytes );
  else if (DATA(gc)->dynamic_area)
    return oh_data_load_area( DATA(gc)->dynamic_area, nbytes );
  else {
    rtn = yh_data_load_area( gc->young_area, nbytes );
    assert( rtn != NULL ); /* FIXME (it can happen) */
    return rtn;
  }
}

static int iflush( gc_t *gc, int generation )
{
  return (int)(DATA(gc)->globals[G_CACHE_FLUSH]);
}

static word creg_get( gc_t *gc ) 
  { return yh_creg_get( gc->young_area ); }

static void creg_set( gc_t *gc, word k ) 
  { yh_creg_set( gc->young_area, k ); }

static void stack_overflow( gc_t *gc ) 
  { yh_stack_overflow( gc->young_area ); }

static void stack_underflow( gc_t *gc ) 
  { yh_stack_underflow( gc->young_area ); }

/* Strategy: generations report the data for themselves and their 
   remembered sets.  Everything else is handled here.
   */
static void stats_following_gc( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  stack_stats_t stats_stack;
  gclib_stats_t stats_gclib;
  int i;

  yh_stats( gc->young_area );

  for ( i=0 ; i < DATA(gc)->ephemeral_area_count ; i++ )
    oh_stats( DATA(gc)->ephemeral_area[i] );

  if (DATA(gc)->dynamic_area)
    oh_stats( DATA(gc)->dynamic_area );

  if (gc->static_area)
    sh_stats( gc->static_area );

  memset( &stats_stack, 0, sizeof( stack_stats_t ) );
  stk_stats( data->globals, &stats_stack );
  stats_add_stack_stats( &stats_stack );

#if defined(SIMULATE_NEW_BARRIER)
  swb_stats( ... );
  stats_add_swb_stats( ... );
#endif

  memset( &stats_gclib, 0, sizeof( gclib_stats_t ) );
  gclib_stats( &stats_gclib );

#define assert_geq_and_assign( lhs, rhs ) \
  do { assert( lhs <= rhs ); lhs = rhs; } while (0)
  assert_geq_and_assign(stats_gclib.max_remset_scan,
                        gc->stat_max_remset_scan);
  assert_geq_and_assign(stats_gclib.max_remset_scan_cpu,
                        gc->stat_max_remset_scan_cpu);
  assert_geq_and_assign(stats_gclib.total_remset_scan,
                        gc->stat_total_remset_scan);
  assert_geq_and_assign(stats_gclib.total_remset_scan_cpu,
                        gc->stat_total_remset_scan_cpu);
  assert_geq_and_assign(stats_gclib.remset_scan_count,
                        gc->stat_remset_scan_count);
  assert_geq_and_assign(stats_gclib.max_entries_remset_scan,
                        gc->stat_max_entries_remset_scan);
  assert_geq_and_assign(stats_gclib.total_entries_remset_scan,
                        gc->stat_total_entries_remset_scan);

  stats_gclib.last_ms_gc_truegc_pause = DATA(gc)->last_pause_elapsed;
  stats_gclib.last_ms_gc_truegc_pause_cpu = DATA(gc)->last_pause_cpu;
  stats_gclib.last_ms_gc_cheney_pause = 
    gc->stat_last_ms_gc_cheney_pause;
  stats_gclib.last_ms_gc_cheney_pause_cpu = 
    gc->stat_last_ms_gc_cheney_pause_cpu;
  stats_gclib.last_gc_pause_ismajor      = gc->stat_last_gc_pause_ismajor;
  if (gc->stat_last_gc_pause_ismajor) {
    stats_gclib.length_minor_gc_run = DATA(gc)->stat_length_minor_gc_run;
    DATA(gc)->stat_length_minor_gc_run = 0;
  } else {
    stats_gclib.length_minor_gc_run = -1;
    DATA(gc)->stat_length_minor_gc_run += 1;
  }
  stats_gclib.last_ms_remset_sumrize     = 
    DATA(gc)->stat_last_ms_remset_sumrize;
  stats_gclib.last_ms_remset_sumrize_cpu = 
    DATA(gc)->stat_last_ms_remset_sumrize_cpu;
  stats_gclib.last_ms_smircy_mark     = 
    DATA(gc)->stat_last_ms_smircy_mark;
  stats_gclib.last_ms_smircy_mark_cpu = 
    DATA(gc)->stat_last_ms_smircy_mark_cpu;
  stats_gclib.last_ms_smircy_refine     = 
    DATA(gc)->stat_last_ms_smircy_refine;
  stats_gclib.last_ms_smircy_refine_cpu = 
    DATA(gc)->stat_last_ms_smircy_refine_cpu;

  {
    unsigned major, minor;
    osdep_pagefaults( &major, &minor );
    stats_gclib.last_major_page_faults = 
      (major - DATA(gc)->major_page_fault_count_at_gc_start);
    stats_gclib.last_minor_page_faults = 
      (minor - DATA(gc)->minor_page_fault_count_at_gc_start);
    assert( stats_gclib.last_major_page_faults >= 0 );
    assert( stats_gclib.last_minor_page_faults >= 0 );
  }

  stats_add_gclib_stats( &stats_gclib );

  stats_dumpstate();                /* Dumps stats state if dumping is on */
}

static void force_collector_to_make_progress( gc_t *gc )
{
  word *globals = DATA(gc)->globals;
  word *p;
  int nbytes;
  word *lim;
  p = (word*)globals[ G_ETOP ];
  lim = ((word*)globals[ G_STKP ]-SCE_BUFFER);
  nbytes = (lim-p)*sizeof(word);

  if (nbytes > 0) {
#if 0
    consolemsg( "force_collector_to_make_progress"
                " p:0x%08x lim:0x%08x nbytes:%d",
                p, lim, nbytes );
#endif

    *p = mkheader( nbytes-sizeof(word),BIGNUM_HDR);
    if (p+1 < lim)
      *(p+1) =  0xBACDBACD;
    globals[ G_ETOP ] += nbytes;
  }

  DATA(gc)->mutator_effort.forcing_collector_to_progress = TRUE;
}

static int compact_all_ssbs( gc_t *gc )
{
  int overflowed, i;
  word *bot, *top;
  int cN, mut_effort_full,  mut_effort_sumz;
  bool force_progress;

  force_progress = FALSE;
  if (DATA(gc)->mut_activity_bounded && DATA(gc)->summaries != NULL) {
    cN = calc_cN( gc );
    mut_effort_full = 
      (DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.full_cycle + 
       DATA(gc)->mutator_effort.words_promoted_this.full_cycle);
    mut_effort_sumz = 
      (DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle + 
       DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle);
    FIXME_UNUSED_VARIABLE(mut_effort_full);
    if (mut_effort_sumz > cN) {
      annoyingmsg( "compact_all_ssbs force_progress because "
                   "mut_effort_sumz=%d=%d+%d > cN=%d, while max_live_words=%d",
                  mut_effort_sumz, 
                  DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle,
                  DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle,
                  cN, DATA(gc)->max_live_words );
      force_progress = TRUE;
    }
  }

  overflowed = 0;
  for (i = 0; i < gc->gno_count; i++) {
    bot = *gc->ssb[i]->bot;
    top = *gc->ssb[i]->top;
    FIXME_UNUSED_VARIABLE(bot);
    FIXME_UNUSED_VARIABLE(top);
    overflowed = process_seqbuf( gc, gc->ssb[i] ) || overflowed;
  }

  if (force_progress) {
    force_collector_to_make_progress( gc );
  }

  return overflowed;
}

static void np_remset_ptrs( gc_t *gc, word ***ssbtop, word ***ssblim )
{
  assert(0);
}

static int dump_image( gc_t *gc, const char *filename, bool compact )
{
  if (DATA(gc)->is_partitioned_system) 
    return dump_generational_system( gc, filename, compact );
  else
    return dump_stopcopy_system( gc, filename, compact );
}

static int
dump_generational_system( gc_t *gc, const char *filename, bool compact )
{
  hardconsolemsg( "Can't dump generational heaps (yet)." );
  return 0;
}

static int dump_semispace( heapio_t *heap, int type, semispace_t *ss )
{
  int i, r;

  for ( i=0 ; i <= ss->current ; i++ ) {
    r = hio_dump_segment( heap, type, ss->chunks[i].bot, ss->chunks[i].top );
    if (r < 0) return r;
  }
  return 0;
}

static int dump_stopcopy_system( gc_t *gc, const char *filename, bool compact )
{
  int type, r;
  word *p;
  heapio_t *heap;

  /* Felix believes this is intended to perform the collection
   * required by the heap dumping system (this requirement is
   * documented as being due to the assumption in the heap dumper that
   * pad words are not garbage).
   */
  if (compact)
    compact_all_areas( gc );

  type = (gc->static_area ? HEAP_SPLIT : HEAP_SINGLE);
  heap = create_heapio();
  if ((r = hio_create( heap, filename, type )) < 0) goto fail;

  /* Dump an existing text area as the text area, and the existing data
     and young areas together as the data area.  Therefore, when the
     heap is loaded, the young area will effectively have been promoted
     into the static area.  The static area can subsequently be reorganized.
     It's awkward, but an OK temporary solution.
     */
  if ((r = hio_dump_initiate( heap, DATA(gc)->globals )) < 0) goto fail;

  if (gc->static_area) {
    if (gc->static_area->text_area) {
      r = dump_semispace( heap, TEXT_SEGMENT, gc->static_area->text_area );
      if (r < 0) goto fail;
    }
    if (gc->static_area->data_area) {
      r = dump_semispace( heap, DATA_SEGMENT, gc->static_area->data_area );
      if (r < 0) goto fail;
    }
  }
  r = dump_semispace( heap, DATA_SEGMENT, yhsc_data_area( gc->young_area ) );
  if (r < 0) goto fail;

  p = 0;
  while ((p = los_walk_list ( gc->los->object_lists[0], p )) != 0) {
    r = hio_dump_segment( heap, DATA_SEGMENT,
                          p, p + bytes2words(sizefield(*ptrof(p))) + 1 );
    if (r < 0) goto fail;
  }

  r = hio_dump_commit( heap );
  if (r < 0) goto fail;

  return hio_close( heap );

 fail:
  hio_close( heap );
  return r;
}

static void compact_all_areas( gc_t *gc )
{
  /* This is a crock!  Compacts young heap only. */
  gc_collect( gc, 0, 0, GCTYPE_PROMOTE );
}

static void effect_heap_limits( gc_t *gc )
{
  if (DATA(gc)->dynamic_max) {
    int lim = DATA(gc)->dynamic_max+DATA(gc)->nonexpandable_size;
    annoyingmsg( "*** Changing heap limit to %d", lim );
    gclib_set_heap_limit( lim );
  }
}

static word *make_handle( gc_t *gc, word obj )
{
  gc_data_t *data = DATA(gc);
  int i;

  for ( i=0 ; i < data->nhandles && data->handles[i] != 0 ; i++ )
    ;
  if ( i == data->nhandles ) {  /* table full */
    word *h = must_malloc( words2bytes(data->nhandles*2) );
    memcpy( h, data->handles, words2bytes(data->nhandles) );
    memset( h+data->nhandles, 0, words2bytes(data->nhandles) );
    data->handles = h;
    data->nhandles *= 2;
  }
  data->handles[i] = obj;
  return &data->handles[i];
}

static void free_handle( gc_t *gc, word *handle )
{
  gc_data_t *data = DATA( gc );
  
  assert( handle >= data->handles && handle < data->handles + data->nhandles );
  assert( *handle != 0 );
  *handle = 0;
}

static gno_state_t gno_state( gc_t *gc, int gno )
{
  return gno_state_normal;
}

/* Returns generation number appropriate for a fresh area.  The
 * returned number is not yet accomodated by gc; it is merely
 * suggested as an appropriate value to make room for in the internal
 * gc structure.  (Attempts to make room with other values may or may
 * not work...)
 *
 * A more expressive interface would allow the client code to
 * influence this decision, but this is not meant to be all things to
 * all people.
 */
static int find_fresh_gno( gc_t *gc ) 
{
  int fresh_gno;
  /* A simple policy would be to just use the successor of the max
   * generation number in gc.  However, we want to maintain 
   * the following invarants:
   * - the static area, if present, always has the maximum gno (to
   *   simplify the write barrier).
   * - the returned gno is appropriate for insertion into
   *   gc->ephemeral_area[] (after it is appropriately expanded).
   * 
   * This relies on the invariant that the ephemeral area always comes
   * immediately after the nursery (which has gno 0),
   */
  old_heap_t *heap;
  semispace_t *ss; 

  assert( DATA(gc)->ephemeral_area_count > 0 );

  heap = DATA(gc)->ephemeral_area[DATA(gc)->ephemeral_area_count-1];
  ss = ohsc_data_area( heap );
  return ss->gen_no+1;
}

static old_heap_t* expand_ephemeral_area_gnos( gc_t *gc, int fresh_gno ) 
{
  int i;
  old_heap_t* new_heap;
  int old_area_count = DATA(gc)->ephemeral_area_count;
  int new_area_count = old_area_count + 1;
  old_heap_t** new_ephemeral_area = 
    (old_heap_t**)must_malloc( new_area_count*sizeof( old_heap_t* ));
  
  annoyingmsg( "memmgr: expand_ephemeral_area_gnos "
               "fresh_gno %d area_count: %d",
               fresh_gno, old_area_count );

  assert( old_area_count > 0 );
  
  new_heap = 
    clone_sc_area( DATA(gc)->ephemeral_area[ old_area_count-1 ], fresh_gno );
  region_group_enq( new_heap, 
                    region_group_nonrrof, region_group_unfilled );

  for( i=0 ; i < old_area_count; i++) {
    new_ephemeral_area[ i ] = DATA(gc)->ephemeral_area[ i ];
  }
  new_ephemeral_area[ old_area_count ] = new_heap;

  free( DATA(gc)->ephemeral_area );
  DATA(gc)->ephemeral_area = new_ephemeral_area;
  DATA(gc)->ephemeral_area_count = new_area_count;
  
  return new_heap;
}

static void expand_dynamic_area_gnos( gc_t *gc, int fresh_gno ) 
{
  semispace_t *ss;
  if (DATA(gc)->dynamic_area != NULL) {
    ss = ohsc_data_area( DATA(gc)->dynamic_area );
    assert( ss != NULL );
    assert( DATA(gc)->dynamic_area->set_gen_no != NULL );
    if (ss->gen_no >= fresh_gno) {
      oh_set_gen_no( DATA(gc)->dynamic_area, ss->gen_no+1 );
    }
  }
}

static void expand_static_area_gnos( gc_t *gc, int fresh_gno ) 
{
  int new_static_gno;
  if (DATA(gc)->static_generation >= fresh_gno) {
    new_static_gno = DATA(gc)->static_generation + 1;
    if (gc->static_area->data_area)
      ss_set_gen_no( gc->static_area->data_area, new_static_gno );
    if (gc->static_area->text_area)
      ss_set_gen_no( gc->static_area->text_area, new_static_gno );
    DATA(gc)->static_generation = new_static_gno;
    DATA(gc)->globals[ G_FILTER_REMSET_RHS_NUM ] = new_static_gno;
  }
}


static int ssb_process_rrof( gc_t *gc, word *bot, word *top, void *ep_data );
static void expand_remset_gnos( gc_t *gc, int fresh_gno )
{
  int i;
  int new_gno_count = gc->gno_count + 1;
  seqbuf_t ** new_ssb = 
    (seqbuf_t**)must_malloc( sizeof( seqbuf_t*)*new_gno_count );
  word **new_ssb_bot = (word**)must_malloc( sizeof(word*)*new_gno_count );
  word **new_ssb_top = (word**)must_malloc( sizeof(word*)*new_gno_count );
  word **new_ssb_lim = (word**)must_malloc( sizeof(word*)*new_gno_count );
  assert( fresh_gno < new_gno_count );

  urs_expand_remset_gnos( gc->the_remset, fresh_gno );

  for( i = 0; i < fresh_gno; i++ ) {
    new_ssb[i] = gc->ssb[i];
    seqbuf_swap_in_ssb( gc->ssb[i], &new_ssb_bot[i], 
                        &new_ssb_top[i], &new_ssb_lim[i] );
    seqbuf_set_sp_data( gc->ssb[i], /* XXX */(void*) i );
  }
  /* XXX This code only works with RROF, but I do not have a
   * reasonable way to assert that precondition. */
  new_ssb[fresh_gno] = 
    create_seqbuf( DATA(gc)->ssb_entry_count, 
                   &new_ssb_bot[fresh_gno], &new_ssb_top[fresh_gno], 
                   &new_ssb_lim[fresh_gno], ssb_process_rrof, 
                   /* XXX */(void*) fresh_gno );
  for( i = fresh_gno+1; i < new_gno_count; i++ ) {
    new_ssb[i] = gc->ssb[i-1];
    seqbuf_swap_in_ssb( gc->ssb[i-1], 
                        &new_ssb_bot[i], &new_ssb_top[i], &new_ssb_lim[i] );
    seqbuf_set_sp_data( gc->ssb[i-1], /* XXX */(void*)i );
  }

  free( gc->ssb );
  free( DATA(gc)->ssb_bot );
  free( DATA(gc)->ssb_top );
  free( DATA(gc)->ssb_lim );
  if (gc->smircy != NULL) 
    smircy_expand_gnos( gc->smircy, fresh_gno );
  gc->ssb = new_ssb;
  DATA(gc)->ssb_bot = new_ssb_bot;
  DATA(gc)->ssb_top = new_ssb_top;
  DATA(gc)->ssb_lim = new_ssb_lim;
  DATA(gc)->globals[ G_SSBTOPV ] = /* XXX */(word) DATA(gc)->ssb_top;
  DATA(gc)->globals[ G_SSBLIMV ] = /* XXX */(word) DATA(gc)->ssb_lim;
  gc->gno_count = new_gno_count;
}



static old_heap_t* expand_gc_area_gnos( gc_t *gc, int fresh_gno ) 
{
  old_heap_t *heap;
  expand_los_gnos( gc->los, fresh_gno );

  /* hypothesis: young_area gno == 0; implicit accommodation */
  
  heap = expand_ephemeral_area_gnos( gc, fresh_gno );

  expand_dynamic_area_gnos( gc, fresh_gno );
  expand_static_area_gnos( gc, fresh_gno );
  expand_remset_gnos( gc, fresh_gno );
  if (DATA(gc)->summaries != NULL)
    sm_expand_gnos( DATA(gc)->summaries, fresh_gno );
  
  ++(DATA(gc)->generations_after_gc);
  
  return heap;
}

static semispace_t *find_space_expanding( gc_t *gc, unsigned bytes_needed, 
                                          semispace_t *current_space )
{
  ss_expand( current_space, max( bytes_needed, GC_CHUNK_SIZE ) );
  return current_space;
}

static semispace_t *find_space_rgnl( gc_t *gc, unsigned bytes_needed,
                                     semispace_t *current_space )
{
  int los_allocated;
  int cur_allocated;
  int max_allocated = 
    gc_maximum_allotted( gc, gset_singleton( current_space->gen_no ));
  int expansion_amount = max( roundup_page(bytes_needed), GC_CHUNK_SIZE );
  int to_rgn_old = DATA(gc)->rrof_to_region;
  int to_rgn_new;

  oh_synchronize( DATA(gc)->ephemeral_area[ to_rgn_old-1 ] );

  ss_sync( current_space );
  los_allocated =  /* los_bytes_used( gc->los, current_space->gen_no ); */
    los_bytes_used_include_marklists( gc->los, current_space->gen_no );
  cur_allocated = current_space->used+los_allocated;

  if (cur_allocated + expansion_amount <= max_allocated) {
    ss_expand( current_space, expansion_amount );
    return current_space;
  }

  {
    region_group_t grp;
    grp = gc_region_group_for_gno( gc, to_rgn_old );
    assert( (grp == region_group_unfilled)
            || (grp == region_group_wait_nosum) /* XXX */
            || (grp == region_group_wait_w_sum) /* XXX */ );

    region_group_enq( DATA(gc)->ephemeral_area[ to_rgn_old-1 ],
                      grp, region_group_filled );
  }

  assert( gc->scan_update_remset );

  to_rgn_new = find_appropriate_to( gc );

  if (expansion_amount > DATA(gc)->ephemeral_area[ to_rgn_new-1 ]->maximum ) {
    consolemsg("find_space_rgnl: holy cow; bytes_needed=%d and max=%d",
               bytes_needed, DATA(gc)->ephemeral_area[ to_rgn_new-1]->maximum );
  }

  assert( region_group_of( gc_heap_for_gno( gc, to_rgn_new )) 
          == region_group_unfilled );
  DATA(gc)->rrof_to_region = to_rgn_new;
  return oh_current_space( DATA(gc)->ephemeral_area[ to_rgn_new-1 ] );
}

static semispace_t *fresh_space( gc_t *gc ) 
{
  semispace_t *ss;
  old_heap_t *heap;
  int fresh_gno;

  /* Some checks since prototype code relies on unestablished
   * invariants between the static gno and the number of generations. 
   */
  if (gc->static_area != NULL) {
    assert( DATA(gc)->static_generation == DATA(gc)->generations_after_gc - 1 );
    assert( ! gc->static_area->data_area || 
            (DATA(gc)->static_generation == 
             gc->static_area->data_area->gen_no ));
    assert( ! gc->static_area->text_area ||
            (DATA(gc)->static_generation == 
             gc->static_area->text_area->gen_no ));
  }

  /* Allocate a gno to assign to the returned semispace. */
  fresh_gno = find_fresh_gno( gc );
  annoyingmsg( "  fresh_space: gno %d", fresh_gno );

  /* make room for the new space and its associated remembered set. */
  heap = expand_gc_area_gnos( gc, fresh_gno );
  
  ss = ohsc_data_area( heap );
  
  return ss;
}

static int allocated_to_area( gc_t *gc, int gno ) 
{
  int eph_idx = gno-1;
  if (gno == 0) 
    return gc->young_area->allocated;
  else if (DATA(gc)->ephemeral_area && eph_idx < DATA(gc)->ephemeral_area_count) {
#ifdef NDEBUG2
    return DATA(gc)->ephemeral_area[ eph_idx ]->allocated;
#else
    int area_thinks_allocated = 
      DATA(gc)->ephemeral_area[ eph_idx ]->allocated;
    int retval;
    semispace_t *space = oh_current_space( DATA(gc)->ephemeral_area[ eph_idx ] );
    ss_sync( space );
    retval = space->used + los_bytes_used( gc->los, gno );
    if (area_thinks_allocated != retval) {
      consolemsg("gno: %d area thinks: %d but actually: %d", 
                  gno, area_thinks_allocated, retval);
    }
    assert( area_thinks_allocated == retval ); /* XXX was this ever right? */
    return retval;
#endif
  } else {
    consolemsg( "allocated_to_area: unknown area %d", gno );
    assert(0);
  }
}

static int maximum_allotted_to_area( gc_t *gc, int gno ) 
{
  int eph_idx = gno-1;
  if (gno == 0)
    return gc->young_area->maximum;
  else if (DATA(gc)->ephemeral_area && eph_idx < DATA(gc)->ephemeral_area_count)
    return DATA(gc)->ephemeral_area[ eph_idx ]->maximum;
  else {
    consolemsg( "maximum_allotted_to_area: unknown area %d", gno );
    assert(0);
  }
}

static int allocated_to_areas( gc_t *gc, gset_t gs ) 
{
  int i, sum;
  switch (gs.tag) {
  case gs_singleton:
    return allocated_to_area( gc, gs.g1 );
  case gs_range:
    sum = 0;
    for (i = gs.g1; i < gs.g2; i++) 
      sum += allocated_to_area( gc, i );
    return sum;
  case gs_nil:   return 0;
  case gs_twrng: assert(0); /* not implemented yet */
  }
  assert(0);
}

static int maximum_allotted( gc_t *gc, gset_t gs ) 
{
  int i, sum;
  switch (gs.tag) {
  case gs_singleton:
    return maximum_allotted_to_area( gc, gs.g1 );
  case gs_range:
    sum = 0;
    for (i = gs.g1; i < gs.g2; i++) 
      sum += maximum_allotted_to_area( gc, i );
    return sum;
  case gs_nil:   return 0;
  case gs_twrng: assert(0); /* not implemented yet */
  }
  assert(0);
}

static bool is_nonmoving( gc_t *gc, int gen_no ) 
{
  region_group_t grp;
  if (gen_no == DATA(gc)->static_generation)
    return TRUE;
  
  grp = gc_region_group_for_gno( gc, gen_no );
  if ((grp == region_group_infamous) || (grp == region_group_hasbeen) )
    return TRUE;

  return FALSE;
}

static bool is_address_mapped( gc_t *gc, word *addr, bool noisy ) 
{
  bool ret = FALSE;
  assert(tagof(addr) == 0);
  if (gc->los && los_is_address_mapped( gc->los, addr, noisy )) {
    assert(!ret); ret = TRUE;
  }
  if (gc->young_area && yh_is_address_mapped( gc->young_area, addr )) {
    if (ret) {
      assert( ! los_is_address_mapped( gc->los, addr, TRUE ));
      assert( !ret );
    }
    ret = TRUE;
  }
  if (gc->static_area && sh_is_address_mapped( gc->static_area, addr, noisy )) {
    if (ret) {
      sh_is_address_mapped( gc->static_area, addr, TRUE );
      assert( ! los_is_address_mapped( gc->los, addr, TRUE ));
      assert( ! yh_is_address_mapped( gc->young_area, addr ));
      assert( !ret );
    }
    ret = TRUE;
  }
  if (DATA(gc)->dynamic_area && 
      oh_is_address_mapped( DATA(gc)->dynamic_area, addr, noisy )) {
    if (ret) {
      oh_is_address_mapped( DATA(gc)->dynamic_area, addr, TRUE );
      assert( ! los_is_address_mapped( gc->los, addr, TRUE ));
      assert( ! yh_is_address_mapped( gc->young_area, addr ));
      assert( ! sh_is_address_mapped( gc->static_area, addr, FALSE ));
      assert( !ret );
    }
    ret = TRUE;
  }
  { 
    int i, j;
    for( i = 0; i < DATA(gc)->ephemeral_area_count; i++ ) {
      if (oh_is_address_mapped( DATA(gc)->ephemeral_area[i], addr, noisy )) {
        if (ret) {
          oh_is_address_mapped( DATA(gc)->ephemeral_area[i], addr, TRUE );
          assert( ! los_is_address_mapped( gc->los, addr, TRUE ));
          assert( ! yh_is_address_mapped( gc->young_area, addr ));
          assert( ! sh_is_address_mapped( gc->static_area, addr, TRUE ));
          assert( ! oh_is_address_mapped( DATA(gc)->dynamic_area, addr, TRUE ));
          for ( j = 0; j < i; j++ )
            assert( ! oh_is_address_mapped( DATA(gc)->ephemeral_area[j], 
                                            addr, 
                                            TRUE ));
          assert( !ret ); 
        }
        ret = TRUE;
      }
    }
  }
  return ret;
}

static int allocate_stopcopy_system( gc_t *gc, gc_param_t *info )
{
  char buf[ 100 ];

  DATA(gc)->dynamic_max = info->sc_info.dynamic_max;
  DATA(gc)->dynamic_min = info->sc_info.dynamic_min;
  DATA(gc)->remset_undirected = FALSE;
  DATA(gc)->mut_activity_bounded = FALSE;

  strcpy( buf, "S+C " );

  gc->young_area = create_sc_heap( 0, gc, &info->sc_info, info->globals );
  strcat( buf, gc->young_area->id );

  if (info->use_static_area) {
    gc->static_area = create_static_area( 1, gc );
    strcat( buf, "+" );
    strcat( buf, gc->static_area->id );
  }

  gc->satb_ssb = NULL;

  gc->id = strdup( buf );
  return (info->use_static_area ? 2 : 1);
}

static int ssb_process_gen( gc_t *gc, word *bot, word *top, void *ep_data ) {
  urs_add_elems( gc->the_remset, bot, top );
  return 0; /* urs_add_elems doesn't currently provide an interesting result */
}

static int calc_cN( gc_t *gc )
{
  double F_1 = DATA(gc)->rrof_sumz_params.coverage_inv;
  double F_2 = DATA(gc)->rrof_sumz_params.budget_inv;
  int    F_3 = DATA(gc)->rrof_sumz_params.max_retries;
  double   S = DATA(gc)->rrof_sumz_params.popularity_factor;

  double c = (F_2*F_3 - 1.0)/(F_1 * F_2)*S - 1.0;
  int N = /* FIXME should be incrementally calculated via collection delta */
    gc_allocated_to_areas( gc, gset_range( 1, DATA(gc)->ephemeral_area_count )) 
    / sizeof(word);
  int retval;

  assert2( c > 0.0 );

  N = umax( N, 5*MEGABYTE );

  retval = (int)(c * ((double)N));
  if (retval <= 0 ) {
    consolemsg("c:%g * N:%d yields nonneg.", c, N );
  }
  assert( retval > 0 );
  return retval;
}

/* Returns the mutator activity during this summarization cycle. */

static int calc_mutator_activity_sumz( gc_t *gc ) {
  word *globals;
  word *p;
  word *lim;
  int nwords;              /* words allocated in nursery */
  int mut_activity_sumz;

  globals = DATA(gc)->globals;
  p = (word*)globals[ G_ETOP ];
  lim = ((word*)globals[ G_STKP ]-SCE_BUFFER);
  nwords = (lim-p);

  /* FIXME: just guessing that satb entries should count too */

  mut_activity_sumz = 
    nwords +
    DATA(gc)->mutator_effort.satb_ssb_entries_flushed_this.sumz_cycle + 
    DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle + 
    DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle;

  return mut_activity_sumz;
}


static void zero_all_mutator_effort( gc_data_t *data ) {
  struct mutator_effort *p = &data->mutator_effort;
  p->rrof_ssb_flushes = 0;
  p->rrof_ssb_entries_flushed_total = 0;
  p->rrof_ssb_entries_flushed_this.full_cycle = 0;
  p->rrof_ssb_entries_flushed_this.sumz_cycle = 0;
  p->rrof_ssb_max_entries_flushed_any.full_cycle = 0;
  p->rrof_ssb_max_entries_flushed_any.sumz_cycle = 0;

  p->satb_ssb_flushes = 0;
  p->satb_ssb_entries_flushed_total = 0;
  p->satb_ssb_entries_flushed_this.full_cycle = 0;
  p->satb_ssb_entries_flushed_this.sumz_cycle = 0;
  p->satb_ssb_max_entries_flushed_any.full_cycle = 0;
  p->satb_ssb_max_entries_flushed_any.sumz_cycle = 0;

  p->words_promoted_this.full_cycle = 0;
  p->words_promoted_this.sumz_cycle = 0;
  p->max_words_promoted_any.full_cycle = 0;
  p->max_words_promoted_any.sumz_cycle = 0;

  p->forcing_collector_to_progress = FALSE;
}

static void update_rrof_flush_counts( gc_t *gc, int entries_flushed ) 
{
  struct mutator_effort *p = &(DATA(gc)->mutator_effort);
  p->rrof_ssb_flushes += 1;
  p->rrof_ssb_entries_flushed_total += entries_flushed;
  p->rrof_ssb_entries_flushed_this.full_cycle += entries_flushed;
  if (DATA(gc)->summaries != NULL) {
    p->rrof_ssb_entries_flushed_this.sumz_cycle += entries_flushed;
    p->rrof_ssb_max_entries_flushed_any.full_cycle
      = max( p->rrof_ssb_entries_flushed_this.full_cycle, 
             p->rrof_ssb_max_entries_flushed_any.full_cycle);
    p->rrof_ssb_max_entries_flushed_any.sumz_cycle
      = max( p->rrof_ssb_entries_flushed_this.sumz_cycle, 
             p->rrof_ssb_max_entries_flushed_any.sumz_cycle);
  }
}

static void update_satb_flush_counts( gc_t *gc, int entries_flushed ) 
{
  struct mutator_effort *p = &(DATA(gc)->mutator_effort);
  p->satb_ssb_flushes += 1;
  p->satb_ssb_entries_flushed_total += entries_flushed;
  p->satb_ssb_entries_flushed_this.full_cycle += entries_flushed;

  if (DATA(gc)->summaries != NULL) {
    p->satb_ssb_entries_flushed_this.sumz_cycle += entries_flushed;
    p->satb_ssb_max_entries_flushed_any.full_cycle
      = max( p->satb_ssb_entries_flushed_this.full_cycle, 
             p->satb_ssb_max_entries_flushed_any.full_cycle);
    p->satb_ssb_max_entries_flushed_any.sumz_cycle
      = max( p->satb_ssb_entries_flushed_this.sumz_cycle, 
             p->satb_ssb_max_entries_flushed_any.sumz_cycle);
  }
}

static int ssb_process_satb( gc_t *gc, word *bot, word *top, void *ep_data ) {
  update_satb_flush_counts( gc, (top - bot) );
  if (bot != top) {
    int cN;
    int mut_effort_full = 
      (DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.full_cycle + 
       DATA(gc)->mutator_effort.words_promoted_this.full_cycle);
    int mut_effort_sumz = 
      (DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle + 
       DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle);

    if ( DATA(gc)->region_count > 2) 
      cN = calc_cN( gc ); /* delay (expensive?) valuation */
    if ( DATA(gc)->region_count > 2 && mut_effort_sumz > cN ) {
#if 0
      consolemsg( "ssb_process_satb( gc, bot: 0x%08x, top: 0x%08x ) gc,majors:%d,%d cnt:%d flush:%lld,%d<=%d promote:%d<=%d %d%s%d summ:%d.%d.%d/%d", 
                  bot, top,
                  nativeint( DATA(gc)->globals[ G_GC_CNT ] ), nativeint( DATA(gc)->globals[ G_MAJORGC_CNT ] ),
                  DATA(gc)->mutator_effort.satb_ssb_flushes,
                  DATA(gc)->mutator_effort.satb_ssb_entries_flushed_total,
                  DATA(gc)->mutator_effort.satb_ssb_entries_flushed_this.sumz_cycle,
                  DATA(gc)->mutator_effort.satb_ssb_max_entries_flushed_any.sumz_cycle,
                  DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle,
                  DATA(gc)->mutator_effort.max_words_promoted_any.sumz_cycle,
                  mut_effort_sumz, 
                  (mut_effort_sumz <= cN)?" <= ":" >> ", 
                  cN, 
                  (DATA(gc)->summaries != NULL)?sm_cycle_count( DATA(gc)->summaries ):0,
                  (DATA(gc)->summaries != NULL)?sm_pass_count( DATA(gc)->summaries ):0,
                  (DATA(gc)->summaries != NULL)?sm_scan_count_curr_pass( DATA(gc)->summaries ):0,
                  DATA(gc)->region_count );
#endif
    }
  }
  if (gc->smircy != NULL) {
    if (! smircy_stack_empty_p( gc->smircy )) {
      smircy_push_elems( gc->smircy, bot, top );
    }
  }
  return 0;
}

static int ssb_process_rrof( gc_t *gc, word *bot, word *top, void *ep_data ) 
{
  int retval = 0;
  int g_rhs;
  word *p, *q, w;

  /* XXX this is not really right; there may be duplicate entries in
   * the SSB, and in principle we are supposed to only count the
   * number of distinct locations assigned.
   * 
   * But I do not carry enough information around to know about all
   * duplicate entries... at best for now I can count locations added
   * for a *subset* of the regions (via the summary set addittions)
   * and/or I can count the *objects* added to the remembered set...
   */

  update_rrof_flush_counts( gc, (top - bot) );
  if (bot != top) {
    int cN;
    int mut_effort_full, mut_effort_sumz;
    if (DATA(gc)->mut_activity_bounded
        && DATA(gc)->summaries != NULL) {
      cN = calc_cN( gc );
      mut_effort_full = 
        (DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.full_cycle + 
         DATA(gc)->mutator_effort.words_promoted_this.full_cycle);
      mut_effort_sumz = 
        (DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle + 
         DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle);
      FIXME_UNUSED_VARIABLE(mut_effort_full);
    }
    if (DATA(gc)->mut_activity_bounded 
        && DATA(gc)->summaries != NULL
        && mut_effort_sumz > cN) {
#if 0
      consolemsg( "ssb_process_rrof( gc, bot: 0x%08x, top: 0x%08x ) gc,majors:%d,%d cnt:%d flush:%lld,%d<=%d promote:%d<=%d %d%s%d summ:%d.%d.%d/%d", 
                  bot, top,
                  nativeint( DATA(gc)->globals[ G_GC_CNT ] ), nativeint( DATA(gc)->globals[ G_MAJORGC_CNT ] ),
                  DATA(gc)->mutator_effort.rrof_ssb_flushes,
                  DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_total,
                  DATA(gc)->mutator_effort.rrof_ssb_entries_flushed_this.sumz_cycle,
                  DATA(gc)->mutator_effort.rrof_ssb_max_entries_flushed_any.sumz_cycle,
                  DATA(gc)->mutator_effort.words_promoted_this.sumz_cycle,
                  DATA(gc)->mutator_effort.max_words_promoted_any.sumz_cycle,
                  mut_effort_sumz, 
                  (mut_effort_sumz <= cN)?" <= ":" >> ", 
                  cN, 
                  (DATA(gc)->summaries != NULL)?sm_cycle_count( DATA(gc)->summaries ):0,
                  (DATA(gc)->summaries != NULL)?sm_pass_count( DATA(gc)->summaries ):0,
                  (DATA(gc)->summaries != NULL)?sm_scan_count_curr_pass( DATA(gc)->summaries ):0,
                  DATA(gc)->ephemeral_area_count );
#endif
    }
  }
  retval |= urs_add_elems( gc->the_remset, bot, top );

  g_rhs = (int)ep_data; /* XXX is (int) of void* legal C? */
  if (DATA(gc)->summaries != NULL
#if 0
      && g_rhs <= DATA(gc)->region_count /* XXX is region_count sane? */
#endif
      ) {
    sm_add_ssb_elems_to_summary( DATA(gc)->summaries, bot, top, g_rhs );
  }
  return retval;
}

static int allocate_generational_system( gc_t *gc, gc_param_t *info )
{
  char buf[ 256 ], buf2[ 100 ];
  int gen_no, i, size;
  gc_data_t *data = DATA(gc);

  data->globals[ G_FILTER_REMSET_GEN_ORDER ] =  TRUE;
  gen_no = 0;
  data->is_partitioned_system = 1;
  data->use_np_collector = info->use_non_predictive_collector;
  data->remset_undirected = FALSE;
  data->mut_activity_bounded = FALSE;
  size = 0;

  strcpy( buf, "GEN " );

  if (info->use_non_predictive_collector) {
    DATA(gc)->dynamic_max = info->dynamic_np_info.dynamic_max;
    DATA(gc)->dynamic_min = info->dynamic_np_info.dynamic_min;
  }
  else {
    DATA(gc)->dynamic_max = info->dynamic_sc_info.dynamic_max;
    DATA(gc)->dynamic_min = info->dynamic_sc_info.dynamic_min;
  }

  /* Create nursery.
     */
  gc->young_area =
    create_nursery( gen_no, gc, &info->nursery_info, info->globals );
  data->globals[ G_FILTER_REMSET_LHS_NUM ] = gen_no;
  size += info->nursery_info.size_bytes;
  gen_no += 1;
  strcat( buf, gc->young_area->id );

  /* Create ephemeral areas. 
     */
  { int e = DATA(gc)->ephemeral_area_count = info->ephemeral_area_count;
    DATA(gc)->fixed_ephemeral_area = TRUE;
    DATA(gc)->ephemeral_area =
      (old_heap_t**)must_malloc( e*sizeof( old_heap_t* ) );

    for ( i = 0 ; i < e ; i++ ) {
      size += info->ephemeral_info[i].size_bytes;
      DATA(gc)->ephemeral_area[ i ] = 
        create_sc_area( gen_no, gc, &info->ephemeral_info[i], 
                        OHTYPE_EPHEMERAL );
      gen_no += 1;
    }
  }
  if (DATA(gc)->ephemeral_area_count > 0) {
    sprintf( buf2, "+%d*%s",
             DATA(gc)->ephemeral_area_count, DATA(gc)->ephemeral_area[0]->id );
    strcat( buf, buf2 );
  }

  data->nonexpandable_size = size;

  /* Create dynamic area.
     */
  if (info->use_non_predictive_collector) {
#if ROF_COLLECTOR
    int gen_allocd;

    DATA(gc)->dynamic_area = 
      create_np_dynamic_area( gen_no, &gen_allocd, gc, &info->dynamic_np_info);
    gen_no += gen_allocd;
#else
    panic_exit( "ROF collector not compiled in" );
#endif
  }
  else {
    DATA(gc)->dynamic_area = 
      create_sc_area( gen_no, gc, &info->dynamic_sc_info, OHTYPE_DYNAMIC );
    gen_no += 1;
  }
  strcat( buf, "+" );
  strcat( buf, DATA(gc)->dynamic_area->id );

  /* Create static area.
     */
  if (info->use_static_area) {
    gc->static_area = create_static_area( gen_no, gc );
    data->globals[ G_FILTER_REMSET_RHS_NUM ] = gen_no;
    data->static_generation = gen_no;
    gen_no += 1;
    strcat( buf, "+" );
    strcat( buf, gc->static_area->id );
  } else {
    data->globals[ G_FILTER_REMSET_RHS_NUM ] = -1;
  }

  /* Create remembered sets and SSBs.  Entry 0 is not used.
     If the non-predictive area is used, then the last remembered set
     is the non-predictive 'extra' set (contains only young->old pointers).
     */
  if (info->use_non_predictive_collector)
    gc->gno_count = gen_no + 1;
  else
    gc->gno_count = gen_no;

  if (info->chose_rhashrep) { 
    gc->the_remset = alloc_uremset_array( gc, info );
  } else if (info->chose_rbitsrep) {
    gc->the_remset = alloc_uremset_extbmp( gc, info );
  } else {
    gc->the_remset = alloc_uremset_array( gc, info );
  }

  data->ssb_bot = (word**)must_malloc( sizeof(word*)*gc->gno_count );
  data->ssb_top = (word**)must_malloc( sizeof(word*)*gc->gno_count );
  data->ssb_lim = (word**)must_malloc( sizeof(word*)*gc->gno_count );
  gc->ssb = (seqbuf_t**)must_malloc( sizeof(seqbuf_t*)*gc->gno_count );
  DATA(gc)->ssb_entry_count = info->ssb;
  for ( i = 0; i < gc->gno_count ; i++ ) {
    /* nursery has one too, an artifact of RROF.
     * XXX consider using different structures for n-YF vs ROF vs RROF;
     * RROF needs points-into information implicit with index here,
     * but others do not. */
    gc->ssb[i] = 
      create_seqbuf( DATA(gc)->ssb_entry_count, 
                     &data->ssb_bot[i], &data->ssb_top[i], &data->ssb_lim[i],
                     ssb_process_gen, 0 );
  }
  gc->satb_ssb = NULL;

  if (info->use_non_predictive_collector)
    gc->np_remset = gc->gno_count - 1;

  gc->id = strdup( buf );

  return gen_no;
}

static int allocate_regional_system( gc_t *gc, gc_param_t *info )
{
  char buf[ 256 ], buf2[100]; /* are these numbers large enough in general? */
  gc_data_t *data = DATA(gc);
  int gen_no, size;
  
  data->globals[ G_FILTER_REMSET_GEN_ORDER ] = FALSE;
  gen_no = 0;
  data->is_partitioned_system = 1;
  assert( ! info->use_non_predictive_collector );
  data->use_np_collector = 0; /* RROF is not ROF. */
  data->remset_undirected = TRUE;
  data->mut_activity_bounded = TRUE;
  size = 0;

  strcpy( buf, "RGN " );
  
  data->dynamic_max = info->dynamic_sc_info.dynamic_max;
  data->dynamic_min = info->dynamic_sc_info.dynamic_min;
  data->rrof_load_factor_soft = info->dynamic_sc_info.load_factor;
  data->rrof_load_factor_hard = info->dynamic_sc_info.load_factor_hard;

  /* Create nursery. */
  gc->young_area = 
    create_nursery( gen_no, gc, &info->nursery_info, info->globals );
  data->globals[ G_FILTER_REMSET_LHS_NUM ] = gen_no;
  size += info->nursery_info.size_bytes;
  gen_no += 1;
  strcat( buf, gc->young_area->id );
  
  /* Create ephemeral areas. */
  { 
    int i;
    int e = data->ephemeral_area_count = info->ephemeral_area_count;
    int words;
    double popular_factor;
    double infamy_factor;
    assert( e > 0 );
    data->region_count = e;
    data->fixed_ephemeral_area = FALSE;
    data->ephemeral_area = (old_heap_t**)must_malloc( e*sizeof( old_heap_t* ));
    
    popular_factor = (info->has_popularity_factor 
                      ? info->popularity_factor
                      : default_popularity_factor);
    infamy_factor = (info->has_infamy_factor 
                     ? info->infamy_factor
                     : default_infamy_factor );
    data->rrof_sumz_params.popularity_factor = popular_factor;
    data->rrof_sumz_params.popularity_limit_words =
      (info->ephemeral_info[ e-1 ].size_bytes
       * popular_factor / sizeof(word));
    assert( data->rrof_sumz_params.popularity_limit_words > 0 );
    data->rrof_sumz_params.infamy_factor = infamy_factor;
    data->rrof_sumz_params.budget_inv = 
      (info->has_sumzbudget
       ? info->sumzbudget_inv
       : default_sumz_budget_inv);
    data->rrof_sumz_params.coverage_inv = 
      (info->has_sumzcoverage
       ? info->sumzcoverage_inv
       : default_sumz_coverage_inv);
    data->rrof_sumz_params.max_retries =
      (info->has_sumz_retries
       ? info->max_sumz_retries
       : default_sumz_max_retries);

    {
      double F_1 = DATA(gc)->rrof_sumz_params.coverage_inv;
      double F_2 = DATA(gc)->rrof_sumz_params.budget_inv;
      int    F_3 = DATA(gc)->rrof_sumz_params.max_retries;
      double   S = DATA(gc)->rrof_sumz_params.popularity_factor;
      double c = (F_2*F_3 - 1.0)/(F_1 * F_2)*S - 1.0;
      double max_utilized_regions;
      if (c <= 0.0) {
        consolemsg("c:%g must be positive; "
                   "c = (F_2*F_3 - 1)/(F_1*F_2)*S-1.0 "
                   """= (%g*%d - 1)/(%g*%g)*%g-1.0",
                   c,   F_2, F_3,   F_1, F_2, S);
      }
      assert( c > 0.0 );

      max_utilized_regions =
        (1.0 / S 
         + ((double)F_3)/F_1 
         + 1.0/(F_1*F_2*((double)F_3)));
      if (max_utilized_regions > 1.0) {
        consolemsg("Invalid parameter selection; need\n"
                   " 1/S + F_3/F_1 + 1/(F_1 F_2 F_3) = "
                   " 1/%g + %d/%g  + 1/(%g %g %d) <= 1"
                   ,   S,  F_3, F_1, F_1, F_2, F_3);
      }
      assert( max_utilized_regions <= 1.0 );
    }

    for ( i = 0; i < e; i++ ) {
      assert( info->ephemeral_info[i].size_bytes > 0 );
      words = info->ephemeral_info[i].size_bytes/sizeof(word); 
      data->max_live_words = words;
      size += info->ephemeral_info[i].size_bytes;
      DATA(gc)->rrof_words_per_region_max = 
        max( words, DATA(gc)->rrof_words_per_region_max );
      DATA(gc)->rrof_words_per_region_min = 
        ((DATA(gc)->rrof_words_per_region_min < 0) ? 
         words : min( words, DATA(gc)->rrof_words_per_region_min ));
      data->ephemeral_area[ i ] = 
        create_sc_area( gen_no, gc, &info->ephemeral_info[i], 
                        OHTYPE_REGIONAL );
      region_group_enq( data->ephemeral_area[ i ], 
                        region_group_nonrrof, region_group_unfilled );
      gen_no += 1;
    }
  }
  assert( data->rrof_words_per_region_min > 0 );
  assert( data->rrof_words_per_region_max > 0 );
  if (data->ephemeral_area_count > 0) {
    sprintf( buf2, "+%d*%s",
             DATA(gc)->ephemeral_area_count, DATA(gc)->ephemeral_area[0]->id );
    strcat( buf, buf2 );
  }
  data->nonexpandable_size = size; /* is this sensible in RROF? */
  
  /* There is no dynamic area in the RROF 
     (the ephemeral area array is expanded instead).
     */
  data->dynamic_area = 0;
  
  /* Create static area.
     */
  if (info->use_static_area) {
    gc->static_area = create_static_area( gen_no, gc );
    data->globals[ G_FILTER_REMSET_RHS_NUM ] = gen_no;
    data->static_generation = gen_no;
    gen_no += 1;
    strcat( buf, "+" );
    strcat( buf, gc->static_area->id );
  } else {
    data->globals[ G_FILTER_REMSET_RHS_NUM ] = -1;
  }

  /* Create remembered sets and SSBs.  Entry 0 is not used.
     */
  { 
    int i;
    gc->gno_count = gen_no;

    if (info->chose_rhashrep) { 
      gc->the_remset = alloc_uremset_array( gc, info );
    } else if (info->chose_rbitsrep) {
      gc->the_remset = alloc_uremset_extbmp( gc, info );
    } else {
      gc->the_remset = alloc_uremset_extbmp( gc, info );
    }
#if USE_URS_WRAPPER_TO_CHECK_REMSET_SANITY
    gc->the_remset = alloc_uremset_debug( gc->the_remset );
#endif

    data->ssb_bot = (word**)must_malloc( sizeof(word*)*gc->gno_count );
    data->ssb_top = (word**)must_malloc( sizeof(word*)*gc->gno_count );
    data->ssb_lim = (word**)must_malloc( sizeof(word*)*gc->gno_count );
    gc->ssb = (seqbuf_t**)must_malloc( sizeof(seqbuf_t*)*gc->gno_count );
    DATA(gc)->ssb_entry_count = info->ssb;
    for ( i = 0; i < gc->gno_count ; i++ ) {
      /* nursery has one too! */
      gc->ssb[i] = 
        create_seqbuf( DATA(gc)->ssb_entry_count, 
                       &data->ssb_bot[i], &data->ssb_top[i], &data->ssb_lim[i],
                       ssb_process_rrof, 0 );
    }
    gc->satb_ssb = create_seqbuf( DATA(gc)->ssb_entry_count, 
                                  &data->satb_ssb_bot, 
                                  &data->satb_ssb_top, &data->satb_ssb_lim, 
                                  ssb_process_satb, 0 );
  }

  gc->id = strdup( buf );

  if (info->mark_period) {
    assert(! info->has_refine_factor );
    data->rrof_has_refine_factor = FALSE;
    data->rrof_refine_mark_period = info->mark_period;
    data->rrof_refine_mark_countdown 
      = data->rrof_refine_mark_period;
  }
  if (info->has_refine_factor) {
    double R = info->refinement_factor;
    int countdown_to_first_mark;
    assert(! info->mark_period);
    data->rrof_has_refine_factor = TRUE;
    data->rrof_refinement_factor = R;
    /* semi-arbitrary guess at an expression for initial value */
    countdown_to_first_mark = 
      (int)(R*(double)size)/((double)info->nursery_info.size_bytes);
    assert( countdown_to_first_mark >= 0 );
    data->rrof_refine_mark_countdown = countdown_to_first_mark;
    if (0) consolemsg("initial mark countdown: %d", countdown_to_first_mark );
  }
  data->rrof_alloc_mark_bmp_once = info->alloc_mark_bmp_once;

  data->oracle_countdown = info->oracle_countdown;

  data->print_float_stats_each_cycle  = info->print_float_stats_cycle;
  data->print_float_stats_each_major  = info->print_float_stats_major;
  data->print_float_stats_each_minor  = info->print_float_stats_minor;
  data->print_float_stats_each_refine = info->print_float_stats_refine;

  DATA(gc)->summaries = NULL;

  return gen_no;
}

static word last_origin_ptr_added = 0;
static void points_across_callback( gc_t *gc, word lhs, int offset, word rhs ) 
{
  int g_lhs = gen_of(lhs);
  int g_rhs = gen_of(rhs);
  assert2( g_lhs != 0 ); /* gf_filter_remset_lhs == 0 */
  if (! gc_is_nonmoving( gc, g_rhs )) {
    {
      assert2(g_lhs > 0);
      assert2(g_rhs >= 0);

      /* enqueue lhs in remset. */
      if (last_origin_ptr_added != lhs) {
        urs_add_elem_new( gc->the_remset, lhs );
        last_origin_ptr_added = lhs;
      }

      if (DATA(gc)->summaries != NULL) {
        sm_points_across_callback( DATA(gc)->summaries, lhs, offset, g_rhs );
      }
    }
  }
}

static 
old_heap_t *heap_for_gno(gc_t *gc, int gen_no ) {
  assert( gen_no > 0 );
  assert( (gen_no-1) < DATA(gc)->ephemeral_area_count );
  return DATA(gc)->ephemeral_area[ gen_no-1 ];
}

static 
region_group_t region_group_for_gno(gc_t *gc, int gen_no ) {
  assert( gen_no >= 0 );
  if (gen_no == 0) {
    return region_group_nonrrof;
  } else if ((gen_no-1) < DATA(gc)->ephemeral_area_count ) {
    return DATA(gc)->ephemeral_area[ gen_no-1 ]->group;
  } else if ( gen_no == DATA(gc)->static_generation ) {
    return region_group_nonrrof;
  } else {
    assert(0);
  }
}

static void check_invariants_between_fwd_and_free( gc_t *gc, int gen_no )
{
  verify_fwdfree_via_oracle_gen_no = gen_no;
  FWDFREE_VERIFICATION_POINT( gc );
  return;
}

/*     Allocates and initializes the gc structure.
 *     Collector-specific initialization comes later.
 */

static gc_t *alloc_gc_structure( word *globals, gc_param_t *info )
{
  gc_data_t *data;
  gc_t *ret;
  semispace_t *(*my_find_space)( gc_t *gc, unsigned bytes_needed, 
                                 semispace_t *current_space );
  void (*my_collect)( gc_t *gc, int rgn, int bytes_needed, gc_type_t request );
  void (*my_incremental)( gc_t *gc );
  void (*my_check_remset_invs)( gc_t *gc, word src, word tgt );
  
  if (info->is_regional_system) {
    my_find_space = find_space_rgnl;
    my_collect = collect_rgnl;
    my_incremental = incremental_rgnl;
    my_check_remset_invs = check_remset_invs_rgnl;    
  } else {
    my_find_space = find_space_expanding;
    my_collect = collect_generational;
    my_incremental = incremental_nop;
    my_check_remset_invs = check_remset_invs;
  }

  data = (gc_data_t*)must_malloc( sizeof( gc_data_t ) );

  /* Through v0.99, this code contained a latent bug by relying   */
  /* on the operating system to zero out the allocated structure. */
  /* The fields that were left uninitialized through v0.99 are    */
  /* initialized to zero here.  Some may be set to nonzero values */
  /* by later processing, but they should start out with some     */
  /* definite value rather than garbage, if only for debugging.   */

  memset( data, 0, sizeof(gc_data_t) );

  data->use_np_collector = FALSE;
  data->remset_undirected = FALSE;
  data->mut_activity_bounded = FALSE;
  data->generations = 0;
  data->generations_after_gc = 0;
  data->static_generation = 0;
  data->satb_ssb_bot = 0;
  data->satb_ssb_top = 0;
  data->satb_ssb_lim = 0;
  data->ssb_entry_count = 0;
  data->rrof_load_factor_soft = 2.0;
  data->rrof_load_factor_hard = 2.0;
  /* Rely on memset for data->rrof_sumz_params. */
  data->rrof_alloc_mark_bmp_once = FALSE;
  data->rrof_prefer_big_summ = FALSE;
  data->rrof_prefer_lil_summ = FALSE;
  data->rrof_prefer_lat_summ = FALSE;
  data->enumerate_major_with_minor_remsets = FALSE;
  /* Rely on memset for data->summary. */
  data->next_summary_to_use = 0;
  data->summaries = 0;
  data->secondary_space = 0;
  /* Rely on memset for pause_timer_elapsed, pause_timer_cpu. */
  data->last_pause_elapsed = 0;
  data->last_pause_cpu = 0;
  data->major_page_fault_count_at_gc_start = 0;
  data->minor_page_fault_count_at_gc_start = 0;
  data->stat_last_ms_remset_sumrize = 0;
  data->stat_last_ms_remset_sumrize_cpu = 0;
  data->stat_last_ms_smircy_mark = 0;
  data->stat_last_ms_smircy_mark_cpu = 0;
  data->stat_last_ms_smircy_refine = 0;
  data->stat_last_ms_smircy_refine_cpu = 0;
  data->stat_length_minor_gc_run = 0;         /* cause of ticket #784 */
  data->print_float_stats_each_cycle = FALSE;
  data->print_float_stats_each_major = FALSE;
  data->print_float_stats_each_minor = FALSE;
  data->print_float_stats_each_refine = FALSE;
  /* Rely on memset for since_finished_snapshot_began. */
  /* Rely on memset for since_developing_snapshot_began. */
  /* Rely on memset for since_cycle_began. */
  /* Rely on memset for since_finished_snapshot_at_time_cycle_began_began. */
  /* Rely on memset for mutator_effort. */
  data->oracle_countdown = 0;
  data->oracle_pointsrun = 0;

  /* End of initialization of previously uninitialized fields. */

  data->globals = globals;
  data->is_partitioned_system = 0;
  data->shrink_heap = 0;
  data->in_gc = 0;
  data->handles = (word*)must_malloc( sizeof(word)*10 );
  data->nhandles = 10;
  memset( data->handles, 0, sizeof(word)*data->nhandles );
  data->ssb_bot = 0;
  data->ssb_top = 0;
  data->ssb_lim = 0;
  data->dynamic_max = 0;
  data->dynamic_min = 0;
  data->nonexpandable_size = 0;
  data->ephemeral_area = 0;
  data->ephemeral_area_count = 0;
  data->region_count = 0;
  data->fixed_ephemeral_area = TRUE;
  data->dynamic_area = 0;

  data->rrof_currently_minor_gc = FALSE;
  data->rrof_to_region = 1;
  data->rrof_next_region = 1;
  data->rrof_last_tospace = -1;
  data->rrof_cycle_majors_sofar = 0;
  data->rrof_cycle_majors_total = 1;

  data->rrof_has_refine_factor = TRUE;
  data->rrof_refinement_factor = 1.0;
  data->rrof_refine_mark_period = -1;
  data->rrof_refine_mark_countdown = -1;

  data->rrof_last_live_estimate = 0;
  data->rrof_cycle_count = 0;

  data->rrof_last_gc_rolled_cycle = FALSE;

  data->use_summary_instead_of_remsets = FALSE;       /* FIXME */
  summary_vs_remset_msg( "FIXME: setting FALSE (initially)" );

  data->last_live_words = 0;
  data->max_live_words = 0;

  /* FIXME
   *
   * max_live_words_at_time_cycle_began, aka P_old, should be
   * initialized to the live storage plus some headroom, but
   * we don't know the live storage at this point.
   *
   * The headroom will be added by rrof_calc_target_allocation,
   * but the first major cycle is allowing only that headroom
   * to be allocated.  For large initial heaps, it would be
   * better to take the initial heap size into account.
   */

  data->last_live_words_at_time_cycle_began = 0;
  data->max_live_words_at_time_cycle_began = 0;

  data->total_heap_words_allocated = 0;
  data->allocation_target_for_cycle = 0;

  data->rrof_words_per_region_max = 0;
  data->rrof_words_per_region_min = -1;

  zero_all_mutator_effort( data );

#if GATHER_MMU_DATA
  if (info->mmu_buf_size >= 0) { 
    int mmu_buf_size;
    static int mmu_window_lengths[] = {
      100, 
      200,
      300,
      400,
      500,
      600,
      700,
      800,
      900, 
      1000, 
      2000,
      3000,
      4000,
      5000,
      6000,
      7000,
      8000,
      9000,
      10000, 
      /*
      100000,
      1000000,
      */
      -1 /* end of array marker */
    };

    mmu_buf_size = ((info->mmu_buf_size == 0) 
                    ? DEFAULT_MMU_BUFFER_SIZE 
                    : info->mmu_buf_size );
    data->mmu_log = 
      create_gc_mmu_log( mmu_window_lengths, 
                         mmu_buf_size, 
                         gc_log_phase_misc_memmgr );
  } else {
    data->mmu_log = NULL;
  }
#endif

  data->rrof_prefer_big_summ = info->rrof_prefer_big_summ;
  data->rrof_prefer_lil_summ = info->rrof_prefer_lil_summ;
  data->rrof_prefer_lat_summ = info->rrof_prefer_lat_summ;

  data->rrof_smircy_step_on_minor_collections_alone = TRUE;
  data->rrof_mark_cycles_begun_in_this_full_cycle = 0;
  data->rrof_mark_cycles_run_in_this_full_cycle = 0;

  ret = 
    create_gc_t( "*invalid*",
                 (void*)data,
                 initialize, 
                 allocate,
                 allocate_nonmoving,
                 make_room,
                 my_collect,
                 my_incremental,
                 set_policy,
                 data_load_area,
                 text_load_area,
                 iflush,
                 creg_get,
                 creg_set,
                 stack_overflow,
                 stack_underflow,
                 compact_all_ssbs,
#if defined(SIMULATE_NEW_BARRIER)
                 isremembered,
#endif
                 0,
                 np_remset_ptrs,
                 0,                /* load_heap */
                 dump_image,
                 make_handle,
                 free_handle,
                 gno_state, 
                 enumerate_roots,
                 enumerate_smircy_roots,
                 enumerate_remsets_complement,
                 enumerate_remembered_locations, 
                 enumerate_hdr_address_ranges, 
                 fresh_space,
                 my_find_space,
                 allocated_to_areas,
                 maximum_allotted,
                 is_nonmoving, 
                 is_address_mapped,
                 my_check_remset_invs,
                 points_across_callback,
                 heap_for_gno, 
                 region_group_for_gno,
                 check_invariants_between_fwd_and_free
                 );
  ret->scan_update_remset = info->is_regional_system;

  zeroed_promotion_counts( ret );

  return ret;
}

/* eof */
