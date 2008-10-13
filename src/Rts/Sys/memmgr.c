/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny  -- precise garbage collector, top level.
 */

const char *larceny_gc_technology = "precise";

#define GC_INTERNAL

#include <string.h>
#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "stats.h"
#include "memmgr.h"
#include "young_heap_t.h"
#include "old_heap_t.h"
#include "static_heap_t.h"
#include "remset_t.h"
#include "los_t.h"
#include "semispace_t.h"
#include "gset_t.h"
#include "gclib.h"
#include "heapio.h"
#include "barrier.h"
#include "stack.h"
#include "msgc-core.h"
#include "smircy.h"
#include "summary_t.h"
#include "seqbuf_t.h"
#include "math.h"

/* Checking code */
#define CHECK_HEAP_INVARIANTS 0

typedef struct gc_data gc_data_t;

static struct {
  remset_t **elem;
  int len;
} remset_pool = { NULL, 0 };
static remset_t* grab_from_remset_pool() 
{
  annoyingmsg("            grab_from_remset_pool");
  { int i; 
    for (i = 0; i < remset_pool.len; i++) {
      if (remset_pool.elem[i] != NULL) {
        remset_t *rtn;
        rtn = remset_pool.elem[i];
        remset_pool.elem[i] = NULL;
        assert2(rtn->live == 0);
        return rtn;
      }
    }
  }
  /* if we get here, then all remsets are in use and we need to expand
   * the pool. */
  {
    int newlen = remset_pool.len + 1;
    remset_t **elem = (remset_t**)must_malloc(newlen*sizeof(remset_t*));
    free(remset_pool.elem);
    remset_pool.elem = elem;
    remset_pool.len = newlen;
    /* we do not store the new remset in the pool yet; that will
     * happen when it is freed. */
    memset(elem, 0, newlen*sizeof(remset_t*));
    return create_remset( 0, 0 );
  }
}
static void return_to_remset_pool( remset_t *rs ) 
{
  int i;
  annoyingmsg("            return_to_remset_pool");
  assert2(rs->live == 0);
  for (i = 0; i < remset_pool.len; i++) {
    if (remset_pool.elem[i] == NULL) {
      remset_pool.elem[i] = rs;
      return;
    }
  }
  consolemsg("no NULL entries in a pool of length %d", remset_pool.len);
  assert(0); /* (should never get here) */
}

/* The 'remset' table in the gc structure has one extra element if the
   collector uses the non-predictive dynamic area: that extra element is
   the young->old remembered set for the non-predictive collector.  The
   extra element is always the last one, and the slots in the SSB tables
   for that remset are the last slots.  The index is indicated by the
   np_remset member of gc_t.
   */
struct remset_as_summary { 
  remset_t *sum_remset;
  int       gen;
  bool      valid;
  int       words;
  int       max_words;
};
typedef struct remset_as_summary remset_as_summary_t;

struct gc_data {
  bool is_partitioned_system;   /* True if system has partitioned heap */
  bool use_np_collector;	/* True if dynamic area is non-predictive */
  bool shrink_heap;		/* True if heap can be shrunk */
  bool fixed_ephemeral_area;    /* True iff ephemeral_area_count is invariant */

  int  dynamic_min;		/* 0 or lower limit of expandable area */
  int  dynamic_max;		/* 0 or upper limit of expandable area */
  int  nonexpandable_size;	/* Size of nonexpandable areas (but RROF?) */

  word *globals;
  word *handles;               /* array of handles */
  int  nhandles;               /* current array length */
  int  in_gc;                  /* a counter: > 0 means in gc */
  int  generations;            /* number of generations (incl. static) */
  int  generations_after_gc;   /* number of generations in rts after gc complete */
  int  static_generation;	/* Generation number of static area */
  word **ssb_bot;
  word **ssb_top;
  word **ssb_lim;

  old_heap_t **ephemeral_area;
    /* In precise collectors: An array of pointers to ephemeral areas;
       the number of entries is held in ephemeral_area_count.  May be NULL.
       */
  int ephemeral_area_count;
    /* The number of entries in the ephemeral_area table.
       */
  int region_count;
    /* For regional collector.  During cycle of collections, the regions
        { ephemeral_area[i] | region_count <= i < ephemeral_area_count }
       are new and should not be processed until cycle completes
       */
  old_heap_t *dynamic_area;
    /* In precise collectors: A pointer to a dynamic area, or NULL.
       */

  int rrof_to_region;
    /* In RROF collector, the to-space for minor collections. */
  int rrof_next_region;
    /* In RROF collector, the next region scheduled for major collection. */
  int rrof_last_tospace;
    /* In RROF collector, the region used as a to-space in the last collect */
  double rrof_load_factor;
    /* Lars put a load factor in each old-heap; RROF needs one load_factor */

  double rrof_sumz_budget;
    /* In RROF collector, B*N/R (where B = budget) is number of
       summaries (and thus major collections) that we have available
       before starting a wave of summary construction to support the
       next major collection cycle. */
  double rrof_sumz_coverage;
    /* In RROF collector, C*N/R (where C = coverage) is initial number
       of summaries that we will try to construct during each heap
       scan during a wave of summary construction. */

  bool   rrof_has_refine_factor; /* With factor R,                         */
  double rrof_refinement_factor; /*   countdown = ceil((R*heap) / nursery) */
  int rrof_refine_mark_period;
  int rrof_refine_mark_countdown;
    /* In RROF collector, #nursery evacuations until refine remset via mark.
       If negative, then that is number of nursery evacuations we've had
       since the mark was scheduled to occur. */

  int rrof_last_live_estimate; 
    /* In RROF collector, gradual approximation of live storage in bytes.
     * (It is continually reset based on marking results and then 
     *  refined by repeated averaging with the sum of major collection
     *  sizes.)
     */

  summary_t summary;            /* NULL or summarization of remset array */
  bool      use_summary_instead_of_remsets;
  remset_as_summary_t **remset_summaries; /* points-into summaries */
  int       remset_summaries_count;
  bool      summarized_genset_valid;
  gset_t    summarized_genset;
  int       next_summary_to_use;
  int       popularity_limit;   /* Maximum summary size allowed (in words) */

  remset_t *nursery_remset;     /* Points-into remset for the nursery. */

  semispace_t *secondary_space; /* NULL or space for when tospace overflows */

  int stat_last_ms_remset_sumrize;
  int stat_last_ms_remset_sumrize_cpu;
  int stat_last_ms_mark_refinement;
  int stat_last_ms_mark_refinement_cpu;
  int stat_length_minor_gc_run;

  bool print_float_stats_each_cycle;
  bool print_float_stats_each_major;
  bool print_float_stats_each_minor;
  bool print_float_stats_each_refine;

  int last_live_words;
  int max_live_words;
};

#define DATA(gc) ((gc_data_t*)(gc->data))

static gc_t *alloc_gc_structure( word *globals, gc_param_t *info );
static word *load_text_or_data( gc_t *gc, int size_bytes, int load_text );
static int allocate_generational_system( gc_t *gc, gc_param_t *params );
static int allocate_stopcopy_system( gc_t *gc, gc_param_t *info );
static int allocate_regional_system( gc_t *gc, gc_param_t *info );
static void before_collection( gc_t *gc );
static void after_collection( gc_t *gc );
static void stats_following_gc( gc_t *gc );
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
  int est_live_next_gc = live;	/* == M/L */

  if (lower_limit && upper_limit == lower_limit) {
    /* Fixed heap size */
    return roundup_page( upper_limit - (int)(upper_limit/L) );
  }

  if (lower_limit) {
    /* Must adjust estimate of live at next GC to avoid inflating
       the allocation budget beyond reason, if not much is live at
       present.
       */
    M = max( M, lower_limit );
    est_live_next_gc = max( M/L, est_live_next_gc );
  }
  
  if (!DATA(gc)->shrink_heap) {
    gclib_stats_t stats;

    gclib_stats( &stats );
    M = max( M, stats.heap_allocated_max ); /* use no less than before */
  }

  if (upper_limit > 0) {
    int newM = min( M, upper_limit );
    int avail = (newM - live - est_live_next_gc);
    if (avail < 0)
      M = newM + roundup_page( abs( avail ) );	/* _Minimal_ amount */
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
    else if (op-1 == DATA(gc)->ephemeral_area_count && DATA(gc)->dynamic_area) {
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
	      (data->use_np_collector ? data->generations-1 : -1 ),
	      gc->np_remset
	     );
  }
  else
    wb_disable_barrier( data->globals );

  annoyingmsg( "\nGC type: %s", gc->id );

  return 1;
}
  
static word *allocate( gc_t *gc, int nbytes, bool no_gc, bool atomic )
{
  assert( nbytes > 0 );

  nbytes = roundup_balign( nbytes );
  if (nbytes > LARGEST_OBJECT)
    panic_exit( "Can't allocate an object of size %d bytes: max is %d bytes.",
	        nbytes, LARGEST_OBJECT );

  return yh_allocate( gc->young_area, nbytes, no_gc );
}

static word *allocate_nonmoving( gc_t *gc, int nbytes, bool atomic )
{
  assert( nbytes > 0 );

  if (gc->static_area == 0)
    panic_exit( "Cannot allocate nonmoving in a system without a static heap." );

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
    after_collection( gc );
    stats_following_gc( gc );

    gclib_stats( &stats );
    annoyingmsg( "  Memory usage: heap %d, remset %d, RTS %d words",
		 stats.heap_allocated, stats.remset_allocated, 
		 stats.rts_allocated );
    annoyingmsg( "  Max heap usage: %d words", stats.heap_allocated_max );
  }
}

typedef struct remset_summary_data remset_summary_data_t;
struct remset_summary_data {
  /* _current_ representation: summaries[g] is non-null ==> (g in genset && summaries[g] non-empty). */
  gset_t genset;
  remset_as_summary_t **remset_summaries;
  int objects_visited;
  int objects_added;
  int words_added;
};
static void add_object_to_sum_rs( remset_as_summary_t *rs_sum, 
                                  int gen, 
                                  word ptr ) 
{
  if (rs_sum->words <= rs_sum->max_words) {
    if (rs_sum->sum_remset == NULL) {
      rs_sum->sum_remset = grab_from_remset_pool();
    }
    if (!rs_isremembered( rs_sum->sum_remset, ptr )) {
      if (tagof(ptr) == PAIR_TAG) {
        rs_sum->words += 2;
      } else {
        rs_sum->words += sizefield( *ptrof(ptr) ) / 4;
      }

      if (rs_sum->words > rs_sum->max_words) {
        annoyingmsg("summary for rgn %d overflowed on 0x%08x (%d): %d max %d",
                    gen, ptr, gen_of(ptr), rs_sum->words, rs_sum->max_words);
        rs_clear( rs_sum->sum_remset );
        return_to_remset_pool( rs_sum->sum_remset );
        rs_sum->sum_remset = NULL;
        rs_sum->valid = FALSE;
      } else {
        rs_add_elem( rs_sum->sum_remset, ptr );
      }
    }
  }
}
static void add_object_to_summary( remset_summary_data_t *remsum, int gen, word ptr ) 
{
  assert2( gen_of(ptr) != gen );
  add_object_to_sum_rs( remsum->remset_summaries[ gen ], gen, ptr );
}
static bool scan_object_for_remset_summary( word ptr, void *data, unsigned *count )
{
  word *loc = ptrof(ptr);
  word scanned = 0;
  bool do_enqueue = FALSE;
  remset_summary_data_t *remsum = (remset_summary_data_t*)data;
  gset_t genset = remsum->genset;
  int mygen = gen_of(ptr); 
  /* XXX fixme: the way remset's are scanned, we should not need to reextract this */

  static const bool instrumented = FALSE;

  if (instrumented) {
    annoyingmsg("scan_object_for_remset_summary( 0x%08x (%d), data, count )",
                ptr, gen_of(ptr) );
    switch (genset.tag) {
    case gs_nil: assert(0);
    case gs_singleton: 
      annoyingmsg("  for pointers into { %d }", genset.g1 );
      break;
    case gs_range: 
      annoyingmsg("  for pointers into [%d,%d)", genset.g1, genset.g2 );
      break;
    }
  }
  if (tagof( ptr ) == PAIR_TAG) {
    /* handle car */
    if (isptr(*loc)) {
      int gen = gen_of(*loc);
      if (instrumented) 
        annoyingmsg("scan_object_for_remset_summary "
                    "pair car: 0x%08d (%d)", *loc, gen);
      if (mygen != gen && gset_memberp(gen,genset)) {
        do_enqueue = TRUE;
        add_object_to_summary( remsum, gen, ptr );
      }
    }
    ++loc;
    /* handle cdr */
    if (isptr(*loc)) {
      int gen = gen_of(*loc);
      if (instrumented) 
        annoyingmsg("scan_object_for_remset_summary "
                    "pair cdr: 0x%08d (%d)", *loc, gen);
      if (mygen != gen && gset_memberp(gen,genset)) {
        do_enqueue = TRUE;
        add_object_to_summary( remsum, gen, ptr );
      }
    }
    scanned = 2;
  } else { /* vector or procedure */
    assert( (tagof(ptr) == VEC_TAG) || (tagof(ptr) == PROC_TAG) );
    word words = sizefield( *loc ) / 4;
    scanned = words;
    while (words--) {
      ++loc;
      if (isptr(*loc)) {
        int gen = gen_of(*loc);
        if (instrumented) 
          annoyingmsg("scan_object_for_remset_summary "
                      "vecproc : 0x%08d (%d)", *loc, gen);
        if (mygen != gen && gset_memberp(gen,genset)) {
          do_enqueue = TRUE;
          add_object_to_summary( remsum, gen, ptr );
        }
      }
    }
  }
  
  remsum->objects_visited += 1;
  if (do_enqueue) {
    remsum->objects_added += 1;
    remsum->words_added += scanned;
  }

  *count += scanned;
  return TRUE; /* don't remove entries from the remembered set we are summarizing! */  
}

static void build_remset_summaries( gc_t *gc, gset_t genset )
{
  remset_summary_data_t remsum;
  int i;
  int remset_count = gc->remset_count;
  int summ_len = gset_max_elem(genset); /* (some entries can be null) */

  /* XXX potentially assert DATA(gc)->summarized_genset is nullset */

  remsum.genset = genset;
  remsum.remset_summaries = DATA(gc)->remset_summaries;
  remsum.objects_visited = 0;
  remsum.objects_added = 0;
  remsum.words_added = 0;

  /* Optimistically assume that summarization will succeed for all
   * elems of genset; if one of them overflows, it will be
   * responsibility of scan_object_for_remset_summary to set valid
   * field to FALSE.
   */
  for( i=0 ; i < DATA(gc)->ephemeral_area_count; i++ ) {
    if (gset_memberp( i, genset )) {
      DATA(gc)->remset_summaries[i]->valid = TRUE;
      DATA(gc)->remset_summaries[i]->words = 0;
      /* Construction assumes that summaries start off empty. */
      assert2( remsum.remset_summaries[ i ]->sum_remset == NULL ||
               remsum.remset_summaries[ i ]->sum_remset->live == 0);
    }
  }

  for(i = 1; i < remset_count; i++) {
    /* TODO: use rs_enumerate_partial here? XXX */
    rs_enumerate( gc->remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
    rs_enumerate( gc->major_remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
  }
  if (genset.tag == gs_singleton) {
    remset_t *rs = remsum.remset_summaries[genset.g1]->sum_remset;
    assert( genset.g1 < DATA(gc)->remset_summaries_count );
  } else if (genset.tag == gs_range ) {
    remset_t *rs1, *rs2;
    assert( genset.g2 <= DATA(gc)->remset_summaries_count );
    rs1 = remsum.remset_summaries[genset.g1]->sum_remset;
    rs2 = remsum.remset_summaries[genset.g2-1]->sum_remset;
  } else { assert(0); }

  { /* XXX review me XXX */
    DATA(gc)->summarized_genset = genset;
    DATA(gc)->summarized_genset_valid = TRUE;
  }
}

static int next_rgn( int rgn, int num_rgns ) {
  rgn++;
  if (rgn > num_rgns) 
    rgn = 1;
  return rgn;
}

/* The number represents how many cycles per expansion. (first guess is 1) */
#define EXPAND_RGNS_FROM_LOAD_FACTOR 1
#define INCLUDE_POP_RGNS_IN_LOADCALC 1
#define WEIGH_PREV_ESTIMATE_LOADCALC 0
#define USE_ORACLE_TO_VERIFY_REMSETS 0
#define NO_COPY_COLLECT_FOR_POP_RGNS 1
#define POP_RGNS_LIVE_FOREVER 0
#define USE_ORACLE_TO_VERIFY_SUMMARIES 0

static const double default_popularity_factor = 2.0;
static const double default_sumz_budget_factor = 0.1;
static const double default_sumz_coverage_factor = 0.1;

static bool msvfy_object_marked_p( msgc_context_t *c, word x ) {
  return msgc_object_marked_p( c, x );
}
static void msvfy_set_object_visitor( msgc_context_t *c, 
                                      void* (*visitor)( word obj, 
                                                        word src,
                                                        void *data ), 
                                      void *data ) {
  msgc_set_object_visitor( c, visitor, data );
}
static void msvfy_mark_objects_from_roots( msgc_context_t *c ) {
  int marked, traced, words_marked;
  msgc_mark_objects_from_roots( c, &marked, &traced, &words_marked );
}
static void msvfy_mark_objects_from_roots_and_remsets( msgc_context_t *c ) {
  int m, t, wm;
  msgc_mark_objects_from_roots_and_remsets( c, &m, &t, &wm );
}

static bool msfloat_object_marked_p( msgc_context_t *c, word x ) {
  return msgc_object_marked_p( c, x );
}
static void msfloat_mark_objects_from_roots( msgc_context_t *c ) {
  int marked, traced, words_marked;
  msgc_mark_objects_from_roots( c, &marked, &traced, &words_marked );
}
static void msfloat_mark_objects_from_roots_and_remsets( msgc_context_t *c ) {
  int m, t, wm;
  msgc_mark_objects_from_roots_and_remsets( c, &m, &t, &wm );
}

static void* verify_remsets_msgc_fcn( word obj, word src, void *data ) 
{
  gc_t *gc = (gc_t*)data;
  if (isptr(src) && isptr(obj) &&
      gen_of(src) != gen_of(obj) &&
      ! gc_is_nonmoving( gc, gen_of(obj) )) {
    assert( gen_of(src) >= 0 );
    if (gen_of(src) > 0) {
      assert( *gc->ssb[gen_of(src)]->bot == *gc->ssb[gen_of(src)]->top );
      assert( *gc->ssb[gen_of(obj)]->bot == *gc->ssb[gen_of(obj)]->top );
      if (gen_of(obj) == 0) {
        assert( rs_isremembered( DATA(gc)->nursery_remset, src ));
      }
      if (!rs_isremembered( gc->remset[ gen_of(src) ], src ) &&
	  !rs_isremembered( gc->major_remset[ gen_of(src) ], src )) {
	consolemsg( " src: 0x%08x (%d) points to obj: 0x%08x (%d),"
		    " but not in remsets @0x%08x @0x%08x",
		    src, gen_of(src), obj, gen_of(obj), 
		    gc->remset[ gen_of(src) ],
		    gc->major_remset[ gen_of(src) ]);
	assert( gc_is_address_mapped( gc, ptrof(src), TRUE ));
	assert( gc_is_address_mapped( gc, ptrof(obj), TRUE ));
	assert(0);
      }
    }
  }
  return data;
}

struct verify_remsets_traverse_rs_data {
  msgc_context_t *conserv_context;
  gc_t *gc;
  int region;
  bool major;
  bool pointsinto;
};
/* verify that (X in remset R implies X in reachable(roots+remsets));
 * (may be silly to check, except when R = nursery_remset...) */
static bool verify_remsets_traverse_rs( word obj, void *d, unsigned *stats )
{
  struct verify_remsets_traverse_rs_data *data;
  data = (struct verify_remsets_traverse_rs_data*)d;
  assert( msvfy_object_marked_p( data->conserv_context, obj ));
  return TRUE;
}
/* verify that (X in R implies X in minor_remset for X);
 * another invariant for R = nursery_remset. */
static bool verify_nursery_traverse_rs( word obj, void *d, unsigned *stats )
{
  struct verify_remsets_traverse_rs_data *data;
  data = (struct verify_remsets_traverse_rs_data*)d;
  assert( rs_isremembered( data->gc->remset[ gen_of(obj) ], obj ));
  return TRUE;
}

static void verify_remsets_via_oracle( gc_t *gc ) 
{
  msgc_context_t *context;
  int marked, traced, words_marked; 
  struct verify_remsets_traverse_rs_data data;
  context = msgc_begin( gc );
  msvfy_set_object_visitor( context, verify_remsets_msgc_fcn, gc );
  msvfy_mark_objects_from_roots( context );
  msgc_end( context );
  context = msgc_begin( gc );
  msvfy_mark_objects_from_roots_and_remsets( context );
  data.conserv_context = context;
  data.gc = gc;
  data.region = 0;
  data.major = FALSE;
  data.pointsinto = TRUE;
  rs_enumerate( DATA(gc)->nursery_remset, verify_nursery_traverse_rs, &data );
  rs_enumerate( DATA(gc)->nursery_remset, verify_remsets_traverse_rs, &data );
  /* Originally had code to verify_remsets_traverse_rs on all remsets,
   * but that does not seem like an interesting invariant to check. */
  msgc_end( context );
}

static void* verify_summaries_msgc_fcn( word obj, word src, void *data )
{
  gc_t *gc = (gc_t*)data;
  int src_gen, tgt_gen;

  if (isptr(src) && isptr(obj) &&
      ((src_gen = gen_of(src)) != (tgt_gen = gen_of(obj))) &&
      ! gc_is_nonmoving( gc, tgt_gen )) {
    assert( src_gen >= 0 );
    if (src_gen > 0) {
      assert( *gc->ssb[src_gen]->bot == *gc->ssb[src_gen]->top );
      assert( *gc->ssb[tgt_gen]->bot == *gc->ssb[tgt_gen]->top );
      if (DATA(gc)->summarized_genset_valid &&
          gset_memberp( tgt_gen, DATA(gc)->summarized_genset ) &&
          DATA(gc)->remset_summaries[ tgt_gen ]->valid ) {
        assert( (DATA(gc)->remset_summaries[ tgt_gen ]->sum_remset) != NULL );
        assert( rs_isremembered( DATA(gc)->remset_summaries[ tgt_gen ]->sum_remset, src ));
      }
    }
  }
  return data;
}

struct verify_summaries_remset_fcn_data {
  msgc_context_t *conserv_context;
  msgc_context_t *aggress_context;
  int summary_for_region; 
};

static bool verify_summaries_remset_fcn( word obj, 
					 void *the_data, unsigned *stats )
{
  struct verify_summaries_remset_fcn_data *data;
  data = (struct verify_summaries_remset_fcn_data*)the_data;
  /* Any object in a summary should be reachable from the 
   * union of roots+remsets */
  annoyingmsg( "VERIFY SUMM REMS 0x%08x (%d) marked {agg: %s, con: %s}", 
	       obj, gen_of(obj), 
	       msvfy_object_marked_p( data->aggress_context, obj )?"Y":"N", 
	       msvfy_object_marked_p( data->conserv_context, obj )?"Y":"N" );
  assert( msvfy_object_marked_p( data->conserv_context, obj ));
  assert( gen_of(obj) != data->summary_for_region );
  return TRUE;
}

static void verify_summaries_via_oracle( gc_t *gc ) 
{
  msgc_context_t *conserv_context;
  msgc_context_t *aggress_context;
  int marked, traced, words_marked;
  if (DATA(gc)->summarized_genset_valid) {
    conserv_context = msgc_begin( gc );
    msvfy_set_object_visitor( conserv_context, verify_summaries_msgc_fcn, gc );
    /* (useful to have a pre-pass over reachable(roots) so that the
       stack trace tells you whether a problem is due solely to a
       reference chain that somehow involves remembered sets.) */
    msvfy_mark_objects_from_roots( conserv_context );
    /* Summaries are based on (conservative) info in remsets; 
       therefore they may have references to "dead" objects 
       that would not be identified as such if we used 
       only msgc_mark_objects_from_roots
    */
    assert(! DATA(gc)->use_summary_instead_of_remsets );
    msvfy_mark_objects_from_roots_and_remsets( conserv_context );

    /* a postpass over the summaries to make sure that their contents
       are sane.
    */
    {
      struct verify_summaries_remset_fcn_data data;
      int i;
      msgc_context_t *aggress_context;
      aggress_context = msgc_begin( gc );
      msvfy_mark_objects_from_roots( aggress_context );
      data.conserv_context = conserv_context;
      data.aggress_context = aggress_context;
      for (i = 0; i < gc->remset_count; i++) {
	if (gset_memberp( i, DATA(gc)->summarized_genset )){
	  assert( DATA(gc)->remset_summaries[i] != NULL);

	  assert( i < DATA(gc)->remset_summaries_count );
	  /* we do not grab a remset_t if no entries are added, 
	     so this is a guard rather than an assertion. */
	  if ( DATA(gc)->remset_summaries[i]->sum_remset != NULL) {
	    data.summary_for_region = i;
	    rs_enumerate( DATA(gc)->remset_summaries[ i ]->sum_remset, 
	                  verify_summaries_remset_fcn, 
	                  &data );
	  }
	}
      }
      msgc_end( aggress_context );
    }
    msgc_end( conserv_context );
  }
}

struct float_counts {
  int zzflt; /* float according to remsets and globals */
  int rsflt; /* float according to globals; live according to remsets */
  int total; /* total occupancy count */
};

struct visit_measuring_float_data {
  msgc_context_t *context;
  msgc_context_t *context_incl_remsets;
  struct float_counts words;
  struct float_counts objs;
};

void zero_float_counts( struct float_counts *counts ) 
{
  counts->zzflt = 0;
  counts->rsflt = 0;
  counts->total = 0;
}

void zero_measuring_float_data( struct visit_measuring_float_data *data ) 
{
  zero_float_counts( &data->words );
  zero_float_counts( &data->objs );
}

static void* visit_measuring_float( word *addr, int tag, void *accum ) 
{
  struct visit_measuring_float_data *data = 
    (struct visit_measuring_float_data*)accum;
  word obj; 
  bool marked;
  bool marked_via_remsets;
  int words;
  struct float_counts *type_counts;
  obj = tagptr( addr, tag );
  marked = 
    msfloat_object_marked_p( data->context, obj );
  marked_via_remsets = 
    msfloat_object_marked_p( data->context_incl_remsets, obj );

  data->objs.total += 1 ;
  if (!marked && !marked_via_remsets) {
    data->objs.zzflt += 1;
  }
  if (!marked && marked_via_remsets) {
    data->objs.rsflt += 1;
  }

  switch (tag) {
  case PAIR_TAG:
    words = 2; 
    break;
  case VEC_TAG:
  case BVEC_TAG:
  case PROC_TAG:
    words = roundup8( sizefield( *addr )+4 ) / 4;
    break;
  default:
    assert(0);
  }
  data->words.total += words;
  if (!marked && !marked_via_remsets)
    data->words.zzflt += words;
  if (!marked && marked_via_remsets)
    data->words.rsflt += words;
  return data;
}

static bool scan_refine_remset( word loc, void *data, unsigned *stats )
{
  smircy_context_t *context = (smircy_context_t*)data;
  if (smircy_object_marked_p( context, loc )) {
    return TRUE;
  } else {
    return FALSE;
  }
}

static void refine_remsets_via_marksweep( gc_t *gc ) 
{
  /* use the mark/sweep system to refine (*all* of) the
   * remembered sets. */
  smircy_context_t *context;
  int i, rgn;
  int marked=0, traced=0, words_marked=0; 
  int total_float_words = 0, total_float_objects = 0;
  context = smircy_begin( gc, gc->remset_count );
  smircy_push_roots( context );
  smircy_push_remset( context, DATA(gc)->nursery_remset );
  smircy_progress( context, -1, -1, -1, &marked, &traced, &words_marked );
  
  /* static objects die; remset_count includes static remset (thus
   * refinement eliminates corpses with dangling pointers). */
  for( i=1; i < gc->remset_count; i++) {
    rs_enumerate( gc->remset[ i ], scan_refine_remset, context );
    rs_enumerate( gc->major_remset[ i ], scan_refine_remset, context );
  }

  /* XXX refining the summaries as well as the remsets based on the
     marksweep info.  This may or may not be necessary in an improved
     version of the refinement code.
  */
  if (DATA(gc)->summarized_genset_valid) {
    int i;
    for (i = 0; i < gc->remset_count; i++) {
      if (gset_memberp( i, DATA(gc)->summarized_genset)) {
        assert( DATA(gc)->remset_summaries[i] != NULL);
        if (DATA(gc)->remset_summaries[i]->sum_remset != NULL) {
          rs_enumerate( DATA(gc)->remset_summaries[i]->sum_remset,
                        scan_refine_remset, 
                        context );
        }
      }
    }
  }

  if (DATA(gc)->rrof_refine_mark_period > 0) {
    DATA(gc)->rrof_refine_mark_countdown = 
      DATA(gc)->rrof_refine_mark_period;
  } else if (DATA(gc)->rrof_has_refine_factor) {
    double R = DATA(gc)->rrof_refinement_factor;
    int new_countdown;
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
      max( DATA(gc)->max_live_words, words_marked );
    if (0) consolemsg("revised mark countdown: %d", new_countdown );
  } else {
    assert(0);
  }
  
  smircy_end( context );
}

static int cycle_count = 0;
#define BAR_LENGTH 20
static int fill_up_to( char *bar, char mark, char altmark, int amt, int max ) {
  int i;
  int count;
  int rtn = max;
  if (max == 0) 
    return rtn;
  else
    count = (int)((amt*BAR_LENGTH)/max);
  assert(count >= 0);
  if (count > BAR_LENGTH) {
    rtn = amt;
    count = BAR_LENGTH;
    mark = altmark;
  }
  assert(count <= BAR_LENGTH);
  for(i = 0; i < count; i++) {
    bar[i] = mark;
  }
  return rtn;
}

static void print_float_stats_for_rgn( char *caller_name, gc_t *gc, int i, 
                                       struct visit_measuring_float_data data )
{
  int rgn;
  int newmax;
  int data_count, easy_float, hard_float;
  { 
    char bars[BAR_LENGTH+2];
    bars[BAR_LENGTH] = '\0';
    bars[BAR_LENGTH+1] = '\0';
    {
      data_count = data.words.total*4;
      easy_float = data.words.zzflt*4+data.words.rsflt*4;
      hard_float = data.words.rsflt*4;
      newmax = DATA(gc)->ephemeral_area[i]->maximum;
      newmax = fill_up_to( bars, ' ', '@', newmax, newmax );
      newmax = fill_up_to( bars, '.', '!', data_count, newmax );
      newmax = fill_up_to( bars, 'Z', 'z', easy_float, newmax );
      newmax = fill_up_to( bars, 'R', 'r', hard_float, newmax );
    }

    { 
      bool rgn_summarized;
      int rgn_summarized_live;
      old_heap_t *heap = DATA(gc)->ephemeral_area[ i ];
      rgn = i+1;
      rgn_summarized = 
        DATA(gc)->summarized_genset_valid && 
        gset_memberp( rgn, DATA(gc)->summarized_genset );
      if (rgn_summarized) {
        if (DATA(gc)->remset_summaries[ rgn ]->sum_remset == NULL ) {
          rgn_summarized_live = 0;
        } else {
          rgn_summarized_live = 
            DATA(gc)->remset_summaries[ rgn ]->words;
        }
      } else {
        rgn_summarized_live = -DATA(gc)->remset_summaries[ rgn ]->words - 1;
      }
      oh_synchronize( heap );
      consolemsg( "%scycle %d region% 4d "
                  "remset live: %7d %7d %8d lastmajor: %7d "
                  "float{ objs: %7d/%7d words: %7d/%7d %7d }%s %s %s", 
                  caller_name,
                  cycle_count, 
                  rgn, 
                  gc->remset[ rgn ]->live, gc->major_remset[ rgn ]->live, 
                  rgn_summarized_live, 
                  heap->live_last_major_gc/4, 
                  data.objs.zzflt+data.objs.rsflt,
                  data.objs.total,
                  data.words.zzflt+data.words.rsflt,
                  data.words.total, 
                  heap->allocated/4, 
                  (( rgn == DATA(gc)->rrof_to_region &&
                     rgn == DATA(gc)->rrof_next_region ) ? "*" :
                   ( rgn == DATA(gc)->rrof_to_region )   ? "t" :
                   ( rgn == DATA(gc)->rrof_next_region ) ? "n" :
                   ( rgn_summarized )                    ? "s" :
                   ( rgn >= DATA(gc)->region_count     ) ? "e" : 
                   /* else                              */ " "),
                  bars,
                  (DATA(gc)->ephemeral_area[ i ]->
                   has_popular_objects ? "(popular)" : "")
                  );
    }
  }
}
static void print_float_stats( char *caller_name, gc_t *gc ) 
{
  /* every collection cycle, lets use the mark/sweep system to 
   * measure how much float has accumulated. */
  {
    msgc_context_t *context;
    msgc_context_t *context_incl_remsets;
    int i, rgn;
    int marked=0, traced=0, words_marked=0; 
    int marked_incl=0, traced_incl=0, words_marked_incl=0; 
    int total_float_words = 0, total_float_objects = 0;
    int estimated_live = 0;
    struct visit_measuring_float_data data;
    context = msgc_begin( gc );
    msfloat_mark_objects_from_roots( context );

    context_incl_remsets = msgc_begin( gc );
    msfloat_mark_objects_from_roots_and_remsets( context_incl_remsets );

    for( i=0; i < DATA(gc)->ephemeral_area_count; i++) {
      data.context = context;
      data.context_incl_remsets = context_incl_remsets;
      zero_measuring_float_data( &data );
      DATA(gc)->ephemeral_area[ i ]->enumerate
        ( DATA(gc)->ephemeral_area[ i ], visit_measuring_float, &data );
      print_float_stats_for_rgn( caller_name, gc, i, data );
      total_float_objects += data.objs.zzflt+data.objs.rsflt;
      total_float_words += data.words.zzflt+data.objs.rsflt;
      if (INCLUDE_POP_RGNS_IN_LOADCALC || 
          ! DATA(gc)->ephemeral_area[i]->has_popular_objects)
        estimated_live += DATA(gc)->ephemeral_area[ i ]->live_last_major_gc/sizeof(word);
    }
    consolemsg( "cycle %d total float { objs: %dk words: %dK (%3d%%,%3d%%) } nextrefine: %d "
                "live{ est: %dK act: %dK max: %dK } estdelta: %0.2f ",
                cycle_count, 
                total_float_objects/1000, 
                total_float_words/1024, 
                (int)(100.0*(double)total_float_words/(double)words_marked), 
                DATA(gc)->max_live_words?(int)(100.0*(double)total_float_words/(double)DATA(gc)->max_live_words):0, 
                DATA(gc)->rrof_refine_mark_countdown, 
                estimated_live/1024, words_marked/1024, DATA(gc)->max_live_words/1024, 
                estimated_live?(((double)estimated_live)/(double)words_marked):0.0 );

    msgc_end( context_incl_remsets );
    msgc_end( context );
  }
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

static void rrof_completed_regional_cycle( gc_t *gc ) 
{
  cycle_count += 1;

  if (DATA(gc)->print_float_stats_each_cycle)
    print_float_stats( "cycle ", gc );

#if EXPAND_RGNS_FROM_LOAD_FACTOR
  /* every K collection cycles, lets check and see if we should expand
   * the number of regions so that we can satisfy the inverse load
   * factor. */
  if ((cycle_count % EXPAND_RGNS_FROM_LOAD_FACTOR) == 0) {
    int i;
    int total_live_at_last_major_gc = 0;
    int maximum_allotted = 0;
    int live_estimated_calc = 0;
    int live_predicted_at_next_gc;
    int nursery_size = gc->young_area->maximum;
    /* Below does not account for role of reserve region at array end;
     * arguably should distinquish "allotted" and "expected size." */
    for( i=0; i < DATA(gc)->ephemeral_area_count; i++) {
      if (INCLUDE_POP_RGNS_IN_LOADCALC || 
	  ! DATA(gc)->ephemeral_area[ i ]->has_popular_objects) {
	total_live_at_last_major_gc += 
	  max(0, (DATA(gc)->ephemeral_area[ i ]->live_last_major_gc 
	          - nursery_size));
	maximum_allotted += 
	  DATA(gc)->ephemeral_area[ i ]->maximum;
      }
    }
    live_estimated_calc = 
      ( DATA(gc)->rrof_last_live_estimate*WEIGH_PREV_ESTIMATE_LOADCALC 
        + total_live_at_last_major_gc)
      / (WEIGH_PREV_ESTIMATE_LOADCALC + 1);
    live_predicted_at_next_gc = 
      (int)(DATA(gc)->rrof_load_factor * live_estimated_calc);
    DATA(gc)->rrof_last_live_estimate = live_estimated_calc;

    annoyingmsg( "completed_regional_cycle total: %d max: %d marked: %d est: %d predict: %d",
		 total_live_at_last_major_gc, 
		 maximum_allotted, 
		 DATA(gc)->last_live_words*sizeof(word),
		 live_estimated_calc, 
		 live_predicted_at_next_gc );

    if (live_predicted_at_next_gc > maximum_allotted) {
      semispace_t *ss = gc_fresh_space(gc);
      int eidx = ss->gen_no - 1;
      old_heap_t *fresh_heap = DATA(gc)->ephemeral_area[ eidx ];
      assert2( fresh_heap->live_last_major_gc == 0 );
      maximum_allotted += fresh_heap->maximum;
    }
    while ( (DATA(gc)->last_live_words*DATA(gc)->rrof_load_factor*sizeof(word)) > maximum_allotted) {
      semispace_t *ss = gc_fresh_space(gc);
      int eidx = ss->gen_no - 1;
      old_heap_t *fresh_heap = DATA(gc)->ephemeral_area[ eidx ];
      assert2( fresh_heap->live_last_major_gc == 0 );
      maximum_allotted += fresh_heap->maximum;
    }

    annoyingmsg( "completed_regional_cycle region_count: %d ephemeral_area_count: %d", 
		 DATA(gc)->region_count, DATA(gc)->ephemeral_area_count );
  }
#endif
  
  /* With an inverse load factor < 2.0, we can get into a situation
   * where all of the candidate regions to be the tospace for a minor
   * collection are also popular.  (In particular, we could have the
   * situation where we have 2 regions, the first is popular, and the
   * second is the emergency space.)
   * 
   * To catch this case, here is a last minute double check that
   * ensures that some region other than the last is non-popular.
   *
   * (Felix is pretty sure this situation cannot occur with an inverse
   * load factor >= 2.0, but has not proven this, so he's just going
   * to do it unconditionally.)
   */
  { 
    int i;
    int area_count = DATA(gc)->ephemeral_area_count;
    bool found_non_popular = FALSE;
    for( i=0; i < area_count-1; i++) {
      if (! DATA(gc)->ephemeral_area[ i ]->has_popular_objects) {
	found_non_popular = TRUE;
	break;
      }
    }
    if (! found_non_popular) {
      assert(! DATA(gc)->ephemeral_area[ area_count-1 ]->has_popular_objects);
      consolemsg("ALERT: All evacuation spaces are popular! "
		 "Allocating fresh region! load: %lf", 
		 DATA(gc)->rrof_load_factor);
      gc_fresh_space(gc);
    }
  }

  DATA(gc)->rrof_next_region = 1;
  DATA(gc)->region_count = DATA(gc)->ephemeral_area_count-1;
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

static void stop_refinem_timers( gc_t *gc, 
				 stats_id_t *timer1, stats_id_t *timer2 )
{
  int ms, ms_cpu;
  ms     = stats_stop_timer( *timer1 );
  ms_cpu = stats_stop_timer( *timer2 );
  
  DATA(gc)->stat_last_ms_mark_refinement     = ms;
  DATA(gc)->stat_last_ms_mark_refinement_cpu = ms_cpu;
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

static bool fold_from_nursery( word ptr, void *data, unsigned *count ) {
  gc_t *gc = (gc_t*)data;
  if (gen_of(ptr) != DATA(gc)->next_summary_to_use) {
    rs_add_elem( DATA(gc)->remset_summaries[ DATA(gc)->next_summary_to_use ]->
		 sum_remset,
                 ptr );
  }
  return TRUE;
}
struct filter_objects_from_sum_remset_data {
  gc_t *gc;
  int gen; /* collected gen */
};
static bool filter_objects_from_sum_remset( word ptr, 
                                            void *the_data, 
                                            unsigned *count )
{
  struct filter_objects_from_sum_remset_data *data;
  data = (struct filter_objects_from_sum_remset_data *)the_data;
  assert(isptr(ptr));
  if (gen_of(ptr) == data->gen) {
    return FALSE;
  } else {
    return TRUE;
  }
}
static void invalidate_summaries( gc_t *gc ) {
  int i;
  for (i=1; i<DATA(gc)->remset_summaries_count; i++) {
    assert2( DATA(gc)->remset_summaries[i]->sum_remset == NULL );
  }
  DATA(gc)->summarized_genset_valid = FALSE;
}
static void collect_rgnl( gc_t *gc, int rgn, int bytes_needed, gc_type_t request )
{
  gclib_stats_t stats;
  gc_data_t *data = DATA(gc);

  { 
    char *type_str;
    switch (request) {
    case GCTYPE_PROMOTE:  type_str = "PROMOTE"; break;
    case GCTYPE_COLLECT:  type_str = "COLLECT"; break;
    case GCTYPE_EVACUATE: type_str = "EVACUATE"; break;
    case GCTYPE_FULL:     type_str = "FULL"; break;
    default: assert(0);
    }
    annoyingmsg("collect_rgnl(gc, %d, %d, %s)", rgn, bytes_needed, type_str );
  }

  assert( rgn >= 0 );
  assert( rgn > 0 || bytes_needed >= 0 );
  assert( data->in_gc >= 0 );

  if (data->in_gc++ >= 0) {
    gc_signal_moving_collection( gc );
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
      /* only forward data out of the nursery, if possible */
      int rgn_to, rgn_next, nursery_sz, rgn_to_cur, rgn_next_cur, rgn_to_max;
      int free_rgn_space, nursery_max; 
      int num_rgns = DATA(gc)->region_count;
      int num_occupied_rgns;

      DATA(gc)->rrof_refine_mark_countdown -= 1;
      
    collect_evacuate_nursery:
      rgn_to = DATA(gc)->rrof_to_region;
      rgn_next = DATA(gc)->rrof_next_region;

      annoyingmsg("collect_rgnl decide major or minor.  to: %d next: %d",
		  rgn_to, rgn_next );

      nursery_sz = gc_allocated_to_areas( gc, gset_singleton( 0 ));
      rgn_to_cur = gc_allocated_to_areas( gc, gset_singleton( rgn_to ));
      rgn_next_cur = gc_allocated_to_areas( gc, gset_singleton( rgn_next ));
      rgn_to_max = gc_maximum_allotted( gc, gset_singleton( rgn_to ));
      { 
        int allot, alloc;
        assert(rgn_next > 0);
        if (rgn_to == rgn_next) {
          allot = gc_maximum_allotted( gc, gset_singleton( rgn_to ));
          alloc = gc_allocated_to_areas( gc, gset_singleton( rgn_to ));
        } else if (rgn_to < rgn_next) {
          /* (to,..., next) free; [1,..., to], [next,..., N/R] occupied */
          allot = gc_maximum_allotted( gc, gset_range( rgn_to, rgn_next ));
          alloc = gc_allocated_to_areas( gc, gset_range( rgn_to, rgn_next ));
        } else {
          /* (next,..., to) occupied; [1,..., next], [to,..., N/R-1] free */
          allot = gc_maximum_allotted( gc, gset_range( 1, rgn_next )) +
            gc_maximum_allotted( gc, gset_range( rgn_to, num_rgns ));
          alloc = gc_allocated_to_areas( gc, gset_range( 1, rgn_next )) +
            gc_allocated_to_areas( gc, gset_range( rgn_to, num_rgns ));
        }
        free_rgn_space = allot - alloc;
        num_occupied_rgns = (num_rgns + rgn_to - rgn_next+1)%num_rgns;
      }
      nursery_max = gc->young_area->maximum;

      annoyingmsg( "collect_rgnl rgn_to: %d rgn_next: %d nursery_sz: %d nursery_max: %d num_occupied_rgns: %d "
		   "rgn_to_cur: %d rgn_to_max: %d rgn_next_cur: %d free_rgn_space: %d", 
		   rgn_to, rgn_next, nursery_sz, nursery_max, num_occupied_rgns, 
		   rgn_to_cur, rgn_to_max, rgn_next_cur, free_rgn_space );

      if (free_rgn_space < (rgn_next_cur + num_occupied_rgns*nursery_max)
          /* XXX what is correct policy/logic here??? */) {

	/* TODO: assert summarization complete (once it is incrementalized) */
	int n;

	assert( *gc->ssb[rgn_to]->bot == *gc->ssb[rgn_to]->top );
	if (DATA(gc)->ephemeral_area[ rgn_next-1 ]->has_popular_objects) {
	  /* choose next region for major collection so that we can summarize its remsets */
	  /* TODO: add loop to skip to next if n is popular. */
	  n = next_rgn(DATA(gc)->rrof_next_region, num_rgns);
	  DATA(gc)->rrof_next_region = n;
	  if (n == rrof_first_region) {
	    assert2( DATA(gc)->region_count == num_rgns );
	    rrof_completed_regional_cycle( gc );
	    num_rgns = DATA(gc)->region_count;
	  }
	  goto collect_evacuate_nursery;
	}

	if (DATA(gc)->rrof_refine_mark_countdown <= 0) {
	  stats_id_t timer1, timer2;
	  if (DATA(gc)->print_float_stats_each_refine)
	    print_float_stats( "prefin", gc );
	  start_timers( &timer1, &timer2 );
	  refine_remsets_via_marksweep( gc );
	  stop_refinem_timers( gc, &timer1, &timer2 );
	}

	if (!DATA(gc)->summarized_genset_valid) {
	  stats_id_t timer1, timer2;
	  int coverage;
	  gset_t range;
	  coverage = 
	    (int)ceil(DATA(gc)->region_count*DATA(gc)->rrof_sumz_coverage);
	  annoyingmsg("summary coverage: %d:%d of count: %d", 
	              rgn_next, coverage, DATA(gc)->region_count);
	  start_timers( &timer1, &timer2 );
#if 0 
	  /* when in doubt, can go back to this... */
	  build_remset_summaries( gc, gset_singleton(rgn_next) );
#else
	  range = gset_range(rgn_next, min(DATA(gc)->region_count+1, 
	                                   rgn_next+coverage) );
	  build_remset_summaries( gc, range );
#endif
	  stop_sumrize_timers( gc, &timer1, &timer2 );
	  if (USE_ORACLE_TO_VERIFY_SUMMARIES)
	    verify_summaries_via_oracle( gc );
	  DATA(gc)->next_summary_to_use = rgn_next;
	} else {
	  annoyingmsg("using preconstructed summary for %d", rgn_next );
	}
	assert(gset_memberp( DATA(gc)->next_summary_to_use, 
                             DATA(gc)->summarized_genset ));

	assert( rgn_next == DATA(gc)->next_summary_to_use ); /* XXX */

	if ( ! NO_COPY_COLLECT_FOR_POP_RGNS ||
	     (DATA(gc)->remset_summaries[ rgn_next ]->words 
	      <= DATA(gc)->popularity_limit)) { 

	  /* XXX for now, fold the nursery remset into the remset
	   * we're using for this major collection.  Better long term
	   * approach may be to do two separate scans rather than a
	   * fold-then-scan-combined XXX */
	  {
	    remset_t *rs = 
	      DATA(gc)->
	      remset_summaries[ DATA(gc)->next_summary_to_use ]->
	      sum_remset;
	    if (rs != NULL) {
	      rs_enumerate( DATA(gc)->nursery_remset, fold_from_nursery, gc );
	    } else {
	      rs = DATA(gc)->nursery_remset;
	    }
	    annoyingmsg( "construct rs (%d) summary", rs->live);
	    rs_init_summary( rs, -1, &DATA(gc)->summary );
	    if (USE_ORACLE_TO_VERIFY_SUMMARIES)
	      verify_summaries_via_oracle( gc );
	    DATA(gc)->use_summary_instead_of_remsets = TRUE;
	  }

	  /* clear contribution of rgn_next to all summaries */
	  { 
	    int i;
	    remset_t *rs;
	    struct filter_objects_from_sum_remset_data data;
	    data.gen = rgn_next;
	    for(i=0; i<DATA(gc)->remset_summaries_count; i++ ) {
	      if (i == rgn_next) 
	        continue; /* the entire summary for i is cleared down below */
	      /* (and shouldn't summary have nothing from region_i anyway?) */
	      annoyingmsg( "clear summary [%d] of entries from %d", i, rgn_next );
	      if (gset_memberp( i, DATA(gc)->summarized_genset ) &&
	          DATA(gc)->remset_summaries[ i ]->sum_remset != NULL) {
	        rs = DATA(gc)->remset_summaries[ i ]->sum_remset;
	        rs_enumerate( rs, 
	                      filter_objects_from_sum_remset, 
	                      &data );
	      }
	    }
	  }
	  oh_collect_into( DATA(gc)->ephemeral_area[ rgn_next-1 ], GCTYPE_COLLECT,
	                   DATA(gc)->ephemeral_area[ rgn_to-1 ] );
	
	  summary_dispose( &DATA(gc)->summary );
	  DATA(gc)->use_summary_instead_of_remsets = FALSE;
	  rs_clear( DATA(gc)->nursery_remset );
	  DATA(gc)->rrof_last_tospace = rgn_to;
	  
	  handle_secondary_space( gc );
	  /* Special case: if the emergency region has grown so large
	   * that this region (immediately post major collection) is
	   * smaller, then we swap them.
	   */
	  { 
	    int curr_gno = rgn_to;
	    int emergency_gno = DATA(gc)->ephemeral_area_count;
	    int curr_sz, emergency_sz;
	    oh_synchronize( DATA(gc)->ephemeral_area[ curr_gno-1 ] );
	    oh_synchronize( DATA(gc)->ephemeral_area[ emergency_gno-1 ] );
	    curr_sz = 
	      gc_allocated_to_areas( gc, gset_singleton( curr_gno ));
	    emergency_sz = 
	      gc_allocated_to_areas( gc, gset_singleton( emergency_gno ));
	    if (curr_sz < emergency_sz) {
	      old_heap_t *curr = 
		DATA(gc)->ephemeral_area[ curr_gno-1 ];
	      old_heap_t *emergency = 
		DATA(gc)->ephemeral_area[ emergency_gno-1 ];
	      remset_t *curr_rs = gc->remset[ curr_gno ];
	      remset_t *curr_mrs = gc->major_remset[ curr_gno ];
	      remset_t *emergency_rs = gc->remset[ emergency_gno ];
	      remset_t *emergency_mrs = gc->major_remset[ emergency_gno ];
	      annoyingmsg("SWAP %d <=> %d", curr_gno, emergency_gno );
	      oh_set_gen_no( curr, emergency_gno );
	      oh_set_gen_no( emergency, curr_gno );
	      DATA(gc)->ephemeral_area[ curr_gno-1 ] = emergency;
	      DATA(gc)->ephemeral_area[ emergency_gno-1 ] = curr;
	      gc->remset[ curr_gno ] = emergency_rs;
	      gc->major_remset[ curr_gno ] = emergency_mrs;
	      gc->remset[ emergency_gno ] = curr_rs;
	      gc->major_remset[ emergency_gno ] = curr_mrs;
	      los_swap_gnos( gc->los, curr_gno, emergency_gno );
	    }
	  }
	  rrof_completed_major_collection( gc );

	  /* clear the summary that guided this collection. */
	  {
	    remset_t *rs = DATA(gc)->remset_summaries[ rgn_next ]->
	      sum_remset;
	    if (rs != NULL) { 
	      rs_clear( rs );
	      return_to_remset_pool( rs );
	      DATA(gc)->remset_summaries[ rgn_next ]->sum_remset = NULL;
	      DATA(gc)->remset_summaries[ rgn_next ]->words = 0;
	    }
	    DATA(gc)->summarized_genset = 
	      gset_remove( rgn_next, DATA(gc)->summarized_genset);

	    { 
	      gset_t genset = DATA(gc)->summarized_genset;
	      if (genset.tag == gs_singleton) {
	        assert(genset.g1 <  DATA(gc)->remset_summaries_count);
	      } else if (genset.tag == gs_range) {
	        assert(genset.g2 <= DATA(gc)->remset_summaries_count);
	      } else { 
	        assert(0); }
	    }

	    DATA(gc)->next_summary_to_use =
	      next_rgn( DATA(gc)->next_summary_to_use, 
	                DATA(gc)->region_count );
	    if (! gset_memberp( DATA(gc)->next_summary_to_use,
	                        DATA(gc)->summarized_genset)) {
	      annoyingmsg("   invalidate_summaries( gc )");
	      invalidate_summaries( gc );
	    }
	  }

	  /* choose next region for major collection so that we can summarize its remsets */
	  /* TODO: add loop to skip to next if n is popular. */
	  n = next_rgn(DATA(gc)->rrof_next_region,  num_rgns);
	  DATA(gc)->rrof_next_region = n;
	  if (n == rrof_first_region) {
	    assert2( DATA(gc)->region_count == num_rgns );
	    rrof_completed_regional_cycle( gc );
	    num_rgns = DATA(gc)->region_count;
	  }
	} else {
	  annoyingmsg( "remset summary says region %d too popular to collect", 
		       rgn_next );
#if POP_RGNS_LIVE_FOREVER
	  DATA(gc)->ephemeral_area[ rgn_next-1 ]->has_popular_objects = TRUE;
#endif
	  DATA(gc)->ephemeral_area[ rgn_next-1 ]->live_last_major_gc = 
	    DATA(gc)->ephemeral_area[ rgn_next-1 ]->allocated;

	  /* clear the summary that guided this collection. */
	  {
	    remset_t *rs = DATA(gc)->remset_summaries[ rgn_next ]->
	      sum_remset;
	    if (rs != NULL) { 
	      rs_clear( rs );
	      return_to_remset_pool( rs );
	      DATA(gc)->remset_summaries[ rgn_next ]->sum_remset = NULL;
	      DATA(gc)->remset_summaries[ rgn_next ]->words = 0;
	    }
	    DATA(gc)->summarized_genset = 
	      gset_remove( rgn_next, DATA(gc)->summarized_genset);

	    { 
	      gset_t genset = DATA(gc)->summarized_genset;
	      if (genset.tag == gs_singleton) {
	        assert(genset.g1 <  DATA(gc)->remset_summaries_count);
	      } else if (genset.tag == gs_range) {
	        assert(genset.g2 <= DATA(gc)->remset_summaries_count);
	      } else { 
	        assert(0); }
	    }

	    DATA(gc)->next_summary_to_use =
	      next_rgn( DATA(gc)->next_summary_to_use, 
	                DATA(gc)->region_count );
	    if (! gset_memberp( DATA(gc)->next_summary_to_use,
	                        DATA(gc)->summarized_genset)) {
	      annoyingmsg("   invalidate_summaries( gc )");
	      invalidate_summaries( gc );
	    }
	  }

	  /* choose next region for major collection so that we can summarize its remsets */
	  /* TODO: add loop to skip to next if n is popular. */
	  n = next_rgn(DATA(gc)->rrof_next_region, num_rgns);
	  DATA(gc)->rrof_next_region = n;
	  if (n == rrof_first_region) {
	    assert2( DATA(gc)->region_count == num_rgns );
	    rrof_completed_regional_cycle( gc );
	    num_rgns = DATA(gc)->region_count;
	  }
	  goto collect_evacuate_nursery;
	}
      } else if (rgn_to_cur + nursery_sz < rgn_to_max &&
		 ! DATA(gc)->ephemeral_area[ rgn_to ]->has_popular_objects ) {
	/* if there's room, minor collect the nursery into current region. */

        /* check that SSB is flushed. */
        assert( *gc->ssb[rgn_to]->bot == *gc->ssb[rgn_to]->top );

	rs_init_summary( DATA(gc)->nursery_remset, -1, &(DATA(gc)->summary));
	DATA(gc)->use_summary_instead_of_remsets = TRUE;
	oh_collect( DATA(gc)->ephemeral_area[ rgn_to-1 ], GCTYPE_PROMOTE );
	rs_clear( DATA(gc)->nursery_remset );
	DATA(gc)->use_summary_instead_of_remsets = FALSE;
	summary_dispose( &(DATA(gc)->summary) );
	DATA(gc)->rrof_last_tospace = rgn_to;

        handle_secondary_space( gc );
        rrof_completed_minor_collection( gc );
	/* TODO: add code to incrementally summarize by attempting to
	 * predict how many minor collections will precede the next
	 * major collection. */
      } else {
	int n;
	int num_minor_rgns = 
	  max( DATA(gc)->region_count, DATA(gc)->ephemeral_area_count - 1 );
	/* the to-space is full, so shift to the next to-space */
	annoyingmsg("collect_rgnl shift to next to-space %d => %d out of %d max(%d,%d)",
		    DATA(gc)->rrof_to_region,
		    next_rgn(DATA(gc)->rrof_to_region,  num_minor_rgns),
		    num_minor_rgns, DATA(gc)->region_count, DATA(gc)->ephemeral_area_count - 1 );
	n = next_rgn(DATA(gc)->rrof_to_region,  num_minor_rgns);
	if (DATA(gc)->rrof_next_region == 0) {
	  DATA(gc)->rrof_next_region = 1;
	}
	DATA(gc)->rrof_to_region = n;
	/* TODO: double check that minor gc's haven't filled up to-spaces
	 * so fast that major GC hasn't had a chance to go (which should
	 * only happen when a summary is abandoned. */
	goto collect_evacuate_nursery;
      }
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
  
  if (--data->in_gc == 0) {
    after_collection( gc );
    stats_following_gc( gc );
    gclib_stats( &stats );
    annoyingmsg( "  Memory usage: heap %d, remset %d, RTS %d words",
		 stats.heap_allocated, stats.remset_allocated, 
		 stats.rts_allocated );
    annoyingmsg( "  Max heap usage: %d words", stats.heap_allocated_max );
  }
}

static void check_remset_invs_rgnl( gc_t *gc, word src, word tgt ) 
{
  supremely_annoyingmsg( "check_remset_invs_rgnl( gc, 0x%08x (%d), 0x%08x (%d) )", 
			 src, src?gen_of(src):0, tgt, gen_of(tgt) );
  /* XXX Felix is not convinced this assertion is sound. */
  assert( src == 0 ||
	  gen_of(src) != gen_of(tgt) ||
	  gen_of(src) != 0 ||
	  gen_of(tgt) != DATA(gc)->static_generation ||
	  rs_isremembered( gc->remset[ gen_of(src) ], tgt ) ||
	  rs_isremembered( gc->major_remset[ gen_of(src) ], tgt ));
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
	  rs_isremembered( gc->remset[ gen_of(src) ], src ) ||
	  rs_isremembered( gc->major_remset[ gen_of(src) ], src ));
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

static void before_collection( gc_t *gc )
{
  int e;

  gc->stat_last_ms_gc_pause = 0;
  gc->stat_last_ms_gc_pause_cpu = 0;
  gc->stat_last_gc_pause_ismajor = -1;
  DATA(gc)->stat_last_ms_remset_sumrize = -1;
  DATA(gc)->stat_last_ms_remset_sumrize_cpu = -1;
  DATA(gc)->stat_last_ms_mark_refinement = -1;
  DATA(gc)->stat_last_ms_mark_refinement_cpu = -1;

  gc_compact_all_ssbs( gc );

  /* For debugging of prototype;
   * double check heap consistency via mark/sweep routines.
   * (before collection means that mutator introduced inconsistency) */
#if CHECK_HEAP_INVARIANTS
  {
    int marked, traced, words_marked;
    msgc_context_t *msgc_ctxt = msgc_begin( gc );
    supremely_annoyingmsg("before GC, heap consistency check");
    msvfy_mark_objects_from_roots( msgc_ctxt );
    msgc_end( msgc_ctxt );
  }
#endif

  yh_before_collection( gc->young_area );
  for ( e=0 ; e < DATA(gc)->ephemeral_area_count ; e++ )
    oh_before_collection( DATA(gc)->ephemeral_area[ e ] );
  if (DATA(gc)->dynamic_area)
    oh_before_collection( DATA(gc)->dynamic_area );

  if (USE_ORACLE_TO_VERIFY_REMSETS)
    verify_remsets_via_oracle( gc );
  if (USE_ORACLE_TO_VERIFY_SUMMARIES)
    verify_summaries_via_oracle( gc );
}

static void after_collection( gc_t *gc )
{
  int e;

  DATA(gc)->generations = DATA(gc)->generations_after_gc;

 if (USE_ORACLE_TO_VERIFY_REMSETS)
   verify_remsets_via_oracle( gc );
 if (USE_ORACLE_TO_VERIFY_SUMMARIES) 
   verify_summaries_via_oracle( gc );

  yh_after_collection( gc->young_area );
  for ( e=0 ; e < DATA(gc)->ephemeral_area_count ; e++ )
    oh_after_collection( DATA(gc)->ephemeral_area[ e ] );
  if (DATA(gc)->dynamic_area)
    oh_after_collection( DATA(gc)->dynamic_area );

#if CHECK_HEAP_INVARIANTS
  /* For debugging of prototype;
   * double check heap consistency via mark/sweep routines. */
 {
    int marked, traced, words_marked;
    msgc_context_t *msgc_ctxt = msgc_begin( gc );
    supremely_annoyingmsg("after  GC, heap consistency check");
    msvfy_mark_objects_from_roots( msgc_ctxt );
    msgc_end( msgc_ctxt );
  }
#endif

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
 * propogate it via the remset summary.
 */
static void
enumerate_remsets_complement( gc_t *gc,
			      gset_t gset,
			      bool (*f)(word obj, void *data, unsigned *count),
			      void *fdata,
			      bool enumerate_np_young )
{
  int i;
  int ecount;

  if (!DATA(gc)->is_partitioned_system) return;

  if (DATA(gc)->use_summary_instead_of_remsets) {
    void (*g)(word obj, void *data, unsigned *count);
    g = (void*)f;
    summary_enumerate( &DATA(gc)->summary, g, fdata );
    return;
  }

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
  for ( i=0 ; i <= ecount; i++ ) {
    process_seqbuf( gc, gc->ssb[i] );
  }

  for( i = 1; i <= ecount; i++ ) {
    if (! gset_memberp( i, gset )) {
      rs_enumerate( gc->remset[i], f, fdata);
    }
  }
  
  if (DATA(gc)->dynamic_area) {
    i = oh_current_space(DATA(gc)->dynamic_area)->gen_no;
    if (! gset_memberp( i, gset )) {
      rs_enumerate( gc->remset[i], f, fdata);
    }
  }
    
  if (gc->static_area) {
    i = DATA(gc)->static_generation;
    { 
      rs_enumerate( gc->remset[i], f, fdata );
    }
  }

  if (enumerate_np_young) {
    rs_enumerate( gc->remset[ gc->np_remset ], f, fdata );
  }
}

static word *data_load_area( gc_t *gc, int size_bytes )
{
  return load_text_or_data( gc, size_bytes, 0 );
}

static word *text_load_area( gc_t *gc, int size_bytes )
{
  return load_text_or_data( gc, size_bytes, 1 );
}

static word *load_text_or_data( gc_t *gc, int nbytes, int load_text )
{
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
  else
    return yh_data_load_area( gc->young_area, nbytes );
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

#if defined(SIMULATE_NEW_BARRIER)
/* Note we do not check whether it's in the NP extra remembered set.
   This is correct, because that check will not be performed by the
   new barrier -- it can only check whether it's in a normal set.
   */
static int isremembered( gc_t *gc, word w )
{
  unsigned g;

  g = gen_of( w );
  assert( g >= 0 && g < gc->remset_count );
  if (g > 0)
    return rs_isremembered( gc->remset[g], w ); /*XXX major_remsets too? XXX*/
  else
    return 0;
}
#endif

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

  stats_gclib.last_ms_gc_pause           = gc->stat_last_ms_gc_pause;
  stats_gclib.last_ms_gc_pause_cpu       = gc->stat_last_ms_gc_pause_cpu;
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
  stats_gclib.last_ms_mark_refinement     = 
    DATA(gc)->stat_last_ms_mark_refinement;
  stats_gclib.last_ms_mark_refinement_cpu = 
    DATA(gc)->stat_last_ms_mark_refinement_cpu;
  stats_add_gclib_stats( &stats_gclib );

  stats_dumpstate();		/* Dumps stats state if dumping is on */
}

static int compact_all_ssbs( gc_t *gc )
{
  int overflowed, i;
  word *bot, *top;

  overflowed = 0;
  for (i = 0; i < gc->remset_count; i++) {
    bot = *gc->ssb[i]->bot;
    top = *gc->ssb[i]->top;
    overflowed = process_seqbuf( gc, gc->ssb[i] ) || overflowed;
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

static remset_as_summary_t* allocate_remset_as_summary(int gen, int poplimit) 
{
  remset_as_summary_t *rast;
  rast = (remset_as_summary_t*)must_malloc(sizeof(remset_as_summary_t));
  rast->sum_remset = NULL;
  rast->gen        = gen;
  rast->valid      = FALSE;
  rast->words      = 0;
  rast->max_words  = poplimit;
  return rast;
}

static int ssb_process_rrof( gc_t *gc, word *bot, word *top, void *ep_data );
static void expand_remset_gnos( gc_t *gc, int fresh_gno )
{
  int i;
  int new_remset_count = gc->remset_count + 1;
  remset_t** new_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*new_remset_count );
  remset_t** new_major_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*new_remset_count );
  seqbuf_t ** new_ssb = 
    (seqbuf_t**)must_malloc( sizeof( seqbuf_t*)*new_remset_count );
  word **new_ssb_bot = (word**)must_malloc( sizeof(word*)*new_remset_count );
  word **new_ssb_top = (word**)must_malloc( sizeof(word*)*new_remset_count );
  word **new_ssb_lim = (word**)must_malloc( sizeof(word*)*new_remset_count );
  remset_as_summary_t** new_remset_summaries = 
    (remset_as_summary_t**)must_malloc( sizeof( remset_as_summary_t*)*new_remset_count );
  assert( fresh_gno < new_remset_count );

  for( i = 0; i < fresh_gno; i++ ) {
    new_remset[i] = gc->remset[i];
    new_major_remset[i] = gc->major_remset[i];
    new_ssb[i] = gc->ssb[i];
    seqbuf_swap_in_ssb( gc->ssb[i], &new_ssb_bot[i], 
                        &new_ssb_top[i], &new_ssb_lim[i] );
    seqbuf_set_sp_data( gc->ssb[i], /* XXX */(void*) i );
    new_remset_summaries[i] = DATA(gc)->remset_summaries[i];
  }
  new_remset[fresh_gno] = create_remset( 0, 0 );
  new_major_remset[fresh_gno] = create_remset( 0, 0 );
  /* XXX This code only works with RROF, but I do not have a
   * reasonable way to assert that precondition. */
  new_ssb[fresh_gno] = 
    create_seqbuf( 0, &new_ssb_bot[fresh_gno], &new_ssb_top[fresh_gno], 
                   &new_ssb_lim[fresh_gno], ssb_process_rrof, /* XXX */(void*) fresh_gno );
  new_remset_summaries[fresh_gno] = 
    allocate_remset_as_summary( fresh_gno, DATA(gc)->popularity_limit );
  for( i = fresh_gno+1; i < new_remset_count; i++ ) {
    new_remset[i] = gc->remset[i-1];
    new_major_remset[i] = gc->major_remset[i-1];
    new_ssb[i] = gc->ssb[i-1];
    seqbuf_swap_in_ssb( gc->ssb[i-1], 
                        &new_ssb_bot[i], &new_ssb_top[i], &new_ssb_lim[i] );
    seqbuf_set_sp_data( gc->ssb[i-1], /* XXX */(void*)i );
    new_remset_summaries[i] = DATA(gc)->remset_summaries[i-1];
  }

  free( gc->remset );
  free( gc->major_remset );
  free( gc->ssb );
  free( DATA(gc)->ssb_bot );
  free( DATA(gc)->ssb_top );
  free( DATA(gc)->ssb_lim );
  free( DATA(gc)->remset_summaries );
  gc->remset = new_remset;
  gc->major_remset = new_major_remset;
  gc->ssb = new_ssb;
  DATA(gc)->ssb_bot = new_ssb_bot;
  DATA(gc)->ssb_top = new_ssb_top;
  DATA(gc)->ssb_lim = new_ssb_lim;
  DATA(gc)->globals[ G_SSBTOPV ] = /* XXX */(word) DATA(gc)->ssb_top;
  DATA(gc)->globals[ G_SSBLIMV ] = /* XXX */(word) DATA(gc)->ssb_lim;
  gc->remset_count = new_remset_count;
  DATA(gc)->remset_summaries = new_remset_summaries;
  
}

static void expand_summary_gnos( gc_t *gc, int fresh_gno ) 
{
  /* check that inserting fresh_gno does not upset
     the existing summarization data (that is, that there are *no*
     summaries pointing into regions >= fresh_gno
  */
  if (DATA(gc)->summarized_genset_valid) {
    assert( ! gset_min_elem_greater_than( DATA(gc)->summarized_genset, 
                                          fresh_gno-1 ));
  }

  /* even though inserting the fresh gno will not upset the 
     summarization data, we still need to expand the 
     gc fields related to summaries (because right now 
     I keep that structure proportional to the number of 
     regions
  */
  {
    int len = DATA(gc)->remset_summaries_count+1;
    int i;
    remset_as_summary_t **remset_summaries;
    remset_summaries = 
      (remset_as_summary_t**)must_malloc(len*sizeof(remset_as_summary_t*));
    remset_summaries[0] = NULL;
    for( i = 1; i < fresh_gno; i++ )
      remset_summaries[i] = DATA(gc)->remset_summaries[i];
    remset_summaries[ fresh_gno ] = 
      allocate_remset_as_summary( fresh_gno, DATA(gc)->popularity_limit );
    for( i = fresh_gno+1; i < len; i++ )
      remset_summaries[i] = DATA(gc)->remset_summaries[i-1];
    free(DATA(gc)->remset_summaries);
    DATA(gc)->remset_summaries = remset_summaries;
    DATA(gc)->remset_summaries_count = len;
  }
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
  expand_summary_gnos( gc, fresh_gno );
  
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
  int cur_allocated;
  int max_allocated = 
    gc_maximum_allotted( gc, gset_singleton( current_space->gen_no ));
  int expansion_amount = max( bytes_needed, GC_CHUNK_SIZE );
  int to_rgn_old = DATA(gc)->rrof_to_region;
  int to_rgn_new = next_rgn( to_rgn_old, DATA(gc)->region_count );

  ss_sync( current_space );
  cur_allocated = 
    current_space->used+los_bytes_used( gc->los, current_space->gen_no );

  if (cur_allocated + expansion_amount <= max_allocated) {
    ss_expand( current_space, expansion_amount );
    return current_space;
  } else if ( to_rgn_new != DATA(gc)->rrof_next_region ) {
    do {
      if (gc_allocated_to_areas( gc, gset_singleton( to_rgn_new )) + expansion_amount
          < gc_maximum_allotted( gc, gset_singleton( to_rgn_new ))) {
        DATA(gc)->rrof_to_region = to_rgn_new;
        return oh_current_space( DATA(gc)->ephemeral_area[ to_rgn_new-1 ] );
      }
      to_rgn_new = next_rgn( to_rgn_new, DATA(gc)->region_count );
    } while (to_rgn_new != to_rgn_old );
    /* failure! */
    annoyingmsg("find_space_rgnl: failed shift of to");
    DATA(gc)->rrof_to_region = to_rgn_new;
  } 

  {
    int last_gen_no = DATA(gc)->ephemeral_area_count;
    old_heap_t *heap = DATA(gc)->ephemeral_area[ last_gen_no-1 ];
    int allocated_there;
    int allotted_there = 
      gc_maximum_allotted( gc, gset_singleton( last_gen_no ));
    ss_sync( oh_current_space( heap ));
    allocated_there = 
      oh_current_space( heap )->used + 
      los_bytes_used( gc->los, last_gen_no );

    if (DATA(gc)->secondary_space != NULL) {
      int gen_no = DATA(gc)->secondary_space->gen_no;

      /* I do not know how to handle this case.  I thought I could
       * assimilate seconary to reserve, replace secondary fresh, and
       * continue collection, but that is unsound when cheney is in
       * the midst of scanning secondary and requests more space.
       * (Probably should just ensure this case can never happen.) */
      assert(0);

      oh_assimilate( DATA(gc)->ephemeral_area[ gen_no-1 ],
		     DATA(gc)->secondary_space );
      DATA(gc)->secondary_space = NULL;
      return gc_fresh_space( gc );
    }    
    if (current_space->gen_no == last_gen_no) {
      return gc_fresh_space( gc );
    }

    /* Putting in GC_CHUNK_SIZE as a fudge factor, since we sometimes
     * end up with gaps in the tospace as we seal off chunks (and
     * insert alignment padding). */
    if (allocated_there + gc->young_area->allocated + GC_CHUNK_SIZE 
	<= allotted_there ) {
      assert(DATA(gc)->secondary_space == NULL);
      DATA(gc)->secondary_space =
	create_semispace( GC_CHUNK_SIZE, last_gen_no );
      return DATA(gc)->secondary_space;
    } else {
      return gc_fresh_space( gc );
    }
  }
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
      annoyingmsg("gno: %d area thinks: %d but actually: %d", 
                  gno, area_thinks_allocated, retval);
    }
    /* assert( area_thinks_allocated == retval ); */ /* XXX was this ever right? */
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
  }
  assert(0);
}

static bool is_nonmoving( gc_t *gc, int gen_no ) 
{
  if (gen_no == DATA(gc)->static_generation)
    return TRUE;
  if (gen_no > 0 &&
      gen_no < DATA(gc)->ephemeral_area_count &&
      DATA(gc)->ephemeral_area[ gen_no-1 ]->has_popular_objects)
    return TRUE;
  
  return FALSE;
}

static bool is_address_mapped( gc_t *gc, word *addr, bool noisy ) 
{
  assert(tagof(addr) == 0);
  bool ret = FALSE;
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

  strcpy( buf, "S+C " );

  gc->young_area = create_sc_heap( 0, gc, &info->sc_info, info->globals );
  strcat( buf, gc->young_area->id );

  if (info->use_static_area) {
    gc->static_area = create_static_area( 1, gc );
    strcat( buf, "+" );
    strcat( buf, gc->static_area->id );
  }

  gc->id = strdup( buf );
  return (info->use_static_area ? 2 : 1);
}

static int ssb_process_gen( gc_t *gc, word *bot, word *top, void *ep_data ) {
  remset_t **remset = gc->remset;
  return rs_add_elems_distribute( remset, bot, top );
}

/* XXX stolen from remset.c; should be factored out to somewhere else */
static word retagptr( word w ) 
{
  if (tagof(w) == 0) {
    switch (header(*(word*)w)) {
    case VEC_HDR :
      return (word)tagptr( w, VEC_TAG );
    case BV_HDR : 
      return 0; /* signal that entry should be removed! */
    case PROC_HDR :
      return (word)tagptr( w, PROC_TAG );
    default:
      panic_abort( "memmgr.c: word is nonptr." );
    }
  } else {
    return w;
  }
}

static int ssb_process_rrof( gc_t *gc, word *bot, word *top, void *ep_data ) 
{
  remset_t **remset;
  remset_t *rs;
  int retval = 0;
  int g_rhs;
  word *p, *q, w;
  remset = gc->remset;
  rs = DATA(gc)->nursery_remset;
  retval |= rs_add_elems_distribute( remset, bot, top );
  retval |= rs_add_elems_funnel( rs, bot, top );

  g_rhs = (int)ep_data; /* XXX is (int) of void* legal C? */
  if ( DATA(gc)->summarized_genset_valid &&
       gset_memberp( g_rhs, DATA(gc)->summarized_genset )) {
    p = bot; 
    q = top; 
    while (q > p) {
      q--;
      w = *q;
      w = retagptr(w);
      if (!w) 
        continue;
      add_object_to_sum_rs( DATA(gc)->remset_summaries[ g_rhs ],
                            g_rhs, w );
    }
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
    gc->remset_count = gen_no + 1;
  else
    gc->remset_count = gen_no;

  gc->remset = (remset_t**)must_malloc( sizeof( remset_t* )*gc->remset_count );
  gc->major_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*gc->remset_count );

  gc->remset[0] = (void*)0xDEADBEEF;
  gc->major_remset[0] = (void*)0xDEADBEEF;
  for ( i = 1 ; i < gc->remset_count ; i++ ) {
    gc->remset[i] =
      create_remset( info->rhash, 0 );
    gc->major_remset[i] =
      create_remset( info->rhash, 0 );
  }

  data->ssb_bot = (word**)must_malloc( sizeof(word*)*gc->remset_count );
  data->ssb_top = (word**)must_malloc( sizeof(word*)*gc->remset_count );
  data->ssb_lim = (word**)must_malloc( sizeof(word*)*gc->remset_count );
  gc->ssb = (seqbuf_t**)must_malloc( sizeof(seqbuf_t*)*gc->remset_count );
  for ( i = 0; i < gc->remset_count ; i++ ) {
    /* nursery has one too, an artifact of RROF.
     * XXX consider using different structures for n-YF vs ROF vs RROF;
     * RROF needs points-into information implicit with index here,
     * but others do not. */
    gc->ssb[i] = 
      create_seqbuf( info->ssb, 
                     &data->ssb_bot[i], &data->ssb_top[i], &data->ssb_lim[i],
                     ssb_process_gen, 0 );
  }

  if (info->use_non_predictive_collector)
    gc->np_remset = gc->remset_count - 1;

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
  size = 0;

  strcpy( buf, "RGN " );
  
  data->dynamic_max = info->dynamic_sc_info.dynamic_max;
  data->dynamic_min = info->dynamic_sc_info.dynamic_min;
  data->rrof_load_factor = info->dynamic_sc_info.load_factor;

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
    data->region_count = e-1;
    data->fixed_ephemeral_area = FALSE;
    data->ephemeral_area = (old_heap_t**)must_malloc( e*sizeof( old_heap_t* ));
    
    data->rrof_sumz_budget = 
      (info->has_sumzbudget?info->sumzbudget:default_sumz_budget_factor);
    data->rrof_sumz_coverage = 
      (info->has_sumzcoverage?info->sumzcoverage:default_sumz_coverage_factor);

    for ( i = 0; i < e; i++ ) {
      assert( info->ephemeral_info[i].size_bytes > 0 );
      size += info->ephemeral_info[i].size_bytes;
      /* P * regionsize limits size of incoming summary */
      { double popular_factor = (info->has_popularity_factor 
	                         ? info->popularity_factor 
	                         : default_popularity_factor);
	data->popularity_limit = 
	  info->ephemeral_info[i].size_bytes * popular_factor / sizeof(word);
      }
      data->ephemeral_area[ i ] = 
	create_sc_area( gen_no, gc, &info->ephemeral_info[i], 
			OHTYPE_REGIONAL );
      gen_no += 1;
    }
  }
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
    gc->remset_count = gen_no;
    gc->remset = 
      (remset_t**)must_malloc( sizeof( remset_t* )*gc->remset_count );
    gc->major_remset = 
      (remset_t**)must_malloc( sizeof( remset_t* )*gc->remset_count );
    gc->remset[0] = (void*)0xDEADBEEF;
    gc->major_remset[0] = (void*)0xDEADBEEF;
    for ( i = 1 ; i < gc->remset_count ; i++ ) {
      gc->remset[i] =
	create_remset( info->rhash, 0 );
      gc->major_remset[i] =
	create_remset( info->rhash, 0 );
    }

    {
      int len = gc->remset_count+1, i;
      data->remset_summaries = 
        (remset_as_summary_t**)must_malloc(len*sizeof(remset_as_summary_t*));
      data->remset_summaries[0] = NULL;
      for( i = 1; i < len; i++ ) {
        data->remset_summaries[i] = 
          allocate_remset_as_summary( i, data->popularity_limit );
      }
      data->remset_summaries_count = len;
    }

    data->nursery_remset = create_remset( 0, 0 );

    data->ssb_bot = (word**)must_malloc( sizeof(word*)*gc->remset_count );
    data->ssb_top = (word**)must_malloc( sizeof(word*)*gc->remset_count );
    data->ssb_lim = (word**)must_malloc( sizeof(word*)*gc->remset_count );
    gc->ssb = (seqbuf_t**)must_malloc( sizeof(seqbuf_t*)*gc->remset_count );
    for ( i = 0; i < gc->remset_count ; i++ ) {
      /* nursery has one too! */
      gc->ssb[i] = 
        create_seqbuf( info->ssb, 
                       &data->ssb_bot[i], &data->ssb_top[i], &data->ssb_lim[i],
                       ssb_process_rrof, 0 );
    }
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
    /* XXX eventually I should be switching this on and off depending
     * on whether a concurrent mark is actually happening... */
    DATA(gc)->globals[G_CONCURRENT_MARK] = 1;
  }

  data->print_float_stats_each_cycle  = info->print_float_stats_cycle;
  data->print_float_stats_each_major  = info->print_float_stats_major;
  data->print_float_stats_each_minor  = info->print_float_stats_minor;
  data->print_float_stats_each_refine = info->print_float_stats_refine;

  return gen_no;
}

static word last_origin_ptr_added = 0;
static void points_across_callback( gc_t *gc, word lhs, word rhs ) 
{
  int g_lhs = gen_of(lhs);
  int g_rhs = gen_of(rhs);
  assert2( g_lhs != 0 ); /* gf_filter_remset_lhs == 0 */
  if (! gc_is_nonmoving( gc, g_rhs )) {
    {
      assert2(g_lhs > 0);
      assert2(g_rhs >= 0);
      assert2(gc->major_remset != NULL);

      /* enqueue lhs in remset. */
      if (last_origin_ptr_added != lhs) {
        rs_add_elem_new( gc->major_remset[g_lhs], lhs );
        last_origin_ptr_added = lhs;
      }

      if ( DATA(gc)->summarized_genset_valid &&
           gset_memberp( g_rhs, DATA(gc)->summarized_genset )) {
        add_object_to_sum_rs( DATA(gc)->remset_summaries[ g_rhs ], 
                              g_rhs, lhs );
      }
    }
  }
}

static gc_t *alloc_gc_structure( word *globals, gc_param_t *info )
{
  gc_data_t *data;
  gc_t *ret;
  semispace_t *(*my_find_space)( gc_t *gc, unsigned bytes_needed, 
				 semispace_t *current_space );
  void (*my_collect)( gc_t *gc, int rgn, int bytes_needed, gc_type_t request );
  void (*my_check_remset_invs)( gc_t *gc, word src, word tgt );
  
  if (info->is_regional_system) {
    my_find_space = find_space_rgnl;
    my_collect = collect_rgnl;
    my_check_remset_invs = check_remset_invs_rgnl;    
  } else {
    my_find_space = find_space_expanding;
    my_collect = collect_generational;
    my_check_remset_invs = check_remset_invs;
  }

  data = (gc_data_t*)must_malloc( sizeof( gc_data_t ) );

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

  data->rrof_to_region = 1;
  data->rrof_next_region = 1;
  data->rrof_last_tospace = -1;

  data->rrof_has_refine_factor = TRUE;
  data->rrof_refinement_factor = 3.0;
  data->rrof_refine_mark_period = -1;
  data->rrof_refine_mark_countdown = -1;

  data->rrof_last_live_estimate = 0;

  data->remset_summaries = 0;
  data->remset_summaries_count = 0;
  data->summarized_genset_valid = FALSE;
  data->next_summary_to_use = -2;
  data->nursery_remset = 0;

  data->last_live_words = 0;
  data->max_live_words = 0;

  ret = 
    create_gc_t( "*invalid*",
		 (void*)data,
		 initialize, 
		 allocate,
		 allocate_nonmoving,
		 make_room,
		 my_collect,
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
		 0,		/* load_heap */
		 dump_image,
		 make_handle,
		 free_handle,
		 enumerate_roots,
		 enumerate_remsets_complement,
		 fresh_space,
		 my_find_space,
		 allocated_to_areas,
		 maximum_allotted,
		 is_nonmoving, 
		 is_address_mapped,
		 my_check_remset_invs,
		 points_across_callback
		 );
  ret->scan_update_remset = info->is_regional_system;
  return ret;
}

/* eof */
