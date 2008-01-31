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

/* Checking code */
#define CHECK_HEAP_INVARIANTS 0

typedef struct gc_data gc_data_t;

/* The 'remset' table in the gc structure has one extra element if the
   collector uses the non-predictive dynamic area: that extra element is
   the young->old remembered set for the non-predictive collector.  The
   extra element is always the last one, and the slots in the SSB tables
   for that remset are the last slots.  The index is indicated by the
   np_remset member of gc_t.
   */

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
  word *ssb_bot;
  word *ssb_top;
  word *ssb_lim;

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
  int rrof_first_region;
    /* In RROF collector, the first region collected in this cycle */

  remset_t *remset_summary;     /* NULL or summarization of remset array */
  bool      remset_summary_valid;

  semispace_t *secondary_space; /* NULL or space for when tospace overflows */
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
	      &data->ssb_top,
	      &data->ssb_lim, 
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

static void collect( gc_t *gc, int gen, int bytes_needed, gc_type_t request )
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
  gset_t genset;
  remset_t *remset;
  int objects_visited;
  int objects_added;
};
static bool scan_object_for_remset_summary( word ptr, void *data, unsigned *count )
{
  word *loc = ptrof(ptr);
  word scanned = 0;
  bool do_enqueue = FALSE;
  remset_summary_data_t *remsum = (remset_summary_data_t*)data;
  gset_t genset = remsum->genset;
  assert( ! gset_memberp(gen_of(ptr), genset ));
  do {
    if (tagof( ptr ) == PAIR_TAG) {
      /* handle car */
      if (isptr(*loc)) {
	int gen = gen_of(*loc);
	if (gen == 0 || gset_memberp(gen,genset)) {
	  do_enqueue = TRUE;
	  scanned = 1;
	  break;
	}
      }
      ++loc;
      /* handle cdr */
      if (isptr(*loc)) {
	int gen = gen_of(*loc);
	if (gen == 0 || gset_memberp(gen,genset)) {
	  do_enqueue = TRUE;
	  scanned = 2;
	  break;
	}
      }
    } else { /* vector or procedure */
      assert( (tagof(ptr) == VEC_TAG) || (tagof(ptr) == PROC_TAG) );
      word words = sizefield( *loc ) / 4;
      scanned = words; /* start at upper bound */
      while (words--) {
	++loc;
	if (isptr(*loc)) {
	  int gen = gen_of(*loc);
	  if (gen == 0 || gset_memberp(gen,genset)) {
	    do_enqueue = TRUE;
	    scanned -= words; /* then take away what's left */
	    break;
	  }
	}
      }
    }
  } while (0);
  
  remsum->objects_visited += 1;
  if (do_enqueue) {
    remsum->objects_added += 1;
    rs_add_elem( remsum->remset, ptr );
  }

  *count += scanned;
  return TRUE; /* don't remove entries from the remembered set we are summarizing! */  
}

static void build_remset_summary( gc_t *gc, int gen )
{
  remset_summary_data_t remsum;
  gset_t genset;
  int i;
  int remset_count = gc->remset_count;
  
  genset = gset_singleton( gen );
  remsum.genset = genset;
  remsum.remset = DATA(gc)->remset_summary; 
  remsum.objects_visited = 0;
  remsum.objects_added = 0;
  for(i = 1; i < remset_count; i++) {
    if (gset_memberp(i, genset))
      continue;
    /* TODO: use rs_enumerate_partial here? */
    rs_enumerate( gc->remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
  }
  DATA(gc)->remset_summary_valid = TRUE;
  annoyingmsg( "remset summary for collecting {0, %d}, live: %d", 
	       gen, DATA(gc)->remset_summary->live );

}

static void invalidate_remset_summary( gc_t *gc )
{
  rs_clear( DATA(gc)->remset_summary );
  DATA(gc)->remset_summary_valid = FALSE;
}

static int next_rgn( int rgn, int num_rgns ) {
  rgn++;
  if (rgn > num_rgns) 
    rgn = 1;
  return rgn;
}

#define USE_ORACLE_TO_VERIFY_REMSETS 0
#define USE_ORACLE_TO_UPDATE_REMSETS 0
#define NO_COPY_COLLECT_FOR_POP_RGNS 1
#define POPULARITY_LIMIT 40000

static void* verify_remsets_fcn( word obj, word src, void *data ) 
{
  gc_t *gc = (gc_t*)data;
  if (isptr(src) && isptr(obj) &&
      gen_of(src) != gen_of(obj) &&
      gen_of(obj) != DATA(gc)->static_generation) {
    assert( gen_of(src) >= 0 );
    if (gen_of(src) > 0) {
      process_seqbuf( gc, gc->ssb );
      if (!rs_isremembered( gc->remset[ gen_of(src) ], src )) {
	consolemsg( " src: 0x%08x (%d) points to obj: 0x%08x (%d),"
		    " but not in remset @0x%08x",
		    src, gen_of(src), obj, gen_of(obj), 
		    gc->remset[ gen_of(src) ]);
	assert( gc_is_address_mapped( gc, ptrof(src), TRUE ));
	assert( gc_is_address_mapped( gc, ptrof(obj), TRUE ));
	assert(0);
      }
    }
  }
  return data;
}

static void verify_remsets_via_oracle( gc_t *gc ) 
{
  msgc_context_t *context;
  int marked, traced, words_marked; 
  context = msgc_begin( gc );
  msgc_set_object_visitor( context, verify_remsets_fcn, gc );
  msgc_mark_objects_from_roots( context, &marked, &traced, &words_marked );
  msgc_end( context );
}

static void* update_remsets_msgc_fcn( word obj, word src, void *data ) 
{
  gc_t *gc = (gc_t*)data;
  if (isptr(src) && isptr(obj) &&
      gen_of(src) != gen_of(obj) &&
      gen_of(obj) != DATA(gc)->static_generation) {
    assert( gen_of(src) > 0 );
    if (gen_of(src) > 0)
      rs_add_elem( gc->remset[ gen_of(src) ], src );
  }
  return data;
}

static void update_remsets_via_oracular_msgc( gc_t *gc ) 
{
  msgc_context_t *context;
  int marked, traced, words_marked; 
  context = msgc_begin( gc );
  msgc_set_object_visitor( context, update_remsets_msgc_fcn, gc );
  msgc_mark_objects_from_roots_and_remsets
    ( context, &marked, &traced, &words_marked );
  msgc_end( context );
}

static void* update_remsets_visitor( word *addr, int tag, void *accum ) 
{
  word w, words;
  gc_t *gc = (gc_t*)accum;
  word *src = tagptr( addr, tag );
  int src_gen = gen_of( src );
  assert(src_gen > 0);

  switch (tag) {
  case PAIR_TAG:
    if (isptr(*addr) &&
	src_gen != gen_of( *addr ) &&
	gen_of( *addr ) != DATA(gc)->static_generation) {
      rs_add_elem( gc->remset[ src_gen ], src );
      break;
    }
    addr++;
    if (isptr(*addr) &&
	src_gen != gen_of( *addr ) &&
	gen_of( *addr ) != DATA(gc)->static_generation) {
      rs_add_elem( gc->remset[ src_gen ], src );
      break;
    }
    break;
  case VEC_TAG:
  case PROC_TAG:
    w = *addr;
    words = sizefield( w ) >> 2;
    addr++;
    while (words) {
      words--;
      if (isptr(*addr) &&
	  src_gen != gen_of( *addr ) &&
	  gen_of( *addr ) != DATA(gc)->static_generation) {
	rs_add_elem( gc->remset[ src_gen ], src );
	break;
      }
      addr++;
    }
  }

  return accum;
}

static void update_remsets_via_oracular_ss( gc_t *gc )
{
  { 
    int rgn = DATA(gc)->rrof_last_tospace;
    old_heap_t *heap = DATA(gc)->ephemeral_area[ rgn - 1];
    heap->enumerate( heap, update_remsets_visitor, gc );
  }

  /* we also might have evacuated objects to an additional region 
   * if the last tospace ran out of room. */
  { 
    int rgn = DATA(gc)->ephemeral_area_count;
    old_heap_t *heap;
    if (rgn != DATA(gc)->rrof_last_tospace) {
      heap = DATA(gc)->ephemeral_area[ rgn - 1];
      heap->enumerate( heap, update_remsets_visitor, gc );
    }
  }
}

static void update_remsets_via_oracle( gc_t *gc )
{
  if (gc->remset == NULL) 
    return;
  if (0)
    update_remsets_via_oracular_msgc( gc );
  else 
    update_remsets_via_oracular_ss( gc );
}

#define ANALYZE_POPULARITY 0

static remset_t *popularity_analysis_remset = NULL;
struct popularity_analysis_data {
  int rgn;
  word fst_obj;
  word fin_obj;
  int *popularity;
  int popularity_len;
  remset_t *summary_remset;
  int live_extra_objects_in_summary_remset;
  int dead_extra_objects_in_summary_remset;
  int* dead_extra_objects_in_region;
  msgc_context_t *init_context;
  gc_t *gc;
};

static void* find_bounds_fcn( word obj, word src, void *data ) 
{
  struct popularity_analysis_data *my_data = 
    (struct popularity_analysis_data*)data;
  if (isptr(obj) && gen_of(obj) == my_data->rgn) {
    if (obj < my_data->fst_obj)
      my_data->fst_obj = obj;
    if (obj > my_data->fin_obj)
      my_data->fin_obj = obj;
  }
  return data;
}

static int popularity_analysis_addr2index( struct popularity_analysis_data *pa_data, 
					   word addr )
{
  return ((word)addr - (word)pa_data->fst_obj) >> 3;
}
static int popularity_analysis_index2addr( struct popularity_analysis_data *pa_data,
					   int index )
{
  return (((byte*)(pa_data->fst_obj&~0x7)) + (index<<3));
}
					   
static void* calc_popularity_fcn( word obj, word src, void *data )
{
  struct popularity_analysis_data *my_data =
    (struct popularity_analysis_data*)data;
  if (isptr(obj) && 
      gen_of(obj) == my_data->rgn &&
      (src == 0 || (gen_of(src) != 0 && gen_of(src) != my_data->rgn))
      ) {
    int idx = popularity_analysis_addr2index( my_data, obj );
    if (idx < 0 || idx >= my_data->popularity_len) {
      consolemsg( " invalid obj: 0x%08x (idx: %d) for pop data"
		  " {rgn: %d, addrs: [0x%08x,0x%08x), len: %d}",
		  obj, idx, my_data->rgn, my_data->fst_obj, my_data->fin_obj, 
		  my_data->popularity_len );
    }
    assert(idx >= 0 );
    assert(idx < my_data->popularity_len );
    if ( src != 0 && 
	 ! rs_isremembered( my_data->summary_remset, src )) {
      consolemsg(" pop analysis mark, obj: 0x%08x (%d) not in remset summary", 
		 src, gen_of(src) );
      assert( rs_isremembered( my_data->summary_remset, src ));
    }

    rs_add_elem( popularity_analysis_remset, src );
    my_data->popularity[idx]++;
  }
  return data;
}

static bool categorize_extra_rs_members( word obj, void *data, unsigned *count )
{
  struct popularity_analysis_data *my_data = 
    (struct popularity_analysis_data*)data;
  assert( rs_isremembered( my_data->summary_remset, obj ));
  if ( rs_isremembered( popularity_analysis_remset, obj )) {
    /* we do not worry about members of both sets. */
  } else if ( msgc_object_marked_p( my_data->init_context, obj )) {
    my_data->live_extra_objects_in_summary_remset++;
  } else {
    /* the object is dead, tell me that! */
    my_data->dead_extra_objects_in_summary_remset++;

    assert( gen_of(obj) >= 0 );
    assert( gen_of(obj) < my_data->gc->remset_count );
    my_data->dead_extra_objects_in_region[ gen_of(obj) ]++;
  }
  return TRUE;
}

static void popularity_analysis( gc_t *gc, int rgn ) 
{
  msgc_context_t *init_context, *context;
  struct popularity_analysis_data my_data;
  int marked, traced, words_marked;
  int range;
  
  /* figure out bounds of the region's objects */
  init_context = msgc_begin( gc );
  msgc_set_object_visitor( init_context, find_bounds_fcn, &my_data );
  my_data.gc      = gc;
  my_data.rgn     = rgn;
  my_data.fst_obj = (word)-1;
  my_data.fin_obj = (word)0;
  my_data.init_context = init_context;
  msgc_mark_objects_from_roots( init_context, &marked, &traced, &words_marked );
  range = 1 + (popularity_analysis_addr2index(&my_data, my_data.fin_obj) -
	       popularity_analysis_addr2index(&my_data, my_data.fst_obj));

  /* calculate popularity of each object in the region. */
  context = msgc_begin( gc );

  if (popularity_analysis_remset == NULL)
    popularity_analysis_remset = create_remset( 0, 0 );
  else
    rs_clear( popularity_analysis_remset );

  my_data.popularity_len = range;
  my_data.popularity = (int*)must_malloc( range*sizeof(int) );
  my_data.summary_remset = DATA(gc)->remset_summary;
  assert(my_data.popularity != NULL);
  msgc_set_object_visitor( context, calc_popularity_fcn, &my_data );
  msgc_mark_objects_from_roots( context, &marked, &traced, &words_marked );

  consolemsg(""); /* A blank line to make output more readable... */
  {
    my_data.live_extra_objects_in_summary_remset = 0;
    my_data.dead_extra_objects_in_summary_remset = 0;
    my_data.dead_extra_objects_in_region = 
      (int*)must_malloc( gc->remset_count*sizeof(int) );
    { 
      int i;
      for( i = 0; i < gc->remset_count; i++ ) {
	my_data.dead_extra_objects_in_region[i] = 0;
      }
    }
    rs_enumerate( DATA(gc)->remset_summary, 
		  categorize_extra_rs_members, 
		  &my_data );
    if (my_data.live_extra_objects_in_summary_remset != 0) {
      consolemsg("summary remset size: %8d"
		 " marker remset size: %8d"
		 " extra live objects: %8d"
		 " extra dead objects: %8d",
		 DATA(gc)->remset_summary->live,
		 popularity_analysis_remset->live,
		 my_data.live_extra_objects_in_summary_remset,
		 my_data.dead_extra_objects_in_summary_remset);
    } else {
      consolemsg("summary remset size: %8d"
		 " marker remset size: %8d"
		 " extra dead objects: %8d",
		 DATA(gc)->remset_summary->live,
		 popularity_analysis_remset->live,

		 my_data.dead_extra_objects_in_summary_remset);
    }
    { 
      int i;
      for( i=0; i < gc->remset_count; i++ ) {
	consolemsg(" dead_objects[%3d]: %8d", 
		   i, 
		   my_data.dead_extra_objects_in_region[i] );
      }
    }
    free( my_data.dead_extra_objects_in_region );
    my_data.dead_extra_objects_in_region = NULL;
  }
  { 
    int i;
    int entries = 0;
    int entries_due_to_popular_objects = 0;
    for( i = 0; i < my_data.popularity_len; i++ ) {
      entries += my_data.popularity[i];
      if (my_data.popularity[i] > 100) {
	word *ptr = (word*) popularity_analysis_index2addr(&my_data, i);
	word w = *ptr;
	if ( ishdr( w )) {
	  word h = header(w);
	  int s = sizefield(w);
	  char* type;
	  if (h == BV_HDR) {
	    type = "BVEC";
	  } else if (h == VEC_HDR) {
	    type = " VEC";
	  } else if (h == PROC_HDR) {
	    type = "PROC";
	  }
	  consolemsg( "popularity 0x%08x (%s[%d]): %8d", 
		      ptr, type, s, my_data.popularity[i] );
	} else {
	  consolemsg( "popularity 0x%08x (PAIR): %8d", 
		      ptr, my_data.popularity[i] );
	}
	
	entries_due_to_popular_objects += my_data.popularity[i];
      }
    }
    consolemsg( " pop analysis rgn %d:"
		" %d of (summary: %d, mark: %d, popset: %d)"
		" to pop objects",
		rgn, 
		entries_due_to_popular_objects, 
		DATA(gc)->remset_summary->live,
		entries, 
		popularity_analysis_remset->live );
  }
  free(my_data.popularity);
  msgc_end( context );
  msgc_end( init_context );
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
      int rgn_to, rgn_next, nursery_sz, rgn_to_cur, rgn_to_max;
      int num_rgns = DATA(gc)->region_count;
      
    collect_evacuate_nursery:
      rgn_to = DATA(gc)->rrof_to_region;
      rgn_next = DATA(gc)->rrof_next_region;

      annoyingmsg("collect_rgnl decide major or minor.  to: %d next: %d",
		  rgn_to, rgn_next );

      nursery_sz = gc_allocated_to_areas( gc, gset_singleton( 0 ));
      rgn_to_cur = gc_allocated_to_areas( gc, gset_singleton( rgn_to ));
      rgn_to_max = gc_maximum_allotted( gc, gset_singleton( rgn_to ));

      annoyingmsg( "collect_rgnl rgn_to: %d rgn_next: %d nursery_sz: %d "
		   "rgn_to_cur: %d rgn_to_max: %d ", 
		   rgn_to, rgn_next, nursery_sz, rgn_to_cur, rgn_to_max );
      
      if (rgn_to == rgn_next /* && summarization is complete */) {
	/* ideal case for major collect */
	int rgn_idx = rgn_next;
	int n;

	process_seqbuf( gc, gc->ssb );
	if (DATA(gc)->ephemeral_area[ rgn_idx-1 ]->has_popular_objects) {
	  if (ANALYZE_POPULARITY) {
	    /* (building summary is just to gather stats to feed to
	     * popularity analysis; it is not necessary for skipping
	     * collection of the popular region. */
	    build_remset_summary( gc, rgn_idx );
	    popularity_analysis( gc, rgn_idx );
	    invalidate_remset_summary( gc );
	  }
	  n = next_rgn(DATA(gc)->rrof_next_region, num_rgns);
	  if (n == DATA(gc)->rrof_first_region) {
	    /* completed a collection cycle */
	    if (DATA(gc)->region_count != DATA(gc)->ephemeral_area_count) {
	      num_rgns = DATA(gc)->ephemeral_area_count;
	      n = DATA(gc)->region_count;
	      DATA(gc)->region_count = num_rgns;
	      DATA(gc)->rrof_first_region = n;
	    }
	  }
	  DATA(gc)->rrof_next_region = n;
	  DATA(gc)->rrof_to_region = n;
	  goto collect_evacuate_nursery;
	}
	build_remset_summary( gc, rgn_idx );

	/* Temporary detective code: if the summary is overly large,
	 * get more info on the popularity of the objects in region.
	 */
	if ( ANALYZE_POPULARITY &&
	     DATA(gc)->remset_summary->live > POPULARITY_LIMIT) {
	  consolemsg( "   large summary for region %d: %d objects", 
		      rgn_idx, 
		      DATA(gc)->remset_summary->live );
	  popularity_analysis( gc, rgn_idx );
	}
	
	if ( ! NO_COPY_COLLECT_FOR_POP_RGNS ||
	     DATA(gc)->remset_summary->live <= POPULARITY_LIMIT ) {
	  if (USE_ORACLE_TO_UPDATE_REMSETS) 
	    gc->scan_update_remset = FALSE;
	  oh_collect( DATA(gc)->ephemeral_area[ rgn_idx-1 ], GCTYPE_COLLECT );
	  invalidate_remset_summary( gc );
	  DATA(gc)->rrof_last_tospace = rgn_idx;
	  
	  n = next_rgn(DATA(gc)->rrof_next_region,  num_rgns);
	  /* If we're about to start from the beginning of the array, 
	   * then we are guaranteed to attempt to collect all regions 
	   * in current cycle before newly generated regions.
	   * Therefore it is safe to update the region_count. */
	  if (n < DATA(gc)->rrof_next_region)
	    DATA(gc)->region_count = DATA(gc)->ephemeral_area_count;
	  DATA(gc)->rrof_next_region = n;
	} else {
	  DATA(gc)->ephemeral_area[ rgn_idx-1 ]->has_popular_objects = TRUE;
	  invalidate_remset_summary( gc );

	  n = next_rgn(DATA(gc)->rrof_next_region, num_rgns);
	  if (n == DATA(gc)->rrof_first_region) {
	    /* completed a collection cycle. */
	    if (DATA(gc)->region_count != DATA(gc)->ephemeral_area_count) {
	      num_rgns = DATA(gc)->ephemeral_area_count;
	      n = DATA(gc)->region_count;
	      DATA(gc)->region_count = num_rgns;
	      DATA(gc)->rrof_first_region = n;
	    }
	  }
	  DATA(gc)->rrof_next_region = n;
	  DATA(gc)->rrof_to_region = n;
	  goto collect_evacuate_nursery;
	}
      } else if (rgn_to_cur + nursery_sz < rgn_to_max) {
	/* if there's room, minor collect the nursery into current region. */
	int rgn_idx = rgn_to; 

	process_seqbuf( gc, gc->ssb );
	build_remset_summary( gc, 0 );
	if (USE_ORACLE_TO_UPDATE_REMSETS) 
	  gc->scan_update_remset = FALSE;
	oh_collect( DATA(gc)->ephemeral_area[ rgn_idx-1 ], GCTYPE_PROMOTE );
	invalidate_remset_summary( gc );
	DATA(gc)->rrof_last_tospace = rgn_idx;

	/* TODO: add code to incrementally summarize by attempting to
	 * predict how many minor collections will precede the next
	 * major collection. */
      } else {
	int n;
	/* the to-space is full, so shift to the next to-space */
	annoyingmsg("collect_rgnl shift to next to-space %d => %d",
		    DATA(gc)->rrof_to_region,
		    next_rgn(DATA(gc)->rrof_to_region,  num_rgns ));
	n = next_rgn(DATA(gc)->rrof_to_region,  num_rgns);
	if (n < DATA(gc)->rrof_to_region)
	  DATA(gc)->region_count = DATA(gc)->ephemeral_area_count;
	DATA(gc)->rrof_to_region = n;
	/* TODO: double check that minor gc's haven't filled up to-spaces
	 * so fast that major GC hasn't had a chance to go (which should
	 * only happen when a summary is abandoned. */
	goto collect_evacuate_nursery;
#if 0
      } else {
	/* if there's not room, major collect "eldest" region. */
	int rgn_idx = DATA(gc)->rrof_next_region;
	old_heap_t *oh = DATA(gc)->ephemeral_area[ rgn_idx-1 ];
	semispace_t *tospace = oh_current_space( oh );
	annoyingmsg("collect_rgnl major collect of %d", rgn_idx);
	gclib_stopcopy_collect_genset( gc, gset_singleton( rgn_idx ), tospace );
#endif
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

  if (data->secondary_space != NULL) {
    int gen_no = data->secondary_space->gen_no;
    oh_assimilate( data->ephemeral_area[ gen_no-1 ],
		   data->secondary_space );
    data->secondary_space = NULL;
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

static void check_remset_invs_rgnl( gc_t *gc, word src, word tgt ) 
{
  supremely_annoyingmsg( "check_remset_invs_rgnl( gc, 0x%08x (%d), 0x%08x (%d) )", 
			 src, src?gen_of(src):0, tgt, gen_of(tgt) );
  assert( src == 0 ||
	  gen_of(src) != gen_of(tgt) ||
	  gen_of(src) != 0 ||
	  gen_of(tgt) != DATA(gc)->static_generation ||
	  rs_isremembered( gc->remset[ gen_of(src) ], tgt ));
}
static void check_remset_invs( gc_t *gc, word src, word tgt ) 
{
  supremely_annoyingmsg( "check_remset_invs( gc, 0x%08x (%d), 0x%08x (%d) )", 
			 src, src?gen_of(src):0, tgt, gen_of(tgt) );
  assert( !src || 
	  gen_of(src)  < gen_of(tgt) ||
	  gen_of(src) != 0 ||
	  gen_of(tgt) != DATA(gc)->static_generation ||
	  rs_isremembered( gc->remset[ gen_of(src) ], src ));
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

  if (USE_ORACLE_TO_VERIFY_REMSETS)
    verify_remsets_via_oracle( gc );

  /* For debugging of prototype;
   * double check heap consistency via mark/sweep routines.
   * (before collection means that mutator introduced inconsistency) */
#if CHECK_HEAP_INVARIANTS
  {
    int marked, traced, words_marked;
    msgc_context_t *msgc_ctxt = msgc_begin( gc );
    supremely_annoyingmsg("before GC, heap consistency check");
    msgc_mark_objects_from_roots( msgc_ctxt, &marked, &traced, &words_marked );
    msgc_end( msgc_ctxt );
  }
#endif

  yh_before_collection( gc->young_area );
  for ( e=0 ; e < DATA(gc)->ephemeral_area_count ; e++ )
    oh_before_collection( DATA(gc)->ephemeral_area[ e ] );
  if (DATA(gc)->dynamic_area)
    oh_before_collection( DATA(gc)->dynamic_area );
}

static void after_collection( gc_t *gc )
{
  int e;

  DATA(gc)->generations = DATA(gc)->generations_after_gc;

 if (USE_ORACLE_TO_UPDATE_REMSETS)
   update_remsets_via_oracle( gc );
 if (USE_ORACLE_TO_VERIFY_REMSETS)
   verify_remsets_via_oracle( gc );

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
    msgc_mark_objects_from_roots( msgc_ctxt, &marked, &traced, &words_marked );
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

  if (DATA(gc)->remset_summary_valid) {
    /* If summarization complete, then just use that instead
     * of iterating over the remset array. */
    rs_enumerate( DATA(gc)->remset_summary, f, fdata );
    return;
  }

  /* Felix is pretty sure that this method is intended only
   * for use by clients who are always attempting to enumerate
   * the elements of the static area's remembered set,
   * and therefore the static area should not be in gset. 
   */
  assert2( ! gset_memberp( DATA(gc)->static_generation, gset ));

  /* Add elements to regions outside collection set. 
   *
   * I might need to extend this interface in some way so that we
   * don't waste time adding elements to remset[gno] for 
   * gno <= generation
   */
  process_seqbuf( gc, gc->ssb );

  /* (ephemeral_area_count may be incremented by f, so we have to
   * checkpoint its value here.) */
  ecount = DATA(gc)->ephemeral_area_count;
  
  /* The ecount corresponds to the largest region index
   * in the ephemeral_area array. */
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
    return rs_isremembered( gc->remset[g], w );
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

  stats_add_gclib_stats( &stats_gclib );

  stats_dumpstate();		/* Dumps stats state if dumping is on */
}

static int compact_all_ssbs( gc_t *gc )
{
  int overflowed, i;

  overflowed = process_seqbuf( gc, gc->ssb );
  return overflowed;
}

static void np_remset_ptrs( gc_t *gc, word ***ssbtop, word ***ssblim )
{
  if (gc->np_remset != -1) {
    *ssbtop = &DATA(gc)->ssb_top;
    *ssblim = &DATA(gc)->ssb_lim;
  }
  else {
    *ssbtop = *ssblim = 0;
  }
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

static void expand_remset_gnos( gc_t *gc, int fresh_gno )
{
  int i;
  int new_remset_count = gc->remset_count + 1;
  remset_t** new_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*new_remset_count );
  assert( fresh_gno < new_remset_count );

  for( i = 0; i < fresh_gno; i++ ) {
    new_remset[i] = gc->remset[i];
  }
  new_remset[fresh_gno] = create_remset( 0, 0 );
  for( i = fresh_gno+1; i < new_remset_count; i++ ) {
    new_remset[i] = gc->remset[i-1];
  }

  free( gc->remset );
  gc->remset = new_remset;
  gc->remset_count = new_remset_count;
  
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

  ss_sync( current_space );
  cur_allocated = 
    current_space->used+los_bytes_used( gc->los, current_space->gen_no );

  if (cur_allocated + expansion_amount <= max_allocated) {
    ss_expand( current_space, expansion_amount );
    return current_space;
  } else {
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
    assert( area_thinks_allocated == retval );
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

static int ssb_process( gc_t *gc, word *bot, word *top, void *ep_data ) {
  remset_t **remset = gc->remset;
  return rs_add_elems( remset, bot, top );
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

  gc->remset[0] = (void*)0xDEADBEEF;
  for ( i = 1 ; i < gc->remset_count ; i++ ) {
    gc->remset[i] =
      create_remset( info->rhash, 0 );
  }
  gc->ssb =
    create_seqbuf( /* XXX the remset_count factor is an attempt to 
                    * correct for comparison with non-refactored. XXX */
                   info->ssb*gc->remset_count, 
		   &data->ssb_bot, &data->ssb_top, &data->ssb_lim, 
		   ssb_process, 0 );

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
    data->region_count = e;
    data->fixed_ephemeral_area = FALSE;
    data->ephemeral_area = (old_heap_t**)must_malloc( e*sizeof( old_heap_t* ));
    
    for ( i = 0; i < e; i++ ) {
      assert( info->ephemeral_info[i].size_bytes > 0 );
      size += info->ephemeral_info[i].size_bytes;
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
    gc->remset = (remset_t**)must_malloc( sizeof( remset_t* )*gc->remset_count );
    gc->remset[0] = (void*)0xDEADBEEF;
    for ( i = 1 ; i < gc->remset_count ; i++ ) {
      gc->remset[i] =
	create_remset( info->rhash, 0 );
    }

    data->remset_summary = create_remset( 0, 0 );

    gc->ssb =
      create_seqbuf( /* XXX the remset_count factor is an attempt to 
		      * correct for comparison with non-refactored. XXX */
		    info->ssb*gc->remset_count, 
		    &data->ssb_bot, &data->ssb_top, &data->ssb_lim, 
		    ssb_process, 0 );
  }

  gc->id = strdup( buf );

  return gen_no;
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
    my_collect = collect;
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
  data->rrof_next_region = 2;
  data->rrof_last_tospace = -1;
  data->rrof_first_region = 1;

  data->remset_summary = 0;
  data->remset_summary_valid = FALSE;

  ret = 
    create_gc_t( "*invalid*",
		 (void*)data,
		 initialize, 
		 allocate,
		 allocate_nonmoving,
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
		 is_address_mapped,
		 my_check_remset_invs
		 );
  ret->scan_update_remset = info->is_regional_system;
  return ret;
}

/* eof */
