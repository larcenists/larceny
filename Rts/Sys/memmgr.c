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
#include "gclib.h"
#include "heapio.h"
#include "barrier.h"
#include "stack.h"

typedef struct gc_data gc_data_t;

/* The 'remset' table in the gc structure has one extra element if the
   collector uses the non-predictive dynamic area: that extra element is
   the young->old remembered set for the non-predictive collector.  The
   extra element is always the last one, and the slots in the SSB tables
   for that remset are the last slots.  The index is indicated by the
   np_remset member of gc_t.
   */

struct gc_data {
  bool is_generational_system;  /* True if system has multiple generations */
  bool uses_dof_collector;      /* True if system uses DOF collector */
  bool uses_np_collector;       /* True if dynamic area is non-predictive */
  bool shrink_heap;		/* True if heap can be shrunk */
  int  dynamic_min;		/* 0 or lower limit of expandable area */
  int  dynamic_max;		/* 0 or upper limit of expandable area */
  int  nonexpandable_size;	/* Size of nonexpandable areas */

  word *globals;
  word *handles;               /* array of handles */
  int  nhandles;               /* current array length */
  int  in_gc;                  /* a counter: > 0 means in gc */
  int  generations;            /* number of generations (incl. static) */
  int  static_generation;	/* Generation number of static area */
  bool have_stats;		/* For DOF collection */
  word **ssb_bot;
  word **ssb_top;
  word **ssb_lim;
};

#define DATA(gc) ((gc_data_t*)(gc->data))


static gc_t *alloc_gc_structure( word *globals, gc_param_t *info );
static word *load_text_or_data( gc_t *gc, int size_bytes, int load_text );
static int allocate_generational_system( gc_t *gc, gc_param_t *params );
static int allocate_stopcopy_system( gc_t *gc, gc_param_t *info );
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

  assert( info->is_generational_system || info->is_stopcopy_system );

  gclib_init();
  gc = alloc_gc_structure( info->globals, info );

  /* Number of generations includes static heap, if any */
  if (info->is_generational_system) 
    *generations = allocate_generational_system( gc, info );
  else
    *generations = allocate_stopcopy_system( gc, info );

  DATA(gc)->generations = *generations;

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

  M = max( M, lower_limit );

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
    ans[0] = data->is_generational_system;
    ans[1] = data->generations;
  }
  else {
    /* info about generation op-1
       ans[0] = type: 0=nursery, 1=two-space, 2=np-old, 3=np-young, 4=static,
                      6=one-space-dof
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
      if (data->is_generational_system) {
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
    else if (op-1 < gc->ephemeral_area_count) {
      ans[0] = 1;
      ans[1] = gc->ephemeral_area[op-1]->maximum;
      ans[2] = 0;
    }
    else if (op < data->static_generation &&
	     gc->dynamic_area &&
	     data->uses_np_collector) {
      int k, j;

      /* Non-predictive dynamic area */
      np_gc_parameters( gc->dynamic_area, &k, &j );
      ans[2] = k;
      ans[3] = j;
      if (op-1 == gc->ephemeral_area_count) {
	/* NP old */
	ans[0] = 2;
	ans[1] = (gc->dynamic_area->maximum / k) * (k - j);
      }
      else {
	ans[0] = 3;
	ans[1] = (gc->dynamic_area->maximum / k) * j;
      }
    }
    else if (op < data->static_generation &&
	     gc->dynamic_area &&
	     data->uses_dof_collector) {
      /*  DOF area -- fixed number of same-size generations */
      int size;

      dof_gc_parameters( gc->dynamic_area, &size );
      ans[0] = 6;
      ans[1] = size;
      ans[2] = 1;		/* Expandable */
    }
    else if (op-1 == gc->ephemeral_area_count && gc->dynamic_area) {
      /* Dynamic area */
      ans[0] = 1;
      ans[1] = gc->dynamic_area->maximum;
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

  if (gc->ephemeral_area)
    for ( i = 0 ; i < gc->ephemeral_area_count  ; i++ )
      if (!oh_initialize( gc->ephemeral_area[i] ))
	return 0;

  if (gc->dynamic_area)
    if (!oh_initialize( gc->dynamic_area ))
      return 0;

  if (gc->static_area)
    if (!sh_initialize( gc->static_area ))
      return 0;

  if (data->is_generational_system) {
    wb_setup( gclib_desc_g,
	      (unsigned*)gclib_pagebase,
	      data->generations,
	      data->globals,
	      data->ssb_top,
	      data->ssb_lim, 
	      (data->uses_np_collector ? data->generations-1 : -1 ),
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
    panic( "Can't allocate an object of size %d bytes: max is %d bytes.",
	   nbytes, LARGEST_OBJECT );

  return yh_allocate( gc->young_area, nbytes, no_gc );
}

static word *allocate_nonmoving( gc_t *gc, int nbytes, bool atomic )
{
  assert( nbytes > 0 );

  if (gc->static_area == 0)
    panic( "Cannot allocate nonmoving in a system without a static heap." );

  nbytes = roundup_balign( nbytes );
  if (nbytes > LARGEST_OBJECT)
    panic( "Can't allocate an object of size %d bytes: max is %d bytes.",
	   nbytes, LARGEST_OBJECT );

  return sh_allocate( gc->static_area, nbytes );
}

/* Shuffle remembered sets
   Shuffle SSB pointer table entries
   Shuffle the pointers in the remsets to the ssb pointer tables
   
   The permutation encodes the destination: v[i] -> v[perm[i]]
   */
static void permute_remembered_sets( gc_t *gc, int permutation[] )
{
  struct {
    remset_t *r;
    word *ssb_bot;
    word *ssb_top;
    word *ssb_lim;
  } tmp[ MAX_GENERATIONS ];
  int i, j;
  remset_t *r;
  gc_data_t *data = DATA(gc);

  for ( i=1 ; i < gc->remset_count ; i++ ) {
    tmp[i].r = gc->remset[i];
    tmp[i].ssb_bot = data->ssb_bot[i];
    tmp[i].ssb_top = data->ssb_top[i];
    tmp[i].ssb_lim = data->ssb_lim[i];
  }

  for ( i=1 ; i < gc->remset_count ; i++ ) {
    j = permutation[i];
    assert( j > 0 );
    gc->remset[j] = r = tmp[i].r;       /* remset */
    data->ssb_bot[j] = tmp[i].ssb_bot;  /* ssb pointers */
    data->ssb_top[j] = tmp[i].ssb_top;
    data->ssb_lim[j] = tmp[i].ssb_lim;
    r->ssb_bot = data->ssb_bot + j;     /* ssb pointer locations */
    r->ssb_top = data->ssb_top + j;
    r->ssb_lim = data->ssb_lim + j;
  }
}

static void collect( gc_t *gc, int gen, int bytes_needed, gc_type_t request )
{
  gclib_stats_t stats;
  gc_data_t *data = DATA(gc);

  assert( gen >= 0 );
  assert( gen > 0 || bytes_needed >= 0 );

  assert( data->in_gc >= 0 );

  if (data->in_gc++ == 0) {
    data->globals[ G_GC_CNT ] += fixnum(1);
    if (data->globals[ G_GC_CNT ] == 0)
      hardconsolemsg( "Congratulations! "
		      "You have survived 1,073,741,824 garbage collections!" );
    before_collection( gc );
  }

  if (gen == 0)
    yh_collect( gc->young_area, bytes_needed, request );
  else if (gen-1 < gc->ephemeral_area_count)
    oh_collect( gc->ephemeral_area[ gen-1 ], request );
  else if (gc->dynamic_area)
    oh_collect( gc->dynamic_area, request );
  else
    yh_collect( gc->young_area, bytes_needed, request );

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

/* DOF collection is a little different. */
/* hack */
void gc_start_gc( gc_t *gc )
{
  gc_data_t *data = DATA(gc);

  if (!data->have_stats) {
    data->have_stats = TRUE;
    data->globals[ G_GC_CNT ] += fixnum(1);
    before_collection( gc );
  }
}

void gc_end_gc( gc_t *gc )
{
  gc_data_t *data = DATA(gc);

  if (data->have_stats) {
    data->have_stats = FALSE;
    after_collection( gc );
  }
}

static void 
dof_collect( gc_t *gc, int gen, int bytes_needed, gc_type_t request )
{
  gc_data_t *data = DATA(gc);
  gclib_stats_t stats;

  assert( gen >= 0 );
  assert( gen > 0 || bytes_needed >= 0 );

  data->in_gc++;
  gc_start_gc( gc );

  if (gen == 0)
    yh_collect( gc->young_area, bytes_needed, request );
  else if (gen-1 < gc->ephemeral_area_count)
    oh_collect( gc->ephemeral_area[ gen-1 ], request );
  else if (gc->dynamic_area)
    oh_collect( gc->dynamic_area, request );
  else
    yh_collect( gc->young_area, bytes_needed, request );

  gc_end_gc( gc );
  if (--data->in_gc == 0) {
    stats_following_gc( gc );

    gclib_stats( &stats );
    annoyingmsg( "  Memory usage: heap %d, remset %d, RTS %d words",
		 stats.heap_allocated, stats.remset_allocated,
		 stats.rts_allocated );
    annoyingmsg( "  Max heap usage: %d words", 
		 stats.heap_allocated_max );
  }
}

static void before_collection( gc_t *gc )
{
  int e;

  yh_before_collection( gc->young_area );
  for ( e=0 ; e < gc->ephemeral_area_count ; e++ )
    oh_before_collection( gc->ephemeral_area[ e ] );
  if (gc->dynamic_area)
    oh_before_collection( gc->dynamic_area );
}

static void after_collection( gc_t *gc )
{
  int e;

  yh_after_collection( gc->young_area );
  for ( e=0 ; e < gc->ephemeral_area_count ; e++ )
    oh_after_collection( gc->ephemeral_area[ e ] );
  if (gc->dynamic_area)
    oh_after_collection( gc->dynamic_area );
}

static void set_policy( gc_t *gc, int gen, int op, int value )
{
  if (gen == 0)
    yh_set_policy( gc->young_area, op, value );
  else if (gen-1 <= gc->ephemeral_area_count)
    oh_set_policy( gc->ephemeral_area[gen-1], op, value );
  else if (gc->dynamic_area)
    oh_set_policy( gc->dynamic_area, op, value );
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
enumerate_remsets_older_than( gc_t *gc,
			      int generation,
			      bool (*f)(word obj, void *data, unsigned *count),
			      void *fdata,
			      bool enumerate_np_young )
{
  int i;

  if (!DATA(gc)->is_generational_system) return;

  for ( i = generation+1 ; i < DATA(gc)->generations ; i++ ) {
    rs_compact( gc->remset[i] );
    rs_enumerate( gc->remset[i], f, fdata );
  }

  if (enumerate_np_young) {
    rs_compact( gc->remset[ gc->np_remset ] );
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
  else if (gc->dynamic_area)
    return oh_data_load_area( gc->dynamic_area, nbytes );
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

  g = gclib_desc_g[ pageof( w ) ];
  assert( g >= 0 && g < gc->remset_count );
  if (g > 0)
    return rs_isremembered( gc->remset[g], w );
  else
    return 0;
}
#endif

/* Not a method anymore */
static void stats( gc_t *gc )
{
  assert( 0 );
}

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

  for ( i=0 ; i < gc->ephemeral_area_count ; i++ )
    oh_stats( gc->ephemeral_area[i] );

  if (gc->dynamic_area)
    oh_stats( gc->dynamic_area );

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
  stats_add_gclib_stats( &stats_gclib );

  stats_dumpstate();		/* Dumps stats state if dumping is on */
}

static int compact_all_ssbs( gc_t *gc )
{
  int overflowed, i;

  overflowed = 0;
  for ( i=1 ; i < gc->remset_count ; i++ )
    overflowed = rs_compact( gc->remset[i] ) || overflowed;
  return overflowed;
}

static void compact_np_ssb( gc_t *gc )
{
  if (gc->np_remset != -1)
    rs_compact_nocheck( gc->remset[gc->np_remset] );
}

static void np_remset_ptrs( gc_t *gc, word ***ssbtop, word ***ssblim )
{
  if (gc->np_remset != -1) {
    *ssbtop = &DATA(gc)->ssb_top[gc->np_remset];
    *ssblim = &DATA(gc)->ssb_lim[gc->np_remset];
  }
  else {
    *ssbtop = *ssblim = 0;
  }
}

static int dump_image( gc_t *gc, const char *filename, bool compact )
{
  if (DATA(gc)->is_generational_system) 
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

static int allocate_stopcopy_system( gc_t *gc, gc_param_t *info )
{
  char buf[ 100 ];

  DATA(gc)->dynamic_max = info->sc_info.dynamic_max;
  DATA(gc)->dynamic_min = info->sc_info.dynamic_min;

  gc->young_area = create_sc_heap( 0, gc, &info->sc_info, info->globals );
  strcpy( buf, gc->young_area->id );

  if (info->use_static_area) {
    gc->static_area = create_static_area( 1, gc );
    strcat( buf, "+" );
    strcat( buf, gc->static_area->id );
  }

  gc->id = strdup( buf );
  return (info->use_static_area ? 2 : 1);
}

static int allocate_generational_system( gc_t *gc, gc_param_t *info )
{
  char buf[ 256 ], buf2[ 100 ];
  int gen_no, i, size;
  gc_data_t *data = DATA(gc);

  gen_no = 0;
  data->is_generational_system = 1;
  data->uses_dof_collector = info->use_dof_collector;
  data->uses_np_collector = info->use_non_predictive_collector;
  size = 0;

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
  size += info->nursery_info.size_bytes;
  gen_no += 1;
  strcpy( buf, gc->young_area->id );

  /* Create ephemeral areas. 
     */
  { int e = gc->ephemeral_area_count = info->ephemeral_area_count;
    gc->ephemeral_area =
      (old_heap_t**)must_malloc( e*sizeof( old_heap_t* ) );

    for ( i = 0 ; i < e ; i++ ) {
      size += info->ephemeral_info[i].size_bytes;
      gc->ephemeral_area[ i ] = 
	create_sc_area( gen_no, gc, &info->ephemeral_info[i], 1 );
      gen_no += 1;
    }
  }
  if (gc->ephemeral_area_count > 0) {
    sprintf( buf2, "+%d*%s",
	     gc->ephemeral_area_count, gc->ephemeral_area[0]->id );
    strcat( buf, buf2 );
  }

  data->nonexpandable_size = size;

  /* Create dynamic area.
     */
  if (info->use_non_predictive_collector) {
    int gen_allocd;

    gc->dynamic_area = 
      create_np_dynamic_area( gen_no, &gen_allocd, gc, &info->dynamic_np_info);
    gen_no += gen_allocd;
  }
  else if (info->use_dof_collector) {
    int gen_allocd;

    gc->dynamic_area = 
      create_dof_area( gen_no, &gen_allocd, gc, &info->dynamic_dof_info );
    gen_no += gen_allocd;
  }
  else {
    gc->dynamic_area = create_sc_area( gen_no, gc, &info->dynamic_sc_info, 0 );
    gen_no += 1;
  }
  strcat( buf, "+" );
  strcat( buf, gc->dynamic_area->id );

  /* Create static area.
     */
  if (info->use_static_area) {
    gc->static_area = create_static_area( gen_no, gc );
    data->static_generation = gen_no;
    gen_no += 1;
    strcat( buf, "+" );
    strcat( buf, gc->static_area->id );
  }

  /* Create remembered sets and SSBs.  Entry 0 is not used.
     If the non-predictive area is used, then the last remembered set
     is the non-predictive 'extra' set (contains only young->old pointers).
     */
  if (info->use_non_predictive_collector)
    gc->remset_count = gen_no + 1;
  else
    gc->remset_count = gen_no;

  data->ssb_bot = (word**)must_malloc( sizeof( word* )*gc->remset_count );
  data->ssb_top = (word**)must_malloc( sizeof( word* )*gc->remset_count );
  data->ssb_lim = (word**)must_malloc( sizeof( word* )*gc->remset_count );
  gc->remset = (remset_t**)must_malloc( sizeof( remset_t* )*gc->remset_count );

  data->ssb_bot[0] = 0;
  data->ssb_top[0] = 0;
  data->ssb_lim[0] = 0;

  gc->remset[0] = (void*)0xDEADBEEF;
  for ( i = 1 ; i < gc->remset_count ; i++ )
    gc->remset[i] =
      create_remset( info->rhash, 0, info->ssb,
		     &data->ssb_bot[i], &data->ssb_top[i], &data->ssb_lim[i] );

  if (info->use_non_predictive_collector)
    gc->np_remset = gc->remset_count - 1;

  gc->id = strdup( buf );

  return gen_no;
}

static gc_t *alloc_gc_structure( word *globals, gc_param_t *info )
{
  gc_data_t *data;
  
  data = (gc_data_t*)must_malloc( sizeof( gc_data_t ) );

  data->globals = globals;
  data->is_generational_system = 0;
  data->shrink_heap = 0;
  data->in_gc = 0;
  data->have_stats = 0;
  data->handles = (word*)must_malloc( sizeof(word)*10 );
  data->nhandles = 10;
  memset( data->handles, 0, sizeof(word)*data->nhandles );
  data->ssb_bot = 0;
  data->ssb_top = 0;
  data->ssb_lim = 0;
  data->dynamic_max = 0;
  data->dynamic_min = 0;
  data->nonexpandable_size = 0;

  return 
    create_gc_t( "*invalid*",
		 data,
		 initialize, 
		 allocate,
		 allocate_nonmoving,
		 (info->use_dof_collector ? dof_collect : collect),
		 permute_remembered_sets,
		 set_policy,
		 data_load_area,
		 text_load_area,
		 iflush,
		 creg_get,
		 creg_set,
		 stack_overflow,
		 stack_underflow,
		 stats,
		 compact_all_ssbs,
#if defined(SIMULATE_NEW_BARRIER)
		 isremembered,
#endif
		 compact_np_ssb,
		 np_remset_ptrs,
		 0,		/* load_heap */
		 dump_image,
		 make_handle,
		 free_handle,
		 enumerate_roots,
		 enumerate_remsets_older_than );
}

/* eof */
