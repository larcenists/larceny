/* Rts/Sys/new-policy.c.
 * Larceny run-time system -- memory manager.
 *
 * $Id: memmgr.c,v 1.22 1997/05/31 01:38:14 lth Exp lth $
 *
 * This file contains procedures for memory allocatation and garbage 
 * collection policy.
 *
 * The algorithm for how policy decisions are made is described in memmgr.h.
 *
 * Musings:
 *
 * I think it would be reasonable to export all the heaps in the interface,
 * so that we can get rid of some of the obvious trampolinish methods on
 * the collector.
 *
 * Dept. of Magic:
 *
 * On systems where the low-level allocator has some control over the
 * addresses being assigned to memory blocks (i.e., where we can guarantee
 * that memory allocated "early" will have lower addresses than all
 * memory allocated later), the code generator is allowed to take advantage
 * of the allocation order of the heaps: on such systems, the ephemeral
 * semispaces will be allocated at a lower address than all other heap
 * memory.  The write barrier can therefore use a fast check to see
 * whether more tests are necessary.
 */

const char *gc_technology = "precise";

#define GC_INTERNAL        /* certain globals and functions are not visible */

#include <string.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

#define MAX_GENERATIONS (2*MAX_HEAPS)

typedef struct gc_data gc_data_t;

/* NOTE!  Some internal tables exist that map heap numbers and
 * generation numbers to misc. data.  The non-predictive collector must
 * be _extremely_ careful if it wants to renumber generations!  We
 * need to discover what types of renumberings are legal and under
 * which conditions.
 */ 

/* What a mess. */

struct gc_data {
  word          *globals;                       /* globals[] array */
  int           critical_section;               /* 1 if doing a gc */
  unsigned      oldest_generation;              /* Index of oldest gen. */
  unsigned      oldest_collectable_generation;  /* May be smaller */
  int           *prom_bits;                     /* [0..number_of_old_heaps] */

  young_heap_t  *young_heap;

  int           number_of_old_heaps;
  old_heap_t    **old_heaps;                    /* [0..number_of_old_heaps] */
  struct old_gen {
    remset_t *remset;
    old_heap_t *heap;
  } old_gen[ MAX_GENERATIONS ];

  remset_t      *remsets[ MAX_GENERATIONS ];    /* [0..oldest_generation] */
  word          **ssb_bot;                      /* ditto */
  word          **ssb_top;                      /* ditto */
  word          **ssb_lim;                      /* ditto */

  /* For non-predictive collector */
  remset_t      *np_remset;                     /* Extra remset for NP heap */
  int           np_heap_no;                     /* -1 or index of np heap */
  int           np_gen_no;                      /* index of 'old' gen */
  int           collecting_np_heap;             /* state variable */
  int           np_ssbidx;
/*
  word          *np_ssbbot;
  word          *np_ssbtop;
  word          *np_ssblim;
*/

  static_heap_t *static_heap;                   /* Static heap or 0 */
};

#define DATA(gc) ((gc_data_t*)(gc->data))


static void allocate_generational_system( gc_param_t *params, gc_t *gc );
static void allocate_stopcopy_system( gc_param_t *params, gc_t *gc );
static void allocate_young_heap( gc_param_t *params, gc_t *gc, int *gen_no );
static void allocate_old_heaps( int number_of_old_heaps, gc_param_t *params, 
			        gc_t *gc, int *gen_no, int *heap_no );
static void allocate_static_heap( gc_param_t *params, gc_t *gc, int *gen_no, 
				  int *heap_no );
static void allocate_stopcopy_heap( gc_param_t *params, gc_t *gc, int *gen_no);
static void allocate_ssb_pointers( gc_t *gc );
static void allocate_remembered_sets(gc_param_t *params, gc_t *gc, int gen_no);
static gc_t *alloc_gc_structure( int number_of_old_heaps, word *globals );
static void collect_in( gc_t *gc, int generation );
static char *compute_gc_ID( int number_of_old_heaps, gc_data_t *data );
static word *load_text_or_data( gc_t *gc, unsigned size_bytes, int load_text );


/* Intialize the garbage collector.
 *
 * Returns the collector structure, and the number of generations
 * allocated through the _generations_ parameter.
 */
gc_t *
create_gc( gc_param_t *params, int *generations )
{
  gc_t *gc;

  assert( params->heaps > 0 && params->heaps <= MAX_HEAPS );
  assert( params->use_static_heap || params->static_size == 0 );

  /* HACK! FIXME!  Static area disabled for stop+copy system. */
  assert( params->heaps > 1 || params->use_static_heap == 0 ); 

  gclib_init();  /* Initialize underlying memory management */

  gc = alloc_gc_structure( params->heaps-1, params->globals );
  if (params->heaps > 1) 
    allocate_generational_system( params, gc );
  else
    allocate_stopcopy_system( params, gc );
  *generations = DATA(gc)->oldest_generation + 1;

  gc->id = compute_gc_ID( params->heaps-1, DATA(gc) );

  return gc;
}


static void
allocate_generational_system( gc_param_t *params, gc_t *gc )
{
  int heap_no, gen_no;
  gc_data_t *data = DATA(gc);

  debugmsg( "Creating a generational system." );

  gen_no = 0;
  heap_no = 0;

  /* BEGIN ORDER-CRITICAL SECTION */

  allocate_young_heap( params, gc, &gen_no );
  assert( gen_no == 1 );
  allocate_old_heaps( params->heaps-1, params, gc, &gen_no, &heap_no );
  allocate_static_heap( params, gc, &gen_no, &heap_no );

  /* END ORDER-CRITICAL SECTION */

  data->oldest_generation = data->oldest_collectable_generation = gen_no-1;
  if (data->static_heap != 0)
    data->oldest_collectable_generation -= 1;

  allocate_ssb_pointers( gc );
  allocate_remembered_sets( params, gc, gen_no );
}

static void
allocate_stopcopy_system( gc_param_t *params, gc_t *gc )
{
  int gen_no;
  gc_data_t *data = DATA(gc);

  debugmsg( "Creating a stop-and-copy system." );

  gen_no = 0;

  allocate_stopcopy_heap( params, gc, &gen_no );
  assert( gen_no == 1 );
  data->oldest_generation = 0;
  data->oldest_collectable_generation = 0;
}


static void
allocate_stopcopy_heap( gc_param_t *params, gc_t *gc, int *gen_no )
{
  gc_data_t *data = DATA(gc);

  data->young_heap =
    create_sc_heap( gen_no, 0, 
		   params->heap_info[0].size_bytes,
		   params->heap_info[0].hi_mark,
		   params->heap_info[0].lo_mark,
		   params->globals );
  data->young_heap->collector = gc;
}

static void
allocate_young_heap( gc_param_t *params, gc_t *gc, int *gen_no )
{
  gc_data_t *data = DATA(gc);

  data->young_heap =
    create_young_heap( gen_no, 0,
		       params->heap_info[0].size_bytes,
		       params->heap_info[0].oflo_mark,
		       params->globals );
  data->young_heap->collector = gc;
}


static void
allocate_old_heaps( int number_of_old_heaps, gc_param_t *params, gc_t *gc,
		    int *gen_no, int *heap_no )
{
  gc_data_t *data = DATA(gc);
  int h;

  for ( h = 1 ; h <= number_of_old_heaps ; h++ ) {
    int g = *gen_no;
    int i;
    if (h < number_of_old_heaps || !params->use_np_heap)
      data->old_heaps[h] = 
	create_old_heap( gen_no, h,
			 params->heap_info[h].size_bytes,
			 params->heap_info[h].hi_mark,
			 params->heap_info[h].lo_mark,
			 params->heap_info[h].oflo_mark );
    else {
      data->np_gen_no = *gen_no;
      data->old_heaps[h] = 
	create_old_np_sc_heap( gen_no, h,
			       params->np_stepsize,
			       params->np_steps,
			       params->heap_info[h].size_bytes,
			       params->heap_info[h].hi_mark,
			       params->heap_info[h].lo_mark,
			       params->heap_info[h].oflo_mark );
      data->np_heap_no = h;
    }
    data->old_heaps[h]->collector = gc;
    data->old_heaps[h]->oldest = 0;
    for ( i = g ; i < *gen_no ; i++ )
      data->old_gen[i].heap = data->old_heaps[h];
  }
  data->old_heaps[h-1]->oldest = 1;
  *heap_no = h;
}


static void
allocate_static_heap( gc_param_t *params, gc_t *gc, int *gen_no, int *heap_no )
{
  gc_data_t *data = DATA(gc);

  if (params->use_static_heap) {
    data->static_heap =
      create_static_heap( *heap_no, *gen_no, params->static_size );
    *heap_no = *heap_no + 1;
    *gen_no = *gen_no + 1;
    data->static_heap->collector = gc;
  }
  else
    data->static_heap = 0;
}


static void
allocate_ssb_pointers( gc_t *gc )
{
  gc_data_t *data = DATA( gc );
  int ssbs = data->oldest_generation+1;  /* entry 0 is not used */

#if NP_EXTRA_REMSET
  data->np_ssbidx = data->oldest_generation+1;
  ssbs += 1;
#endif

  data->ssb_bot = (word**)must_malloc( sizeof( word * )*ssbs );
  data->ssb_top = (word**)must_malloc( sizeof( word * )*ssbs );
  data->ssb_lim = (word**)must_malloc( sizeof( word * )*ssbs );
}

static void
allocate_remembered_sets( gc_param_t *params, gc_t *gc, int gen_no )
{
  gc_data_t *data = DATA( gc );
  int i;

  /* FIXME: the creation call should pass better remset size info. */

  for ( i = 1 ; i < gen_no ; i++ )
    data->old_gen[i].remset =
      data->remsets[i] =
	create_remset( 0, 0, 0,
		       &data->ssb_bot[i],
		       &data->ssb_top[i],
		       &data->ssb_lim[i] );

#if NP_EXTRA_REMSET
  if (data->np_heap_no != -1)
    data->np_remset = create_remset( 0, 0, 0,
				     &data->ssb_bot[data->np_ssbidx],
				     &data->ssb_top[data->np_ssbidx],
				     &data->ssb_lim[data->np_ssbidx] );
#endif
}

static int  initialize( gc_t *gc );
static word *alloc_from_heap( gc_t *gc, unsigned nbytes );
static void collect( gc_t *gc, int gen, gc_type_t type, unsigned nbytes );
static void promote_out_of( gc_t *gc, int generation );
static void before_promotion_all( gc_t *gc, int generation );
static void after_promotion_all( gc_t *gc, int generation );
static void enumerate_roots( gc_t *gc, void (*f)( word*, void* ), void *data );
static void enumerate_remsets_older_than( gc_t *gc, int generation,
					  int (*f)( word, void*, unsigned * ),
					  void *data );
static word *data_load_area( gc_t *gc, unsigned size_bytes );
static word *text_load_area( gc_t *gc, unsigned size_bytes );
static int  iflush( gc_t *gc, int gen );
static void stats( gc_t *gc, int generation, heap_stats_t *stats );
static word creg_get( gc_t *gc );
static void creg_set( gc_t *gc, word k );
static void stack_underflow( gc_t *gc );
static void stack_overflow( gc_t *gc );
static int  compact_all_ssbs( gc_t *gc );
static void clear_remset( gc_t *gc, int generation );
static int  isremembered( gc_t *gc, word w );
static void compact_np_ssb( gc_t *gc );
static void clear_np_remset( gc_t *gc );
static void np_remset_ptrs( gc_t *gc, word ***ssbtop, word ***ssblim );
static void set_np_collection_flag( gc_t *gc );
static void np_merge_and_clear_remset( gc_t *gc, int gen );
static void reorganize_static( gc_t *, semispace_t **, semispace_t ** );
static void set_policy( gc_t *, int, int, unsigned );

static gc_t *alloc_gc_structure( int number_of_old_heaps, word *globals )
{
  gc_t *gc;
  gc_data_t *data;
  old_heap_t **old_heaps;
  int *prom_bits;
  
  gc = (gc_t*)must_malloc( sizeof( gc_t ) );
  data = (gc_data_t*)must_malloc( sizeof( gc_data_t ) );

  /* The old_heaps[] and prom_bits[] arrays don't use element 0, but for
   * simplicity it's allocated, hence the +1 on the two calls.
   */
  old_heaps =
    (old_heap_t**)must_malloc( sizeof(old_heap_t*)*(number_of_old_heaps+1) );

  prom_bits = (int*)must_malloc( sizeof(int)*(number_of_old_heaps+1) );
  memset( prom_bits, 0, sizeof(int)*(number_of_old_heaps+1) );

  data->number_of_old_heaps = number_of_old_heaps;
  data->old_heaps = old_heaps;
  data->prom_bits = prom_bits;
  data->np_heap_no = -1;
  data->np_gen_no = -1;
  data->np_remset = 0;
  data->np_ssbidx = -1;
  data->collecting_np_heap = 0;
  data->critical_section = 0;
  data->oldest_generation = 0;
  data->oldest_collectable_generation = 0;

  data->globals = globals;

  gc->initialize = initialize;
  gc->allocate = alloc_from_heap;
  gc->collect = collect;
  gc->data_load_area = data_load_area;
  gc->text_load_area = text_load_area;
  gc->promote_out_of = promote_out_of;
  gc->enumerate_roots = enumerate_roots;
  gc->enumerate_remsets_older_than = enumerate_remsets_older_than;
  gc->compact_all_ssbs = compact_all_ssbs;
  gc->clear_remset = clear_remset;
  gc->isremembered = isremembered;
  gc->set_policy = set_policy;

  gc->creg_set = creg_set;
  gc->creg_get = creg_get;
  gc->stack_underflow = stack_underflow;
  gc->stack_overflow = stack_overflow;

  gc->compact_np_ssb = compact_np_ssb;
  gc->clear_np_remset = clear_np_remset;
  gc->np_remset_ptrs = np_remset_ptrs;
  gc->set_np_collection_flag = set_np_collection_flag;
  gc->np_merge_and_clear_remset = np_merge_and_clear_remset;

  gc->reorganize_static = reorganize_static;

  gc->iflush = iflush;
  gc->stats = stats;
  gc->data = data;

  return gc;
}

/* This procedure computes the ID string for the collector as a function
 * of the ID strings of the individual heaps.
 *
 * FIXME: does not allow old heaps of different types.
 */
static char *
compute_gc_ID( int number_of_old_heaps, gc_data_t *data )
{ 
  char buf[100];

  if (number_of_old_heaps == 0)
    strcpy( buf, data->young_heap->id );
  else if (number_of_old_heaps == 1)
    sprintf( buf, "%s+%s", data->young_heap->id, data->old_heaps[1]->id );
  else
    sprintf( buf, "%s+%d*%s", data->young_heap->id,
	    number_of_old_heaps,
	    data->old_heaps[1]->id );
  if (data->static_heap != 0) {
    strcat( buf, "+" );
    strcat( buf, data->static_heap->id );
  }
  return strdup( buf );
}

static int initialize( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  int i;

  if (data->static_heap && !data->static_heap->initialize( data->static_heap ))
    return 0;

  if (!data->young_heap->initialize( data->young_heap ))
    return 0;

  for ( i = 1 ; i <= data->number_of_old_heaps ; i++ )
    if (!data->old_heaps[i]->initialize( data->old_heaps[i] ))
      return 0;

  /* Setup the barrier if a generational system; disable it otherwise */
  if (data->number_of_old_heaps > 0) {
    wb_setup( gclib_desc_g, gclib_pagebase,
	     data->oldest_generation+1, data->globals,
	     data->ssb_top, data->ssb_lim, 
	     data->np_gen_no+1,
	     data->np_ssbidx
	     );
  }
  else {
    wb_setup0();
    wb_disable();
  }

  return 1;
}
  
static void
set_policy( gc_t *gc, int heap, int op, unsigned value )
{
  gc_data_t *data = DATA(gc);

  if (heap < 0 || heap > data->oldest_collectable_generation) return;

  if (heap == 0)
    data->young_heap->set_policy( data->young_heap, op, value );
  else 
    data->old_heaps[heap]->set_policy( data->old_heaps[heap], op, value );
}

/* Allocate a chunk of ephemeral memory. `n' is a number of bytes. */

static word *
alloc_from_heap( gc_t *gc, unsigned n )
{
  young_heap_t *young_heap = DATA(gc)->young_heap;
  
  return young_heap->allocate( young_heap, n );
}


/* Garbage collection interface.
 *
 * 'generation' is the number of the generation we want to collect where
 *   0 is the youngest.
 * 'type' is the kind of collection requested: either a collection of the
 *   generation or all younger generations (GC_COLLECT), or a promotion
 *   into the generation (GC_PROMOTE).
 * 'request_bytes' is the number of bytes needed to be freed up in the
 *   youngest generation by the collection.
 *
 * It is not always possible for the collector to follow the requests
 * given.  For example, in any system it is impossible to promote into
 * generation 0, and in a system with a simple nursery, it is impossible
 * to collect in generation 0 (if one considers the nursery generation
 * to be 0 and the young generation to be 1).  In such cases, the next
 * 'bigger' collection is chosen.
 *
 * Furthermore, with multi-generational heaps (e.g. the NP heap), asking
 * for a collection in any generation not the oldest in that heap is
 * wrong, so we must 'normalize' the generation to overcome this.
 */
static void 
collect( gc_t *gc, int generation, gc_type_t type, unsigned request_bytes )
{
  gc_data_t *data = DATA(gc);
  young_heap_t *young_heap = data->young_heap;

  assert(!data->critical_section);
  data->critical_section = 1;

  debugmsg( "[debug] ---------- Commencing gc." );

  assert( generation >= 0 );
  if (generation > data->oldest_collectable_generation)
    generation = data->oldest_collectable_generation;

  /* Normalize the generation */
  while (generation > 0 &&
	 data->old_gen[generation-1].heap == data->old_gen[generation].heap)
    generation--;

  stats_before_gc();

  /* Collection starting */

  /* The collector must set collecting_np_heap if applicable */
  data->collecting_np_heap = 0;

  if (generation == 0)
    young_heap->collect( young_heap, request_bytes );
  else if (type == GC_PROMOTE)
    gc->promote_out_of( gc, generation-1 );
  else
    collect_in( gc, generation );

  /* This test succeeds iff the young generation can accomodate the object. 
   * The young heap is at liberty to expand itself or indeed call another
   * round of collections (promoting out, say).
   */
  young_heap->assert_free_space( young_heap, request_bytes );

  data->collecting_np_heap = 0;

  /* Collection done */

  stats_after_gc();

  data->critical_section = 0;
}


/* Private method: perform a garbage collection in the given generation,
 * where the generation > 0.
 */
static void
collect_in( gc_t *gc, int generation )
{
  old_heap_t *old;

  assert( generation > 0 );

  old = DATA(gc)->old_gen[generation].heap;

  before_promotion_all( gc, generation-1 );
  old->collect( old );
  after_promotion_all( gc, generation-1 );
}


/* This can be called to promote out of the oldest generation in a heap
 * to the youngest generation in the next heap; note that all younger
 * objects will also be promoted.
 */
static void
promote_out_of( gc_t *gc, int generation )
{
  gc_data_t *data = DATA(gc);
  struct old_gen *old_gen = data->old_gen;
  old_heap_t *old;

  if (data->number_of_old_heaps > 0 && generation > 0) {
    assert( !old_gen[generation].heap->oldest );
    assert( old_gen[ generation ].heap != old_gen[ generation+1 ].heap );
    assert( old_gen[ generation ].heap != old_gen[ generation-1 ].heap );
  }
  else if (data->number_of_old_heaps == 0)
    assert( 0 );

  before_promotion_all( gc, generation );

  old = old_gen[generation+1].heap;
  old->promote_from_younger( old );

  after_promotion_all( gc, generation );
}


static void 
before_promotion_all( gc_t *gc, int generation )
{
  gc_data_t *data = DATA(gc);
  struct old_gen *old_gen = data->old_gen;
  old_heap_t *prev_h;
  int g, h;

  if (!data->prom_bits[0]) {
    data->young_heap->before_promotion( data->young_heap );
    data->prom_bits[0] = 1;
  }
  for ( g=1, prev_h=0, h=1 ; g <= generation ; g++ ) {
    if (data->old_gen[g].heap != prev_h) {
      if (data->prom_bits[h] == 0) {
	data->old_heaps[h]->before_promotion( data->old_heaps[h] );
	data->prom_bits[h] = 1;
      }
      prev_h = data->old_heaps[h];
      h++;
    }
  }
}

static void
after_promotion_all( gc_t *gc, int generation )
{
  gc_data_t *data = DATA(gc);
  struct old_gen *old_gen = data->old_gen;
  old_heap_t *prev_h;
  int g, h;

  for ( g=1, prev_h=0, h=1; g <= generation ; g++ ) {
    if (data->old_gen[g].heap != prev_h) {
      if (data->prom_bits[h] == 1) {
	data->old_heaps[h]->after_promotion( data->old_heaps[h] );
	data->prom_bits[h] = 0;
      }
      prev_h = data->old_heaps[h];
      h++;
    }
    data->remsets[g+1]->clear( data->remsets[g+1] );
  }
  if (data->prom_bits[0]) {
    data->young_heap->after_promotion( data->young_heap );
    data->remsets[1]->clear( data->remsets[1] );
    data->prom_bits[0] = 0;
  }
}


static void
enumerate_roots( gc_t *gc, void (*f)(word *addr, void *data), void *data )
{
  int i;
  word *globals = DATA(gc)->globals;

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    f( &globals[ i ], data );
}


static void
enumerate_remsets_older_than( gc_t *gc,
			      int generation,
			      int (*f)(word obj, void *data, unsigned *count),
			      void *fdata )
{
  gc_data_t *data = DATA(gc);
  remset_t **remsets = data->remsets;
  int i;

  if (generation < data->oldest_generation) {
    for ( i = data->oldest_generation ; i > generation ; i-- ) {
      remsets[i]->compact( remsets[i] );
      remsets[i]->enumerate( remsets[i], f, fdata );
    }
  }
#if NP_EXTRA_REMSET
  if (data->collecting_np_heap) {
    data->np_remset->compact( data->np_remset );
    data->np_remset->enumerate( data->np_remset, f, fdata );
  }
#endif
}


/* Return a pointer to a data area with the following properties:
 * - it is contiguous
 * - it will hold exactly as many bytes as requested 
 * - it is uninitialized and may hold garbage
 * - it is actually allocated - further calls to this function will not
 *   return a pointer to the area
 *
 * The number of bytes requested must be a multiple of the allocation
 * unit (in 32-bit Larceny, the allocation unit is a doubleword = 8 bytes).
 *
 * Implementation: if the gc has only a young generation, then the
 * area is allocated there.  If the gc has any older generations, then the
 * area is allocated in the oldest generation (intermediate generations
 * are not considered).  The static generation is older than any other
 * generation.  If the generation cannot accomodate the data area, 0 is
 * returned.
 *
 * Rationale: this function returns space for bootstrap heap loading _only_.
 * It can afford to be primitive.
 */

static word *
data_load_area( gc_t *gc, unsigned size_bytes )
{
  return load_text_or_data( gc, size_bytes, 0 );
}

static word *
text_load_area( gc_t *gc, unsigned size_bytes )
{
  return load_text_or_data( gc, size_bytes, 1 );
}

static word *
load_text_or_data( gc_t *gc, unsigned size_bytes, int load_text )
{
  gc_data_t *data = DATA(gc);

  assert( size_bytes % 8 == 0 );
  
  if (data->static_heap) {
    if (load_text)
      return data->static_heap
	->text_load_area( data->static_heap, size_bytes );
    else
      return data->static_heap
	->data_load_area( data->static_heap, size_bytes );
  }
  else if (data->number_of_old_heaps)
    return data->old_heaps[data->number_of_old_heaps]
      ->data_load_area( data->old_heaps[data->number_of_old_heaps],
		        size_bytes );
  else
    return data->young_heap
      ->data_load_area( data->young_heap, size_bytes );
}

static int iflush( gc_t *gc, int generation )
{
  return (int)(DATA(gc)->globals[G_CACHE_FLUSH]);
}

static void
stats( gc_t *gc, int generation, heap_stats_t *stats )
{
  gc_data_t *data = DATA(gc);

  memset( stats, 0, sizeof( heap_stats_t ) );
  
  if (generation == 0) {
    data->young_heap->stats( data->young_heap, stats );
  }
  else if (data->static_heap && generation == data->oldest_generation) {
    remset_stats_t rs_stats;

    data->static_heap->stats( data->static_heap, stats );
    data->remsets[generation]->stats( data->remsets[generation], &rs_stats );
    stats->ssb_recorded = rs_stats.ssb_recorded;
    stats->hash_recorded = rs_stats.hash_recorded;
    stats->hash_scanned = rs_stats.hash_scanned;
    stats->words_scanned = rs_stats.words_scanned;
    stats->hash_removed = rs_stats.hash_removed;
  }
  else {
    remset_stats_t rs_stats;
    old_heap_t *heap;

    heap = data->old_gen[generation].heap;
    heap->stats( heap, generation, stats );
    data->remsets[generation]->stats( data->remsets[generation], &rs_stats );
    stats->ssb_recorded = rs_stats.ssb_recorded;
    stats->hash_recorded = rs_stats.hash_recorded;
    stats->hash_scanned = rs_stats.hash_scanned;
    stats->words_scanned = rs_stats.words_scanned;
    stats->hash_removed = rs_stats.hash_removed;
#if NP_EXTRA_REMSET
    if (stats->np_young && data->np_remset != 0) {
      data->np_remset->stats( data->np_remset, &rs_stats );
      stats->np_ssb_recorded = rs_stats.ssb_recorded;
      stats->np_hash_recorded = rs_stats.hash_recorded;
      stats->np_hash_scanned = rs_stats.hash_scanned;
      stats->np_words_scanned = rs_stats.words_scanned;
      stats->np_hash_removed = rs_stats.hash_removed;
    }
#endif
  }
}

static word creg_get( gc_t *gc )
{
  return DATA(gc)->young_heap->creg_get( DATA(gc)->young_heap );
}

static void creg_set( gc_t *gc, word k )
{
  DATA(gc)->young_heap->creg_set( DATA(gc)->young_heap, k );
}

static void stack_underflow( gc_t *gc )
{
  DATA(gc)->young_heap->stack_underflow( DATA(gc)->young_heap );
}

static void stack_overflow( gc_t *gc )
{
  DATA(gc)->young_heap->stack_overflow( DATA(gc)->young_heap );
}

static int compact_all_ssbs( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  int overflowed, i;

  overflowed = 0;
/*  wb_sync_remsets(); */
  for ( i=1 ; i <= data->oldest_generation ; i++ )
    overflowed = data->remsets[i]->compact( data->remsets[i] ) || overflowed;
/*  wb_sync_ssbs(); */
#if NP_EXTRA_REMSET
  if (data->np_remset)
    overflowed = data->np_remset->compact( data->np_remset ) || overflowed;
#endif
  return overflowed;
}


static void clear_remset( gc_t *gc, int generation )
{
  gc_data_t *data = DATA(gc);

  assert( generation > 0 && generation <= data->oldest_generation );

  data->remsets[generation]->clear( data->remsets[generation] );
}


static void compact_np_ssb( gc_t *gc )
{
#if NP_EXTRA_REMSET
  if (DATA(gc)->np_remset)
    DATA(gc)->np_remset->compact( DATA(gc)->np_remset );
#else
  remset_t *rs = DATA(gc)->remsets[DATA(gc)->np_gen_no+1];
  rs->compact( rs );
#endif
}

static void clear_np_remset( gc_t *gc )
{
#if NP_EXTRA_REMSET
  if (DATA(gc)->np_remset)
    DATA(gc)->np_remset->clear( DATA(gc)->np_remset );
#else
  remset_t *rs = DATA(gc)->remsets[DATA(gc)->np_gen_no+1];
  rs->clear( rs );
#endif
}

static void np_remset_ptrs( gc_t *gc, word ***ssbtop, word ***ssblim )
{
#if NP_EXTRA_REMSET
  if (DATA(gc)->np_remset) {
    *ssbtop = &DATA(gc)->ssb_top[DATA(gc)->np_ssbidx];
    *ssblim = &DATA(gc)->ssb_lim[DATA(gc)->np_ssbidx];
  }
  else {
    *ssbtop = *ssblim = 0;
  }
#else
  *ssbtop = &DATA(gc)->ssb_top[DATA(gc)->np_gen_no+1];
  *ssblim = &DATA(gc)->ssb_lim[DATA(gc)->np_gen_no+1];
#endif
}

static void set_np_collection_flag( gc_t *gc )
{
  DATA(gc)->collecting_np_heap = 1;
}

static void np_merge_and_clear_remset( gc_t *gc, int gen )
{
#if NP_EXTRA_REMSET
  remset_t *r;

  r = DATA(gc)->remsets[gen];
  DATA(gc)->np_remset->assimilate( DATA(gc)->np_remset, r );
  r->clear( r );
#endif  
}

static int isremembered( gc_t *gc, word w )
{
  unsigned g;

  g = gclib_desc_g[ pageof( w ) ];
  if (g > 0)
    return DATA(gc)->remsets[g]->isremembered( DATA(gc)->remsets[g], w );
  else
    return 0;
}

static void
reorganize_static( gc_t *gc, semispace_t **data, semispace_t **text )
{
  if (!DATA(gc)->static_heap)
    panic( "gc::get_static_data_areas: no static heap." );
  DATA(gc)->static_heap->reorganize( DATA(gc)->static_heap );
  DATA(gc)->static_heap->get_data_areas( DATA(gc)->static_heap, data, text );
}
  
/* eof */

