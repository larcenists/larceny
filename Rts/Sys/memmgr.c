/* Rts/Sys/new-policy.c.
 * Larceny run-time system -- memory manager.
 *
 * $Id: memmgr.c,v 1.11 1997/02/11 14:30:55 lth Exp $
 *
 * This file contains procedures for memory allocatation and garbage 
 * collection policy.
 *
 * Some things to do (for various collectors):
 *   - use of madvise() is indicated but not implemented, below.
 *   - may be good to lock the ephemeral area, the ssb, and the remsets
 *     in memory to avoid paging them, using mlock().
 *   - may be good to set the protection for the static area to read-only.
 *   - may be good to have a read-only static area mmap()'d in from the
 *     heap file so as to allow the static area to be shared among processes
 *     using the same heap file.
 */

/* NOTE! THE GC DEPENDS ON THE STATIC HEAP BEING ALLOCATED BELOW THE
   EPHEMERAL HEAP, AND THE EPHEMERAL HEAP BELOW THE TENURED HEAP.
   
   THESE RESTRICTIONS ARE ARTIFACTS OF THE WRITE BARRIER IMPLEMENTATION
   AND THE CHECKING FOR INTERGENERATIONAL POINTERS.  IT'S FAST BUT TOO
   CLEVER AND RESTRICTS IMPLEMENTATIONS TOO MUCH.
   
   FOR THE TIME BEING, IN ORDER TO KEEP THINGS SIMPLE DURING DEVELOPMENT,
   THE ORDER OF ALLOCATION CALLS IN create_gc() ENSURES THAT AREAS ARE
   ALLOCATED IN THE MENTIONED ORDER, ALLOWING WORKING CODE TO CONTINUE
   TO WORK.
   
   WHEN THE NEW WRITE BARRIER IS IMPLEMENTED, SUCH ALLOCATION NEED NO
   LONGER BE GUARANTEED BY create_gc().  HOWEVER, CHANGES TO THE ASSEMBLER
   ARE NEEDED, WHICH IS WHY THE CHANGE HAS BEEN PUT OFF.
*/

#define GC_INTERNAL        /* certain globals and functions are not visible */

#include <string.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

#define MAX_GENERATIONS       32
#define STATIC_HEAP           0

typedef struct gc_data gc_data_t;

/* NOTE!  Some internal tables exist that map heap numbers and
 * generation numbers to misc. data.  The non-predictive collector must
 * be _extremely_ careful if it wants to renumber generations!  We
 * need to discover what types of renumberings are legal and under
 * which conditions.
 */ 

struct gc_data {
  word          *globals;
  young_heap_t  *young_heap;
  old_heap_t    **old_heaps;                       /* [0..number_of_old] */
  remset_t      *remsets[ MAX_GENERATIONS ];
  unsigned      oldest_generation;
  int           *prom_bits;                        /* [0..number_of_old] */
  int           critical_section;
  int           number_of_old_heaps;
#if STATIC_HEAP
  static_heap_t *static_heap;
#endif
  struct old_gen {
    remset_t *remset;
    old_heap_t *heap;
  } old_gen[ MAX_GENERATIONS ];
};

#define DATA(gc) ((gc_data_t*)(gc->data))

static int  initialize( gc_t *gc );
static word *alloc_from_heap( gc_t *gc, unsigned nbytes );
static void collect( gc_t *gc, int gen, gc_type_t type, unsigned nbytes );
static void collect_in( gc_t *gc, int generation );
static void promote_out_of( gc_t *gc, int generation );
static void before_promotion_all( gc_t *gc, int generation );
static void after_promotion_all( gc_t *gc, int generation );
static void enumerate_roots( gc_t *gc, void (*f)( word*, void* ), void *data );
static void enumerate_remsets_older_than( gc_t *gc, int generation,
					  int (*f)( word, void*, unsigned * ),
					  void *data );
static word *data_load_area( gc_t *gc, unsigned size_bytes );
static int  iflush( gc_t *gc, int gen );
static void stats( gc_t *gc, int generation, heap_stats_t *stats );
static word creg_get( gc_t *gc );
static void creg_set( gc_t *gc, word k );
static void stack_underflow( gc_t *gc );
static int  compact_all_ssbs( gc_t *gc );
static int  isremembered( gc_t *gc, word w );


/* 
 * Procedure to intialize garbage collector.  All sizes are in bytes. 
 */
gc_t *
create_gc( unsigned ephemeral_size, /* size of espace; 0 = default */
	   unsigned ewatermark,     /* watermark in %, 0 = default */
	   unsigned static_size,    /* size of sspace; 0 = default */
           unsigned rhash,          /* # elements in remset hash tbl */
           unsigned ssb,            /* # elements in remset SSB */
           unsigned old_generations,/* # of old generations */
           old_param_t *old_gen_info,  /* info about old generations */
           word     *globals        /* globals array */
	  )
{
  int gen_no = 0;
  int i, heap_no;
  gc_t *gc;
  gc_data_t *data;
  old_heap_t **old_heaps;
  int number_of_old_heaps = old_generations;  /* one-to-one mapping here */
  int *prom_bits;
  char buf[50];

  assert( number_of_old_heaps < MAX_GENERATIONS );
  if (number_of_old_heaps == 0) number_of_old_heaps = 1;   /* default */

  gclib_init();

 again:
  data = (gc_data_t*)malloc( sizeof( gc_data_t ) );
  gc = (gc_t*)malloc( sizeof( gc_t ) );
  old_heaps = (old_heap_t**)malloc(sizeof(old_heap_t*)*number_of_old_heaps+1);
  prom_bits = (int*)malloc( sizeof(int)*number_of_old_heaps+1 );
  if (gc == 0 || data == 0 || old_heaps == 0 || prom_bits == 0) {
    if (gc) free( gc );
    if (data) free( data );
    if (old_heaps) free( old_heaps );
    if (prom_bits) free( prom_bits );
    memfail( MF_MALLOC, "Could not allocate gc metadata." );
    goto again;
  }

  data->number_of_old_heaps = number_of_old_heaps;
  data->old_heaps = old_heaps;
  data->prom_bits = prom_bits;

  gen_no = 0;

  /* This is where the ordering is critical, for the time being */

#if STATIC_HEAP
  data->static_heap = create_static_heap( FIXME );
#endif
  data->young_heap = 
    create_young_heap( &gen_no, 0, ephemeral_size, ewatermark, globals );
  data->young_heap->collector = gc;

  assert( gen_no == 1 );

  for ( heap_no = 1 ; heap_no <= number_of_old_heaps ; heap_no++ ) {
    int g = gen_no;
    data->old_heaps[heap_no] = 
      create_old_heap( &gen_no, heap_no,
		       old_gen_info[heap_no-1].size,
		       old_gen_info[heap_no-1].hiwatermark,
		       old_gen_info[heap_no-1].lowatermark );
    data->old_heaps[heap_no]->collector = gc;
    data->old_heaps[heap_no]->oldest = 0;
    for ( i = g ; i < gen_no ; i++ )
      data->old_gen[i].heap = data->old_heaps[heap_no];
  }
  data->old_heaps[heap_no-1]->oldest = 1;

  /* End order-critical section */

  data->globals = globals;
  data->oldest_generation = gen_no - 1;
  memset( data->prom_bits, 0, sizeof( data->prom_bits ) );
  data->critical_section = 0;

  /* FIXME: the creation call should pass better remset size info. */

  for ( i = 1 ; i < gen_no ; i++ )
    data->old_gen[i].remset = data->remsets[i] = create_remset( 0, 0, 0 );

  if (number_of_old_heaps == 0)
    strcpy( buf, data->young_heap->id );
  else if (number_of_old_heaps == 1)
    sprintf( buf, "%s+%s", data->young_heap->id, data->old_heaps[1]->id );
  else
    sprintf( buf, "%s+%d*%s", data->young_heap->id,
	                      number_of_old_heaps,
   	                      data->old_heaps[1]->id );
  gc->id = strdup( buf );
  gc->initialize = initialize;
  gc->allocate = alloc_from_heap;
  gc->collect = collect;
  gc->data_load_area = data_load_area;
  gc->text_load_area = data_load_area;    /* [sic] */
  gc->promote_out_of = promote_out_of;
  gc->enumerate_roots = enumerate_roots;
  gc->enumerate_remsets_older_than = enumerate_remsets_older_than;
  gc->compact_all_ssbs = compact_all_ssbs;
  gc->isremembered = isremembered;

  gc->creg_set = creg_set;
  gc->creg_get = creg_get;
  gc->stack_underflow = stack_underflow;

  gc->iflush = iflush;
  gc->stats = stats;
  gc->data = data;

  return gc;
}


static int initialize( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  int i;

#if STATIC_HEAP
  if (!data->static_heap->initialize( data->static_heap ))
    return 0;
#endif

  if (!data->young_heap->initialize( data->young_heap ))
    return 0;

  for ( i = 1 ; i <= data->number_of_old_heaps ; i++ )
    if (!data->old_heaps[i]->initialize( data->old_heaps[i] ))
      return 0;

  wb_setup( data->remsets, gclib_desc_g, gclib_pagebase,
 	    data->oldest_generation+1, data->globals );

  return 1;
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
 */
static void 
collect( gc_t *gc, int generation, gc_type_t type, unsigned request_bytes )
{
  young_heap_t *young_heap = DATA(gc)->young_heap;

  assert(!DATA(gc)->critical_section);
  DATA(gc)->critical_section = 1;

  assert( generation >= 0 );
  if (generation > DATA(gc)->oldest_generation)
    generation = DATA(gc)->oldest_generation;

  stats_before_gc();
  wb_sync_remsets();

  /* Collection starting */

  if (generation == 0)
    young_heap->collect( young_heap );
  else if (type == GC_PROMOTE)
    gc->promote_out_of( gc, generation-1 );
  else {
    collect_in( gc, generation );
  }

  if (request_bytes > young_heap->free_space( young_heap )) {
    debugmsg( "[debug] gc: Failed free-espace %d.", request_bytes );

    gc->promote_out_of( gc, 0 );

    if (request_bytes > young_heap->free_space( young_heap ))
      panic( "Cannot allocate object of size %u bytes "
	     "(object is too large for heap).", request_bytes );
  }

  /* Collection done */

  wb_sync_ssbs();
  stats_after_gc();

  DATA(gc)->critical_section = 0;
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
  old_heap_t *prev;
  int i, j;

  if (!data->prom_bits[0]) {
    data->young_heap->before_promotion( data->young_heap );
    data->prom_bits[0] = 1;
  }
  for ( i=1, prev=0, j=1 ; i <= generation ; i++ ) {
    if (!data->prom_bits[j]) {
      if (data->old_heaps[j] != prev) {
	data->old_heaps[j]->before_promotion( data->old_heaps[j] );
	data->prom_bits[j] = 1;
	prev = data->old_heaps[j];
	j++;
      }
    }
  }
}

static void
after_promotion_all( gc_t *gc, int generation )
{
  gc_data_t *data = DATA(gc);
  struct old_gen *old_gen = data->old_gen;
  old_heap_t *prev;
  int i, j;

  for ( i=1, prev=0, j=1; i <= generation ; i++ ) {
    if (data->prom_bits[i]) {
      if (data->old_heaps[j] != prev) {
	data->old_heaps[j]->after_promotion( data->old_heaps[j] );
	data->prom_bits[j] = 0;
	prev = data->old_heaps[j];
	j++;
      }
      data->remsets[i+1]->clear( data->remsets[i+1] );
    }
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
  struct old_gen *old_gen = data->old_gen;
  int i;

  if (generation < data->oldest_generation) {
    for ( i = data->oldest_generation ; i > generation ; i-- ) {
      old_gen[i].remset->compact( old_gen[i].remset );
      old_gen[i].remset->enumerate( old_gen[i].remset, f, fdata );
    }
  }
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
 * are not considered).  If the generation cannot accomodate the data
 * area, 0 is returned.
 *
 * Rationale: this function returns space for bootstrap heap loading _only_.
 * It can afford to be primitive.
 */

static word *
data_load_area( gc_t *gc, unsigned size_bytes )
{
  gc_data_t *data = DATA(gc);

  assert( size_bytes % 8 == 0 );
  
  if (data->number_of_old_heaps)
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
  if (generation == 0) {
    DATA(gc)->young_heap->stats( DATA(gc)->young_heap, stats );
    stats->ssb_recorded = 0;
    stats->hash_recorded = 0;
    stats->hash_scanned = 0;
    stats->words_scanned = 0;
    stats->hash_removed = 0;
  }
  else {
    remset_stats_t rs_stats;
    old_heap_t *heap;

    heap = DATA(gc)->old_gen[generation].heap;
    heap->stats( heap, generation, stats );
    stats->stack = 0;
    stats->stacks_created = 0;
    stats->frames_flushed = 0;
    stats->frames_restored = 0;
    stats->bytes_flushed = 0;
    DATA(gc)->remsets[generation]
      ->stats( DATA(gc)->remsets[generation], &rs_stats );
    stats->ssb_recorded = rs_stats.ssb_recorded;
    stats->hash_recorded = rs_stats.hash_recorded;
    stats->hash_scanned = rs_stats.hash_scanned;
    stats->words_scanned = rs_stats.words_scanned;
    stats->hash_removed = rs_stats.hash_removed;
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

static int compact_all_ssbs( gc_t *gc )
{
  gc_data_t *data = DATA(gc);
  int overflowed, i;

  overflowed = 0;
  for ( i=1 ; i <= data->oldest_generation ; i++ )
    overflowed = data->remsets[i]->compact( data->remsets[i] ) || overflowed;
  return overflowed;
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
  
/* eof */

