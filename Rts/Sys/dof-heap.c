/* Copyright 1999 Lars T Hansen    -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Deferred-older-first garbage collector.
 *
 * (Inside this big program there's a small program struggling to get out.)
 *
 * See dof.txt, sim-dof.sch, and dof-larceny.txt for more information.
 * There is purposely very little documentation in this file.
 *
 * FIXME/BUGS
 *   Must incorporate information about other areas when recomputing
 *   heap size (can we use gc_compute_dynamic_size()?).
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>

#define GC_INTERNAL

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
#include "msgc-core.h"

#if defined(NDEBUG2)
# define consolemsg annoyingmsg /* Debug messages */
#endif

#define INVARIANT_CHECKING    0 /* Fairly inexpensive */
#define EXPENSIVE_CHECKS_TOO  0 /* Quite expensive */
#define REMSET_TRACE          0 /* Massive output */
#define REMSET_TRACE_ALL      0 /* Even more massive output! */

#define USE_DOF_REMSET        1 /* Generally desirable */
  /* Separate the sets that track pointers among dof generations
     from the sets that track pointers to ephemeral area.
     */

#define SPLIT_REMSETS         1 /* Generally desirable */
  /* Split the dof remsets so that there is one for every pair of
     generations, reducing scanning work.
     */

#define SHADOW_TABLE          1 /* Generally desirable */
  /* If the large descriptor table is being used, shadow the table
     pointer in the GC routines to speed up access.
     */

#define TRUTHFUL_FREE_SPACE   1 /* Generally desirable */
  /* When computing heap free space, ask the lowlevel allocator about
     the space free and use that, since large objects may have used some
     of the heap memory.
     */

#define DYNAMIC_ALLOCATION    1 /* Beneficial. */
  /* Allocate memory for the reserve and unused heap areas on demand.
     This is intended to make memory allocation more flexible so that 
     LOS allocation does not run out of storage even if we're running
     in a fixed-size heap.

     Cost: proportional to the heap size, with a very low proportionality
     constant; in practice zero.
     */

#define FULLGC_ON_LOW_RESERVE 1 /* Beneficial */
  /* If DYNAMIC_ALLOCATION is 1, and the memory available for the reserve
     is found to be smaller than the reserve, then trigger a full collection
     to reclaim any dead storage not yet reclaimed.  This is probably most
     effective in conjunction with FULLGC_SWEEP_LOS.
     
     Cost: not yet quantified.  There is always the chance that it kicks
     in when it's not needed (i.e, the memory available is low but the
     collection needs even less), thus costing extra.
     */
     
#define FULLGC_SWEEP_LOS      1 /* Beneficial */
  /* Sweep the LOS following full GC, freeing any unmarked objects.
     If this option is 0, then unmarked LOS objects are reclaimed only
     when the generation they belong to are garbage collected using
     a normal DOF collection.  Eager freeing of large objects may make
     more memory available for future LOS allocation, avoiding starvation
     in a fixed-size heap.
     
     Cost: proportional to the number of large objects; usually there
     are very few.  Cost in practice is close to zero.
     */

#define FULLGC_COMPACT_DOF_REMSET    1 /* Experimental */
  /* When sweeping a DOF remset following a full GC, compact it and
     free the unused pool nodes.  By eagerly freeing pool nodes we
     hope to keep the sizes of the DOF remsets lower than they would
     otherwise be.
     
     If FULLGC_COMPACT_DOF_REMSET is 0, then unused entries are instead 
     set to 0.

     Cost: not yet quantified.
     Benefit: not yet quantified.
     */

#define COMPACT_DOF_REMSET   0 /* Experimental */
  /* When scanning a DOF remset, remove those objects that no longer
     contain appropriate intergenerational pointers, and enable compaction
     of the set in-place to save space.

     This is _not_ generally useful.  A DOF set is used only once before
     it is cleared, thus removing objects does not have any practical
     effect.  However, by turning this on, gathering data, and then analyzing
     that data (notably the number of ptrs removed from each DOF set) it
     may be possible to draw conclusions about whether the sets contribute
     to float.  (I haven't thought much about this yet.)
     */

#define SHADOW_REMSETS        1 /* Experimental */
  /* Maintain a shadow table of remembered sets to allow us to avoid
     scanning the (large) sets during promotions, which dominate the cost
     of GC.  The shadow sets shadow the "normal" sets and allow
     duplicates to be filtered.  After promotion, these sets contain
     references to objects that do not reference the ephemeral area.
     Thus they will not be interesting during the next promotion, and
     we don't want to scan them.

     The shadow sets are independent of the DOF remsets.

     Shadow remset use is also controlled by a command line parameter.
     */

/* GC write barrier control.  
   
   There are four settings here to control the code that does the
   duplicate filtering.  Filtering works as follows.  An object is a
   candidate for addition to the remembered set only once between each
   time it's moved, and if it is in generation i, it is to be added to
   those sets (i,j) for all j into which it contains pointers.  However,
   if it contains several pointers into j, then it should only be added
   to set (i,j) once.
   
   Logically, therefore, the barrier keeps an array of boolean values,
   initially 0, and when it discovers a pointer into generation j, it
   adds the object to set (i,j) and sets the bit to 1, unless the bit
   is already 1, in which case it does nothing.
   
   1. If EXTRA_GEN_LOOKUP and EXTRA_GEN_BITVECTOR are both 0, then the
   barrier implements the above logic, except that the barrier only sets
   the bit and code running after the object has been fully scanned
   loops over the array, checking bits and adding pointers to the set.
   At that time it also clears the array, which must in any case be
   done.
   
   2. If EXTRA_GEN_BITVECTOR is set, then the number of generations must
   be no more than 32, and a single word of 32 bits is used as the
   array of booleans.  Otherwise the code is as for the first case,
   except that it's cheaper to clear the bitvector than the vector.
   It is not necessary to run a loop after the barrier here; see
   case 4.
   
   3. If EXTRA_GEN_LOOKUP is set, then an array is again used but instead
   of storing booleans it stores, for each j, the last object that was
   stored into set (i,j).  Thus the array need never be cleared, and
   the cost of the barrier is proportional to the number of pointers
   in the object, not to the length of the array.  This is a win on
   pairs, in particular.  In addition, the code for the remembered set
   insertion is in the barrier.
   
   4. If both EXTRA_GEN_LOOKUP and EXTRA_GEN_BITVECTOR are set, then
   the code uses a bitvector (cleared after the object has been scanned)
   but has the remembered set insertion code inside the barrier code.
   Really, a different name should be found for this combination.

   The big payoff comes from using EXTRA_GEN_LOOKUP; the bitvector appears
   to boost performance some more under some configurations (notably when
   EXTRA_GEN_LOOKUP is set).
   */
#define EXTRA_GEN_LOOKUP      1 
#define EXTRA_GEN_BITVECTOR   1

#define BLOCK_SIZE            (64*KILOBYTE)   /* Block granularity */
#define DOF_REMSET_SIZE       (16*KILOBYTE)   /* Remset block */

typedef struct gen gen_t;
typedef struct dof_data dof_data_t;
typedef struct dof_remset dof_remset_t;
typedef struct dof_pool dof_pool_t;

struct dof_pool {
  dof_pool_t *next;             /* Next node */
  word *bot;                    /* First elt */
  word *top;                    /* Next free */
  word *lim;                    /* First past end */
};

struct dof_remset {
  stats_id_t     self;          /* Remset identity */
  dof_pool_t     *first;        /* First pool */
  dof_pool_t     *curr;         /* Current pool */
  int            curr_pools;    /* Number of pools now */
  int            max_pools;     /* Larges number of pools ever */
  remset_stats_t stats;         /* Accumulators */
#if COMPACT_DOF_REMSET
  int            free;          /* Number of free slots total in all but
                                   last segment */
#endif
};

struct gen {
  stats_id_t  self;             /* Generation identity */
  int         id;               /* Generation ID (internal) */
  int         size;             /* Size in bytes */
  int         claim;            /* Bytes of memory allotted */
  int         order;            /* Generation number (for barrier/remset) */
  semispace_t *live;            /* The data */
  gen_stats_t stats;            /* Counters */
#if USE_DOF_REMSET
  /* For GC barrier */
# if SPLIT_REMSETS
  /* dof_remsets[i] contains those objects with pointers from this
     generation to generation i, where i is an order number.
     */
  dof_remset_t *dof_remsets[ MAX_GENERATIONS ];
# else
  dof_remset_t *dof_remset;
# endif
#endif
#if INVARIANT_CHECKING
  remset_t    *remset;          /* The remembered set */
  los_list_t  *los;             /* The LOS list */
#endif
};

struct dof_data {
  int    area_size;             /* Total memory allocated in bytes,
                                   divisible by quantum. */
  int    heap_limit;            /* 0 or the max area size in bytes,
                                   divisible by quantum */
  int    quantum;               /* Heap growth quantum in bytes */
  double load_factor;
  int    full_frequency;        /* 0 or frequency of full GCs */
  bool   retry_ok;              /* TRUE if OK to do full GC on heap-full */
  int    gc_counter;            /* counter for full GC management */
  double growth_divisor;        /* Controls heap expansion */
  double free_before_promotion; /* Controls full gc triggering */
  double free_before_collection;/* Controls full gc triggering */
  double free_after_collection; /* Controls gc completion */
  bool   use_shadow_remsets;     /* Use shadow sets */
  bool   fullgc_generational;   /* Use generational full GC */
  bool   fullgc_on_reset;       /* Count resets, not collections */
  bool   fullgc_on_collection;  /* Count collections, not resets */
  bool   fullgc_on_promotion;   /* Count promotions, not collections/resets */
  int    ephemeral_size;        /* Max amount that can be promoted in from
                                   the ephemeral areas */
  int    first_gen_no;          /* Generation number of lowest-numbered gen. */

  gen_t  **gen;                 /* Array of generation structures */
  int    s;                     /* Generation size in bytes */
  int    n;                     /* Number of generations */
  int    ap;                    /* Allocation pointer (index into gen) */
  int    cp;                    /* Collection pointer (index into gen) */
  int    rp;                    /* Reserve pointer (index into gen) */

  remset_t *shadow_remsets[ MAX_GENERATIONS ];
  struct { 
    word *ssb_bot;
    word *ssb_top;
    word *ssb_lim;
  } shadow_ssb_loc[ MAX_GENERATIONS ];

  /* Statistics */
  int        consing;           /* Amount of consing since reset */
  int        copying;           /* Amount of copying since reset */
  double     total_consing;     /* Total amount of consing */
  double     total_copying;     /* Total amount of copying */
  int        max_size;          /* Maximum area size */
  gc_stats_t stats;             /* Collector's exportable stats */
};

#define DATA(h) ((dof_data_t*)(h->data))

/* Instrumentation data */
static gc_event_stats_t dof;

#if GC_EVENT_COUNTERS
# define COUNT_WORDS_FORWARDED()  dof.words_forwarded++
# define COUNT_PTRS_FORWARDED()   dof.ptrs_forwarded++
# define COUNT_GC_BARRIER_HIT()   dof.gc_barrier_hit++
# define COUNT_REMSET_LARGE_OBJ(x)                      \
  do { if (x > GC_LARGE_OBJECT_LIMIT) {                 \
         dof.remset_large_objs_scanned++;               \
         dof.remset_large_obj_words_scanned += x;       \
       }                                                \
  } while(0) 
#else
# define COUNT_WORDS_FORWARDED()   (void)0
# define COUNT_PTRS_FORWARDED()    (void)0
# define COUNT_GC_BARRIER_HIT()    (void)0
# define COUNT_REMSET_LARGE_OBJ(x) (void)0
#endif

#if GC_HIRES_TIMERS
static struct {
  hrtime_t hrtimer;
  hrtime_t *ptr;
} dof_event;

static void start( hrtime_t *acc )
{
  dof_event.hrtimer = gethrtime();
  dof_event.ptr = acc;
}

static void stop( void )
{
  *dof_event.ptr += gethrtime() - dof_event.hrtimer;
}
#else
# define start( x )  (void)0
# define stop()      (void)0
#endif

static int dof_copy_into_with_barrier( old_heap_t *heap,
                                       int younger_than, 
                                       gen_t **tospaces, 
                                       gc_type_t type,
                                       dof_data_t *data );
  /* Copy objects from generations younger than `younger_than' into the
     areas of `tospaces', filling the areas in strict order and never
     extending an area (overflow is a fatal error).  Large objects are
     not counted in the calculation of area usage.  There is a traditional
     write barrier on the areas of `tospaces'.
     
     All tospaces but the first must be empty.

     Returns the index of the last tospace used.
     */

static int gen_used( gen_t *g )
{
  ss_sync( g->live );
  return g->live->used;
}

static int gen_free( gen_t *g )
{
  return g->claim - gen_used( g );
}

static void gen_clear( gen_t *g )
{
  ss_reset( g->live );
}

static int free_in_allocation_area( dof_data_t *data )
{
  int free = 0, i;
  
  for ( i=0 ; i <= data->ap ; i++ )
    free += gen_free( data->gen[ i ] );
  return free;
}

static int space_needed_for_promotion( dof_data_t *data )
{
  int major_fragments;          /* Crossing generations */
  int minor_fragments;          /* Crossing blocks */
  int major_overhead, minor_overhead;

  major_fragments = 
    (int)ceil( max( 0.0, data->ephemeral_size-gen_free(data->gen[data->ap])) /
               (double)(data->s) );
  major_overhead  = major_fragments * GC_LARGE_OBJECT_LIMIT;

  minor_fragments = (int)ceil( (data->ephemeral_size + major_overhead) / 
                               (double)BLOCK_SIZE );
  minor_overhead = minor_fragments * GC_LARGE_OBJECT_LIMIT;

  /*  annoyingmsg( "   (Total promotion overhead computed as %d)",
                   major_overhead + minor_overhead );
  */
  
  return data->ephemeral_size + major_overhead + minor_overhead;
}

#if USE_DOF_REMSET
static dof_pool_t *make_dof_pool( bool with_data )
{
  dof_pool_t *p = must_malloc( sizeof( dof_pool_t ) );

  if (with_data) {
    p->bot = gclib_alloc_rts( DOF_REMSET_SIZE, MB_REMSET );
    p->top = p->bot;
    p->lim = p->bot + bytes2words(DOF_REMSET_SIZE);
  }
  else {
    /* Gross hack -- the GC core can't deal with empty segments
       in the remembered sets.  Otherwise, all these pointers
       could be pointers to a dummy word somewhere.  Note this
       1-word chunk of memory is not freed.
       */
    p->bot = must_malloc( sizeof(word) );
    p->top = p->bot;
    p->lim = p->bot + 1;
  }
  p->next = 0;
  return p;
}

static dof_remset_t *make_dof_remset( int major_id, int minor_id )
{
  dof_remset_t *r = must_malloc( sizeof(dof_remset_t) );

  r->self = stats_new_remembered_set( major_id, minor_id );
  r->first = r->curr = make_dof_pool( FALSE );
  r->max_pools = 1;
  r->curr_pools = 1;
#if COMPACT_DOF_REMSET
  r->free = 0;
#endif
  memset( &r->stats, 0, sizeof( remset_stats_t ) );
  return r;
}

static int free_dof_pools( dof_pool_t *p )
{
  if (p != 0) {
    int n = free_dof_pools( p->next );
    gclib_free( p->bot, DOF_REMSET_SIZE );
    free( p );
    return n+1;
  }
  else
    return 0;
}

static void clear_one_dof_set( dof_remset_t *r )
{
  if (r == 0) return;
  
  free_dof_pools( r->first->next );
  r->curr_pools = 1;
  r->first->next = 0;
  r->curr = r->first;
  r->first->top = r->first->bot;
  r->stats.cleared++;
#if COMPACT_DOF_REMSET
  r->free = 0;
#endif
}

/* If points_in is true, then clear those sets in other generations
   that remember objects with pointers into generation 'order'.
   */
static void clear_dof_remset( old_heap_t *heap, int order, bool points_in )
{
  dof_data_t *data = DATA(heap);
  int i, j;

# if SPLIT_REMSETS
  for ( i=0 ; i < data->n ; i++ ) {
    if (data->gen[i]->order == order) {
      for ( j=0 ; j < data->n ; j++ )
        clear_one_dof_set( data->gen[i]->dof_remsets[j] );
      break;
    }
  }
  if (points_in)
    for ( i=0 ; i < data->n ; i++ )
      clear_one_dof_set( data->gen[i]->dof_remsets[order] );
# else
  /* 'points_in' means nothing here */
  for ( i=0 ; i < data->n ; i++ ) {
    if (data->gen[i]->order == order) {
      clear_one_dof_set( data->gen[i]->dof_remset );
      break;
    }
  }
# endif
}

static void advance_dof_remset( dof_remset_t *r )
{
  assert( r->curr->next == 0 );

#if COMPACT_DOF_REMSET
  /* FIXME here */
#endif

  r->curr->next = make_dof_pool( TRUE );
  r->curr = r->curr->next;
  r->curr_pools++;
  r->max_pools = max( r->max_pools, r->curr_pools );
}

static int dof_rs_size( dof_remset_t *r ) /* Number of entries */
{
  return (r->curr_pools - 1) * bytes2words( DOF_REMSET_SIZE )
       + (r->curr->top - r->curr->bot);
}

#if INVARIANT_CHECKING
static void dof_remset_consistency_check( dof_remset_t *r, int gen_no )
{
  dof_pool_t *segment;
  word *slot;

  for ( segment=r->first ; segment ; segment=segment->next )
    for ( slot=segment->bot ; slot < segment->top ; slot++ )
      if (*slot) {
        assert(isptr(*slot));
        if (gen_of(*slot) != gen_no)
          panic_abort( "dof_remset: Failed consistency check: want %d, got %d",
                       gen_no, gen_of(*slot) );
      }
}
#endif
#endif

static void print_generation_stats( dof_data_t *data )
{
  int i;
  for ( i=0 ; i < data->n ; i++ ) {
    gen_t *g = data->gen[i];

    consolemsg( "Gen %2d: "
                "id=%2d, order=%2d, size=%7d, claim=%7d, live=%7d, allocd=%7d",
                i, g->id, g->order, g->size, g->claim, gen_used( g ),
                g->live->allocated );
  }
}
 
#if INVARIANT_CHECKING
static int inv1( dof_data_t *data )/* all-same-size */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(data->gen[i]->size == data->s)) return 0;
  return 1;
}

static int inv2( dof_data_t *data ) /* no-overuse */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(gen_used( data->gen[i] ) <= data->gen[i]->claim )) return 0;
  return 1;
}

static int inv3( dof_data_t *data ) /* no-overflow */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(data->gen[i]->claim <= data->gen[i]->size )) return 0;
  return 1;
}

static int inv4( dof_data_t *data ) /* be-real */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(data->gen[i]->claim >= 0)) return 0;
  return 1;
}

static int inv5( dof_data_t *data ) /* fixed-memory */
{
  int i, claims =0;

  for ( i=0 ; i < data->n ; i++ )
    claims += data->gen[i]->claim;
  return claims == data->s * (data->n - 1);
}

static int inv6( dof_data_t *data ) /* ordered-hps */
{
  return data->ap <= data->cp
      && data->cp < data->rp
      && data->rp <= data->cp + 2;
}

static int inv7( dof_data_t *data ) /* stay-inside-1 */
{
  return data->ap >= 0;
}

static int inv8( dof_data_t *data ) /* stay-inside-2 */
{
  return data->rp < data->n;
}

static int inv9( dof_data_t *data ) /* memory-location */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(i==0 ||
          (data->rp == data->cp + 2 && i == data->cp + 1) ||
          data->gen[i]->claim == data->gen[i]->size))
      return 0;
  return 1;
}

static int inv10( dof_data_t *data ) /* can-collect-1 */
{
  return !(data->rp == data->cp+1)
    || (data->gen[data->rp]->claim == data->gen[data->rp]->size &&
        data->gen[data->rp]->size == gen_free( data->gen[data->rp] ));
}

static int inv11( dof_data_t *data ) /* can-collect-2 */
{
  return !(data->rp == data->cp+2)
    || ((data->gen[data->rp]->claim + 
         data->gen[data->rp-1]->claim - 
         gen_used( data->gen[ data->rp ] ))
        >= (data->s + GC_LARGE_OBJECT_LIMIT));
}

static int inv12( dof_data_t *data ) /* can-allocate */
{
  return free_in_allocation_area( data ) >= space_needed_for_promotion( data );
}

static int inv20( dof_data_t *data ) /* size-block-aligned */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(data->gen[i]->size % BLOCK_SIZE == 0)) return 0;
  return 1;
}

static int inv21( dof_data_t *data ) /* claim-block-aligned */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!(data->gen[i]->claim % BLOCK_SIZE == 0)) return 0;
  return 1;
}

static int inv22( dof_data_t *data ) /* reserve-order */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!((data->gen[i]->id <= data->cp) ||
          (data->gen[i]->order == data->n - data->gen[i]->id + data->cp)))
      return 0;
  return 1;
}

static int inv23( dof_data_t *data ) /* alloc-order */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (!((data->gen[i]->id > data->cp) ||
          (data->gen[i]->order == data->cp - data->gen[i]->id)))
      return 0;
  return 1;
}

static int inv90( dof_data_t *data ) /* gen-id */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (data->gen[i]->id != i) return 0;
  return 1;
}

static int inv91( dof_data_t *data ) /* order=gen */
{
  int i;
  for ( i=0 ; i < data->n ; i++ )
    if (data->gen[i]->order + data->first_gen_no != data->gen[i]->live->gen_no)
      return 0;
  return 1;
}

static int inv92( old_heap_t *heap )  /* remset-order */
{
  dof_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;

  int i;

  for ( i=0 ; i < data->n ; i++ )
    if (data->gen[i]->remset != 0 &&
        data->gen[i]->remset != 
          gc->remset[data->first_gen_no+data->gen[i]->order])
      return 0;
  return 1;
}

static int inv93( old_heap_t *heap )  /* remset-content */
{
  dof_data_t *data = DATA(heap);

  int i;

  for ( i=0 ; i < data->n ; i++ )
    if (data->gen[i]->remset != 0 &&
        (i < data->ap || 
         i == data->ap && gen_used( data->gen[i] ) == 0 ||
         i > data->cp && i < data->rp ||
         i == data->rp && gen_used( data->gen[i] ) == 0)) {
      rs_compact( data->gen[i]->remset );
      if (data->gen[i]->remset->live != 0) {
        hardconsolemsg( "inv93: %d", i );
        return 0;
      }
    }
  return 1;
}

static int inv94( old_heap_t *heap )  /* remset-data */
{
#if EXPENSIVE_CHECKS_TOO
  dof_data_t *data = DATA(heap);

  int i;

  for ( i=0 ; i < data->n ; i++ )
    if (data->gen[i]->remset != 0)
      /* rs_consistency check signals the error */
      rs_consistency_check( data->gen[i]->remset, 
                            data->gen[i]->order + data->first_gen_no );
#endif
  return 1;
}

static int inv95( old_heap_t *heap )  /* los-list-order */
{
  dof_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;

  int i;

  for ( i=0 ; i < data->n ; i++ )
    if (data->gen[i]->los != 0 &&
        data->gen[i]->los != 
          gc->los->object_lists[data->first_gen_no+data->gen[i]->order]) {
      hardconsolemsg( "inv95: %d", i );
      return 0;
    }
  return 1;
}

#if USE_DOF_REMSET
static bool dof_set_empty( dof_remset_t *r )
{
  return r->curr == r->first && r->first->top == r->first->bot;
}

static int inv96_helper( old_heap_t *heap, dof_remset_t *r, int i )
{
  dof_data_t *data = DATA(heap);

  if (r != 0 &&
      (i < data->ap || 
       i == data->ap && gen_used( data->gen[i] ) == 0 ||
       i > data->cp && i < data->rp ||
       i == data->rp && gen_used( data->gen[i] ) == 0)) {
    if (!dof_set_empty( r )) {
      hardconsolemsg( "inv96#1: %d", i );
      return 0;
    }
  }
  return 1;
}

static int inv96( old_heap_t *heap ) /* dof-remset-content */
{
  dof_data_t *data = DATA(heap);
  int i, j;

  for ( i=0 ; i < data->n ; i++ ) {
# if SPLIT_REMSETS
    for ( j=0 ; j < data->n ; j++ ) {
      if (!inv96_helper( heap, data->gen[i]->dof_remsets[j], i ))
        return 0;
      if (data->gen[i]->order <= data->gen[j]->order)
        if (!dof_set_empty( data->gen[i]->dof_remsets[data->gen[j]->order])) {
          hardconsolemsg( "inv96#3: %d %d %d", i, j, 
                          dof_rs_size( data->gen[i]->dof_remsets[j] ) );
          for ( i=0 ; i < data->n ; i++ ) {
            for ( j=0 ; j < data->n ; j++ )
              printf( "%6d ", dof_rs_size( data->gen[i]->dof_remsets[j] ) );
            printf( "\n" );
          }
          return 0;
        }
    }
# else
    if (!inv96_helper( heap, data->gen[i]->dof_remset, i ))
      return 0;
# endif
  }
  return 1;
}

static int inv97( old_heap_t *heap ) /* dof-remset-data */
{
#if EXPENSIVE_CHECKS_TOO
  dof_data_t *data = DATA(heap);
  int i, j;

# if SPLIT_REMSETS
  /* dof_remset_consistency_check signals the error */
  for ( i=0 ; i < data->n ; i++ )
    for ( j=0 ; j < data->n ; j++ )
      dof_remset_consistency_check( data->gen[i]->dof_remsets[j], 
                                    data->gen[i]->order + data->first_gen_no );
# else
  for ( i=0 ; i < data->n ; i++ )
    dof_remset_consistency_check( data->gen[i]->dof_remset, 
                                  data->gen[i]->order + data->first_gen_no );
# endif
#endif
  return 1;
}
#endif  /* USE_DOF_REMSETS */

static int inv98( dof_data_t *data ) /* semispace */
{
  int i;

  for ( i=0 ; i < data->n ; i++ ) {
    if (data->gen[i]->claim == 0 && data->gen[i]->live->current != -1)
      return 0;
    if (i < data->ap || 
        i == data->ap && gen_used( data->gen[i] ) == 0 ||
        i > data->cp && i < data->rp ||
        i == data->rp && gen_used( data->gen[i] ) == 0) {
      if (data->gen[i]->live->current > 0) {
        hardconsolemsg( "inv98#2: %d %d", i, data->gen[i]->live->current );
        return 0;
      }
    }
  }
  return 1;
}

static int fail_inv( dof_data_t *data, char *token )
{
  print_generation_stats( data );
  hardconsolemsg( "AP=%d, CP=%d, RP=%d", data->ap, data->cp, data->rp );
  panic_abort( "Invariant failed: %s.", token );
  return 0;
}

static void check_invariants( old_heap_t *heap, bool can_allocate )
{
  if (! inv1( DATA(heap) )) fail_inv( DATA(heap), "all-same-size" );
  if (! inv2( DATA(heap) )) fail_inv( DATA(heap), "no-overuse" );
  if (! inv3( DATA(heap) )) fail_inv( DATA(heap), "no-overflow" );
  if (! inv4( DATA(heap) )) fail_inv( DATA(heap), "be-real" );
  if (! inv5( DATA(heap) )) fail_inv( DATA(heap), "fixed-memory" );
  if (! inv6( DATA(heap) )) fail_inv( DATA(heap), "ordered-hps" );
  if (! inv7( DATA(heap) )) fail_inv( DATA(heap), "stay-inside-1" );
  if (! inv8( DATA(heap) )) fail_inv( DATA(heap), "stay-inside-2" );
  if (! inv9( DATA(heap) )) fail_inv( DATA(heap), "memory-location" );
  if (!inv10( DATA(heap) )) fail_inv( DATA(heap), "can-collect-1" );
  if (!inv11( DATA(heap) )) fail_inv( DATA(heap), "can-collect-2" );
  if (can_allocate)
    if (!inv12( DATA(heap) )) fail_inv( DATA(heap), "can-allocate" );
  if (!inv20( DATA(heap) )) fail_inv( DATA(heap), "size-block-aligned" );
  if (!inv21( DATA(heap) )) fail_inv( DATA(heap), "claim-block-aligned" );
  if (!inv22( DATA(heap) )) fail_inv( DATA(heap), "reserve-order" );
  if (!inv23( DATA(heap) )) fail_inv( DATA(heap), "alloc-order" );
  if (!inv90( DATA(heap) )) fail_inv( DATA(heap), "gen-id" );
  if (!inv91( DATA(heap) )) fail_inv( DATA(heap), "order=gen" );
  if (!inv92( heap )) fail_inv( DATA(heap), "remset-order" );
  if (!inv93( heap )) fail_inv( DATA(heap), "remset-content" );
  if (!inv94( heap )) fail_inv( DATA(heap), "remset-data" );
  if (!inv95( heap )) fail_inv( DATA(heap), "los-list-order" );
#if USE_DOF_REMSET
  if (!inv96( heap )) fail_inv( DATA(heap), "dof-remset-content" );
  if (!inv97( heap )) fail_inv( DATA(heap), "dof-remset-data" );
#endif
  if (!inv98( DATA(heap) )) fail_inv( DATA(heap), "semispace" );
}
#else  /* !INVARIANT_CHECKING */
static void check_invariants( old_heap_t *heap, bool can_allocate )
{
}
#endif /* if INVARIANT_CHECKING */

static int heap_free_space( old_heap_t *heap )
{
  const int infinity = -1;
  dof_data_t *data = DATA(heap);
  int free;
#if TRUTHFUL_FREE_SPACE
  int actual_free;
  gclib_stats_t stats;
#endif
  
  if (data->heap_limit == 0)
    return infinity;

  free = data->heap_limit - data->area_size;
#if TRUTHFUL_FREE_SPACE
  gclib_stats( &stats );
  if (stats.heap_limit > 0) {
    actual_free = words2bytes( stats.heap_limit - stats.heap_allocated );
    /* Round down */
    actual_free = data->quantum * (actual_free / data->quantum); 
    if (actual_free < free) {
      annoyingmsg( "  Less free than expected: %d < %d", actual_free, free );
      free = actual_free;
    }
  }
#endif
  return free;
}

/* A permuation is an int array of length MAX_GENERATIONS and it
   encodes the destination: v[i] -> v[perm[i]]
*/
static void init_permutation( int permutation[] )
{
  int i;

  for ( i=0 ; i < MAX_GENERATIONS ; i++ ) 
    permutation[i] = i;
}

/* Generalized generation shuffler:
   - Shuffles generations according to 'permutation':
     - shuffles generations in heap->data->gen
     - changes ID number on each generation
     - selects order number on each generation based on new location and CP
     - shuffles large-object-spaces to follow generations
     - sets page table bits according to new order number
   - Shuffles remembered sets according to new order numbers
  */
static void 
reorder_generations( old_heap_t *heap, int permutation[] )
{
  dof_data_t *data = DATA(heap);
  gen_t *oldgen[ MAX_GENERATIONS ], *g;
#if SPLIT_REMSETS
  dof_remset_t *oldrem[ MAX_GENERATIONS ];
#endif
#if SHADOW_REMSETS
  remset_t *oldshadow[ MAX_GENERATIONS ];
#endif
  int i, j, k, id, remset_perm[ MAX_GENERATIONS ], old_order;

  init_permutation( remset_perm );

  for ( i=0 ; i < data->n ; i++ )
    oldgen[i] = data->gen[i];

  for ( i=0 ; i < data->n ; i++ ) {
    k = permutation[ i + data->first_gen_no ];
    data->gen[ k - data->first_gen_no ] = g = oldgen[i];
    g->id = id = k - data->first_gen_no;
    old_order = g->order;
    g->order = (id <= data->cp ? 
                data->cp - id : 
                data->n - id + data->cp);
    ss_set_gen_no( g->live, g->order + data->first_gen_no );
    remset_perm[old_order + data->first_gen_no ] = g->order+data->first_gen_no;
  }

#if SPLIT_REMSETS
  /* Must permute the split remsets too. */
  for ( i=0 ; i < data->n ; i++ ) {
    for ( j=0 ; j < data->n ; j++ )
      oldrem[j] = data->gen[i]->dof_remsets[j];
    for ( j=0 ; j < data->n ; j++ ) {
      k = remset_perm[ j + data->first_gen_no ] - data->first_gen_no;
      data->gen[i]->dof_remsets[k] = oldrem[j];
    }
  }
#endif
#if SHADOW_REMSETS
  /* Must permute the shadow remsets also */
  if (data->use_shadow_remsets) {
    for ( i=0 ; i < data->n ; i++ ) 
      oldshadow[i] = data->shadow_remsets[i];
    for ( i=0 ; i < data->n ; i++ ) {
      k = remset_perm[i + data->first_gen_no ] - data->first_gen_no;
      data->shadow_remsets[k] = oldshadow[i];
    }
  }
#endif
  
  gc_permute_remembered_sets( heap->collector, remset_perm );
  los_permute_object_lists( heap->collector->los, remset_perm );
}

static void rotate_CP_to_0( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  int i, permutation[ MAX_GENERATIONS ];

  init_permutation( permutation );
  for ( i=data->first_gen_no ; i < data->first_gen_no+data->cp ; i++ ) 
    permutation[i] = i+1;
  permutation[i] = data->first_gen_no;
  reorder_generations( heap, permutation );
}

static void reset_order_numbers( old_heap_t *heap )
{
  int permutation[ MAX_GENERATIONS ];

  init_permutation( permutation );
  reorder_generations( heap, permutation );
}

static int round_up_to_block_size( int amt )
{
  return (int)(ceil( (double)amt / BLOCK_SIZE )*BLOCK_SIZE);
}

static int round_down_to_block_size( int amt )
{
  return (int)(floor( (double)amt / BLOCK_SIZE )*BLOCK_SIZE);
}

static void move_memory( gen_t *g_from, gen_t *g_to, int amount )
{
  semispace_t *from, *to;
#if !DYNAMIC_ALLOCATION
  int i, k, amt;
#endif
  
  if (amount == 0) return;

  from = g_from->live;
  to = g_to->live;

  assert( amount % BLOCK_SIZE == 0 );
#if !DYNAMIC_ALLOCATION
  assert( from->current >= 0 );

  if (from->chunks[from->current].top == from->chunks[from->current].bot && 
      from->allocated - (from->current+1)*BLOCK_SIZE < amount) {
    annoyingmsg( "  NOTE exceptional case in move_memory." );
    from->current--;
  }
  
  i = from->current+1;
  amt = amount;
  while (amt > 0) {
    k = ss_move_block_to_semispace( from, i, to );
    to->chunks[k].top = to->chunks[k].bot;
    amt -= BLOCK_SIZE;
  }
#endif

  g_from->claim -= amount;
  g_to->claim += amount;
}

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

static void remset_trace( old_heap_t *heap, char *type, int n1, int n2, int n3 )
{
#if REMSET_TRACE
  dof_data_t *data = DATA(heap);
  int i, j, k;
  int dofout[ MAX_GENERATIONS ], dofin[ MAX_GENERATIONS ];

  printf( "\nREMSET-TRACE (%-8s", type );
  if (n1 >= 0) printf( "%d ", n1 ); else printf( "  " );
  if (n2 >= 0) printf( "%d ", n2 ); else printf( "  " );
  if (n3 >= 0) printf( "%d ", n3 ); else printf( "  " );
# if USE_DOF_REMSET
  for ( i=0 ; i < data->n ; i++ ) {
#  if SPLIT_REMSETS
    dofout[i] = 0;
    k = data->gen[i]->order;
    dofin[i] = 0;
    for ( j=0 ; j < data->n ; j++ ) {
      dofout[i] += dof_rs_size(data->gen[i]->dof_remsets[j]);
      dofin[i] += dof_rs_size(data->gen[j]->dof_remsets[k]);
    }
#  else
    dofout[i] = dof_rs_size( data->gen[i]->dof_remset );
    dofin[i] = 0;
#  endif
  }
  printf( "(out ", type );
  for ( i=0 ; i < data->n ; i++ )
    printf( " %8s", eish(dofout[i]) );
  printf( ") (in  " );
  for ( i=0 ; i < data->n ; i++ )
    printf( " %8s", eish(dofin[i]) );
#  if REMSET_TRACE_ALL
  printf( ") " );
#  else
  printf( ")" );
#  endif
# endif
# if REMSET_TRACE_ALL
  /* Always 0 except for the static area, so why bother? */
  printf( "(gen " );
  for ( i=1 ; i < heap->collector->remset_count ; i++ )
    printf( " %8s", eish( heap->collector->remset[i]->live ) );
  printf( ")");
#  if SHADOW_REMSETS
  if (data->use_shadow_remsets) {
    printf( " (sha ");
    for ( i=0 ; i < data->n ; i++ )
      printf( " %8s", eish( data->shadow_remsets[i]->live ) );
    printf( ")" );
  }
#  endif
# endif
  printf( ")\n" );
  fflush( stdout );
#endif
}


typedef struct {
  msgc_context_t *context;
  int removed;
} scan_datum_t;

static bool fullgc_should_keep_p( word loc, void *data, unsigned *stats )
{
  if (msgc_object_marked_p( ((scan_datum_t*)data)->context, loc ))
    return TRUE;
  else {
    ((scan_datum_t*)data)->removed++;
    return FALSE;
  }
}

#if USE_DOF_REMSET
# if FULLGC_COMPACT_DOF_REMSET
static int sweep_one_remset( dof_remset_t *r, msgc_context_t *context )
{
  dof_pool_t *segment, *newsegment;
  word *slot, *slotlim, object, *newslot, *newslotlim;
  int scanned = 0, removed = 0;

  newsegment = r->first;
  newslot = newsegment->bot;
  newslotlim = newsegment->top;
  for ( segment=r->first ; segment ; segment=segment->next ) {
    slotlim = segment->top;
    for ( slot=segment->bot ; slot < slotlim ; slot++ ) {
      object = *slot;
      assert2(isptr(object));
      scanned++;
      if (!msgc_object_marked_p( context, *slot ))
        removed++;
      else {
        if (newslot == newslotlim) {
          newsegment = newsegment->next;
          newslot = newsegment->bot;
          newslotlim = newsegment->top;
        }
        *newslot = object;
        newslot++;
      }
    }
  }
  newsegment->top = newslot;
  r->curr = newsegment;
  r->curr_pools -= free_dof_pools( r->curr->next );
  r->curr->next = 0;

  /* The collector core currently requires that there be at least
     one free slot in a chunk in a set always.
     */
  if (r->curr->top == r->curr->lim)
    advance_dof_remset( r );
  
  r->stats.objs_scanned += scanned;
  r->stats.removed += removed;
  return removed;
}
# else
static int sweep_one_remset( dof_remset_t *r, msgc_context_t *context )
{
  dof_pool_t *segment;
  word *slot, *slotlim, object;
  int scanned = 0, removed = 0;

  for ( segment=r->first ; segment ; segment=segment->next ) {
    slotlim = segment->top;
    for ( slot=segment->bot ; slot < slotlim ; slot++ ) {
      object = *slot;
      if (object != 0) {
        assert2(isptr(object));
        scanned++;
        if (!msgc_object_marked_p( context, *slot )) {
          *slot = 0;
          removed++;
        }
      }
    }
  }
  r->stats.objs_scanned += scanned;
  r->stats.removed += removed;
  return removed;
}
# endif

static int
sweep_dof_remembered_sets( old_heap_t *heap, msgc_context_t *context )
{
  dof_data_t *data = DATA(heap);
  int i, j, removed_total = 0;

  for ( i=0 ; i < data->n ; i++ ) {
# if SPLIT_REMSETS
    for ( j=0 ; j < data->n ; j++ )
      removed_total += sweep_one_remset( data->gen[i]->dof_remsets[j],
                                         context );
# else
    removed_total += sweep_one_remset( data->gen[i]->dof_remset, context );
# endif
  }
  return removed_total;
}
#endif

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

static int sweep_large_objects( old_heap_t *heap, msgc_context_t *context )
{
  los_t *los = heap->collector->los;
  int i, marked=0, deleted=0, bmarked=0, bdeleted=0, size;
  word *p, *q, obj, hdr;
  
  obj = 0;                      /* Shuts up the compiler */
  assert( los_bytes_used( los, LOS_MARK1 ) == 0 );
  assert( los_bytes_used( los, LOS_MARK2 ) == 0 );

  for ( i=0 ; i < los->generations ; i++ ) {
    p = los_walk_list( los->object_lists[i], 0 );   /* first one */
    while (p != 0) {
      q = los_walk_list( los->object_lists[i], p ); /* next one */
      hdr = *p;
      /* Mismatched interfaces -- create tagged ptr from raw address */
      switch (header(hdr)) {
        case VEC_HDR : obj = tagptr( p, VEC_TAG ); break;
        case BV_HDR : obj = tagptr( p, BVEC_TAG ); break;
        case header(PROC_HDR) : obj = tagptr( p, PROC_TAG ); break;
        default : panic( "Bad case in sweep_large_objects()" );
      }
      size = sizefield( hdr );
      if (msgc_object_marked_p( context, obj )) {
        los_mark( los, los->mark1, p, i );
        marked++;
        bmarked += size;
      }
      else {
        deleted++;
        bdeleted += size;
      }
      p = q;
    }
    los_sweep( los, i );
    los_append_and_clear_list( los, los->mark1, i );
  }
  /* DEBUG -- replace with annoyingmsg at some point */
  consolemsg( "  LOS sweep: marked=%d, bmarked=%d, deleted=%d, bdeleted=%d", 
              marked, bmarked, deleted, bdeleted );
  return deleted;
}

/* Generational full GC */

#define PUSH_LIMIT   500       /* Just a guess */

typedef struct {
  int pushes;
  int *marked;
  int *traced;
  int *words_marked;
  msgc_context_t *context;
} push_context_t;

static void push_from_dof_remset( dof_remset_t *r, push_context_t *pcontext )
{
  dof_pool_t *segment;
  word *slot, *slotlim;
  msgc_context_t *context = pcontext->context;
  
  for ( segment=r->first ; segment ; segment=segment->next ) {
    slotlim = segment->top;
    for ( slot=segment->bot ; slot < slotlim ; slot++ ) {
      if (*slot != 0) {
        msgc_push_constituents( context, *slot );
        if (++pcontext->pushes >= PUSH_LIMIT) {
          pcontext->pushes = 0;
          msgc_mark_objects_from_roots( context, 
                                        pcontext->marked, 
                                        pcontext->traced, 
                                        pcontext->words_marked );
        }
      }
    }
  }
}

static bool pusher( word loc, void *data, unsigned *stats )
{
  push_context_t *pcontext = (push_context_t*)data;
  
  msgc_push_constituents( pcontext->context, loc );
  if (++pcontext->pushes >= PUSH_LIMIT) {
    pcontext->pushes = 0;
    msgc_mark_objects_from_roots( pcontext->context,
                                  pcontext->marked, 
                                  pcontext->traced, 
                                  pcontext->words_marked );
  }
  return TRUE;
}

/* When we're doing a full collection, most stuff that is very young
   is going to be live, by the oldest-first principle.  Therefore
   we can assume that the young stuff _is_ live, thereby cutting the
   marking cost.  Since the remembered sets already have exactly the
   right stuff in them, there is no added cost for maintaining the
   remembered sets.
   
   For all practical purposes, this is a mark-sweep radioactive decay
   collector.
   
   The only snag is that the remembered sets tend to be large, so
   we can't push all intergenerational pointers on the stack at once --
   we must interleave pushing and marking.

   It is not correct to call this collector after collection but before
   reset or decrement.
   
   FIXME: be sure to sweep only remsets that are subject to removal.  
   */

/* Fill the array with indices into data->gen[] */
#define FULLGC_GEN_LIMIT 1
static int select_generations_to_premark( old_heap_t *heap, int *gens )
{
  dof_data_t *data = DATA(heap);
  int i, k;
  
  /* We must premark all generations in the _order_ range RP downto x,
     for some x.  It's OK to premark none.
     */
  i = data->rp;
  k = 0;
  /* i != data->cp because that generation probably has a lot of float */
  while (k < FULLGC_GEN_LIMIT && k < data->n && i != data->cp) {
    gens[k] = i;
    k++;
    i = (i+1)%data->n;
  }
  return k;
}

static void 
generational_marking( old_heap_t *heap, msgc_context_t *context,
                      int *marked, int *traced, int *words_marked )
{
  dof_data_t *data = DATA(heap);
  int i, j, k, n;
  push_context_t pcontext;
  word *p;
  los_list_t *list;
  int gens[ MAX_GENERATIONS ];
  semispace_t *live;
  
  pcontext.pushes = 0;
  pcontext.context = context;
  pcontext.marked = marked;
  pcontext.traced = traced;
  pcontext.words_marked = words_marked;

  n = select_generations_to_premark( heap, gens );
  
  /* Mark some young stuff. */
  for ( k=0 ; k < n ; k++ ) {
    i = gens[k];
    for ( j=0; j <= data->gen[i]->live->current ; j++ ) {
      msgc_mark_range( context,
                       data->gen[i]->live->chunks[j].bot,
                       data->gen[i]->live->chunks[j].top );
    }
    list = heap->collector->los->
             object_lists[data->first_gen_no+data->gen[i]->order];
    for ( p=los_walk_list(list,0) ; p != 0 ; p = los_walk_list(list,p) )
      msgc_mark_object( context, (word)p );
  }
  /* Mark the static area */
  if (heap->collector->static_area != 0) {
    if (heap->collector->static_area->data_area != 0) {
      live = heap->collector->static_area->data_area;
      for ( j=0 ; j <= live->current ; j++ ) 
        msgc_mark_range( context, live->chunks[j].bot, live->chunks[j].top );
    }
    if (heap->collector->static_area->text_area != 0) {
      live = heap->collector->static_area->text_area;
      for ( j=0 ; j <= live->current ; j++ ) 
        msgc_mark_range( context, live->chunks[j].bot, live->chunks[j].top );
    }
  }

  /* Then interleave root pushing and tracing */
  for ( k=0; k < n ; k++ ) {
    i = gens[k];
    rs_enumerate( heap->collector->
                    remset[data->first_gen_no+data->gen[i]->order],
                  pusher,
                  &pcontext );

# if SHADOW_REMSETS
    if (data->use_shadow_remsets)
      rs_enumerate( data->shadow_remsets[data->gen[i]->order], 
                    pusher, 
                    &pcontext );
# endif
# if USE_DOF_REMSET
#  if SPLIT_REMSETS
    for ( j=0 ; j < data->n ; j++ )
      push_from_dof_remset( data->gen[i]->dof_remsets[j], &pcontext );
#  else
    push_from_dof_remset( data->gen[i]->dof_remset, &pcontext );
#  endif
# endif      
  }
  if (heap->collector->static_area)
    rs_enumerate( heap->collector->remset[data->first_gen_no+data->n],
                  pusher,
                  &pcontext );
  /* If there's anything left to mark, do it now. */
  msgc_mark_objects_from_roots( context, marked, traced, words_marked );
# if INVARIANT_CHECKING && EXPENSIVE_CHECKS_TOO
  msgc_assert_conservative_approximation( context );
# endif
}
/* End generational full GC */

static void full_collection( old_heap_t *heap )
{
  msgc_context_t *context;
  int marked=0, traced=0, removed=0, words_marked=0, deallocated=0;
  stats_id_t timer1, timer2;
  
  /* DEBUG -- replace with annoyingmsg at some point */
  consolemsg( " Full collection starts." );

  timer1 = stats_start_timer( TIMER_ELAPSED );
  timer2 = stats_start_timer( TIMER_CPU );
  
  context = msgc_begin( heap->collector );
  start( &dof.msgc_mark );
  if (DATA(heap)->fullgc_generational)
    generational_marking( heap, context, &marked, &traced, &words_marked );
  else
    msgc_mark_objects_from_roots( context, &marked, &traced, &words_marked );
  stop();

#if USE_DOF_REMSET
  start( &dof.sweep_dof_sets );
  removed = sweep_dof_remembered_sets( heap, context );
  stop();
#endif
  start( &dof.sweep_remset );
  removed += sweep_remembered_sets( heap->collector->remset,
                                    1,
                                    heap->collector->remset_count-1,
                                    context );
  stop();
#if SHADOW_REMSETS
  if (DATA(heap)->use_shadow_remsets) {
    start( &dof.sweep_shadow );
    removed += sweep_remembered_sets( DATA(heap)->shadow_remsets,
                                      0,
                                      DATA(heap)->n-1,
                                      context );
    stop();
  }
#endif
#if FULLGC_SWEEP_LOS
  start( &dof.sweep_los );
  deallocated = sweep_large_objects( heap, context );
  stop();
#endif
  msgc_end( context );

  DATA(heap)->stats.full_ms_collection += stats_stop_timer( timer1 );
  DATA(heap)->stats.full_ms_collection_cpu += stats_stop_timer( timer2 );
  DATA(heap)->stats.full_collections++;
  DATA(heap)->stats.full_objects_marked += marked;
  DATA(heap)->stats.full_pointers_traced += traced;
  DATA(heap)->stats.full_words_marked += words_marked;
  
  /* DEBUG -- ditto */
  consolemsg( " Full collection ends.  Marked=%d traced=%d removed=%d",
              marked, traced, removed );

  { char buf[20];
    sprintf( buf, "full %s", eish( removed ) );
    remset_trace( heap, buf, -1, -1, -1 );
  }
}

static void do_promote_in( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  gen_t *targets[ MAX_GENERATIONS+1 ];
  int i, j;

  for ( i=data->ap, j=0 ; i >= 0 ; i--, j++ )
    targets[j] = data->gen[i];
  targets[j] = 0;

  data->ap = 
    data->ap - 
    dof_copy_into_with_barrier( heap, data->first_gen_no, targets,
                                GCTYPE_PROMOTE, data );
  assert( data->ap >= 0 );
  /* Note, normal remset removal might have cleared the remset already.
     Might be better to avoid remset removal on the set because wholesale
     clearing is potentially faster.  But don't throw away the rs_clear:
     it also frees up memory used by the set.
     */
  rs_clear( heap->collector->remset[ data->first_gen_no ] );
#if USE_DOF_REMSET
  clear_dof_remset( heap, 0, FALSE );
#endif
#if SHADOW_REMSETS
  if (data->use_shadow_remsets) {
    start( &dof.assimilate_prom );
    rs_clear( data->shadow_remsets[0] );
    for ( i=1 ; i < data->n ; i++ )
      rs_assimilate_and_clear( data->shadow_remsets[i], 
                               heap->collector->remset[data->first_gen_no+i] );
    stop();
  }
#endif
}

static void promote_in( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  stats_id_t timer1, timer2;
  int live_before, live_after, ap_before, i, los_before, los_after;
#if GC_HIRES_TIMERS
  hrtime_t now;
#endif
  
  if (data->fullgc_on_promotion)
    full_collection( heap );
  
  annoyingmsg( " DOF promotion begins" );

  gc_signal_moving_collection( heap->collector );
  timer1 = stats_start_timer( TIMER_ELAPSED );
  timer2 = stats_start_timer( TIMER_CPU );

  ap_before = data->ap;
  live_before = gen_used( data->gen[ap_before] );
  los_before = los_bytes_used( heap->collector->los, 
                               data->gen[ap_before]->order+data->first_gen_no);

#if GC_HIRES_TIMERS /* hack -- start() and stop() don't nest */
  now = gethrtime();
#endif
  do_promote_in( heap );
#if GC_HIRES_TIMERS
  dof.promtime += gethrtime() - now;
#endif
  
  live_after = 0;
  los_after = 0;
  for ( i=ap_before ; i >= data->ap ; i-- ) {
    live_after += gen_used( data->gen[i] );
    los_after += los_bytes_used( heap->collector->los,
                                 data->gen[i]->order + data->first_gen_no );
  }
  data->consing += live_after - live_before;

  /* See comments in perform_collection(). */
  data->stats.words_copied += bytes2words( live_after - live_before );
  data->stats.words_moved += bytes2words( los_after - los_before );
#if GC_EVENT_COUNTERS
  dof.copied_by_prom += bytes2words( live_after - live_before );
  dof.moved_by_prom += bytes2words( los_after - los_before );
#endif
  data->gen[ap_before]->stats.ms_promotion += stats_stop_timer( timer1 );
  data->gen[ap_before]->stats.ms_promotion_cpu += stats_stop_timer( timer2 );
  data->gen[ap_before]->stats.promotions++;

  annoyingmsg( " DOF promotion ends: AP=%d CP=%d RP=%d promoted=%d",
               data->ap, data->cp, data->rp, live_after-live_before );
  remset_trace( heap, "promote", ap_before, data->ap, -1 );
}

static void grow_all_generations( old_heap_t *heap, int per_generation )
{
  dof_data_t *data = DATA(heap);
  int i, blocks;

  assert(per_generation % BLOCK_SIZE == 0);

  data->s += per_generation;
  data->area_size += per_generation*(data->n - 1);
  data->max_size = max( data->max_size, data->area_size );
  for ( i=0 ; i < data->n ; i++ ) {
    gen_t *g = data->gen[i];

    g->size += per_generation;
    if (i > 0) {
      g->claim += per_generation;
      blocks = per_generation / BLOCK_SIZE;
#if !DYNAMIC_ALLOCATION
      { int j;
        for ( j=0 ; j < blocks ; j++ )
          ss_allocate_block_unconditionally( g->live, BLOCK_SIZE );
      }
#endif
    }
  }
}

static int growth_function( old_heap_t *heap, int new, int old ) 
{ 
  return (new-old)/DATA(heap)->growth_divisor;
}

static void
expand_heap_after_reset( old_heap_t *heap, int new_size, double mark_cons )
{
  dof_data_t *data = DATA(heap);
  int growth, per_block, free;

  free = heap_free_space( heap ); /* Negative means "as much as you like" */
  if (free < 0)
    growth = growth_function( heap, 
                              new_size, 
                              data->area_size );
  else
    growth = growth_function( heap,
                              min( new_size, data->area_size+free ),
                              data->area_size );
  per_block = round_down_to_block_size( ceil( growth/(data->n - 1)) );

  if (per_block == 0)           /* Rounding happens */
    return;

  consolemsg( "  Expanding heap.  Old=%d, new=%d, mark/cons=%.3f",
              data->area_size,
              data->area_size + (per_block * (data->n - 1)),
              mark_cons );
  grow_all_generations( heap, per_block );
}

static void reset_after_collection( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  double mark_cons = (double)(data->copying)/(double)(data->consing);
  int free = 0, old_rp, new_size, permutation[ MAX_GENERATIONS ], i;

  if (data->copying == 0 && data->consing == 0)
    panic_abort( "DOF GC bug: mark/cons ratio is 0/0." );

  if (mark_cons > 20.0 && heap_free_space( heap ) == 0) {
    if (data->retry_ok) {
      /* Try a full collection */
      consolemsg( "  Heap is nearly full (mark/cons=%.3f)." );
      full_collection( heap );
      data->gc_counter = 0;
      data->retry_ok = FALSE;
    }
    else
      panic( "Heap is full." );
  }
  else
    data->retry_ok = TRUE;

  start( &dof.reset_after_gc );
  data->stats.resets++;
  if (data->cp > 0) {
    annoyingmsg( "  Reset case 1.  AP=%d, CP=%d, RP=%d, mark/cons=%.3f",
                 data->ap, data->cp, data->rp, mark_cons );
    free = data->cp;
  }
  else if (data->rp == data->cp+2) {
    annoyingmsg( "  Reset case 2.  AP=%d, CP=%d, RP=%d, mark/cons=%.3f",
                 data->ap, data->cp, data->rp, mark_cons );
    free = data->cp+1;
  }
  else 
    panic_abort( "Impossible case reset_after_collection: "
                 "AP=%d CP=%d RP=%d", data->ap, data->cp, data->rp );
  move_memory( data->gen[0], data->gen[data->cp+1], data->gen[0]->claim );
  old_rp = data->rp;
  
  /* CP at least is used by reorder_generations, so don't move these below */
  data->ap = old_rp - 1;
  data->rp = data->n - 1;
  data->cp = data->n - 2;

  /* Shift empty generation to last slot: new reserve */
  init_permutation( permutation );
  for ( i=free+1 ; i < data->n ; i++ )
    permutation[data->first_gen_no+i] = data->first_gen_no+i-1;
  permutation[data->first_gen_no+free] = data->first_gen_no+data->n-1;
  reorder_generations( heap, permutation );
  
  data->total_copying += data->copying;
  data->total_consing += data->consing;
  data->copying = 0;
  data->consing = 0;
  /* FIXME: does not take into account other heap areas */
  new_size = rint( data->load_factor *
                   (data->area_size / ((data->n - 1.0) / (data->n - 2.0) +
                                       (1.0 / mark_cons ))));
  if (heap_free_space( heap ) != 0 && new_size > data->area_size)
    expand_heap_after_reset( heap, new_size, mark_cons );
  stop();

  remset_trace( heap, "reset", -1, -1, -1 );

  if (data->fullgc_on_reset) {
    if (data->full_frequency && ++data->gc_counter == data->full_frequency) {
      full_collection( heap );
      data->gc_counter = 0;
    }
  }
}

static void decrement_after_collection( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  gen_t *reserve, *victim, *first, *middle;
  int need;

  start( &dof.decrement_after_gc );
  reserve = data->gen[data->rp];
  victim = data->gen[data->cp];

  if (reserve->size - gen_used( reserve ) == 0) {
    annoyingmsg( "  Decrement: pre-stepping RP, value was %d", data->rp );
    data->rp--;
    reserve = data->gen[data->rp];
  }

 again:
  if (data->cp == data->rp) {
    annoyingmsg( "  Decrement case 1" );
    data->cp--;
    reset_order_numbers( heap );
  }
  else if (reserve->size - gen_used( reserve ) < GC_LARGE_OBJECT_LIMIT) {
    annoyingmsg( "  Decrement: stepping RP for fragmentation, "
                 "used=%d value was %d",
                 gen_used( reserve ), data->rp );
    data->rp--;
    reserve = data->gen[data->rp];
    goto again;
  }
  else if (data->cp == data->rp - 1) {
    annoyingmsg( "  Decrement case 2" );
    first = data->gen[0];
    move_memory( victim, reserve, data->s - reserve->claim );
    move_memory( victim, 
                 first, 
                 round_down_to_block_size( victim->claim - 
                                           (gen_used( reserve ) +
                                            GC_LARGE_OBJECT_LIMIT )));
    if (victim->claim == 0) {
      rotate_CP_to_0( heap );
      data->ap++;
    }
    else {
      data->cp--;
      reset_order_numbers( heap );
    }
  }
  else if (data->cp == data->rp - 2) {
    annoyingmsg( "  Decrement case 3" );
    middle = data->gen[ data->rp - 1 ];
    first = data->gen[0];
    need = (data->s + GC_LARGE_OBJECT_LIMIT) - 
      (middle->claim + gen_free(reserve));
    move_memory( victim, middle, round_up_to_block_size( need ) );
    move_memory( victim, first, data->s - first->claim );
    rotate_CP_to_0( heap );
    data->ap++;
  }
  else
    panic_abort( "Impossible case decrement_after_collection: "
                 "AP=%d CP=%d RP=%d", data->ap, data->cp, data->rp );
  stop();
}

#if DYNAMIC_ALLOCATION
static void free_unused_memory( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  int i;

  for ( i=0 ; i < data->n ; i++ ) {
    ss_chunk_t *c = &data->gen[i]->live->chunks[ data->gen[i]->live->current ];
    if (c->top == c->bot && c->bytes > 0)
      data->gen[i]->live->current--;
    ss_free_unused_chunks( data->gen[i]->live );
  }
}
#endif

static void post_collection_policy( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  
  gen_clear( data->gen[ data->cp ] );
  if (data->cp > data->ap)
    decrement_after_collection( heap );
  else
    reset_after_collection( heap );
#if DYNAMIC_ALLOCATION
  start( &dof.free_unused );
  /* FIXME: this should be added to the cost of GC, but it is generally
     not terribly large (200ms or so on our benchmarks) and it's unclear
     whether the ms timer has a fine enough resolution to give an accurate
     picture of it.
     */
  free_unused_memory( heap );
  stop();
#endif
}

static void do_perform_collection( old_heap_t *heap )
{
  gen_t *targets[ MAX_GENERATIONS+1 ];
  dof_data_t *data = DATA(heap);
  int i, j;

#if SHADOW_REMSETS
  if (data->use_shadow_remsets) {
    start( &dof.assimilate_gc );
    for ( i=0 ; i < data->n ; i++ )
      rs_assimilate_and_clear( heap->collector->remset[ data->first_gen_no+i ],
                               data->shadow_remsets[ i ] );
    stop();
  }
#endif
  
  /* Copy into gen(RP) and perhaps gen(RP-1). */
  for ( i=data->rp, j=0 ; i > data->cp ; i--, j++ )
    targets[j] = data->gen[i];
  targets[j] = 0;
  
  assert( data->gen[data->cp]->order == 0 );
  data->rp = 
    data->rp -
    dof_copy_into_with_barrier( heap, data->first_gen_no+1, targets, 
                                GCTYPE_COLLECT, data );

  /* See comments about rs_clear() in promote_in(). */
  rs_clear( heap->collector->remset[ data->first_gen_no ] );
  rs_clear( heap->collector->remset[ data->first_gen_no+1 ] );
#if USE_DOF_REMSET
  clear_dof_remset( heap, 0, TRUE );
  clear_dof_remset( heap, 1, FALSE );
#endif
#if SHADOW_REMSETS
  if (data->use_shadow_remsets) {
    rs_clear( data->shadow_remsets[0] );
    rs_clear( data->shadow_remsets[1] );
    start( &dof.assimilate_gc );
    for ( i=2 ; i < data->n ; i++ ) {
      rs_assimilate( data->shadow_remsets[i],
                     heap->collector->remset[ data->first_gen_no+i ] );
      rs_clear( heap->collector->remset[ data->first_gen_no+i ] );
    }
    stop();
  }
#endif
}

static void perform_collection( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  int i, live_after, rp_before, live_before, los_before, los_after, cp_before;
  stats_id_t timer1, timer2;
#if GC_HIRES_TIMERS
  hrtime_t now;
#endif
  
  annoyingmsg("  Collecting: AP=%d CP=%d RP=%d", data->ap, data->cp, data->rp);

  gc_signal_moving_collection( heap->collector );
  timer1 = stats_start_timer( TIMER_ELAPSED );
  timer2 = stats_start_timer( TIMER_CPU );

#if GC_HIRES_TIMERS
  now = gethrtime();            /* hack: start()/stop() don't nest */
#endif
  rp_before = data->rp;
  cp_before = data->cp;
  live_before = gen_used( data->gen[rp_before] );
  los_before = los_bytes_used( heap->collector->los,
                               data->gen[rp_before]->order+data->first_gen_no);

  do_perform_collection( heap );
  
  live_after = 0;
  los_after = 0;
  for ( i=rp_before ; i >= data->rp ; i-- ) {
    live_after += gen_used( data->gen[i] );
    los_after += los_bytes_used( heap->collector->los,
                                 data->gen[i]->order + data->first_gen_no );
  }  
  data->copying += live_after - live_before;
#if GC_HIRES_TIMERS
  dof.gctime += gethrtime() - now;
#endif
  /* For simplicity's sake the time and the collection counter increment 
     is given to the area in which RP was before the collection, even 
     though the collection may have copied data into two generations.
     */
  data->stats.words_copied += bytes2words( live_after - live_before );
  data->stats.words_moved += bytes2words( los_after - los_before );
#if GC_EVENT_COUNTERS
  dof.copied_by_gc += bytes2words( live_after - live_before );
  dof.moved_by_gc += bytes2words( los_after - los_before );
#endif
  data->gen[rp_before]->stats.collections++;
  data->gen[rp_before]->stats.ms_collection += stats_stop_timer( timer1 );
  data->gen[rp_before]->stats.ms_collection_cpu += stats_stop_timer( timer2 );

  remset_trace( heap, "collect", cp_before, rp_before, data->rp );

  /* post_collection_policy changes the meaning of rp_before! */
  
  post_collection_policy( heap );
}

static void maybe_collect( old_heap_t *heap, double factor )
{
  dof_data_t *data = DATA(heap);
  bool repeating = FALSE;
  double copying_before;

  while (free_in_allocation_area(data) < 
         factor*space_needed_for_promotion(data)) {
    annoyingmsg( " DOF collection begins" );
    if (repeating) 
      data->stats.repeats++;

    if (data->fullgc_on_collection) {
      if (data->full_frequency && ++data->gc_counter == data->full_frequency) {
        full_collection( heap );
        check_invariants( heap, FALSE );
        data->gc_counter = 0;
      }
    }
    copying_before = data->copying + data->total_copying;

    perform_collection( heap );
    check_invariants( heap, FALSE );

    annoyingmsg( " DOF collection ends: AP=%d CP=%d RP=%d survivors=%.0f",
                 data->ap, data->cp, data->rp, 
                 data->copying + data->total_copying - copying_before );
    repeating = TRUE;
  }
}

static int initialize( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  int esize, dofsize, i;

  esize = gc->young_area->maximum;
  for ( i=0 ; i < gc->ephemeral_area_count ; i++ )
    esize += gc->ephemeral_area[i]->maximum;

  data->ephemeral_size = esize;

  /* Establish the initial invariant: there is enough space in the DOF
     area to promote from the ephemeral area even if the ephemeral area
     is completely full of live data.
     */
  dofsize = data->s*(data->n-2); /* Actual space available */
  if (dofsize < esize) {
    int growth = roundup( (data->n-1)*(esize-dofsize)/(data->n-2), 
                          data->quantum );
    if (data->heap_limit && data->area_size + growth > data->heap_limit)
      panic( "DOF heap is too small to accomodate promotion." );
    grow_all_generations( heap, growth / (data->n - 1) );
  }
#if INVARIANT_CHECKING
  for ( i=0 ; i < data->n ; i++ ) {
    int x = data->first_gen_no+data->gen[i]->order;
    data->gen[i]->remset = gc->remset[x];
    data->gen[i]->los = gc->los->object_lists[x];
  }
#endif
  return 1;
}

#if DYNAMIC_ALLOCATION && FULLGC_ON_LOW_RESERVE
static bool 
trigger_full_if_reserve_low( old_heap_t *heap, int need, char *who )
{
  dof_data_t *data = DATA(heap);
  gclib_stats_t stats;

  gclib_stats( &stats );
  if (stats.heap_limit > 0 &&
      need > words2bytes(stats.heap_limit-stats.heap_allocated)) {
    /* DEBUG turn this message into annoyingmsg at some point */
    consolemsg( " Triggering full gc for %s -- reserve low by %d bytes.",
                who,
                need - words2bytes(stats.heap_limit-stats.heap_allocated) );
    full_collection( heap );
    check_invariants( heap, FALSE );
    data->gc_counter = 0;
    return TRUE;
  }
  else
    return FALSE;
}
#endif

static void collect( old_heap_t *heap, gc_type_t request )
{
  dof_data_t *data = DATA(heap);
  bool done_full = FALSE;
  
  annoyingmsg( "DOF collection cycle begins: AP=%d CP=%d RP=%d",
               data->ap, data->cp, data->rp );

 
  check_invariants( heap, TRUE );

#if DYNAMIC_ALLOCATION && FULLGC_ON_LOW_RESERVE
  done_full = 
    trigger_full_if_reserve_low( 
      heap, 
      (int)(data->ephemeral_size*data->free_before_promotion),
      "promotion" );
#endif

  promote_in( heap );
  check_invariants( heap, FALSE );

#if DYNAMIC_ALLOCATION && FULLGC_ON_LOW_RESERVE
  if (!done_full) 
    done_full = 
      trigger_full_if_reserve_low( 
        heap, 
        (int)(data->gen[0]->size*data->free_before_collection),
        "collection" );
#endif

  maybe_collect( heap, data->free_after_collection );
  check_invariants( heap, TRUE );

  annoyingmsg( "DOF collection cycle ends" );
}

static void before_collection( old_heap_t *heap )
{
  int i;
  int used;

  heap->maximum = DATA(heap)->area_size;
  
  used = 0;
  for ( i=0 ; i < DATA(heap)->n ; i++ )
    used += gen_used( DATA(heap)->gen[i] );
  heap->allocated = used;
}

static void after_collection( old_heap_t *heap )
{
}

#if USE_DOF_REMSET
static void dof_remset_stats( dof_remset_t *r )
{
  /* The 'live' field is not accurately computed because a little
     bit of machinery is required to track it during GC.  Easy to fix.
     When fixed, can't use memset() to clear the fields.
  */
  r->stats.allocated = bytes2words(r->curr_pools * DOF_REMSET_SIZE);
  r->stats.used = r->stats.allocated - (r->curr->lim - r->curr->top);
  r->stats.live = r->stats.used;                    /* FIXME */
  stats_add_remset_stats( r->self, &r->stats );
  memset( &r->stats, 0, sizeof( remset_stats_t ) );
}
#endif

static void stats( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  int i, j;

  for ( i=0 ; i < data->n ; i++ ) {
    gen_t *g = data->gen[i];
    int gen = data->first_gen_no + g->order;
    int los_words = bytes2words( los_bytes_used( heap->collector->los, gen ) );

    /* generation */
    ss_sync( g->live );
    g->stats.target = bytes2words(g->size);
    g->stats.allocated = bytes2words(g->claim) + los_words;
    g->stats.used = bytes2words(g->live->used) + los_words;
    stats_add_gen_stats( g->self, &g->stats );
    memset( &g->stats, 0, sizeof( gen_stats_t ) );

    /* remset */
    rs_stats( heap->collector->remset[ gen ] );
#if SHADOW_REMSETS
    if (data->use_shadow_remsets)
      rs_stats( data->shadow_remsets[i] );
#endif
#if USE_DOF_REMSET
# if SPLIT_REMSETS
    for ( j=0 ; j < data->n ; j++ )
      dof_remset_stats( g->dof_remsets[j] );
# else
    dof_remset_stats( g->dof_remset );
# endif
#endif
  }

  /* overall */
  stats_add_gc_stats( &data->stats );
  stats_set_gc_event_stats( &dof );
  memset( &data->stats, 0, sizeof( data->stats ) );
}

static word *data_load_area( old_heap_t *heap, int nbytes )
{
  panic( "DOF gc: data_load_area not implemented, use static area." );
  /* Not reached */
  return 0;
}

old_heap_t *
create_dof_area( int gen_no, int *gen_allocd, gc_t *gc, dof_info_t *info )
{
  int generations = info->generations;
  int area_size = info->area_size;
  double load_factor = info->load_factor;
  int dynamic_min = info->dynamic_min;
  int heap_limit = info->dynamic_max;
  int full_frequency = info->full_frequency;
  double growth_divisor = info->growth_divisor;
  dof_data_t *data;
  int i, j, quantum;
  old_heap_t *heap;

  assert( area_size > 0 );
  assert( generations > 0 );
  assert( load_factor >= 1.0 );
  assert( BLOCK_SIZE % PAGESIZE == 0 );
  
  area_size = round_up_to_block_size(max( area_size, dynamic_min ));
  heap_limit = round_up_to_block_size(heap_limit);
  
  data = (dof_data_t*)must_malloc( sizeof( dof_data_t ) );
  memset( data, 0, sizeof( data ) );
  data->load_factor = load_factor;
  data->full_frequency = full_frequency;
  data->retry_ok = TRUE;
  data->growth_divisor = growth_divisor;
  data->free_before_promotion = info->free_before_promotion;
  data->free_before_collection = info->free_before_collection;
  data->free_after_collection = info->free_after_collection;
  data->use_shadow_remsets = !info->no_shadow_remsets;
  data->fullgc_generational = info->fullgc_generational;
  data->fullgc_on_promotion = info->fullgc_on_promotion;
  data->fullgc_on_collection = 
    info->fullgc_on_collection && !info->fullgc_on_promotion;
  data->fullgc_on_reset = 
    !data->fullgc_on_collection && !info->fullgc_on_promotion;
  data->ephemeral_size = 0;     /* Will be set by initialize() */
  data->quantum = quantum = (BLOCK_SIZE * (generations+1));
  /* quantum is divisible by PAGESIZE if BLOCK_SIZE is. */
  data->heap_limit = (heap_limit / quantum) * quantum; /* Rounds */
  data->first_gen_no = gen_no;

  if (heap_limit > 0)
    area_size = min( area_size, heap_limit );
  
  data->n = generations+2;
  data->s = round_down_to_block_size( area_size / (data->n - 1) );
  data->area_size = data->s*(data->n-1);

  assert( data->s > 0 );
  assert( data->area_size > 0 );
  
  data->gen = (gen_t**)must_malloc( data->n*sizeof( gen_t* ) );
  for ( i=0 ; i < data->n ; i++ ) {
    gen_t *g;

    data->gen[i] = g = (gen_t*)must_malloc( sizeof(gen_t) );
    g->self = stats_new_generation( gen_no + i, 0 );
    g->id = i;
    g->size = data->s;
    g->claim = (i == 0 ? 0 : data->s);
    g->order = (i == data->n-1 ? data->n-1 : (data->n-2-i));
    if (i == 0)
      g->live = create_semispace_n( 0, 0, gen_no+g->order );
    else
#if DYNAMIC_ALLOCATION
      /* variable allocation -- allocate nothing */
      g->live = create_semispace_n( 0, 0, gen_no+g->order );
#else
      /* fixed allocation */
      g->live = 
        create_semispace_n( BLOCK_SIZE, 
                            (data->s / BLOCK_SIZE), 
                            gen_no+g->order );
#endif
    memset( &g->stats, 0, sizeof( gen_stats_t ) );
#if USE_DOF_REMSET
# if SPLIT_REMSETS
    for ( j=0 ; j < data->n ; j++ )
      g->dof_remsets[j] = make_dof_remset( gen_no+i, j+1 );
    for ( j=data->n ; j < MAX_GENERATIONS ; j++ )
      g->dof_remsets[j] = 0;
# else
    g->dof_remset = make_dof_remset( gen_no+i, 1 );
# endif
#endif
#if INVARIANT_CHECKING
    g->remset = 0;
    g->los = 0;
#endif
  }

  data->ap = data->n - 2;
  data->cp = data->n - 2;
  data->rp = data->n - 1;
  /* Other fields set to zero by memset() above */

#if SHADOW_REMSETS
  if (data->use_shadow_remsets) {
    for ( i=0 ; i < data->n ; i++ ) 
      data->shadow_remsets[i] = 
        create_labelled_remset( 2048, 0, 2048,  /* FIXME? */
                                &data->shadow_ssb_loc[i].ssb_bot,
                                &data->shadow_ssb_loc[i].ssb_top,
                                &data->shadow_ssb_loc[i].ssb_lim,
                                i+data->first_gen_no, 255 );
  }
#endif
  
  heap = create_old_heap_t( "dof", 
                            HEAPCODE_DOF,
                            initialize,
                            collect,
                            before_collection,
                            after_collection,
                            stats,
                            data_load_area,
                            0,  /* load_prepare */
                            0,  /* load_data */
                            0,  /* set_policy */
                            data );
  heap->collector = gc;
  check_invariants( heap, TRUE );
  *gen_allocd = data->n;
  return heap;
}

void dof_gc_parameters( old_heap_t *heap, int *size )
{
  *size = DATA(heap)->gen[0]->size;
}

/* The following macros are identical to the ones in cheney.c
     remset_scanner_core()
     forw_np()
     forw_np_record()
     forw_np_partial()
     forw_core_np()
     check_space_np()
     remember_pair()
   The following macros have been modified:
     scan_core_partial()  -- fixed a bug
     remember_vec()       -- deep changes
   I've kept the _np names to avoid gratuitous changes.
   */

/* Forwarding header (should be defined elsewhere?).

   This bit pattern is an unused immediate and can be generated in a single
   cycle on most machines (it's -2).
   */
#define FORWARD_HDR      0xFFFFFFFE

#if INVARIANT_CHECKING && EXPENSIVE_CHECKS_TOO
#  define CHECK_EVERY_WORD 1
#else
#  define CHECK_EVERY_WORD 0
#endif

#if CHECK_EVERY_WORD
# define check_memory( ptr, nwords )            \
    gclib_check_memory_validity( ptr, nwords )
# define check_address( ptr )                                           \
    do { if (((word)(ptr) & 7) != 0)                                    \
           panic_abort( "Odd address for forw. ptr: 0x%08x!", (ptr) );  \
    } while(0)
#else
# define check_memory( ptr, nwords ) (void)0
# define check_address( ptr )  (void)0
#endif


#define remset_scanner_core( ptr, p, FORW, count )      \
  p = ptrof( ptr );                                     \
  if (tagof( ptr ) == PAIR_TAG) {                       \
    FORW;                                               \
    ++p;                                                \
    FORW;                                               \
    count += 2;                                         \
  }                                                     \
  else {                                                \
    word words = sizefield( *p ) / 4;                   \
    COUNT_REMSET_LARGE_OBJ( words );                    \
    count += words;                                     \
    while (words--) {                                   \
      ++p;                                              \
      FORW;                                             \
    }                                                   \
  }

#define forw_np( loc, forw_limit_gen, dest, lim, e )                          \
  do { word T_obj = *loc;                                                     \
       if (isptr( T_obj ) && gen_of(T_obj) < (forw_limit_gen)){ \
          forw_core_np( T_obj, loc, dest, lim, e );                           \
       }                                                                      \
  } while( 0 )

/* NOTE that the justification for _not_ recomputing the generation
   of the forwarded object that is used in the GEN and NP collectors
   does not hold here, so we need to recompute.  See cheney.c.
   */
#define forw_np_record( loc, forw_limit_gen, dest, lim, has_intergen_ptr, \
                        old_obj_gen, e )                                  \
  do { word T_obj = *loc;                                                 \
       if (isptr( T_obj )) {                                              \
          unsigned T_obj_gen = gen_of(T_obj);                             \
          if (T_obj_gen < (forw_limit_gen)) {                             \
            forw_core_np( T_obj, loc, dest, lim, e );                     \
            T_obj_gen = gen_of(T_obj);                                    \
          }                                                               \
          if (T_obj_gen < (old_obj_gen)) has_intergen_ptr=1;              \
       }                                                                  \
  } while( 0 )

/* This is slow -- gclib_desc_g is a global variable that can't
   be cached locally because its value may change due to memory 
   allocation.  

   Tuned extra for the case when an object does not need to be
   forwarded, as an experiment.
*/
#if !SPLIT_REMSETS
#define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen,  \
                         must_add_to_extra, e )                         \
  do { word T_obj = *loc;                                               \
       COUNT_WORDS_FORWARDED();                                         \
       if ( isptr( T_obj ) ) {                                          \
           word T_g = gen_of(T_obj);                                    \
           COUNT_PTRS_FORWARDED();                                      \
           if (T_g < (forw_limit_gen)) {                                \
             forw_core_np( T_obj, loc, dest, lim, e );                  \
             T_obj = *loc;                                              \
             T_g = gen_of(T_obj);                                       \
           }                                                            \
           if (T_g < (np_young_gen)) {                                  \
             must_add_to_extra = 1;                                     \
             COUNT_GC_BARRIER_HIT();                                    \
           }                                                            \
       }                                                                \
  } while( 0 )
#else  /* SPLIT_REMSETS */
# if EXTRA_GEN_BITVECTOR && !EXTRA_GEN_LOOKUP
# define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen, \
                         must_add_to_extra, e )                         \
  do { word T_obj = *loc;                                               \
       COUNT_WORDS_FORWARDED();                                         \
       if ( isptr( T_obj ) ) {                                          \
           word T_g = gen_of(T_obj);                                    \
           COUNT_PTRS_FORWARDED();                                      \
           if (T_g < (forw_limit_gen)) {                                \
             forw_core_np( T_obj, loc, dest, lim, e );                  \
             T_obj = *loc;                                              \
             T_g = gen_of(T_obj);                                       \
           }                                                            \
           if (T_g < (np_young_gen)) {                                  \
             extra_gen |= (1 << T_g);                                   \
             COUNT_GC_BARRIER_HIT();                                    \
           }                                                            \
       }                                                                \
  } while( 0 )
# elif EXTRA_GEN_LOOKUP
#  if EXTRA_GEN_BITVECTOR
#    define testit()  ((extra_gen & (1 << T_g)) == 0)
#    define markit()  extra_gen |= (1 << T_g)
#  else
#    define testit()  (extra_gen[T_g] != the_obj)
#    define markit()  extra_gen[T_g] = the_obj
#  endif
  /* Note free variable "the_obj". */
  /* Remset pointers have been commented out because the loads
     have been lifted outside the scanning loop (big win).
     */
# define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen, \
                         must_add_to_extra, e )                         \
  do { word T_obj = *loc;                                               \
       COUNT_WORDS_FORWARDED();                                         \
       if ( isptr( T_obj ) ) {                                          \
           word T_g = gen_of(T_obj);                                    \
           COUNT_PTRS_FORWARDED();                                      \
           if (T_g < (forw_limit_gen)) {                                \
             forw_core_np( T_obj, loc, dest, lim, e );                  \
             T_obj = *loc;                                              \
             T_g = gen_of(T_obj);                                       \
           }                                                            \
           if (T_g < (np_young_gen) && testit()) {                      \
             /* word **remtops = e->tospaces[e->scan_idx].remset_tops; */    \
             /* word **remlims = e->tospaces[e->scan_idx].remset_lims; */    \
             word *remtop = remtops[T_g];                               \
             markit();                                                  \
             *remtop = the_obj; remtop = remtop+1;                      \
             remtops[T_g] = remtop;                                     \
             if (remtop == remlims[T_g])                                \
               dof_remset_advance(e,T_g);                               \
             COUNT_GC_BARRIER_HIT();                                    \
           }                                                            \
       }                                                                \
  } while( 0 )
# else
# define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen, \
                         must_add_to_extra, e )                         \
  do { word T_obj = *loc;                                               \
       COUNT_WORDS_FORWARDED();                                         \
       if ( isptr( T_obj ) ) {                                          \
           word T_g = gen_of(T_obj);                                    \
           COUNT_PTRS_FORWARDED();                                      \
           if (T_g < (forw_limit_gen)) {                                \
             forw_core_np( T_obj, loc, dest, lim, e );                  \
             T_obj = *loc;                                              \
             T_g = gen_of(T_obj);                                       \
           }                                                            \
           if (T_g < (np_young_gen)) {                                  \
             extra_gen[T_g] = 1;                                        \
             must_add_to_extra = 1;                                     \
             COUNT_GC_BARRIER_HIT();                                    \
           }                                                            \
       }                                                                \
  } while( 0 )
# endif
#endif

#define forw_core_np( T_obj, loc, dest, lim, e )        \
  word *TMP_P = ptrof( T_obj );                         \
  word TMP_W = *TMP_P;                                  \
  if (TMP_W == FORWARD_HDR)                             \
    *loc = *(TMP_P+1);                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                \
    check_space_np(dest,lim,8,e);                       \
    *dest = TMP_W;                                      \
    *(dest+1) = *(TMP_P+1);                             \
    check_address( TMP_P );                             \
    *TMP_P = FORWARD_HDR;                               \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);   \
    check_memory( dest, 2 );                            \
    dest += 2;                                          \
  }                                                     \
  else {                                                \
    word *TMPD;                                         \
    check_space_np(dest,lim,sizefield(TMP_W)+4,e);      \
    TMPD = dest;                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD; \
  }

#define check_space_np( dest, lim, wanted, e )                               \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    expand_semispace_np( &CS_LIM, &CS_DEST, (wanted), e );                   \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

#define scan_core_partial( ptr, iflush, FORW, must_add_to_extra, e )          \
  do {                                                                        \
    word T_w = *ptr;                                                          \
    assert2( T_w != FORWARD_HDR);                                             \
    if (ishdr( T_w )) {                                                       \
      word T_h = header( T_w );                                               \
      if (T_h == BV_HDR) {                                                    \
        /* bytevector: skip it, and flush the icache if code */               \
        word *T_oldptr = ptr;                                                 \
        word T_bytes = roundup4( sizefield( T_w ) );                          \
        ptr = (word *)((word)ptr + (T_bytes + 4)); /* doesn't skip padding */ \
        if (!(T_bytes & 4)) *ptr++ = 0;            /* pad. */                 \
        /* Only code vectors typically use a plain bytevector typetag,        \
         * so almost any bytevector will be a code vector that must           \
         * be flushed.                                                        \
         */                                                                   \
        if (iflush && typetag( T_w ) == BVEC_SUBTAG)                          \
          mem_icache_flush( T_oldptr, ptr );                                  \
      }                                                                       \
      else {                                                                  \
        /* vector or procedure: scan in a tight loop */                       \
        /* the_obj is also used by some versions of the forwarding code! */   \
        word T_words = sizefield( T_w ) >> 2;                                 \
        word *T_objp = ptr;                                                   \
        word the_obj = tagptr(T_objp, (T_h == VEC_HDR ? VEC_TAG : PROC_TAG)); \
        int must_add_to_extra = 0;                                            \
        ptr++;                                                                \
        while (T_words--) {                                                   \
          FORW;                                                               \
          ptr++;                                                              \
        }                                                                     \
        if (must_add_to_extra) remember_vec( the_obj, e );                    \
        if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */                   \
      }                                                                       \
    }                                                                         \
    else {                                                                    \
      /* the_obj is also used by some versions of the forwarding code! */     \
      int must_add_to_extra = 0;                                              \
      word the_obj = tagptr(ptr,PAIR_TAG);                                    \
      FORW;                                                                   \
      ptr++;                                                                  \
      FORW;                                                                   \
      ptr++;                                                                  \
      if (must_add_to_extra) remember_pair( the_obj, e );                     \
    }                                                                         \
  } while (0)


#if USE_DOF_REMSET
# if SPLIT_REMSETS

/* Here, add to every remset for which extra_gen is nonzero, and clear the
   extra_gen slot if set.
   
   FIXME: Note invariant: there must be space for at least one entry.  
   This is a holdover from the SSB-based code and it complicates the 
   system slightly throughout; should just fix it.
   */

#  if EXTRA_GEN_BITVECTOR && !EXTRA_GEN_LOOKUP
#   define remember_vec( w, e )                                 \
    do { int I;                                                 \
         word **remtops = e->tospaces[e->scan_idx].remset_tops; \
         word **remlims = e->tospaces[e->scan_idx].remset_lims; \
      for ( I=0 ; extra_gen ; I++, extra_gen >>= 1 )            \
         if (extra_gen & 1) {                                   \
           word *remtop = remtops[I];                           \
           *remtop = w; remtop = remtop+1;                      \
           remtops[I] = remtop;                                 \
           if (remtop == remlims[I])                            \
             dof_remset_advance(e,I);                           \
         }                                                      \
    } while(0)
#  elif EXTRA_GEN_LOOKUP
#  define remember_vec( w, e ) \
     assert2( 0 )
#  else
#   define remember_vec( w, e )                                 \
    do { int I, N = e->ngenerations ;                           \
         word **remtops = e->tospaces[e->scan_idx].remset_tops; \
         word **remlims = e->tospaces[e->scan_idx].remset_lims; \
      for ( I=0 ; I < N ; I++ )                                 \
         if (extra_gen[I]) {                                    \
           word *remtop = remtops[I];                           \
           extra_gen[I] = 0;                                    \
           *remtop = w; remtop = remtop+1;                      \
           remtops[I] = remtop;                                 \
           if (remtop == remlims[I])                            \
             dof_remset_advance(e,I);                           \
         }                                                      \
    } while(0)
#  endif
# else /* ! SPLIT_REMSETS */
#  define remember_vec( w, e )                  \
   do {  *remtop = w; remtop = remtop+1;        \
         if (remtop == remlim) {                \
           scan_remtop(e) = remtop;             \
           dof_remset_advance( e );             \
           remtop = scan_remtop(e);             \
           remlim = scan_remlim(e);             \
         }                                      \
   } while(0)
# endif
#else /* ! USE_DOF_REMSET */
#define remember_vec( w, e )                    \
 do {  word **ssbtop = scan_ssbtop(e);          \
       word **ssblim = scan_ssblim(e);          \
       **ssbtop = w; *ssbtop = *ssbtop+1;       \
       if (*ssbtop == *ssblim) {                \
         rs_compact( scan_remset(e) );          \
       }                                        \
 } while(0)
#endif

#define remember_pair( w, e ) remember_vec( w, e )

typedef struct dof_env dof_env_t;

struct dof_env {
  gc_t *gc;                     /* The collector */
  old_heap_t *heap;             /* The heap */
  int ngenerations;             /* Number of generations */
  int nspaces;                  /* Number of tospaces */
  gclib_desc_t *gclib_desc_g;
  struct {                      /* One of these per tospace */
    int         gen_no;
    remset_t    *remset;
#if USE_DOF_REMSET
# if SPLIT_REMSETS
    dof_remset_t *dof_remsets[ MAX_GENERATIONS ];
    word         *remset_tops[ MAX_GENERATIONS ];
    word         *remset_lims[ MAX_GENERATIONS ];
    int          remset_sizes[ MAX_GENERATIONS ];
# else
    dof_remset_t *dof_remset;
    word         *remset_top;
    word         *remset_lim;
    int          remset_size;
# endif
#else
    word        **ssbtop;
    word        **ssblim;
#endif
    semispace_t *ss;
    los_list_t  *marked;
    word        *mark_context;
#if DYNAMIC_ALLOCATION
    /* Information allowing us to allocate on demand */
    int         n;              /* Limit on number of chunks */
#endif
  } tospaces[ MAX_GENERATIONS ];

  int younger_than;             /* Generation number of youngest uncollected */
  int iflush;                   /* Flush icache? */

  int copy_idx;                 /* Current generation for copying */

  int scan_idx;                 /* Generation being scanned */
  int scan_chunk_idx;           /* Chunk being scanned in that generation */
  word *scan_ptr;               /* Pointer into that chunk */
};

#define copy_gen_no( e ) ((e)->tospaces[(e)->copy_idx].gen_no)
#define copy_remset( e ) ((e)->tospaces[(e)->copy_idx].remset)
#define copy_ss( e )     ((e)->tospaces[(e)->copy_idx].ss)
#define copy_marked( e ) ((e)->tospaces[(e)->copy_idx].marked)
#define copy_mark_context( e ) ((e)->tospaces[(e)->copy_idx].mark_context)
#define copy_n( e )      ((e)->tospaces[(e)->copy_idx].n)

#define scan_gen_no( e ) ((e)->tospaces[(e)->scan_idx].gen_no)
#define scan_remset( e ) ((e)->tospaces[(e)->scan_idx].remset)
#if !USE_DOF_REMSET
# define scan_ssbtop( e ) ((e)->tospaces[(e)->scan_idx].ssbtop)
# define scan_ssblim( e ) ((e)->tospaces[(e)->scan_idx].ssblim)
#endif
#if USE_DOF_REMSET
# if SPLIT_REMSETS
#  define scan_dofset( e,i ) ((e)->tospaces[(e)->scan_idx].dof_remsets[i])
#  define scan_remtop( e,i ) ((e)->tospaces[(e)->scan_idx].remset_tops[i])
#  define scan_remlim( e,i ) ((e)->tospaces[(e)->scan_idx].remset_lims[i])
# else
#  define scan_dofset( e ) ((e)->tospaces[(e)->scan_idx].dof_remset)
#  define scan_remtop( e ) ((e)->tospaces[(e)->scan_idx].remset_top)
#  define scan_remlim( e ) ((e)->tospaces[(e)->scan_idx].remset_lim)
# endif
#endif
#define scan_ss( e )     ((e)->tospaces[(e)->scan_idx].ss)
#define scan_marked( e ) ((e)->tospaces[(e)->scan_idx].marked)
#define scan_mark_context( e ) ((e)->tospaces[(e)->scan_idx].mark_context)


#define ss_lim(ss) ((ss)->chunks[(ss)->current].lim)
#define ss_top(ss) ((ss)->chunks[(ss)->current].top)
#define ss_bot(ss) ((ss)->chunks[(ss)->current].bot)

#define ss_lim2(ss, n) ((ss)->chunks[n].lim)
#define ss_top2(ss, n) ((ss)->chunks[n].top)
#define ss_bot2(ss, n) ((ss)->chunks[n].bot)

#define SETUP_COPY_PTRS( E, DEST, LIM )         \
  word *DEST = ss_top(copy_ss(E));              \
  word *LIM  = ss_lim(copy_ss(E))

#define TAKEDOWN_COPY_PTRS( E, DEST, LIM )      \
  ss_top(copy_ss(E)) = DEST;                    \
  ss_lim(copy_ss(E)) = LIM

#if USE_DOF_REMSET
# if SPLIT_REMSETS
#  define SETUP_REMSET_PTRS( E, TOP, LIM )  int TOP, LIM
#  define TAKEDOWN_REMSET_PTRS( E, TOP, LIM ) ((void)0)
# else
#  define SETUP_REMSET_PTRS( E, TOP, LIM )      \
    word *TOP = scan_remtop( E );               \
    word *LIM = scan_remlim( E )

#  define TAKEDOWN_REMSET_PTRS( E, TOP, LIM )   \
    scan_remtop( E ) = TOP
# endif
#else
# define SETUP_REMSET_PTRS( E, TOP, LIM )  int TOP, LIM
# define TAKEDOWN_REMSET_PTRS( E, TOP, LIM )  ((void)0)
#endif

extern void mem_icache_flush( void *start, void *end );

static void scan_from_globals( word *, void * );
static bool scan_from_remsets( word, void *, unsigned * );
static void scan_from_tospace( dof_env_t * );
static void scan_small_objects( dof_env_t * );
static bool scan_large_objects( dof_env_t *, int gen );
static word forward( word, word **, dof_env_t * );
static void seal_chunk( semispace_t *, word *, word * );
static void expand_semispace_np( word **, word **, unsigned, dof_env_t * );
static word forward_large_object( dof_env_t *, word *, int );
#if USE_DOF_REMSET
# if SPLIT_REMSETS
static void dof_remset_advance( dof_env_t *, int i );
# else
static void dof_remset_advance( dof_env_t * );
# endif
static void scan_from_dof_remset( dof_env_t *e, dof_remset_t *r );
#endif


static int dof_copy_into_with_barrier( old_heap_t *heap,
                                       int younger_than, 
                                       gen_t **tospaces,
                                       gc_type_t type,
                                       dof_data_t *data )
{
  gc_t *gc = heap->collector;
  dof_env_t e;
  int i, j, gen;

  /* Setup collection data */
  for ( i=0 ; tospaces[i] != 0 ; i++ ) {
    gen = e.tospaces[i].gen_no = tospaces[i]->live->gen_no;
    e.tospaces[i].remset = gc->remset[gen];
#if USE_DOF_REMSET
# if SPLIT_REMSETS
    for ( j=0 ; j < data->n ; j++ ) {
      int k = j+data->first_gen_no;
      e.tospaces[i].dof_remsets[k] = tospaces[i]->dof_remsets[j];
      e.tospaces[i].remset_tops[k] = tospaces[i]->dof_remsets[j]->curr->top;
      e.tospaces[i].remset_lims[k] = tospaces[i]->dof_remsets[j]->curr->lim;
      e.tospaces[i].remset_sizes[k] = 
        dof_rs_size( tospaces[i]->dof_remsets[j] );
      assert( e.tospaces[i].remset_tops[k] < e.tospaces[i].remset_lims[k] );
    }
# else
    e.tospaces[i].dof_remset = tospaces[i]->dof_remset;
    e.tospaces[i].remset_top = tospaces[i]->dof_remset->curr->top;
    e.tospaces[i].remset_lim = tospaces[i]->dof_remset->curr->lim;
    e.tospaces[i].remset_size = dof_rs_size( tospaces[i]->dof_remset );
    assert( e.tospaces[i].remset_top < e.tospaces[i].remset_lim );
# endif
#else
    e.tospaces[i].ssbtop = gc->remset[gen]->ssb_top;
    e.tospaces[i].ssblim = gc->remset[gen]->ssb_lim;
    assert( e.tospaces[i].sbtop < e.tospaces[i].ssblim );
#endif
    e.tospaces[i].ss     = tospaces[i]->live;
    e.tospaces[i].marked = create_los_list();
    e.tospaces[i].mark_context = 0;
#if DYNAMIC_ALLOCATION
    assert( tospaces[i]->claim % BLOCK_SIZE == 0 );
    e.tospaces[i].n = tospaces[i]->claim / BLOCK_SIZE;
#endif
  }
  e.gc = gc;
  e.heap = heap;
  e.nspaces = i;
  e.ngenerations = data->first_gen_no + data->n;
  e.gclib_desc_g = gclib_desc_g;
  e.younger_than = younger_than;
  e.copy_idx = 0;
  e.scan_idx = 0;
  e.scan_chunk_idx = scan_ss(&e)->current;
  e.scan_ptr = ss_top(scan_ss(&e));
  e.iflush = gc_iflush( gc );

  /* Collect */
  start( type == GCTYPE_COLLECT ? &dof.root_scan_gc : &dof.root_scan_prom );
  gc_enumerate_roots( gc, scan_from_globals, (void*)&e );
  stop();
  start( type == GCTYPE_COLLECT ? &dof.remset_scan_gc : &dof.remset_scan_prom);
  gc_enumerate_remsets_older_than( gc, 
                                   e.younger_than - 1,
                                   scan_from_remsets,
                                   (void*)&e,
                                   FALSE );
  stop();
  
#if USE_DOF_REMSET
# if SPLIT_REMSETS
  if (type == GCTYPE_COLLECT) {
    start( &dof.dof_remset_scan );
    for ( i=0 ; i < data->n ; i++ )
      if (data->gen[i]->order + data->first_gen_no >= e.younger_than) {
        int j = e.younger_than - 1 - data->first_gen_no;
        assert( j >= 0 && j < data->n );
        annoyingmsg( "Scanning DOF %d,%d (%d)", i, j, data->gen[i]->order );
        scan_from_dof_remset( &e, data->gen[i]->dof_remsets[j] );
        /* hm: data->gen[j]->order+data->first_gen_no */
      }
    stop();
  }
# else
  if (type == GCTYPE_COLLECT)
    for ( i=0 ; i < data->n ; i++ )
      if (data->gen[i]->order + data->first_gen_no >= e.younger_than)
        scan_from_dof_remset( &e, data->gen[i]->dof_remset );
# endif
#endif

  start( type==GCTYPE_COLLECT ? &dof.tospace_scan_gc : &dof.tospace_scan_prom);
  scan_from_tospace( &e );
  stop();
  
#if USE_DOF_REMSET
# if SPLIT_REMSETS
  for ( i=0 ; tospaces[i] != 0 ; i++ )
    for ( j=0 ; j < data->n ; j++ ) {
      int k = j + data->first_gen_no;
      tospaces[i]->dof_remsets[j]->curr->top = e.tospaces[i].remset_tops[k];
      tospaces[i]->dof_remsets[j]->stats.recorded +=
        dof_rs_size( tospaces[i]->dof_remsets[j] ) -
        e.tospaces[i].remset_sizes[k];
    }
# else
  for ( i=0 ; tospaces[i] != 0 ; i++ ) {
    tospaces[i]->dof_remset->curr->top = e.tospaces[i].remset_top;
    tospaces[i]->dof_remset->stats.recorded +=
      dof_rs_size( tospaces[i]->dof_remset ) -
      e.tospaces[i].remset_size;
  }
# endif
#endif

  assert( e.copy_idx == e.scan_idx );
  assert( ss_top(copy_ss(&e)) == e.scan_ptr );

  /* Sweep large object space */
  start( type == GCTYPE_COLLECT ? &dof.los_sweep_gc : &dof.los_sweep_prom );
  for ( i=0 ; i < e.younger_than ; i++ )
    los_sweep( gc->los, i );
  for ( i=0 ; i < e.nspaces ; i++ ) {
    los_append_and_clear_list( gc->los, 
                               e.tospaces[i].marked, 
                               e.tospaces[i].gen_no );
    los_free_list( e.tospaces[i].marked );
  }
  stop();
  
  return e.copy_idx;            /* Last tospace used */
}

#if USE_DOF_REMSET
# if SPLIT_REMSETS
static void dof_remset_advance( dof_env_t *e, int i )
{
  dof_remset_t *r = scan_dofset(e,i);

  r->curr->top = scan_remtop(e,i);
  advance_dof_remset( r );
  scan_remtop(e,i) = r->curr->top;
  scan_remlim(e,i) = r->curr->lim;
}
# else
static void dof_remset_advance( dof_env_t *e )
{
  dof_remset_t *r = scan_dofset(e);

  r->curr->top = scan_remtop(e);
  advance_dof_remset( r );
  scan_remtop(e) = r->curr->top;
  scan_remlim(e) = r->curr->lim;
}
# endif
#endif

static void scan_from_globals( word *ptr, void *data )
{
  dof_env_t *e = (dof_env_t*)data;
  SETUP_COPY_PTRS( e, dest, lim );

  forw_np( ptr, e->younger_than, dest, lim, e );

  TAKEDOWN_COPY_PTRS( e, dest, lim );
}

static bool scan_from_remsets( word object, void *data, unsigned *count )
{
  dof_env_t *e = (dof_env_t*)data;
  unsigned     forw_limit_gen = e->younger_than;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *loc;            /* Used as a temp by scanner and fwd macros */
  SETUP_COPY_PTRS( e, dest, lim );

  remset_scanner_core( object, loc, 
                       forw_np_record( loc, forw_limit_gen, dest, lim,
                                       has_intergen_ptr, old_obj_gen, e ),
                       *count );

  TAKEDOWN_COPY_PTRS( e, dest, lim );
  return has_intergen_ptr;
}

#if USE_DOF_REMSET
static void scan_from_dof_remset( dof_env_t *e, dof_remset_t *r )
{
  unsigned     forw_limit_gen = e->younger_than;
  word         *loc;            /* Used as a temp by scanner and fwd macros */
  dof_pool_t   *segment;
  word         *slot, *slotlim;
  word         object;
  word         scanned = 0;
  word         objects = 0;
  SETUP_COPY_PTRS( e, dest, lim );
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

#if COMPACT_DOF_REMSET
  for ( segment=r->first ; segment ; segment=segment->next ) {
    word *rdest;
    for ( slot=rdest=segment->bot, slotlim=segment->top ;
          slot < slotlim ;
          slot++ ) {
      object = *slot;
      if (object != 0) {
        bool has_intergen_ptr = FALSE;
        int old_obj_gen = gen_of(object);
        
        objects++;
        assert2(isptr( object ));
        remset_scanner_core( object, loc,
                             forw_dof_record( loc, forw_limit_gen, dest, lim,
                                              has_intergen_ptr, old_obj_gen,
                                              e ),
                             scanned );
        if (has_intergen_ptr) 
          *rdest++ = object;
      }
    }
    if (segment->next != 0)
      r->free += segment->top - rdest;
    r->stats.removed += segment->top - rdest;
    segment->top = rdest;
  }
#else
  for ( segment=r->first ; segment ; segment=segment->next ) {
    for ( slot=segment->bot, slotlim=segment->top ; slot < slotlim ; slot++ ) {
      object = *slot;
      if (object != 0) {
        objects++;
        assert2(isptr( object ));
        remset_scanner_core( object, loc, 
                             forw_np( loc, forw_limit_gen, dest, lim, e ),
                             scanned );
      }
    }
  }
#endif
  r->stats.scanned += 1;
  r->stats.words_scanned += scanned;
  r->stats.objs_scanned += objects;
  TAKEDOWN_COPY_PTRS( e, dest, lim );
  annoyingmsg( "DOF remset; scanned=%d, objects=%d", scanned, objects );
}
#endif

static void scan_from_tospace( dof_env_t *e )
{
  bool work;
  int i;

  do {
    work = FALSE;
    if (e->scan_ptr == ss_lim2(scan_ss(e), e->scan_chunk_idx)) {
      /* At end of a chunk */
      if (e->scan_chunk_idx < scan_ss(e)->current) {
        /* More chunks in this ss */
        e->scan_chunk_idx++;
        e->scan_ptr = ss_bot2(scan_ss(e), e->scan_chunk_idx);
      }
      else if (e->scan_idx < e->copy_idx) { 
        /* More generations to scan */
        e->scan_idx++;
        e->scan_chunk_idx = 0;
        e->scan_ptr = ss_bot2(scan_ss(e), e->scan_chunk_idx);
      }
    }

    if (e->scan_ptr != ss_lim2(scan_ss(e), e->scan_chunk_idx) && 
        e->scan_ptr != ss_top(copy_ss(e))) {
      scan_small_objects( e );
      work=TRUE;
    }

    for ( i=0 ; i < e->nspaces ; i++ )
      work |= scan_large_objects( e, i );

  } while (work);
}


static void scan_small_objects( dof_env_t *e )
{
  unsigned forw_younger_than = e->younger_than;
  unsigned barrier_younger_than = scan_gen_no(e);
  SETUP_COPY_PTRS( e, dest, copylim );
#if !SPLIT_REMSETS
  SETUP_REMSET_PTRS( e, remtop, remlim );
#endif
  word *scanptr = e->scan_ptr;
  word *scanlim = ss_lim2(scan_ss(e), e->scan_chunk_idx);
#if EXTRA_GEN_BITVECTOR && !EXTRA_GEN_LOOKUP
# define must_add_to_extra extra_gen
#elif EXTRA_GEN_LOOKUP
  word **remtops = e->tospaces[e->scan_idx].remset_tops;     
  word **remlims = e->tospaces[e->scan_idx].remset_lims;     
# if EXTRA_GEN_BITVECTOR
  word extra_gen = 0;
# else
  word extra_gen[ MAX_GENERATIONS ];
  int  i;
# endif
#else
  int  extra_gen[ MAX_GENERATIONS ];
#endif
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif
  
#if EXTRA_GEN_BITVECTOR
  assert( e->ngenerations <= 32 );
#else
  for ( i=0 ; i < MAX_GENERATIONS ; i++ )
    extra_gen[i] = 0;
#endif
  
  /* FIXME: pass remtop, remlim to the macro! */
  /* FIXME: it's not clear that the special case helps much */
  /* must_add_to_extra is a name used by the scanning and fwd macros as a 
     temp */
  if ( scanptr <= dest && dest < scanlim) {
    while (scanptr != dest && scanptr < scanlim) {
      scan_core_partial( scanptr, e->iflush,
                         forw_np_partial( scanptr, forw_younger_than, dest, 
                                          copylim, barrier_younger_than, 
                                          must_add_to_extra, e ),
                         must_add_to_extra, e );
#if EXTRA_GEN_LOOKUP && EXTRA_GEN_BITVECTOR
      extra_gen = 0;
#endif
    }
  }
  else {
    while (scanptr < scanlim) {
      scan_core_partial( scanptr, e->iflush,
                         forw_np_partial( scanptr, forw_younger_than, dest, 
                                          copylim, barrier_younger_than, 
                                          must_add_to_extra, e ),
                         must_add_to_extra, e );
#if EXTRA_GEN_LOOKUP && EXTRA_GEN_BITVECTOR
      extra_gen = 0;
#endif
    }
  }

  e->scan_ptr = scanptr;
  TAKEDOWN_COPY_PTRS( e, dest, copylim );
  TAKEDOWN_REMSET_PTRS( e, remtop, remlim );
#undef must_add_to_extra
}

static bool scan_large_objects( dof_env_t *e, int gen )
{
  unsigned forw_younger_than = e->younger_than;
  unsigned barrier_younger_than = scan_gen_no(e);
  SETUP_COPY_PTRS( e, dest, copylim );
#if !SPLIT_REMSETS
  SETUP_REMSET_PTRS( e, remtop, remlim );
#endif
  word *p;
  bool work = FALSE;
#if EXTRA_GEN_BITVECTOR && !EXTRA_GEN_LOOKUP
# define must_add_to_extra extra_gen
#elif EXTRA_GEN_LOOKUP
  word **remtops = e->tospaces[e->scan_idx].remset_tops;     
  word **remlims = e->tospaces[e->scan_idx].remset_lims;     
# if EXTRA_GEN_BITVECTOR
  word extra_gen = 0;
# else
  word extra_gen[ MAX_GENERATIONS ];
  int  i;
# endif
#else
  int  extra_gen[ MAX_GENERATIONS ];
#endif
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif
  
#if EXTRA_GEN_BITVECTOR
  assert( e->ngenerations <= 32 );
#else
  for ( i=0 ; i < MAX_GENERATIONS ; i++ )
    extra_gen[i] = 0;
#endif
  
  /* must_add_to_extra is a name used by the scanning and fwd macros as a 
     temp */
  while ((p = los_walk_list( e->tospaces[gen].marked, 
                             e->tospaces[gen].mark_context )) != 0) {
    e->tospaces[gen].mark_context = p;
    work = TRUE;
    assert2( ishdr( *p ) );
    scan_core_partial( p, e->iflush,
                       forw_np_partial( p, forw_younger_than, dest, copylim,
                                        barrier_younger_than, must_add_to_extra, 
                                        e ),
                       must_add_to_extra, e );
#if EXTRA_GEN_LOOKUP && EXTRA_GEN_BITVECTOR
    extra_gen = 0;
#endif
  }

  TAKEDOWN_COPY_PTRS( e, dest, copylim );
  TAKEDOWN_REMSET_PTRS( e, remtop, remlim );
  return work;
#undef must_add_to_extra
}

/* "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address.
 *
 * Most objects are smallish, so this code is biased to small objects.
 */
static word forward( word p, word **dest, dof_env_t *e )
{
  word hdr, newptr, *p1, *p2, tag, *ptr;
  unsigned words;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)p1;
  p2 = ptr;

  hdr = *ptr;
  assert2( ishdr( hdr ) );

  words = roundup8( sizefield( hdr ) + 4 ) / 4;

#if CHECK_EVERY_WORD
    switch (tag) {
    case VEC_TAG : case PROC_TAG :
      gclib_check_memory_validity( p2, (sizefield( hdr ) + 4)/4 );
    }
#endif
  /* 32 is loosely chosen to match overhead of memcpy(). */
  if (words < 32) {
    while (words > 0) {
      p1[0] = p2[0];
      p1 += 2;
      p1[-1] = p2[1];
      p2 += 2;
      words -= 2;
    }
  }
  else if (words > GC_LARGE_OBJECT_LIMIT/4 && e->gc->los) 
    return forward_large_object( e, ptr, tag );
  else {
    memcpy( p1, p2, words*4 );
    p1 += words;
  }
  *dest = p1;

  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  check_address( ptr );
  *ptr = FORWARD_HDR;
  *(ptr+1) = newptr;

  return newptr;
}

static word forward_large_object( dof_env_t *e, word *ptr, int tag )
{
#if CHECK_EVERY_WORD
    switch (tag) {
    case VEC_TAG : case PROC_TAG :
      gclib_check_memory_validity( ptr, (sizefield(*ptr)+4)/4 );
      break;
    }
#endif
  if (attr_of(ptr) & MB_LARGE_OBJECT) {
    los_mark_and_set_generation(e->gc->los, copy_marked(e), ptr, gen_of(ptr),
                                copy_gen_no(e));
    return tagptr( ptr, tag );
  }
  else {
    /* The large object was not allocated specially, so we must copy it. */
    word *new, hdr;
    int bytes;

    /* Copy it */
    hdr = *ptr;
    bytes = roundup8( sizefield( hdr ) + 4 );
    new = los_allocate( e->gc->los, bytes, copy_gen_no(e) );
    memcpy( new, ptr, bytes );
    
    /* Mark it */
    los_mark( e->gc->los, copy_marked(e), new, copy_gen_no(e) );
    
    /* Leave a forwarding pointer */
    check_address( ptr );
    *ptr = FORWARD_HDR;
    *(ptr+1) = tagptr( new, tag );
    return *(ptr+1);
  }
}

static void seal_chunk( semispace_t *ss, word *lim, word *dest )
{
  if (dest < lim) {
    word len = words2bytes(lim - dest);
    *dest = mkheader(len-sizeof(word),STR_HDR);
    *(dest+1) = 0xABCDABCD;
  }
  ss->chunks[ ss->current ].top = dest;
}

static void
expand_semispace_np( word **lim, word **dest, unsigned bytes, dof_env_t *e )
{
  int idx;

  /* If not at last chunk, then
       step to next chunk
     else if not at last generation, then
       step to next generation
     else
       specially handled fragmentation overflow
  */

  seal_chunk( copy_ss(e), *lim, *dest );
  idx = copy_ss(e)->current;
#if DYNAMIC_ALLOCATION
  if (idx+1 < copy_n(e)) {
    int b = ss_allocate_block_unconditionally( copy_ss(e), BLOCK_SIZE );
    copy_ss(e)->current++;
    assert( b == copy_ss(e)->current );
  }
#else
  if (idx+1 < copy_ss(e)->n && copy_ss(e)->chunks[idx+1].bytes > 0) {
    copy_ss(e)->current++;
  }
#endif
  else if (e->copy_idx < e->nspaces-1) {
    e->copy_idx++;
#if DYNAMIC_ALLOCATION
    if (copy_ss(e)->current == -1) {
      int b = ss_allocate_block_unconditionally( copy_ss(e), BLOCK_SIZE );
      copy_ss(e)->current++;
      assert( b == copy_ss(e)->current );
    }
#endif
    assert( copy_ss(e)->current == 0 );
  }
  else {
    /* See dof.txt section Pragmatics:d.2 */
    hardconsolemsg( "***************************************************" );
    hardconsolemsg( "Overflow during collection; expanding by one block." );
    hardconsolemsg( "***************************************************" );
    grow_all_generations( e->heap, BLOCK_SIZE );
#if DYNAMIC_ALLOCATION
    { int b = ss_allocate_block_unconditionally( copy_ss(e), BLOCK_SIZE );
      copy_ss(e)->current++;
      assert( b == copy_ss(e)->current );
    }
#else
    assert( idx+1 < copy_ss(e)->n && copy_ss(e)->chunks[idx+1].bytes > 0 );
    copy_ss(e)->current++;
#endif
  }
  
  assert( copy_ss(e)->current < copy_ss(e)->n );
  assert( copy_ss(e)->chunks[copy_ss(e)->current ].bytes > 0 );

  *dest = copy_ss(e)->chunks[ copy_ss(e)->current ].bot;
  *lim = copy_ss(e)->chunks[ copy_ss(e)->current ].lim;
}

/* eof */
