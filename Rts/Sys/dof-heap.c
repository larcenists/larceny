/* Copyright 1998, 1999 Lars T Hansen.
 *
 * $Id$
 *
 * Deferred-oldest-first dynamic area.
 *
 * The area manages several independently collected areas, each treated 
 * like a generation by the GC infrastructure.
 *
 * There are various ways to do this:
 *  (1) fixed number of variable size areas, of nonuniform size
 *  (2) fixed number of variable size areas, of uniform size
 *  (3) variable number of fixed size areas, of nonuniform size
 *  (4) variable number of fixed size areas, of uniform size
 *  (5) variable number of variable size areas, of nonuniform size
 *  (6) variable number of variable size areas, of uniform size
 *
 * For now, we support only a version of (1) that approaches (2):
 * the chunks grow and are of nonuniform size but policy code attempts
 * to keep them roughly the same size.
 *
 * Here are some questions:
 *  - how do we choose to expand the heap?
 *     We can examine the heap when the window has swept all the way to
 *     the right, and resize it then, using the load factor.  At that
 *     point, we will know the mark/cons ratio for the dynamic area
 *     during the last sweep.
 *
 *  - how do we choose a subset to collect?
 *     We collect the same fraction of the heap every time: one area.
 *     But see item about large circular structures below.
 *
 *  - what is the policy?
 *     If there's room in the DOF area after any promotion into it to
 *     receive the worst case live data from the ephemeral areas one
 *     more time, then do nothing, otherwise collect the oldest area.
 *     It is possible to collect and promote at the same time but it is
 *     important not to put the collected and promoted data together,
 *     as the collected data has a particular age and the promoted data
 *     have a different age [Stefanovic's view].  It's possible to view
 *     this differently: if it is the case that the collector has an
 *     algorithmic advantage rather than a heuristic advantage, ie.,
 *     if it's the scanning that's the win and not the keeping objects
 *     of the same age together, then promoting and collecting at the
 *     same time isn't a problem.  This must be investigated.
 *
 *     Thus we see that there must be as much free space as twice the
 *     size of the ephemeral areas, rounded up to area size, because that's 
 *     worst case???  That seems like a lot...
 *
 *  - how do we deal with large circular garbage structures?
 *     We can collect multiple areas every so often, on a regular basis,
 *     or we can try to detect the situation.  For example, if an area is
 *     collected but yields very little garbage, and if all pointers
 *     into the area is from objects in the remembered set (easy to
 *     determine -- no objects are forwarded by the root scan), then
 *     the area should be collected together with all the areas that
 *     reference it the next time it's collected.
 *
 *  - how do we deal with large objects larger than a chunk?
 *     Size the chunk large enough to hold the object.
 */

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
#include "heap_stats_t.h"
#include "remset_t.h"
#include "static_heap_t.h"

#define MAX_AREAS 128		/* Why not? */

typedef struct dof_data dof_data_t;
typedef struct dof_step dof_step_t;

struct dof_step {
  int gen_no;			/* The generation number */
  int size;			/* Target size in bytes */
  int copied_last_gc;		/* Bytes copied last GC */
  int moved_last_gc;		/* Bytes moved last GC (large objects) */
  semispace_t *ss;		/* The heap data */
};

struct dof_data {
  int gen_no_lo;		/* Low generation number (step 0) */
  int gen_no_hi;		/* High generation number (step nsteps-1) */
  int nsteps;			/* Number of steps in steps[] */
  int current_step;             /* Step to promote into next */
  int dynamic_max;
  int dynamic_min;
  int size;			/* Sum of all sizes of steps */
  double load_factor;
  dof_step_t steps[ MAX_AREAS ]; /* The steps */
};

#define DATA(x)             ((dof_data_t*)((x)->data))

static old_heap_t *allocate_heap( gc_t *gc, dof_data_t *data );
static int perform_promote_in( old_heap_t *heap );
static void perform_promote_out( old_heap_t *heap, int shortfall );
static int used_space( old_heap_t *heap, int area );
static int free_space_overall( old_heap_t *heap );
static int space_required( int size, int nbytes );
static int heap2area( old_heap_t *heap );

old_heap_t *
create_dof_area( int gen_no, int *gen_allocd, gc_t *gc, dof_info_t *info )
{
  dof_data_t *data;
  dof_step_t *step;
  int i, nsteps, stepsize;

  nsteps = info->steps;
  stepsize = roundup_page( info->steps / info->size_bytes );

  data = (dof_data_t*)must_malloc( sizeof( dof_data_t ) );

  data->gen_no_lo = gen_no;
  data->gen_no_hi = gen_no + nsteps - 1;
  data->nsteps = nsteps;
  data->current_step = 0;
  data->load_factor = info->load_factor;
  data->dynamic_min = info->dynamic_min;
  data->dynamic_max = info->dynamic_max;
  data->size = stepsize*nsteps;

  for ( i=0 ; i < nsteps ; i++ ) {
    step = &data->steps[i];

    step->ss = create_semispace( stepsize, 0, gen_no+i );
    step->size = stepsize;
    step->gen_no = gen_no+i;
    step->copied_last_gc = 0;
    step->moved_last_gc = 0;
  }

  *gen_allocd = nsteps;
  return allocate_heap( gc, data );
}

static void perform_promote_from_younger( old_heap_t *heap );
static void perform_collect( old_heap_t *heap, int shortfall );

static void collect( old_heap_t *heap, gc_type_t request )
{
  dof_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  int shortfall, ephemeral_max, i;

  /* Promote in.  Always works. */
  perform_promote_from_younger( heap );

  /* If we can't promote in one more time, then collect some. */
  ephemeral_max = gc->young_area->maximum;
  for ( i=0 ; i < gc->ephemeral_area_count ; i++ )
    ephemeral_max += gc->ephemeral_area[i]->maximum;

  shortfall = 
    space_required( data->size, ephemeral_max ) - free_space_overall( heap );

  if (shortfall > 0)
    perform_collect( heap, shortfall );
}

static void perform_promote_from_younger( old_heap_t *heap )
{
  dof_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  los_t *los = gc->los;
  int before_gc, los_before_gc, copied = 0, moved = 0, last;
  int i, first;
  dof_step_t *step;

  first = data->current_step;

  step = &data->steps[first];

  annoyingmsg( "DOF: promoting into area %d.", data->current_step );

  ss_sync( step->ss );
  before_gc = step->ss->used;
  los_before_gc = los_bytes_used( los, step->gen_no );

  last = perform_promote_in( heap );

  ss_sync( step->ss );
  step->copied_last_gc = step->ss->used - before_gc;
  step->moved_last_gc = los_bytes_used( los, step->gen_no ) - los_before_gc;

  for ( i=first ; i <= last ; i++ ) {
    ss_sync( data->steps[i].ss );
    data->steps[i].copied_last_gc = data->steps[i].ss->used;
    data->steps[i].moved_last_gc =
      los_bytes_used( los, data->steps[i].ss->used );
  }

  data->current_step = last;

  annoyingmsg( "  DOF: promotion into areas %d..%d finished.", first, last );
}


/* Shortfall takes fragmentation into account. */
static void perform_collect( old_heap_t *heap, int shortfall )
{
#if 0
  gc_t *gc = heap->collector;
  dof_data_t *data = DATA(heap);
  int *fromspace, i, j, k, chunks_needed;
  semispace_t *tmp;

  k = data->current_step;
  chunks_needed = ceildiv( shortfall, data->size_bytes );
  annoyingmsg( "  DOF: triggering promotion out, shortfall=%d, chunks=%d", 
               shortfall, chunks_needed );

  fromspace = (int*)must_malloc( sizeof(int)*(chunks_needed+1) );
  for ( i=0 ; i < chunks_needed ; i++ )
    fromspace[i] = data->gen_no_lo+k+i;
  fromspace[i] = 0;

  gc_collect_old_with_selective_fromspace( gc, fromspace );

  for ( i=0 ; i < chunks_needed ; i++ )
    rs_clear( gc->remset[fromspace[i]] );

  /* Now must juggle the chunks and reassign generation numbers.  The memmgr 
     may cache information, so just juggling generation numbers and pointers 
     may wreak havoc!  (For example, the ssb pointer arrays used by the 
     write barrier is such a cache.)  Thus make the memmgr do it.
     */
  gc_rotate_areas_down( gc, data->gen_no_lo, data->gen_no_hi, chunks_needed );
  
  /* Rotate local area down.  Don't need to fix sizes[], they're all the same.
     */ 
  for ( i=0 ; i < chunks_needed ; i++ ) {
    tmp = data->areas[0];
    for ( j=0 ; j < data->nareas-1 ; i++ )
      data->areas[j] = data->areas[j+1];
    data->areas[data->nareas-1] = tmp;
  }
  
  annoyingmsg( "  DOF: promotion out finished." );
  
  free( fromspace );
#else
  panic( "DOF" );
#endif
}


/* Return the number of bytes of chunk space required for promoting
   in nbytes of storage.  Takes into account fragmentation at chunk
   boundaries.
   */
static int space_required( int size, int nbytes )
{
  return nbytes + GC_LARGE_OBJECT_LIMIT*ceildiv( nbytes, size );
}

/* Return last chunk that was used */
static int perform_promote_in( old_heap_t *heap )
{
#if 0
  dof_data_t *data = DATA(heap);
  int k = data->current_area;
  int *sizes = &data->sizes[k];
  int last;
  int dynamic_gen = data->gen_no_hi + 1;
  semispace_t **dest_areas = &data->areas[k];
  
  stats_gc_type( data->gen_no_lo+k, GCTYPE_PROMOTE );

  gclib_stopcopy_promote_into_dof( heap->collector, 
                                   dest_areas,
				   sizes,
				   &last,
				   dynamic_gen );
  return last+k;
#endif
  return -666;
}

static void stats( old_heap_t *heap, int generation, heap_stats_t *stats )
{
#if 0
  dof_data_t *data = DATA(heap);
  int live_los;

  live_los = los_bytes_used( heap->collector->los, generation );
  stats->copied_last_gc = data->copied_last_gc_bytes[generation-1];
  stats->moved_last_gc = data->moved_last_gc_bytes[generation-1];
  stats->target = data->size_bytes;

  stats->live = used_space( heap, generation-1 );
  stats->semispace1 = stats->target;

  data->copied_last_gc_bytes[generation-1] = 0;
  data->moved_last_gc_bytes[generation-1] = 0;
#else
  panic( "DOF" );
#endif
}

/* This protocol would be less painful if the generation number were passed. */
static void before_collection( old_heap_t *heap )
{
#if 0
  int area = heap2area( heap );

  heap->allocated = used_space( heap, area );
  heap->maximum = DATA(heap)->size_bytes;
#endif
}

/* Ditto. */
static void after_collection( old_heap_t *heap )
{
#if 0
  gc_t *gc = heap->collector;
  int area = heap2area( heap );

  heap->allocated = used_space( heap, area );
  heap->maximum = DATA(heap)->size_bytes;
#endif
}

/* Yeah, this is really, really ugly. */
static int heap2area( old_heap_t *heap )
{
  old_heap_t **e = heap->collector->ephemeral_area;
  int n = heap->collector->ephemeral_area_count;
  int i;
  
  for ( i=0 ; i < n && e[i] != heap ; i++ )
    ;
  return i;
}

static int used_space( old_heap_t *heap, int area )  /* Internal */
{
#if 0
  dof_data_t *data = DATA(heap);

  ss_sync( data->areas[area] );
  return data->areas[area]->used + 
    los_bytes_used( heap->collector->los, data->gen_no_lo+area );
#else
  panic( "DOF" );
  return 0;
#endif
}

static int free_space_overall( old_heap_t *heap )
{
  panic( "free_space_overall" );
}

static old_heap_t *allocate_heap( gc_t *gc, dof_data_t *data )
{
  old_heap_t *heap;

  heap = create_old_heap_t( "dof/fixed",
			    HEAPCODE_DOF,
			    0,               /* initialize */
			    collect,
			    0,           /* collect_with_selective_fromspace */
			    before_collection,
			    after_collection,
			    stats,
			    0,	             /* data_load_area */
			    0,               /* load_prepare */
			    0,               /* load_data */
			    0,	             /* set_policy */
			    data
			   );
  heap->collector = gc;

  data->gen_no_lo = 0;
  data->gen_no_hi = 0;
  data->nsteps = 0;
  data->current_step = 0;
  data->dynamic_max = 0;
  data->dynamic_min = 0;
  data->size = 0;
  
  return heap;
}

/* eof */
