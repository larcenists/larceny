/* Rts/Sys/np-sc-heap.c
 * Larceny -- two-generation non-predictive copying dynamic area.
 *
 * $Id: np-sc-heap.c,v 1.12 1997/09/17 15:17:26 lth Exp $
 *
 * The collector sets up the 'old' areas as generation data->gen_no, and
 * the 'young' area as generation data->gen_no+1, to let the existing 
 * write barrier and scanning machinery work.
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

enum action { PROMOTE_TO_OLD, PROMOTE_TO_YOUNG, COLLECT };

typedef struct npsc_data npsc_data_t;

struct npsc_data {
  int gen_no;                 /* generation number of 'old' generation */

  /* Parameters. */
  int size_bytes;             /* Original size */
  int k;                      /* k = current number of steps */
  int j;                      /* j = dividing point */
  int stepsize;               /* bytes */

  /* Policy: j is calculated either as a percentage of free steps, or it's 
     pinned at some value (if possible).  For example, if pin_value == 1
     and there is at least one empty step, then j is set to 1.  
     j_percent and j_pin can't both be -1 at the same time.
     */
  int j_percent;              /* -1 or percentage for calculating j */
  int j_pin;                  /* -1 or value at which to pin j */
  double load_factor;

  semispace_t *old;           /* 'old' generation */
  semispace_t *young;         /* 'young' generation */

  int copied_last_gc_old;     /* bytes */
  int moved_last_gc_old;
  int copied_last_gc_young;   /* bytes */
  int moved_last_gc_young;
};

#define DATA(x)             ((npsc_data_t*)((x)->data))

static old_heap_t *allocate_heap( int gen_no, gc_t *gc );
static void perform_promote_to_old( old_heap_t *heap );
static void perform_promote_to_young( old_heap_t *heap );
static void perform_collect( old_heap_t *heap );
static enum action decision( old_heap_t *heap );
static int used_young( old_heap_t *heap );
static int used_old( old_heap_t *heap );
static int compute_target_size( old_heap_t *heap, int D, int Q );
static int cleanup_scanner( word obj, void *data, unsigned *ignored );

old_heap_t *
create_np_dynamic_area( int gen_no, int *gen_allocd, gc_t *gc, np_info_t *info)
{
  old_heap_t *heap;
  npsc_data_t *data;

  heap = allocate_heap( gen_no, gc );
  data = DATA(heap);

  *gen_allocd = 2;

  data->size_bytes = roundup_page( info->size_bytes );
  data->stepsize = info->stepsize;
  data->k = info->steps;
  data->j_pin = -1;
  data->j_percent = 50;
  data->load_factor = info->load_factor;

  /* This is an OK initial j if the heap is empty.  If the heap is used
     to load the heap image into, then data_load_area(), below,
     computes a more appropriate j.
     */
  data->j = ceildiv( info->steps, 3 );

  data->old = create_semispace( GC_CHUNK_SIZE, gen_no, gen_no );
  data->young = create_semispace( GC_CHUNK_SIZE, gen_no, gen_no+1 );

  return heap;
}

static void collect( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  switch (decision( heap )) {
  case PROMOTE_TO_OLD   : perform_promote_to_old( heap ); break;
  case PROMOTE_TO_YOUNG : perform_promote_to_young( heap ); break;
  case COLLECT          : perform_collect( heap );  break;
  default :
    panic_abort( "Impossible." );
  }
}

/* Cautious strategy: 
     Let X be the amount of memory allocated in all ephmeral generations.
     Let No be the amount of memory available in the 'old' generation.
     Let Ny be the amount of memory available in the 'young' generation.
     if X <= No then
       promote from ephemeral generations to 'old'
     else if X <= No+Ny then
       promote from ephemeral generations to 'young',
         and shuffle steps from 'young' to 'old' afterwards to fill up 'old'
     else
       collect
   */
static enum action decision( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  gc_t *gc = heap->collector;
  int X, No, Ny, i;

  /* Gather data */

  X = gc->young_area->allocated;
  for ( i = 0 ; i < gc->ephemeral_area_count ; i++ )
    X += gc->ephemeral_area[i]->allocated;

  No = data->stepsize * (data->k - data->j) - used_old( heap );
  Ny = data->stepsize * data->j - used_young( heap );

  if (X <= No)
    return PROMOTE_TO_OLD;
  else if (X <= No + Ny)
    return PROMOTE_TO_YOUNG;
  else
    return COLLECT;
}

static void perform_promote_to_old( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int old_before_gc, los_before_gc;

  ss_sync( data->old );
  old_before_gc = data->old->used;
  los_before_gc = los_bytes_used( heap->collector->los, data->gen_no );

  annoyingmsg( "np-sc-heap: Promoting into 'old'.\n"
	       "  live=%d", old_before_gc );
  stats_gc_type( data->gen_no, STATS_PROMOTE );

  gclib_stopcopy_promote_into( heap->collector, data->old );

  ss_sync( data->old );
  data->copied_last_gc_old = data->old->used - old_before_gc;
  data->moved_last_gc_old =
    los_bytes_used( heap->collector->los, data->gen_no ) - los_before_gc;

  annoyingmsg( "np-sc-heap: Finished promoting.\n"
	       "  live=%d", used_old( heap ) );
}

static void perform_promote_to_young( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  int los_moved = 0, chunks_moved = 0;
  bool cleared_chunks, cleared_los;
  los_t *los = heap->collector->los;
  int young_before_gc, i, available, copied_young, copied_old;
  semispace_t *old, *young;
  los_list_t *l;
  word *p;

/* FIXME: stats */
  young_before_gc = used_young( heap );
  annoyingmsg( "np-sc-heap: Promoting into 'young'.\n"
	       "  live old=%d, live young=%d.",
	       used_old( heap ), young_before_gc );
  stats_gc_type( data->gen_no+1, STATS_PROMOTE );

  gclib_stopcopy_promote_into_np_young( heap->collector, data->young );
  copied_young = used_young( heap ) - young_before_gc;
  copied_old = 0;

  /* Move data from 'young' into 'old' to fill it up, using a simple greedy
     algorithm.
     */
  old = data->old;
  young = data->young;
  available = (data->k-data->j)*data->stepsize - used_old( heap );

  /* First move chunks, but never move the 'current' chunk. 
     It's possible to move the current chunk, but the fragmentation costs
     can be considerable, because that chunk is only partially filled.
     Alternatively, we could move parts of chunks, but that requires a
     more sophisticated low-level allocator.
     */
  i = 0;
  while (i < young->current) {	/* Was: <=, but that moves 'current' */
    int used = sizeof(word)*(young->chunks[i].top - young->chunks[i].bot);
    if (used <= available) {
      available -= used;
      ss_move_block_to_semispace( young, i, old ); /* young->current-- */
      copied_young -= used;
      copied_old += used;
      chunks_moved++;
    }
    else 
      i++;
  }
  if (young->current == -1) {
    ss_free( young );
    data->young =
      create_semispace( GC_CHUNK_SIZE, data->gen_no, data->gen_no+1 );
    cleared_chunks = 1;
  }
  else
    cleared_chunks = 0;

  /* Then move large objects */
  cleared_los = 1;
  l = los->object_lists[data->gen_no+1];
  p = los_walk_list( l, 0 );
  while (p != 0) {
    int used = sizefield( *ptrof( p ) );
    word *the_obj = p;

    p = los_walk_list( l, p );

    if (used <= available) {
      available -= used;
      los_mark( los, the_obj, data->gen_no+1 );
      copied_young -= used;
      copied_old += used;
      los_moved++;
    }     
    else
      cleared_los = 0;
  }
  los_append_and_clear_list( los, los->marked, data->gen_no );

  /* Manipulate the remembered set as necessary */
  { gc_t *gc = heap->collector;
    int genx = data->gen_no+1;

    if (cleared_chunks && cleared_los)
      rs_clear( gc->remset[ gc->np_remset ] );
    else if (los_moved > 0 || chunks_moved > 0) {
      rs_compact( gc->remset[ gc->np_remset ] );
      rs_enumerate( gc->remset[ gc->np_remset ], cleanup_scanner, (void*)&genx);
    }
  }
    
  data->copied_last_gc_young = copied_young;
  data->copied_last_gc_old = copied_old;

  { int use, alloc;

    ss_sync( data->old );
    ss_sync( data->young );
    use = data->old->used + data->young->used;
    alloc = data->old->allocated + data->young->allocated;

    annoyingmsg("np-sc-heap: finished promoting.\n"
		"  live old=%d, live young=%d, use=%d, alloc=%d, frag=%f%%\n"
		"  clr chunks=%d, clr los=%d, chunks moved=%d, los moved=%d",
		used_old( heap ), used_young( heap ), 
 		use, alloc, (double)(alloc - use)/(double)use*100.0,
	 	cleared_chunks, cleared_los, chunks_moved, los_moved );
  } 
}

/* Remove something from the remembered set if it doesn't point to an 
   object in the 'young' space.  Data points to the generation number 
   of the 'young' space.
   */
static int cleanup_scanner( word obj, void *data, unsigned *ignored )
{
  return gclib_desc_g[ pageof( obj ) ] == *(int*)data;
}
  
static void perform_collect( old_heap_t *heap )
{
  npsc_data_t *d = DATA(heap);
  los_t *los = heap->collector->los;
  gc_t *gc = heap->collector;
  int old_before_gc, young_before_gc, total_after_gc, free_steps, target_size;

/* FIXME: stats */
  annoyingmsg( "np-sc-heap: garbage collection.\n"
	       "  k=%d, j=%d.",
	       d->k, d->j );

  stats_gc_type( d->gen_no, STATS_COLLECT );

  young_before_gc = used_young( heap );
  old_before_gc = used_old( heap );
  gclib_stopcopy_collect_np( gc, d->young );

  /* Manipulate the semispaces: young becomes old, old is deallocated */
  ss_free( d->old );
  d->old = d->young;

  ss_set_gen_no( d->old, d->gen_no );
  assert( los_bytes_used( los, d->gen_no ) == 0 );
  los_append_and_clear_list( los, los->object_lists[ d->gen_no+1 ], d->gen_no);

  d->young = create_semispace( GC_CHUNK_SIZE, d->gen_no, d->gen_no+1 );

  total_after_gc = used_old( heap );
  d->copied_last_gc_old = total_after_gc - young_before_gc;

  annoyingmsg( "  old before: %d, young before: %d, total after: %d\n"
	       "  copied or moved: %d",
	       old_before_gc, young_before_gc, total_after_gc,
	       d->copied_last_gc_old );
			 
  /* Clear remembered sets not cleared by the collector infrastructure. */
  rs_clear( gc->remset[ d->gen_no+1 ] );
  rs_clear( gc->remset[ gc->np_remset ] );

  /* Compute new k and j, and other policy parameters */
  ss_sync( d->old );
  target_size =
    compute_target_size( heap,
			 d->old->used,
			 los_bytes_used( los, d->gen_no ) );
  d->k = ceildiv( target_size, d->stepsize );
  free_steps = (d->k * d->stepsize - total_after_gc) / d->stepsize;
  if (d->j_percent >= 0)
    d->j = (free_steps * d->j_percent) / 100;
  else if (free_steps >= d->j_pin)
    d->j = d->j_pin;
  else {
    d->j = free_steps / 2;
    supremely_annoyingmsg( "  Could not pin j at %d; chose %d instead",
			   d->j_pin, d->j );
  }
  annoyingmsg( "np-sc-heap: Finished collection.\n"
	       "  chunk bytes=%d, los bytes=%d, target=%d, free steps=%d\n"
	       "  k=%d, j=%d",
	       d->old->used, los_bytes_used( los, d->gen_no ),
	       target_size, free_steps,
	       d->k, d->j );
}

static void stats( old_heap_t *heap, int generation, heap_stats_t *stats )
{
  npsc_data_t *data = DATA(heap);
  int live_los;

  if (generation == data->gen_no) {
    live_los = los_bytes_used( heap->collector->los, data->gen_no );
    stats->np_old = 1;
    stats->copied_last_gc = data->copied_last_gc_old;
    stats->moved_last_gc = data->moved_last_gc_old;
    stats->target = data->stepsize * (data->k-data->j);
    stats->live = used_old( heap );
    stats->semispace1 = data->old->allocated + live_los;

    data->copied_last_gc_old = 0;
    data->moved_last_gc_old = 0;
  }
  else {
    live_los = los_bytes_used( heap->collector->los, data->gen_no+1 );
    stats->np_young = 1;
    stats->copied_last_gc = data->copied_last_gc_young;
    stats->moved_last_gc = data->moved_last_gc_young;
    stats->target = data->stepsize * data->j;
    stats->live = used_young( heap );
    stats->semispace1 = data->young->allocated + live_los;

    data->copied_last_gc_young = 0;
    data->moved_last_gc_young = 0;
  }

  stats->np_j = data->j;
  stats->np_k = data->k;
}

static void before_collection( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  ss_sync( data->young );
  ss_sync( data->old );
  heap->allocated = data->young->used + data->old->used;
  heap->maximum = data->k * data->stepsize;
}

static void after_collection( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);
  
  heap->allocated = data->young->allocated + data->old->allocated 
                  + los_bytes_used( heap->collector->los, data->gen_no )
                  + los_bytes_used( heap->collector->los, data->gen_no+1 );
  heap->maximum = data->stepsize * data->k;
}

static void set_policy( old_heap_t *heap, int op, int value )
{
  npsc_data_t *data = DATA(heap);

  switch (op) {
  case GCCTL_J_FIXED : /* j-fixed */
    data->j_pin = value;
    if (data->j > value) data->j = value;  /* Hack. */
    break;
  case GCCTL_J_PERCENT : /* j-percent */
    data->j_pin = -1;
    data->j_percent = value;
    break;
  }
}

static word *data_load_area( old_heap_t *heap, int nbytes )
{
  npsc_data_t *data = DATA(heap);
  int n;

  assert( nbytes > 0 );
  assert( nbytes % BYTE_ALIGNMENT == 0 );

  n = ss_allocate_and_insert_block( data->old, nbytes );
  return data->old->chunks[ n ].bot;
}

static int used_young( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  ss_sync( data->young );
  return
   data->young->used + los_bytes_used( heap->collector->los, data->gen_no+1 );
}

static int used_old( old_heap_t *heap )
{
  npsc_data_t *data = DATA(heap);

  ss_sync( data->old );
  return 
   data->old->used + los_bytes_used( heap->collector->los, data->gen_no );
}

/* The target size is M - (S+Q+D) - D = L(D+S+Q) - (D+S+Q) - D */
/* Slightly WRONG.  See compute_dynamic_size in old-heap.c */
static int compute_target_size( old_heap_t *heap, int D, int Q )
{
  static_heap_t *s = heap->collector->static_area;
  double L = DATA(heap)->load_factor;
  int S = (s ? s->allocated : 0);

  return roundup_page( (int)(L*(D+S+Q)-(D+S+Q)-D) );
}

static old_heap_t *allocate_heap( int gen_no, gc_t *gc )
{
  old_heap_t *heap;
  npsc_data_t *data;

  data = (npsc_data_t*)must_malloc( sizeof( npsc_data_t ) );
  heap = create_old_heap_t( "npsc/2/variable",
			    HEAPCODE_OLD_2SPACE_NP,
			    0,               /* initialize */
			    collect,
			    before_collection,
			    after_collection,
			    stats,
			    data_load_area,
			    0,               /* FIXME: load_prepare */
			    0,               /* FIXME: load_data */
			    set_policy,
			    data
			   );
  heap->collector = gc;

  data->gen_no = gen_no;
  data->copied_last_gc_old = 0;
  data->moved_last_gc_old = 0;
  data->copied_last_gc_young = 0;
  data->moved_last_gc_young = 0;

  return heap;
}

/* eof */
