/* Rts/Sys/sc-heap.c
 * Larceny run-time system -- variable-sized stop-and-copy young heap.
 *
 * $Id$
 *
 * Contract:
 *  - The stack cache lives in the heap; the stack pointer is the
 *    heap limit and the heap pointer is the stack limit, all within
 *    the current heap chunk.
 *  - The stack cache is flushed into the heap before a gc.
 *  - The collector keeps some pointers in globals[].
 *
 * Strategy:
 *  Keep load factor at about 1/L -- counting the static area -- where
 *  L is user-settable.  A load factor of 1/L means that 1/L of allocated
 *  memory contains live data at a point in time.
 *
 * Policy:
 *  See the discussion at the head of gc_compute_dynamic_size() in memmgr.c.
 *
 * Implementation:
 *  While the mutator is running, only one semispace is allocated, and it's
 *  allocated in chunks on demand.  The other semispace is also allocated on 
 *  demand as the garbage collector runs.
 *
 *  The heap is checked for overflow during allocate, stack_overflow, and
 *  creg_get.  Note that allocation and hence overflow also happens outside
 *  of the control of this code (namely, in millicode), and that we rely
 *  on chunking to eventually cause a trap into this code.
 *
 *  The allocation budget will at times go negative; this is normal.
 *
 * Grunge:
 *  The heap pointers for the current chunk are in globals[], but because
 *  the heap areas are represented as chunked semispaces, the pointers must
 *  periodically  be copied into the current semispace chunk's local pointers,
 *  and vice versa -- ie., the pointers must be in sync at certain points.
 *  The procedures mode_ss_to_globals and mode_globals_to_ss perform the
 *  mode switches.  Mode 'ss' should be confined to small regions within
 *  procedures.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "memmgr.h"
#include "gc.h"
#include "gc_t.h"
#include "los_t.h"
#include "young_heap_t.h"
#include "semispace_t.h"
#include "static_heap_t.h"
#include "stack.h"
#include "gclib.h"
#include "heap_stats_t.h"

typedef struct {
  int gen_no;

  word *globals;
  semispace_t *current_space;

  /* Strategy/Policy/Mechanism */
  int size_bytes;      /* Size requested at startup time */
  int target_size;     /* Size for this area computed following previous gc. */
  double load_factor;  /* That's the L from the formula above */
  int upper_limit;     /* Bound on the heap size */

  /* Statistics */
  unsigned stacks_created;
  unsigned frames_flushed;
  unsigned bytes_flushed;
  int copied_last_gc;       /* Not including large objects */
  int moved_last_gc;        /* The large objects */
} sc_data_t;

#define DATA(heap)  ((sc_data_t*)((heap)->data))

static young_heap_t *allocate_heap( int gen_no, gc_t *gc );
static void make_space_for( young_heap_t *heap, int nbytes, int stack_ok );
static int  collect_if_no_room( young_heap_t *heap, int nbytes );
static int  compute_target_size( young_heap_t *heap, int D, int Q );
static void must_create_stack( young_heap_t *heap );
static void must_restore_frame( young_heap_t *heap );
static void flush_stack( young_heap_t *heap );
static void mode_ss_to_globals( young_heap_t *heap );
static void mode_globals_to_ss( young_heap_t *heap );
static int  free_current_chunk( young_heap_t *heap );
static int  used_stack_space( young_heap_t *heap );
static int  used_space( young_heap_t *heap );
static int  static_used( young_heap_t *heap );
static int  free_space( young_heap_t *heap );

young_heap_t*
create_sc_heap( int gen_no, 
	        gc_t *gc,
	        sc_info_t *info,        /* creation parameters */
	        word *globals           /* the globals array */
	       )
{
  young_heap_t *heap;
  sc_data_t *data;
  int r;

  assert( info->size_bytes >= PAGESIZE );

  heap = allocate_heap( gen_no, gc );
  data = heap->data;

  data->globals = globals;
  data->current_space =
    create_semispace( GC_CHUNK_SIZE, data->gen_no, data->gen_no );

  data->load_factor = info->load_factor;
  data->size_bytes = info->size_bytes;
  data->upper_limit = info->dynamic_max;
  data->target_size =
    compute_target_size( heap, (int)(info->size_bytes/data->load_factor), 0 );

  supremely_annoyingmsg( "sc-heap: initial size = %d", data->target_size );

  mode_ss_to_globals( heap );

  globals[ G_STKUFLOW ] = 0;
  must_create_stack( heap );

  heap->maximum = data->target_size;
  heap->allocated = 0;

  return heap;
}

semispace_t *yhsc_data_area( young_heap_t *heap )
{
  return DATA(heap)->current_space;
}

static word *allocate( young_heap_t *heap, int nbytes, int no_gc )
{
  sc_data_t *data = DATA(heap);
  word *globals = data->globals;
  word *p;

  assert( nbytes > 0 );
  nbytes = roundup_balign( nbytes );

  if (free_current_chunk( heap ) < nbytes && !no_gc)
    collect_if_no_room( heap, nbytes );

  if (nbytes <= GC_LARGE_OBJECT_LIMIT) {
    make_space_for( heap, nbytes, 1 );
    p = (word*)globals[ G_ETOP ];
    globals[ G_ETOP ] += nbytes;
  }
  else
    p = los_allocate( heap->collector->los, nbytes, 0 );

  return p;
}

static void collect( young_heap_t *heap, int request_bytes )
{
  sc_data_t *data = DATA(heap);
  semispace_t *other_space;
  int total_bytes, stack;

  assert( request_bytes >= 0 );
  request_bytes = roundup_balign( request_bytes );

  /* The total number of bytes is stack + frame + object, although
     if it's a large object then the effective object size is 0.
     */
  total_bytes = stk_size_for_top_stack_frame( data->globals );
  if (request_bytes <= GC_LARGE_OBJECT_LIMIT)
    total_bytes += request_bytes;

  stats_gc_type( 0, STATS_COLLECT );

  stack = used_stack_space( heap );
  flush_stack( heap );

  annoyingmsg( "Stop-and-copy heap: Garbage collection." );
  annoyingmsg( "   Avail=%d,  Used=%d", free_space( heap ), used_space( heap ));
  
  /* MODE: ss */
  mode_globals_to_ss( heap );

  ss_sync( data->current_space );
  supremely_annoyingmsg( "ss used=%d, ss allocd=%d, stack=%d, los=%d",
			 data->current_space->used,
			 data->current_space->allocated,
			 stack,
			 los_bytes_used( heap->collector->los, 0 ) );

  other_space =
    create_semispace( GC_CHUNK_SIZE, data->gen_no, data->gen_no );

  gclib_stopcopy_collect_and_scan_static( heap->collector, other_space );

  ss_free( data->current_space );
  ss_sync( other_space );
  data->current_space = other_space;

  mode_ss_to_globals( heap );
  /* END MODE: ss */


  data->copied_last_gc = data->current_space->used;
  data->moved_last_gc = los_bytes_used( heap->collector->los, 0 );

  /* POLICY */
  data->target_size =
    compute_target_size( heap,
			 data->current_space->used, 
			 los_bytes_used( heap->collector->los, 0 ) );

  make_space_for( heap, total_bytes, 0 );

  must_create_stack( heap );
  must_restore_frame( heap );

  assert( free_current_chunk( heap ) >= request_bytes );

  annoyingmsg( "Stop-and-copy heap: Collection finished; Live=%d",
	       used_space( heap ) );

  ss_sync( data->current_space );
  supremely_annoyingmsg( 
    "  size=%d, ss used=%d, ss allocd=%d, los=%d, static=%d",
    data->target_size,
    data->current_space->used,
    data->current_space->allocated,
    los_bytes_used( heap->collector->los, 0 ),
    static_used( heap ) );
}

static int compute_target_size( young_heap_t *heap, int D, int Q )
{
  int s;
  s = gc_compute_dynamic_size( D,
			       static_used( heap ),
			       Q,
			       DATA(heap)->load_factor,
			       DATA(heap)->upper_limit );
  /* gc_compute_dynamic_size() may return with no room for allocation */
  if (s - D < 65536)
    s += 65536;
  return s;
}

static void before_collection( young_heap_t *heap )
{
  heap->maximum = DATA(heap)->target_size;
  heap->allocated = used_space( heap );
}

static void after_collection( young_heap_t *heap )
{
  /* Nothing */
}

static int free_space( young_heap_t *heap ) 
{
  sc_data_t *data = DATA(heap);

  mode_globals_to_ss( heap );
  ss_sync( data->current_space );
  return data->target_size - used_space( heap );
}

static void stats( young_heap_t *heap, heap_stats_t *stats ) 
{
  sc_data_t *data = DATA(heap);
  word *globals = data->globals;

  stats->live = used_space( heap ) - used_stack_space( heap );
  stats->copied_last_gc = data->copied_last_gc;
  stats->moved_last_gc = data->moved_last_gc;
  stats->stack = globals[ G_STKBOT ] - globals[ G_STKP ];
  stats->frames_flushed = data->frames_flushed;
  stats->frames_restored = globals[G_STKUFLOW];
  stats->bytes_flushed = data->bytes_flushed;
  stats->semispace1 = data->current_space->allocated 
                    + los_bytes_used( heap->collector->los, data->gen_no );
  stats->semispace2 = 0;
  stats->target = data->target_size;
  stats->stacks_created = data->stacks_created;

  data->frames_flushed = 0;
  data->bytes_flushed = 0;
  data->stacks_created = 0;
  data->copied_last_gc = 0;
  globals[ G_STKUFLOW ] = 0;
}

static word *data_load_area( young_heap_t *heap, int nbytes )
{
  sc_data_t *data = DATA(heap);
  int n;

  assert( nbytes > 0 );
  assert( nbytes % BYTE_ALIGNMENT == 0 );

  n = ss_allocate_and_insert_block( data->current_space, nbytes );
  return data->current_space->chunks[ n ].bot;
}

/* Here we don't know the size of the frame that overflowed.  If the frame
   is larger than STACK_ROOM then we may conceivably loop forever if
   the chunk allocated is no larger than STACK_ROOM.  Therefore, chunks
   should be larger than the largest possible stack frame.
   */
static void stack_overflow( young_heap_t *heap )
{
  if (!collect_if_no_room( heap, 0 ))
    make_space_for( heap, STACK_ROOM, 1 );
}

static void stack_underflow( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;

  globals[ G_STKUFLOW] += 1;
  if (!stk_restore_frame( globals ))
    stack_overflow( heap );                        /* [sic] */
}

static word creg_get( young_heap_t *heap ) 
{
  int room = stk_size_for_top_stack_frame( DATA(heap)->globals );
  word p;

  collect_if_no_room( heap, room );
  flush_stack( heap );
  make_space_for( heap, room, 0 );
  p = DATA(heap)->globals[ G_CONT ];
  must_create_stack( heap );
  must_restore_frame( heap );
  return p;
}

static void creg_set( young_heap_t *heap, word k )
{
  word *globals = DATA(heap)->globals;

  stk_clear( globals );
  globals[ G_CONT ] = k;
  if (!stk_restore_frame( globals ))
    stack_overflow( heap );                        /* [sic] */
}

/* Internal functions past this point. */

static int collect_if_no_room( young_heap_t *heap, int nbytes )
{
  sc_data_t *data = DATA(heap);

  if (free_space( heap ) < nbytes) {
    supremely_annoyingmsg( "sc-heap: couldn't find room for %d bytes",
			   nbytes );
    if (nbytes <= GC_LARGE_OBJECT_LIMIT)
      gc_collect( heap->collector, 0, nbytes );
    else
      gc_collect( heap->collector, 0, 0 );
    return 1;
  }
  else
    return 0;
}

static void make_space_for( young_heap_t *heap, int nbytes, int stack_ok )
{
  sc_data_t *data = DATA(heap);
  word *globals = data->globals;
  semispace_t *ss = data->current_space;
  int extra = 0, bytes_to_alloc, budget;

  if (free_current_chunk( heap ) < nbytes) {
    if (stack_ok) {
      extra = stk_size_for_top_stack_frame( globals );
      flush_stack( heap );
    }
    budget = free_space( heap );
    bytes_to_alloc = 
      roundup_page( max( min( GC_CHUNK_SIZE, budget ), nbytes+extra ) );

    mode_globals_to_ss( heap );
    ss->chunks[ss->current].top = ss->chunks[ss->current].lim;  /* FULL */
    ss_expand( ss, bytes_to_alloc );
    mode_ss_to_globals( heap );

    if (stack_ok) {
      must_create_stack( heap );
      must_restore_frame( heap );
    }
    assert( free_current_chunk( heap ) >= nbytes );
  }
}

static void must_create_stack( young_heap_t *heap )
{
  stk_create( DATA(heap)->globals ) || panic_abort( "INVARIANT" );
  DATA(heap)->stacks_created++;
}

static void must_restore_frame( young_heap_t *heap )
{
  stk_restore_frame( DATA(heap)->globals ) || panic_abort( "INVARIANT" );
}

static void flush_stack( young_heap_t *heap )
{
  unsigned frames, bytes;

  stk_flush( DATA(heap)->globals, &frames, &bytes );
  DATA(heap)->frames_flushed += frames;
  DATA(heap)->bytes_flushed += bytes;
}

/* Mode in: globals; Mode out: ss */
static void mode_globals_to_ss( young_heap_t *heap )
{
  semispace_t *s = DATA(heap)->current_space;
  word *globals = DATA(heap)->globals;
  
  s->chunks[s->current].top = (word*)globals[ G_ETOP ];
}

/* Mode in: ss; Mode out: globals */
/* Note: this puts the stack at the high end of the current chunk! */
static void mode_ss_to_globals( young_heap_t *heap )
{
  semispace_t *s = DATA(heap)->current_space;
  word *globals = DATA(heap)->globals;

  globals[ G_EBOT ] = (word)s->chunks[s->current].bot;
  globals[ G_ETOP ] = (word)s->chunks[s->current].top;
  globals[ G_ELIM ] = (word)s->chunks[s->current].lim;

  globals[ G_STKP ] = globals[ G_ELIM ];
}

static int static_used( young_heap_t *heap )
{
  static_heap_t *s = heap->collector->static_area;  /* may be NULL */

  return (s ? s->allocated : 0);
}

static int free_current_chunk( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;
  return globals[ G_STKP ] - globals[ G_ETOP ];
}

static int used_stack_space( young_heap_t *heap )
{
  semispace_t *ss = DATA(heap)->current_space;
  word *globals = DATA(heap)->globals;

  return (int)((word)ss->chunks[ss->current].lim - globals[ G_STKP ]);
}

static int used_space( young_heap_t *heap )
{
  semispace_t *ss = DATA(heap)->current_space;

  mode_globals_to_ss( heap );  /* so that we can sync() */
  ss_sync( ss );
  return ss->used +
         used_stack_space( heap ) +
	 los_bytes_used( heap->collector->los, 0 );
}

static young_heap_t *allocate_heap( int gen_no, gc_t *gc )
{
  young_heap_t *heap;
  sc_data_t *data;

  data = (sc_data_t*)must_malloc( sizeof( sc_data_t ) );
  heap = create_young_heap_t( "sc/variable", 
			      HEAPCODE_YOUNG_2SPACE,
			      0,                 /* intialize */
			      allocate,
			      collect,
			      before_collection,
			      after_collection,
			      0,                 /* set_policy */
			      free_space,
			      stats,
			      data_load_area,
			      0,                 /* FIXME: load_prepare */
			      0,                 /* FIXME: load_data */ 
			      creg_get,
			      creg_set,
			      stack_underflow,
			      stack_overflow,
			      data );
  heap->collector = gc;

  data->gen_no = gen_no;
  data->frames_flushed = 0;
  data->bytes_flushed = 0;
  data->stacks_created = 0;
  data->copied_last_gc = 0;

  return heap;
}

/* eof */
