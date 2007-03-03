/* Copyright 1998 Lars T Hansen.       -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- variable-sized stop-and-copy young heap.
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
#include "stats.h"

typedef struct {
  /* The heap */
  stats_id_t  self;             /* Identity */
  int         gen_no;           /* Always 0, actually */
  word        *globals;         /* Register save area & RTS globals */
  semispace_t *current_space;   /* The heap area */

  /* Strategy/Policy/Mechanism */
  int         target_size;      /* Dynamic area size computed following GC. */
  double      load_factor;      /* Really the inverse load, L */
  int         lower_limit;      /* 0 or lower bound on area (bytes) */
  int         upper_limit;      /* 0 or upper bound on area (bytes) */

  /* Statistics */
  int         used_after_last_gc;  /* Bytes */
  gen_stats_t gen_stats;        /* Local statistics */
  gc_stats_t  gc_stats;         /* Global statistics */
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

  assert( info->size_bytes >= PAGESIZE );

  heap = allocate_heap( gen_no, gc );
  data = heap->data;

  data->globals = globals;
  data->current_space =
    create_semispace( GC_CHUNK_SIZE, data->gen_no );

  data->load_factor = info->load_factor;
  data->lower_limit = info->dynamic_min;
  data->upper_limit = info->dynamic_max;
  data->target_size =
    compute_target_size( heap, (int)(info->size_bytes/data->load_factor), 0 );

  supremely_annoyingmsg( "sc-heap: initial size = %d", data->target_size );

  mode_ss_to_globals( heap );

  heap->maximum = data->target_size;
  heap->allocated = 0;

  return heap;
}

static void create_initial_stack( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;
  
  globals[ G_STKUFLOW ] = 0;
  must_create_stack( heap );
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

static void make_room( young_heap_t *heap )
{
  if (free_current_chunk( heap ) < 4*KILOBYTE) {
    collect_if_no_room( heap, 4*KILOBYTE );
    make_space_for( heap, 4*KILOBYTE, 1 );
  }
}

/* Request is ignored -- doesn't make sense in stop/copy heap. */
static void collect( young_heap_t *heap, int request_bytes, int request )
{
  sc_data_t *data = DATA(heap);
  semispace_t *other_space;
  int total_bytes, stack, used_before, used_after;
  stats_id_t timer1, timer2;

  timer1 = stats_start_timer( TIMER_ELAPSED );
  timer2 = stats_start_timer( TIMER_CPU );

  assert( request_bytes >= 0 );
  
  /* [pnkfelix] Tue Jun  6 01:04:55 EDT 2006
   * Ticket #92: If bytes == 0, then we're asking for more core in
   * general, not a particular amount.  Therefore we should grab a
   * hefty chunk.  (All of D might be too hefty though?) */
  if (request_bytes == 0) {
    request_bytes = GC_LARGE_OBJECT_LIMIT;
  }

  request_bytes = roundup_balign( request_bytes );

  /* The total number of bytes is stack + frame + object, although
     if it's a large object then the effective object size is 0.
     */
  total_bytes = stk_size_for_top_stack_frame( data->globals );
  if (request_bytes <= GC_LARGE_OBJECT_LIMIT)
    total_bytes += request_bytes;

  stack = used_stack_space( heap );
  flush_stack( heap );

  used_before = used_space( heap );
  annoyingmsg( "Stop-and-copy heap: Garbage collection." );
  annoyingmsg( "  Avail=%d,  Used=%d", free_space( heap ), used_before );
  
  /* MODE: ss */
  mode_globals_to_ss( heap );

  ss_sync( data->current_space );
  supremely_annoyingmsg( "ss used=%d, ss allocd=%d, stack=%d, los=%d",
                         data->current_space->used,
                         data->current_space->allocated,
                         stack,
                         los_bytes_used( heap->collector->los, 0 ) );

  other_space =
    create_semispace( GC_CHUNK_SIZE, data->gen_no );

  gclib_stopcopy_collect_and_scan_static( heap->collector, other_space );

  ss_free( data->current_space );
  ss_sync( other_space );
  data->current_space = other_space;

  mode_ss_to_globals( heap );
  /* END MODE: ss */

  /* Statistics */
  used_after = used_space( heap );
  data->gc_stats.allocated += 
    bytes2words( used_before - data->used_after_last_gc );
  data->gc_stats.reclaimed += bytes2words( used_before - used_after );
  data->gc_stats.words_copied += bytes2words( data->current_space->used );
  data->gc_stats.words_moved += 
    bytes2words( los_bytes_used( heap->collector->los, data->gen_no ) );
  data->gen_stats.collections += 1;
  data->used_after_last_gc = used_after;

  /* POLICY */
  data->target_size =
    compute_target_size( heap,
                         data->current_space->used, 
                         los_bytes_used( heap->collector->los, 0 ) );

  make_space_for( heap, total_bytes, 0 );

  must_create_stack( heap );
  must_restore_frame( heap );

  assert( free_current_chunk( heap ) >= request_bytes );

  /* More statistics */
  data->gen_stats.ms_collection += stats_stop_timer( timer1 );
  data->gen_stats.ms_collection_cpu += stats_stop_timer( timer2 );

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
  s = gc_compute_dynamic_size( heap->collector,
                               D,
                               static_used( heap ),
                               Q,
                               DATA(heap)->load_factor,
                               DATA(heap)->lower_limit,
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

static void stats( young_heap_t *heap )
{
  sc_data_t *data = DATA(heap);

  data->gen_stats.target = bytes2words( data->target_size );
  data->gen_stats.allocated = 
    bytes2words( data->current_space->allocated 
                 + los_bytes_used( heap->collector->los, data->gen_no ) );
  data->gen_stats.used = 
    bytes2words( used_space( heap ) - used_stack_space( heap ) );

  stats_add_gen_stats( data->self, &data->gen_stats );
  memset( &data->gen_stats, 0, sizeof( gen_stats_t ) );

  stats_add_gc_stats( &data->gc_stats );
  memset( &data->gc_stats, 0, sizeof( gc_stats_t ) );
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
  if (free_space( heap ) < nbytes) {
    supremely_annoyingmsg( "sc-heap: couldn't find room for %d bytes",
                           nbytes );
    if (nbytes <= GC_LARGE_OBJECT_LIMIT)
      gc_collect( heap->collector, 0, nbytes, GCTYPE_COLLECT );
    else
      gc_collect( heap->collector, 0, 0, GCTYPE_COLLECT );
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
    
    /* Seal the chunk */
    word *top = ss->chunks[ss->current].top;
    word *stklim = (word*) globals[G_STKP];
    if (top < stklim) {
      word len = (stklim - top)*sizeof(word);
      *top = mkheader(len-sizeof(word),STR_HDR);
      if (top+1 < stklim) *(top+1) = 0xABCDABCD;
    }
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
  bool r = stk_create( DATA(heap)->globals );
  assert( r );
}

static void must_restore_frame( young_heap_t *heap )
{
  bool r = stk_restore_frame( DATA(heap)->globals );
  assert( r );
}

static void flush_stack( young_heap_t *heap )
{
  stk_flush( DATA(heap)->globals );
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
  return globals[ G_STKP ] - globals[ G_ETOP ] - SCE_BUFFER;
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
			      create_initial_stack,
                              allocate,
                              make_room,
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

  data->self = stats_new_generation( gen_no, 0 );
  data->gen_no = gen_no;
  memset( &data->gen_stats, 0, sizeof( gen_stats_t ) );
  memset( &data->gc_stats, 0, sizeof( gc_stats_t ) );

  return heap;
}

/* eof */
