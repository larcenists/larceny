/* Rts/Sys/sc-heap.c
 * Larceny run-time system -- varsized stop-and-copy young heap.
 *
 * $Id: sc-heap.c,v 1.4 1997/07/07 20:13:53 lth Exp lth $
 *
 * Contract:
 *  - The stack cache lives in the heap; the stack pointer is the
 *    heap limit and the heap pointer is the stack limit, all within
 *    the current heap chunk.
 *  - The stack cache is flushed into the heap before a gc.
 *  - The collector keeps some pointers in globals[]
 *
 * Policy for promotion and collection:
 *  If allocation fails, collect.  If allocation still fails, expand the
 *  heap.  If, after gc, allocation succeeds, but the heap is over the
 *  watermark, then expand the heap.  Noting is ever promoted (FIXME: it
 *  must be possible to promote things into the static area.)
 * 
 * Implementation:
 *  While the mutator is running, only the current semispace is allocated.
 *  The other semispace is allocated on demand as the garbage collector runs.
 *  
 *  The heap pointers for the current chunk are in globals[], but because
 *  the heap areas are represented as chunked semispaces, the pointers must
 *  periodically  be copied into the current semispace chunk's local pointers,
 *  and vice versa -- ie., the pointers must be in sync at certain points.
 *  The procedures mode_ss_to_globals and mode_globals_to_ss perform the
 *  mode switches.  Mode 'ss' should be confined to small regions within
 *  procedures.
 *
 * Issues:
 *  See the design notes (HTML).
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

typedef struct {
  int heap_no;
  int gen_no;

  int size_bytes;              /* target data size */
  unsigned himark;             /* Percent */
  unsigned lomark;             /* Percent */

  semispace_t *current_space;
  word *globals;

  unsigned stacks_created;
  unsigned frames_flushed;
  unsigned bytes_flushed;
  unsigned copied_last_gc;
} sc_data_t;

#define DATA(heap)  ((sc_data_t*)((heap)->data))

static young_heap_t *allocate_sc_heap( int gen_no, int heap_no );
static int new_stack( young_heap_t *heap );
static int create_stack( young_heap_t *heap );
static void clear_stack( young_heap_t *heap );
static void flush_stack( young_heap_t *heap );
static int restore_frame( young_heap_t *heap );
static void mode_ss_to_globals( young_heap_t *heap );
static void mode_globals_to_ss( young_heap_t *heap );
static int move_to_next_chunk( young_heap_t *heap, unsigned nbytes );
static unsigned free_current_chunk( young_heap_t *heap );
static unsigned used_stack_space( young_heap_t *heap );
static unsigned used_space( young_heap_t *heap );
static void post_gc_policy( young_heap_t *heap, unsigned nbytes );
static void expand_heap( young_heap_t *heap, unsigned bytes );
static void contract_heap( young_heap_t *heap, unsigned bytes );


young_heap_t*
create_sc_heap( int *gen_no, 
	        int heap_no,
	        unsigned size_bytes,    /* initial size; 0 = default */
	        unsigned hiwatermark,   /* in percent; 0 = default */
	        unsigned lowatermark,   /* in percent; 0 = default */
	        word *globals           /* the globals array */
	       )
{
  young_heap_t *heap;
  sc_data_t *data;
  int r;

  /* Argument validation */

  if (size_bytes == 0) 
    size_bytes = DEFAULT_SC_SIZE;
  size_bytes = roundup_page( size_bytes );

  if (hiwatermark <= 0 || hiwatermark > 100)
    hiwatermark = DEFAULT_SC_HIWATERMARK;
  if (lowatermark <= 0 || lowatermark > 100)
    lowatermark = DEFAULT_SC_LOWATERMARK;


  /* Allocation */

  heap = allocate_sc_heap( *gen_no, heap_no );
  data = heap->data;

  *gen_no = *gen_no + 1;

  data->current_space =
    create_semispace( size_bytes, data->heap_no, data->gen_no );

  data->himark = hiwatermark;
  data->lomark = lowatermark;
  data->size_bytes = size_bytes;
  data->globals = globals;

  globals[ G_STKUFLOW ] = 0;
  mode_ss_to_globals( heap );

  /* Don't move the call to create_stack() into the assert(). */
  r = create_stack( heap );
  assert( r );

  return heap;
}


static int initialize( young_heap_t *heap );
static word *allocate( young_heap_t *heap, unsigned nbytes );
static void collect( young_heap_t *heap, unsigned nbytes );
static void assert_free_space( young_heap_t *heap, unsigned nbytes );
static void before_promotion( young_heap_t *heap );
static void after_promotion( young_heap_t *heap );
static unsigned free_space( young_heap_t *heap );
static void stats( young_heap_t *heap, heap_stats_t *stats );
static word *data_load_area( young_heap_t *heap, unsigned nbytes );
static word creg_get( young_heap_t *heap );
static void creg_set( young_heap_t *heap, word k );
static void stack_underflow( young_heap_t *heap );
static void stack_overflow( young_heap_t *heap );
static void set_policy( young_heap_t *heap, int rator, unsigned rand );


static young_heap_t *
allocate_sc_heap( int gen_no, int heap_no )
{
  young_heap_t *heap;
  sc_data_t *data;

  heap = (young_heap_t*)must_malloc( sizeof( young_heap_t ) );
  data = heap->data = (sc_data_t*)must_malloc( sizeof( sc_data_t ) );

  heap->id = "sc/variable";

  heap->initialize = initialize;
  heap->allocate = allocate;
  heap->collect = collect;
  heap->assert_free_space = assert_free_space;
  heap->before_promotion = before_promotion;
  heap->after_promotion = after_promotion;

  heap->free_space = free_space;
  heap->stats = stats;
  heap->set_policy = set_policy;

  heap->data_load_area = data_load_area;

  heap->creg_get = creg_get;
  heap->creg_set = creg_set;
  heap->stack_overflow = stack_overflow;
  heap->stack_underflow = stack_underflow;

  data->gen_no = gen_no;
  data->heap_no = heap_no;
  data->frames_flushed = 0;
  data->bytes_flushed = 0;
  data->stacks_created = 0;
  data->copied_last_gc = 0;

  return heap;
}


static int
initialize( young_heap_t *heap )
{
  return 1;
}


static void
set_policy( young_heap_t *heap, int rator, unsigned rand )
{
  /* Nothing yet */
}


static word *
allocate( young_heap_t *heap, unsigned nbytes )
{
  word p;
  word *globals = DATA(heap)->globals;

  nbytes = roundup8( nbytes );
  if (globals[ G_ETOP ] + nbytes > globals[ G_STKP ]) {
    unsigned bytes = nbytes + STACK_ROOM;
    if (!move_to_next_chunk( heap, bytes )) {
      supremely_annoyingmsg( "sc-heap: failed allocation of %u/%u bytes",
			     nbytes, bytes );
      heap->collector->collect( heap->collector, 0, GC_COLLECT, bytes );
    }
  }

  assert( globals[ G_ETOP ] + nbytes <= globals[ G_STKP ] );

  p = globals[ G_ETOP ];
  globals[ G_ETOP ] += nbytes;
  return (word*)p;
}


static void
collect( young_heap_t *heap, unsigned request_bytes )
{
  sc_data_t *data = DATA(heap);
  semispace_t *other_space;

  /* The heap is chunked, so this line moves to the next chunk
   * if there's one, rather than collecting.
   */
  if (move_to_next_chunk( heap, request_bytes+STACK_ROOM )) {
    stats_gc_type( 0, STATS_IGNORE );
    return;
  }

  debugmsg( "sc-heap: collecting; free space is %u", free_space( heap ) );
  mode_globals_to_ss( heap );
  ss_sync( data->current_space );
  debugmsg( "  used=%u, allocd=%u, stack=%u",
	    data->current_space->used,
	    data->current_space->allocated,
	    used_stack_space( heap ) );

  debugmsg( "[debug] stop+copy heap: collecting." );
  stats_gc_type( 0, STATS_COLLECT );

  flush_stack( heap );
  mode_globals_to_ss( heap );

  other_space =
    create_semispace( OLDSPACE_EXPAND_BYTES, data->heap_no, data->gen_no );

  gclib_stopcopy_slow( heap->collector, other_space );

  ss_free( data->current_space );
  data->current_space = other_space;

  mode_ss_to_globals( heap );
  ss_sync( data->current_space );
  data->copied_last_gc = data->current_space->used;

  post_gc_policy( heap, request_bytes );
  if (!new_stack( heap )) {  
    supremely_annoyingmsg( "Failed to create stack after gc." );
    expand_heap( heap, STACK_ROOM );
    if (!move_to_next_chunk( heap, STACK_ROOM ))
      panic_abort( "Double-faulting stack creation." );
  }

  debugmsg( "  post-gc memory free: %u", free_space( heap ) );
  debugmsg( "  semispace=%p, allocated=%u", data->current_space,
	    data->current_space->allocated );
  debugmsg( "[debug] young heap: finished collecting." );
}


/* We must have given amount of free space in a single chunk,
 * and if we can't do that, we must panic.
 *
 * Some strategies to consider:
 * - look for a bigger block down the chain, and use it
 * - allocate a new block and free any unused blocks to keep memory
 *   use under control (most likely, there will be no unused blocks)
 * - allocate a new block and shrink the current block to keep
 *   memory use under control
 */
static void 
assert_free_space( young_heap_t *heap, unsigned nbytes )
{
  /* FIXME: this is too simple, but works for now. */

  if (nbytes+STACK_ROOM > free_current_chunk( heap )) {
    supremely_annoyingmsg( "Failed free space assertion." );
    if (!move_to_next_chunk( heap, nbytes+STACK_ROOM )) {
      expand_heap( heap, nbytes+STACK_ROOM );
      if (!move_to_next_chunk( heap, nbytes+STACK_ROOM ))
	panic( "Cannot allocate %u bytes "
	      "(object is too large for heap?).", nbytes+STACK_ROOM );
    }
  }
}

static void
before_promotion( young_heap_t *heap ) 
{
  /* This will change when we can promote into the static area */
  panic( "sc-heap: before-promotion" );
}


static void
after_promotion( young_heap_t *heap ) 
{
  /* This will change when we can promote into the static area */
  panic( "sc-heap: after-promotion" );
}


static unsigned
free_space( young_heap_t *heap ) 
{
  sc_data_t *data = DATA(heap);
  semispace_t *s = data->current_space;

  mode_globals_to_ss( heap );  /* so that we can sync() */
  ss_sync( s );
  return max( 0, (long)data->size_bytes
	         -(long)s->used
	         -(long)used_stack_space(heap) );
}


static void
stats( young_heap_t *heap, heap_stats_t *stats ) 
{
  word *globals = DATA(heap)->globals;

  stats->live = used_space( heap );
  stats->copied_last_gc = DATA(heap)->copied_last_gc;
  stats->stack = globals[ G_STKBOT ] - globals[ G_STKP ];
  stats->frames_flushed = DATA(heap)->frames_flushed;
  stats->frames_restored = globals[G_STKUFLOW];
  stats->bytes_flushed = DATA(heap)->bytes_flushed;
  stats->semispace1 = DATA(heap)->current_space->allocated;
  stats->semispace2 = 0;
  stats->target = DATA(heap)->size_bytes;
  stats->stacks_created = DATA(heap)->stacks_created;
  DATA(heap)->frames_flushed = 0;
  DATA(heap)->bytes_flushed = 0;
  DATA(heap)->stacks_created = 0;
  DATA(heap)->copied_last_gc = 0;
  globals[G_STKUFLOW] = 0;
}


static word *
data_load_area( young_heap_t *heap, unsigned nbytes )
{
  sc_data_t *data = DATA(heap);

  if (data->globals[G_STKP]-data->globals[G_ETOP] >= nbytes) {
    word *p = (word*)(data->globals[G_ETOP]);
    data->globals[G_ETOP] += nbytes;
    return p;
  }
  else
    return 0;
}


static word 
creg_get( young_heap_t *heap ) 
{
  word k;

  flush_stack( heap );
  k = DATA(heap)->globals[ G_CONT ];
  if (!create_stack( heap )) {
    /* creates stack, restores frame */
    debugmsg( "sc-heap: failed create-stack in creg-get, gcing" );
    heap->collector->collect( heap->collector, 0, GC_COLLECT, STACK_ROOM );
  }
  else
    stack_underflow( heap );
  return k;
}


static void
creg_set( young_heap_t *heap, word k )
{
  clear_stack( heap );
  DATA(heap)->globals[ G_CONT ] = k;
  stack_underflow( heap );
}


static void
stack_underflow( young_heap_t *heap )
{
  if (!restore_frame( heap ))
    stack_overflow( heap );                        /* [sic] */
}


static void
stack_overflow( young_heap_t *heap )
{
  if (!move_to_next_chunk( heap, STACK_ROOM )) {
    supremely_annoyingmsg( "sc-heap: failed next-chunk in stack_overflow" );
    heap->collector->collect( heap->collector, 0, GC_COLLECT, STACK_ROOM );
  }
}


/* Internal */

/* This expands the heap if the stack cannot be created */
static int new_stack( young_heap_t *heap )
{
  sc_data_t *data = DATA(heap);

  if (!create_stack( heap )) {
    debugmsg( "[debug] young heap: Failed create-stack." );
    expand_heap( heap, STACK_ROOM );
    return 0;
  }

  if (!restore_frame( heap )) {
    debugmsg( "[debug] young heap: Failed restore-frame." );
    expand_heap( heap, STACK_ROOM );
    return 0;
  }
  return 1;
}

static int create_stack( young_heap_t *heap )
{
  while (!stk_create( DATA(heap)->globals ))
    if (!move_to_next_chunk( heap, STACK_ROOM )) return 0;
  DATA(heap)->stacks_created++;
  return 1;
}


static int restore_frame( young_heap_t *heap )
{
  return stk_restore_frame( DATA(heap)->globals );
}


static void clear_stack( young_heap_t *heap )
{
  stk_clear( DATA(heap)->globals );
}


static void flush_stack( young_heap_t *heap )
{
  unsigned frames, bytes;

  stk_flush( DATA(heap)->globals, &frames, &bytes );
  DATA(heap)->frames_flushed += frames;
  DATA(heap)->bytes_flushed += bytes;
}


static int
move_to_next_chunk( young_heap_t *heap, unsigned nbytes )
{
  sc_data_t *data = DATA(heap);
  semispace_t *s = data->current_space;
  unsigned free;

  free = max( 0, (long)free_space( heap ) - (long)free_current_chunk( heap ) );

  if (free > 0) {
    flush_stack( heap );
  again:
    mode_globals_to_ss( heap );
    ss_expand( s, max( OLDSPACE_EXPAND_BYTES, nbytes ) );
    mode_ss_to_globals( heap );
    if (!new_stack( heap ))
      goto again;
    assert( free_current_chunk( heap ) >= nbytes );
    return 1;
  }
  else
    return 0;
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

static unsigned free_current_chunk( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;
  return globals[ G_STKP ] - globals[ G_ETOP ];
}

static unsigned used_stack_space( young_heap_t *heap )
{
  semispace_t *s = DATA(heap)->current_space;
  word *globals = DATA(heap)->globals;
  return (word)s->chunks[s->current].lim - globals[ G_STKP ];
}

static unsigned used_space( young_heap_t *heap )
{
  sc_data_t *data = DATA(heap);
  semispace_t *s = data->current_space;

  mode_globals_to_ss( heap );  /* so that we can sync() */
  ss_sync( s );
  return (long)s->used + used_stack_space( heap );
}

static void
post_gc_policy( young_heap_t *heap, unsigned nbytes )
{
  /* FIXME: need to take nbytes into account */
  sc_data_t *data = DATA(heap);
  unsigned used;

  used = data->size_bytes-free_space( heap );
  if (used >= data->size_bytes/100*data->himark)
    expand_heap( heap, nbytes+STACK_ROOM );
  else if (used < data->size_bytes/100*data->lomark)
    contract_heap( heap, nbytes+STACK_ROOM );
}

/* Expand the heap so that after expansion, there is room for the desired
 * number of bytes.  Assumes there's not enough room in the current chunk.
 * There is a minimum expansion, however (that's policy).
 */
static void
expand_heap( young_heap_t *heap, unsigned bytes )
{
  unsigned incr, used, size, expand_min;

  used = used_space( heap )+free_current_chunk( heap );
  size = DATA(heap)->size_bytes;
  expand_min = size/2;
  if (used > size)
    incr = roundup_page( max( used-size+bytes, expand_min ) );
  else
    incr = roundup_page( max( bytes, expand_min ) );
  DATA(heap)->size_bytes += incr;
  annoyingmsg( "sc-heap: Expanding heap by %u bytes, new size %u.", 
	       incr, DATA(heap)->size_bytes );
}

/* FIXME: this is bogus */
static void
contract_heap( young_heap_t *heap, unsigned bytes )
{
  unsigned decr = roundup_page( bytes );
  return;  /* FIXME */
  DATA(heap)->size_bytes -= decr;
  annoyingmsg( "sc-heap: Contracting heap by %u bytes, new size %u.", 
	       decr, DATA(heap)->size_bytes );
}

/* eof */
