/* Rts/Sys/nursery.c
 * Larceny run-time system -- a nursery for the generational gc.
 *
 * $Id: nursery.c,v 1.1.1.1 1998/11/19 21:51:42 lth Exp $
 *
 * Contract
 *  - The stack cache lives in the heap; the stack pointer is the heap 
 *    limit and the heap pointer is the stack limit.
 *  - The stack cache is flushed into the heap before a gc.
 *
 * Strategy
 *   Objects should be copied as few times as possible during their
 *   lifetime.
 *
 * Policy
 *   Keep a fixed-sized nursery and evacuate all live objects from the
 *   nursery into the ephemeral generation every time the nursery fills up.
 *
 * Implementation
 *   The stack lives at the high end of the ephemeral area, growing down.
 *   The only procedures in here that know this are the ones which use
 *   the stack pointer to calculate free and used ephemeral space.
 *   However, if the stack lived outside of the heap and would have to 
 *   be copied into it on a gc, some changes would have to be made in 
 *   this file.
 *
 *   As a matter of implementation, the flushed stack frames are also 
 *   evacuated.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gc.h"
#include "memmgr.h"
#include "gclib.h"
#include "gc_t.h"
#include "los_t.h"
#include "stack.h"
#include "young_heap_t.h"
#include "heap_stats_t.h"

typedef struct young_data young_data_t;

struct young_data {
  int  gen_no;              /* generation number for this heap */

  word *heapbot;            /* address of first word of nursery */
  word *heaplim;            /* address of first word after nursery */
  int  heapsize;            /* size of nursery in bytes */
  word *globals;            /* the globals vector */

  unsigned frames_flushed;  /* stack frames flushed */
  unsigned bytes_flushed;   /* bytes of stack flushed or copied */
  unsigned stacks_created;  /* number of stacks created */
};

#define DATA( heap )  ((young_data_t*)((heap)->data))

static young_heap_t *allocate_nursery( int gen_no, gc_t *gc );
static int must_create_stack( young_heap_t *heap );
static int must_restore_frame( young_heap_t *heap );
static void flush_stack( young_heap_t *heap );
static int free_space( young_heap_t *heap );
static void collect_if_no_room( young_heap_t *heap, int room );

young_heap_t *
create_nursery( int gen_no,
	        gc_t *gc,	        /* the owning GC */
	        nursery_info_t *info,   /* creation parameters */
	        word *globals           /* the globals array (not in heap) */
	       )
{
  int size_bytes = info->size_bytes;
  young_data_t *data;
  young_heap_t *heap;

  heap = allocate_nursery( gen_no, gc );
  data = heap->data;

  assert( size_bytes >= GC_LARGE_OBJECT_LIMIT + MAX_STACK_FRAME );
  size_bytes = roundup_page( size_bytes );

  data->globals = globals;
  data->heapsize = size_bytes;

 again2:
  data->heapbot = (word*)gclib_alloc_heap( size_bytes, data->gen_no );
  if (data->heapbot == 0) {
    memfail( MF_HEAP, "young-heap: Could not allocate heap data area." );
    goto again2;
  }
  data->heaplim = data->heapbot + size_bytes/sizeof(word);

  /* Setup heap pointers needed by RTS */
  globals[ G_EBOT ] = (word)(data->heapbot);
  globals[ G_ETOP ] = (word)(data->heapbot);
  globals[ G_ELIM ] = (word)(data->heaplim);

  /* Must be set up before stack can be initialized */
  globals[ G_STKP ] = globals[ G_ELIM ];
  globals[ G_STKUFLOW ] = 0;

  must_create_stack( heap );

  heap->maximum = DATA(heap)->heapsize;
  heap->allocated = 0;

  return heap;
}

static word *allocate( young_heap_t *heap, int nbytes, int no_gc )
{
  young_data_t *data = DATA(heap);
  word *globals = DATA(heap)->globals;
  word *p;

  assert( nbytes > 0 );
  nbytes = roundup_balign( nbytes );

  if (!no_gc) collect_if_no_room( heap, nbytes );

  if (nbytes <= GC_LARGE_OBJECT_LIMIT) {
    assert( free_space( heap ) >= nbytes );
    p = (word*)globals[ G_ETOP ];
    globals[ G_ETOP ] += nbytes;
  }
  else
    p = los_allocate( heap->collector->los, nbytes, DATA(heap)->gen_no );

  return p;
}

static void collect( young_heap_t *heap, int nbytes )
{
  young_data_t *data = DATA(heap);

  supremely_annoyingmsg( "nursery: promoting (free=%d; request=%d)%s",
			 free_space( heap ), nbytes,
			 (nbytes == 0 ? " [stack overflow]" : "" ) );

  gc_collect( heap->collector, data->gen_no+1, 0 );
  assert( nbytes > GC_LARGE_OBJECT_LIMIT || free_space( heap ) >= nbytes );
}

static void before_collection( young_heap_t *heap )
{
  flush_stack( heap );
  heap->maximum = DATA(heap)->heapsize;
  heap->allocated = heap->maximum - free_space( heap );
}

static void after_collection( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;

  globals[ G_ETOP ] = globals[ G_EBOT ];
  globals[ G_STKP ] = globals[ G_ELIM ];

  must_create_stack( heap );
  must_restore_frame( heap );
}

static int free_space( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;

  return globals[ G_STKP ] - globals[ G_ETOP ];
}

static void stats( young_heap_t *heap, heap_stats_t *stats )
{
  young_data_t *data = DATA(heap);
  word *globals = data->globals;
  int los_live;

  los_live = los_bytes_used( heap->collector->los, data->gen_no );
  stats->stack = globals[ G_STKBOT ] - globals[ G_STKP ];
  stats->live = data->heapsize 
              - (globals[ G_STKBOT ] - globals[ G_ETOP ])
	      + los_live;
  stats->copied_last_gc = 0;
  stats->moved_last_gc = 0;
  stats->frames_flushed = data->frames_flushed;
  stats->frames_restored = globals[ G_STKUFLOW ];
  stats->bytes_flushed = data->bytes_flushed;
  stats->semispace1 = data->heapsize + los_live;
  stats->target = data->heapsize;
  stats->stacks_created = data->stacks_created;

  data->frames_flushed = 0;
  data->bytes_flushed = 0;
  data->stacks_created = 0;
  globals[ G_STKUFLOW ] = 0;
}

static word *data_load_area( young_heap_t *heap, int nbytes )
{
  young_data_t *data = DATA(heap);

  if (data->globals[G_STKP]-data->globals[G_ETOP] >= nbytes) {
    word *p = (word*)(data->globals[G_ETOP]);
    data->globals[G_ETOP] += nbytes;
    return p;
  }
  else
    return 0;
}

static int must_create_stack( young_heap_t *heap )
{
  stk_create( DATA(heap)->globals ) || panic( "nursery: create_stack" );
  DATA(heap)->stacks_created += 1;
}

static int must_restore_frame( young_heap_t *heap )
{
  stk_restore_frame( DATA(heap)->globals ) || panic( "nursery: restore_frame");
}

static void flush_stack( young_heap_t *heap )
{
  unsigned frames, bytes;

  stk_flush( DATA(heap)->globals, &frames, &bytes );
  DATA(heap)->frames_flushed += frames;
  DATA(heap)->bytes_flushed += bytes;
}

/* Here we don't know the size of the frame that overflowed.  If the 
   frame is larger than the nursery then we will loop forever.  This can
   be fixed, but that large a frame (or small a nursery) is not realistic.
   */
static void stack_overflow( young_heap_t *heap )
{
  collect( heap, 0 );
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
  assert( free_space( heap ) >= room );
  flush_stack( heap );
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

static void collect_if_no_room( young_heap_t *heap, int room )
{
  room = roundup_balign( room );
  if (free_space( heap ) < room)
    collect( heap, room );
}

static young_heap_t *allocate_nursery( int gen_no, gc_t *gc )
{
  young_data_t *data;
  young_heap_t *heap;

  data = (young_data_t*)must_malloc( sizeof( young_data_t ) );
  heap = create_young_heap_t( "nursery",
			     HEAPCODE_YOUNG_1SPACE,
			     0,	                      /* initialize */
			     allocate,
			     collect,
			     before_collection,
			     after_collection,
			     0,                       /* set_policy */
			     free_space,
			     stats,
			     data_load_area,
			     0,
			     0,
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

  return heap;
}

/* eof */
