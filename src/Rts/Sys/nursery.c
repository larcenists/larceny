/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- a nursery for the generational gc.
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
#include "stats.h"

/* The code controlled by PROFILE_FOR_FAST_REMSET is here for an
   experiment having to do with Stefanovic's write barrier. 
   */
#define PROFILE_FOR_FAST_REMSET  0              /* Wizards only */
#define BLOCKMASK                (~0xffff)      /* 64 KB */

#if PROFILE_FOR_FAST_REMSET
static struct {
  word words_scanned;
  word ptrs_scanned;
  word ptr_same_block;
  word fast_check_succeeds;
} nursery;
#endif

typedef struct young_data young_data_t;

struct young_data {
  stats_id_t self;		/* Identity for stats module  */
  int        gen_no;		/* generation number for this heap */

  word       *heapbot;		/* address of first word of nursery */
  word       *heaplim;		/* address of first word after nursery */
  int        heapsize;		/* size of nursery in bytes */
  word       *globals;		/* the globals vector */

  bool       havestats;		/* Flag */
  int        los_live;		/* Before gc */
  int        stack_live;	/* Before gc */
  int        not_used;		/* Before gc */
  int        nbytes_wanted;	/* bytes wanted during last gc */
};

#define DATA( heap )  ((young_data_t*)((heap)->data))

static young_heap_t *allocate_nursery( int gen_no, gc_t *gc );
static void must_create_stack( young_heap_t *heap );
static void must_restore_frame( young_heap_t *heap );
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
  data->heaplim = data->heapbot + bytes2words(size_bytes);

  /* Setup heap pointers needed by RTS */
  globals[ G_EBOT ] = (word)(data->heapbot);
  globals[ G_ETOP ] = (word)(data->heapbot);
  globals[ G_ELIM ] = (word)(data->heaplim);

  /* Must be set up before stack can be initialized */
  globals[ G_STKP ] = globals[ G_ELIM ];
  globals[ G_STKUFLOW ] = 0;

  heap->maximum = DATA(heap)->heapsize;
  heap->allocated = 0;

  return heap;
}

static void create_initial_stack( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;
  
  globals[ G_STKUFLOW ] = 0;
  must_create_stack( heap );
}

static word *allocate( young_heap_t *heap, int nbytes, int no_gc )
{
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
  else {
#if 1
    p = los_allocate( heap->collector->los, nbytes, DATA(heap)->gen_no );
#else
    /* Enable this at some point */
    p = los_allocate_or_null( heap->collector->los, 
			      nbytes, 
			      DATA(heap)->gen_no );
    if (p == 0) {
      if (!no_gc)
	gc_collect( heap->collector, 1, 0, GCTYPE_CONDITION_RED );
      p = los_allocate( heap->collector->los, nbytes, DATA(heap)->gen_no );
    }
#endif
  }

  return p;
}

static void make_room( young_heap_t *heap )
{
  collect_if_no_room( heap, 4*KILOBYTE );
}

/* Request is ignored -- doesn't make sense in nursery. */
static void collect( young_heap_t *heap, int nbytes, int request )
{
  young_data_t *data = DATA(heap);

  supremely_annoyingmsg( "nursery: promoting (free=%d; request=%d)%s",
			 free_space( heap ), nbytes,
			 (nbytes == 0 ? " [stack overflow]" : "" ) );

#if PROFILE_FOR_FAST_REMSET
  { word *p;
    word *globals = data->globals;
    for ( p=(word*)globals[G_EBOT]; p < (word*)globals[G_ETOP] ; p++ ) {
      nursery.words_scanned++;
      if ((((word)p ^ *p) & BLOCKMASK) == 0)
	nursery.fast_check_succeeds++;
      if (isptr(*p)) {
	nursery.ptrs_scanned++;
	if ((((word)p ^ *p) & BLOCKMASK) == 0)
	  nursery.ptr_same_block++;
      }
    }
  }
#endif
  /* Why did Lars pass 0 instead of nbytes below? */
  gc_collect( heap->collector, data->gen_no, 0, GCTYPE_EVACUATE );
  data->nbytes_wanted = nbytes;  /* For use in after_collection() */
}

static void before_collection( young_heap_t *heap )
{
  young_data_t *data = DATA(heap);
  word *globals = data->globals;

  flush_stack( heap );
  heap->maximum = data->heapsize;
  heap->allocated = heap->maximum - free_space( heap );

  if (!data->havestats) {
    data->havestats = TRUE;
    data->los_live = 
      bytes2words(los_bytes_used( heap->collector->los, data->gen_no ));
    data->stack_live = bytes2words(globals[G_STKBOT] - globals[G_STKP]);
    data->not_used = bytes2words(globals[G_STKP] - globals[G_ETOP]);
  }
}

static void after_collection( young_heap_t *heap )
{
  young_data_t *data = DATA(heap);
  word *globals = data->globals;

  globals[ G_ETOP ] = globals[ G_EBOT ];
  globals[ G_STKP ] = globals[ G_ELIM ];

  must_create_stack( heap );
  must_restore_frame( heap );
  assert( data->nbytes_wanted > GC_LARGE_OBJECT_LIMIT || 
	  free_space( heap ) >= data->nbytes_wanted );
}

static int free_space( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;

  return (globals[ G_STKP ]-SCE_BUFFER) - globals[ G_ETOP ];
}

static void stats( young_heap_t *heap )
{
  young_data_t *data = DATA(heap);
  gen_stats_t gen_stats;
  gc_stats_t gc_stats;

  assert( data->havestats );

  memset( &gen_stats, 0, sizeof(gen_stats) );
  gen_stats.target = bytes2words(data->heapsize);
  gen_stats.allocated = gen_stats.target + data->los_live;
  gen_stats.used = gen_stats.allocated - data->not_used;
  stats_add_gen_stats( data->self, &gen_stats );

  memset( &gc_stats, 0, sizeof( gc_stats ) );
  gc_stats.allocated = gen_stats.used - data->stack_live;
  stats_add_gc_stats( &gc_stats );

  data->los_live = 0;
  data->stack_live = 0;
  data->not_used = 0;
  data->havestats = FALSE;
}

static word *data_load_area( young_heap_t *heap, int nbytes )
{
  young_data_t *data = DATA(heap);

  if ((data->globals[G_STKP]-SCE_BUFFER)-data->globals[G_ETOP] >= nbytes) {
    word *p = (word*)(data->globals[G_ETOP]);
    data->globals[G_ETOP] += nbytes;
    return p;
  }
  else
    return 0;
}

static void must_create_stack( young_heap_t *heap )
{
  if (!stk_create( DATA(heap)->globals ))
    panic_exit( "nursery: create_stack" );
}

static void must_restore_frame( young_heap_t *heap )
{
  if (!stk_restore_frame( DATA(heap)->globals ))
    panic_exit( "nursery: restore_frame");
}

static void flush_stack( young_heap_t *heap )
{
  stk_flush( DATA(heap)->globals );
}

/* Here we don't know the size of the frame that overflowed.  If the 
   frame is larger than the nursery then we will loop forever.  This can
   be fixed, but that large a frame (or small a nursery) is not realistic.
   */
static void stack_overflow( young_heap_t *heap )
{
  gc_collect( heap->collector, 0, 0, GCTYPE_EVACUATE );
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
    gc_collect( heap->collector, 0, room, GCTYPE_EVACUATE );
}

static bool is_address_mapped( young_heap_t *h, word *addr ) 
{
  return ((DATA(h)->heapbot <= addr) && (addr < DATA(h)->heaplim));
}

static young_heap_t *allocate_nursery( int gen_no, gc_t *gc )
{
  young_data_t *data;
  young_heap_t *heap;

  data = (young_data_t*)must_malloc( sizeof( young_data_t ) );
  heap = create_young_heap_t( "nursery",
			      HEAPCODE_YOUNG_1SPACE,
			      0,                     /* initialize */
			      create_initial_stack,
			      allocate,
			      make_room,
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
			      is_address_mapped,
			      data );
  heap->collector = gc;

  data->self = stats_new_generation( gen_no, 0 );
  data->gen_no = gen_no;
  data->los_live = 0;
  data->stack_live = 0;
  data->not_used = 0;
  data->havestats = FALSE;
  return heap;
}

/* eof */
