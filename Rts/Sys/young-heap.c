/* Rts/Sys/young-heap.c
 * Larceny run-time system -- youngest heap.
 *
 * $Id: young-heap.c,v 1.9 1997/02/11 19:48:21 lth Exp $
 *
 * Contract
 *
 *  - The stack cache lives in the heap; the stack pointer is the heap 
 *    limit and the heap pointer is the stack limit.
 *  - The stack cache is flushed into the heap before a gc.
 *
 * Policy for promoting and collection:
 *
 *   If allocation fails, collect.  If allocation still fails, then promote
 *   everything.  If, after gc allocation succeeds, but the heap is over
 *   the watermark, then schedule so that the next gc will instead promote
 *   everything.
 *
 * Implementation notes
 *
 *   The stack lives at the high end of the ephemeral area, growing down.
 *   The only procedures in here that know this are the ones which use
 *   the stack pointer to calculate free and used ephemeral space.
 *   However, if the stack lived outside of the heap and would have to 
 *   be copied into it on a gc, some changes would have to be made in 
 *   this file.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"


/* The "data" member of the heap structure points to a structure of 
 * this type.
 */
typedef struct {
  word     *espace1_bot; /* address of first word of espace 1 */
  word     *espace1_lim; /* address of first word after espace 1 */
  word     *espace2_bot; /* address of first word of espace 2 */
  word     *espace2_lim; /* address of first word after espace 2 */
  unsigned watermark;    /* ephemeral watermark, in bytes */
  unsigned heapsize;     /* size of semispace in bytes */
  int      gen_no;       /* generation number for this heap (should be 0) */
  int      heap_no;      /* heap number for this heap (should be 0) */
  word     *globals;     /* the globals vector */
  int      must_promote; /* flag: 1 means promote next time */
  int      just_promoted;/* flag: 1 means last cycle promoted */
  unsigned frames_flushed;  /* stack frames flushed */
  unsigned bytes_flushed;   /* bytes of stack flushed or copied */
  unsigned stacks_created;  /* number of stacks created */
} young_data_t;

#define DATA( heap )  ((young_data_t*)((heap)->data))


/* Interface functions */
static int initialize( young_heap_t *heap );
static void collect( young_heap_t *heap );
static word *allocate( young_heap_t *heap, unsigned n );
static void before_promotion( young_heap_t *heap );
static void after_promotion( young_heap_t *heap );
static void stats( young_heap_t *heap, heap_stats_t * );
static unsigned free_space( young_heap_t * );
static word *data_load_area( young_heap_t *, unsigned );
static int create_stack( young_heap_t *heap );
static void clear_stack( young_heap_t *heap );
static void flush_stack( young_heap_t *heap );
static int restore_frame( young_heap_t *heap );
static word creg_get( young_heap_t *heap );
static void creg_set( young_heap_t *heap, word cont );
static void do_restore_frame( young_heap_t *heap );


/* Private */
static void flip( young_heap_t * );


young_heap_t *
create_young_heap( int *gen_no,
		   int heap_no,
		   unsigned size_bytes,  /* size of espace; 0 = default */
		   unsigned ewatermark,  /* promotion limit %, 0=default */
		   word *globals         /* the globals array (not in heap) */
		  )
{
  young_data_t *data;
  young_heap_t *heap;
  word *heapptr;

 again:
  heap = (young_heap_t*)malloc( sizeof( young_heap_t ) );
  data = (young_data_t*)malloc( sizeof( young_data_t ) );

  if (heap == 0 || data == 0) {
    if (!heap) free( heap );
    if (!data) free( data );
    memfail( MF_MALLOC, "Could not allocate heap metadata." );
    goto again;
  }

  heap->id = "sc/fixed";
  heap->initialize = initialize;
#if 0
  heap->create_stack = create_stack;
  heap->clear_stack = clear_stack;
  heap->flush_stack = flush_stack;
  heap->restore_frame = restore_frame;
#endif
  heap->creg_get = creg_get;
  heap->creg_set = creg_set;
  heap->stack_underflow = do_restore_frame;

  heap->collect = collect;
  heap->before_promotion = before_promotion;
  heap->after_promotion = after_promotion;
  heap->allocate = allocate;
  heap->stats = stats;
  heap->free_space = free_space;
  heap->data_load_area = data_load_area;

  heap->data = data;

  data->gen_no = *gen_no;
  data->heap_no = heap_no;

  *gen_no = *gen_no + 1;

  if (size_bytes == 0) size_bytes = DEFAULT_ESIZE;
  size_bytes = roundup_page( size_bytes );

  if (ewatermark <= 0 || ewatermark > 100)
    ewatermark = DEFAULT_EWATERMARK;

 again2:
  heapptr = 
    (word*)gclib_alloc_heap( size_bytes*2, data->heap_no, data->gen_no );
  if (heapptr == 0) {
    memfail( MF_HEAP, "young-heap: Could not allocate heap data area." );
    goto again2;
  }

  data->espace1_bot = heapptr;
  heapptr += size_bytes / sizeof( word );
  data->espace1_lim = heapptr;

  data->espace2_bot = heapptr;
  heapptr += size_bytes / sizeof( word );
  data->espace2_lim = heapptr;

  data->watermark = (size_bytes/100)*50;
  data->globals = globals;
  data->heapsize = size_bytes;
  data->must_promote = 0;
  data->just_promoted = 0;

  /* Setup heap pointers needed by RTS */
  globals[ G_EBOT ] = (word)(data->espace1_bot);
  globals[ G_ETOP ] = (word)(data->espace1_bot);
  globals[ G_ELIM ] = (word)(data->espace1_lim);

  /* Must be set up before stack can be initialized */
  globals[ G_STKP ] = globals[ G_ELIM ];

  /* Now, don't move the call to create_stack() into the assert()... */
  { int r = create_stack( heap );
    assert( r );
  }

  return heap;
}


/* Does nothing interesting -- everything was done during creation */
static int 
initialize( young_heap_t *heap )
{
  return 1;
}


static word *
allocate( young_heap_t *heap, unsigned nbytes )
{
  word p;
  word *globals = DATA(heap)->globals;

  nbytes = roundup8( nbytes );
  if (globals[ G_ETOP ] + nbytes > globals[ G_STKP ])
    heap->collector->collect( heap->collector, 0, GC_COLLECT, nbytes );

  /* Assumption: no gc hooks are run before collect() returns. */

  p = globals[ G_ETOP ];
  globals[ G_ETOP ] += nbytes;
  return (word*)p;
}


static void
collect( young_heap_t *heap )
{
  young_data_t *data = DATA(heap);
  word *globals = data->globals;
  word *oldlo, *oldhi;

  flush_stack( heap );

  if (data->must_promote) goto promote;

  debugmsg( "[debug] young heap: collecting." );
  stats_gc_type( 0, STATS_COLLECT );

  oldlo = (word*)globals[ G_EBOT ];
  oldhi = (word*)globals[ G_ELIM ];

  flip( heap );
  globals[ G_ETOP ] =
    (word)gclib_stopcopy_fast( heap->collector, oldlo, oldhi,
			       (word*)globals[ G_ETOP ] );
  globals[ G_STKP ] = globals[ G_ELIM ];

  data->must_promote = (free_space(heap) < data->watermark);
  data->just_promoted = 0;

  if (!create_stack( heap )) {
    debugmsg( "[debug] young heap: Failed create-stack." );
    goto promote;
  }
  if (!restore_frame( heap )) {
    debugmsg( "[debug] young heap: Failed restore-frame." );
    goto promote;
  }

  debugmsg( "[debug] young heap: finished collecting." );
  return;

 promote:
  debugmsg( "[debug] young heap: promoting." );
  /* the old generation gets to call stats_gc_type(); */
  data->must_promote = 0;
  heap->collector->promote_out_of( heap->collector, data->gen_no );
}


static void
flip( young_heap_t *heap )
{
  young_data_t *data = DATA(heap);
  word *globals = DATA(heap)->globals;

  if (globals[ G_EBOT ] == (word)(data->espace1_bot)) {
    globals[ G_EBOT ] = globals[ G_ETOP ] = (word)(data->espace2_bot);
    globals[ G_ELIM ] = (word)(data->espace2_lim);
  }
  else {
    globals[ G_EBOT ] = globals[ G_ETOP ] = (word)(data->espace1_bot);
    globals[ G_ELIM ] = (word)(data->espace1_lim);
  }
}


static void
before_promotion( young_heap_t *heap )
{
  debugmsg( "[debug] young_heap: before_promotion." );
  flush_stack( heap );
}


static void
after_promotion( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;

  debugmsg( "[debug] young_heap: after_promotion." );
  globals[ G_ETOP ] = globals[ G_EBOT ];
  globals[ G_STKP ] = globals[ G_ELIM ];

  if (!create_stack( heap ))
    panic( "young heap: after_promotion: failed to create stack." );

  if (!restore_frame( heap ))
    panic( "young heap: after_promotion: failed to restore frame." );

  DATA(heap)->must_promote = 0;
  DATA(heap)->just_promoted = 1;
}


static unsigned
free_space( young_heap_t *heap )
{
  word *globals = DATA(heap)->globals;

  return globals[ G_STKP ] - globals[ G_ETOP ];
}


static void
stats( young_heap_t *heap, heap_stats_t *stats )
{
  word *globals = DATA(heap)->globals;

  stats->live = globals[ G_ETOP ] - globals[ G_EBOT ];
  stats->stack = globals[ G_STKBOT ] - globals[ G_STKP ];
  stats->frames_flushed = DATA(heap)->frames_flushed;
  stats->frames_restored = globals[G_STKUFLOW];
  stats->bytes_flushed = DATA(heap)->bytes_flushed;
  stats->semispace1 = stats->semispace2 = DATA(heap)->heapsize;
  stats->stacks_created = DATA(heap)->stacks_created;
  DATA(heap)->frames_flushed = 0;
  DATA(heap)->bytes_flushed = 0;
  DATA(heap)->stacks_created = 0;
  globals[G_STKUFLOW] = 0;
}


static word *
data_load_area( young_heap_t *heap, unsigned nbytes )
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

static int create_stack( young_heap_t *heap )
{
  DATA(heap)->stacks_created++;
  return stk_create( DATA(heap)->globals );
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

static int restore_frame( young_heap_t *heap )
{
  return stk_restore_frame( DATA(heap)->globals );
}

static word creg_get( young_heap_t *heap )
{
  word k;

  flush_stack( heap );
  k = DATA(heap)->globals[ G_CONT ];
  if (!create_stack( heap )) {
    /* creates stack, restores frame */
    heap->collector->collect( heap->collector, 0, GC_PROMOTE, 16 );
  }
  else
    do_restore_frame( heap );
  return k;
}

static void creg_set( young_heap_t *heap, word k )
{
  clear_stack( heap );
  DATA(heap)->globals[ G_CONT ] = k;
  do_restore_frame( heap );
}

static void do_restore_frame( young_heap_t *heap )
{
  if (!restore_frame( heap ))
    heap->collector->collect( heap->collector, 0, GC_PROMOTE, 16 );
}


/* eof */
