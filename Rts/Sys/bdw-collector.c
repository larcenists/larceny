/* Rts/Sys/bdw-collector.c
 * Larceny run-time system -- support for Boehm-Demers-Weiser conservative gc.
 *
 * $Id: bdw-collector.c,v 1.2 1997/05/31 01:38:14 lth Exp lth $
 *
 *
 * Allocation.
 *
 * Allocation has to go through millicode, as per normal procedure.
 * Therefore, the millicode (Sparc/memory.s) has been hacked to call
 * GC_malloc() to allocate memory.
 *
 *
 * The stack cache.
 *
 * The stack cache must have the same appearance to compiled code as it
 * usually has.  This means setting up the pointers in the globals table
 * and so on.  However, since we have no control over when the collector
 * is called, we cannot count on flushing the stack before gc, so the
 * stack must be visible to the collector.  That means that there might
 * be garbage beyond the stack top that will cause retention!  I don't
 * yet know how to fix this, although it can be fixed by periodically
 * clearing the stack cache above the current point.  Clearing can be
 * triggered by overflow/underflow or other events.
 *
 *
 * The static area (data_load_area, text_load_area).
 *
 * These are allocated in the heap as a two large objects, with
 * private (but gc-visible) anchors that ensure that none of the
 * objects in the loaded heap image will be collected.  This is not
 * ideal, but it's not unreasonable (paging from a file would have
 * the same effect).  
 *
 * It is probably important to use a split heap (where the code vectors
 * are in a separate segment) so that the collector will not get confused
 * about instructions looking like data.  This has yet to be substantiated;
 * on repeated 50dynamic, the collector retains about the same amount
 * of storage with either split or unified heaps.  This is worrysome.
 *
 * The code in this file is reentrant.
 * The code in this file does not rely on word size.
 * The code in this file does not depend on header size.
 */

const char *gc_technology = "conservative";

#define GC_INTERNAL

#define STACK_CACHE_SIZE  (1024*65)

#include <string.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"
#include "../bdw-gc/include/gc.h"

typedef struct {
  word *globals;

  unsigned frames_flushed;
  unsigned stacks_created;
  unsigned bytes_flushed;
} bdw_data_t;

#define DATA( gc )   ((bdw_data_t*)gc->data)

static void no_op_warn();
static int initialize( gc_t *gc );
static void collect( gc_t *, int, gc_type_t, unsigned );
static word *allocate( gc_t *gc, unsigned nbytes );
static word *data_load_area( gc_t *gc, unsigned nbytes );
static word *text_load_area( gc_t *gc, unsigned nbytes );
static word creg_get( gc_t *gc );
static void creg_set( gc_t *gc, word k );
static void stack_underflow( gc_t *gc );
static void stack_overflow( gc_t *gc );
static int iflush( gc_t *gc, int generation );
static void stats( gc_t *gc, int generation, heap_stats_t *stats );
static void set_policy( gc_t *gc, int heap, int rator, unsigned rand );

static void init_stack( gc_t *gc );
static void flush_stack( gc_t *gc );


/* Usually you want this to be on, but you can turn it off to allow multiple
 * collectors in the same process.
 */
#ifdef BDW_DEBUG
static word *bdw_globals = 0;
#endif


gc_t *
create_bdw_gc( gc_param_t *params, int *generations )
{
  gc_t *the_gc;

  *generations = 1;

  the_gc = (gc_t*)must_malloc( sizeof( gc_t ) );
  the_gc->data = (bdw_data_t*)must_malloc( sizeof( bdw_data_t ) );

  DATA(the_gc)->globals = params->globals;
  DATA(the_gc)->frames_flushed = 0;
  DATA(the_gc)->bytes_flushed = 0;
  DATA(the_gc)->stacks_created = 0;

  the_gc->id = "bdw/variable";

  the_gc->initialize = initialize;
  the_gc->collect = collect;
  the_gc->allocate = allocate;
  the_gc->data_load_area = data_load_area;
  the_gc->text_load_area = text_load_area;
  the_gc->creg_get = creg_get;
  the_gc->creg_set = creg_set;
  the_gc->stack_underflow = stack_underflow;
  the_gc->stack_overflow = stack_overflow;
  the_gc->iflush = iflush;
  the_gc->stats = stats;
  the_gc->compact_all_ssbs = (int (*)())no_op_warn;
  the_gc->clear_remset = no_op_warn;
  the_gc->isremembered = (int (*)())no_op_warn;
  the_gc->compact_np_ssb = no_op_warn;
  the_gc->clear_np_remset = no_op_warn;
  the_gc->np_remset_ptrs = no_op_warn;
  the_gc->set_np_collection_flag = no_op_warn;
  the_gc->np_merge_and_clear_remset = no_op_warn;
  the_gc->promote_out_of = no_op_warn;
  the_gc->enumerate_roots = no_op_warn;
  the_gc->enumerate_remsets_older_than = no_op_warn;
  the_gc->set_policy = no_op_warn;

  GC_INIT();
  if (params->bdw_incremental)
    GC_enable_incremental();
  init_stack( the_gc );
  wb_disable();

  return the_gc;
}

/* Hook run before gc (if gc has been set up to do it). */

void bdw_before_gc( void )
{
#if BDW_DEBUG
  if (!bdw_globals) return;

  /* Stack sanity check */
  if (bdw_globals[G_ETOP] != bdw_globals[G_EBOT])
    panic( "Foo!  In-line allocation has taken place!  Aborting." );

# if BDW_CLEAR_STACK
  /* UNTESTED CODE!  Test after Quals.  FIXME */
  /* Clear out the unused portion of the stack cache.  Ideally,
   * it would be better to tell the collector to just ignore that
   * region.  FIXME.
   */
  memset( (word*)bdw_globals[G_ETOP], 0,
	  bdw_globals[G_STKP]-bdw_globals[G_ETOP] );
# endif
#endif
  stats_before_gc();
  stats_gc_type( 0, 0 );
}

/* Hook run after gc (if gc has been set up to do it). */

void bdw_after_gc( void )
{
#if BDW_DEBUG
  if (!bdw_globals) return;
#endif
  stats_after_gc();
}

static void no_op_warn()
{
  hardconsolemsg( "bdw-heap: call to unsupported operation." );
}

static int initialize( gc_t *gc )
{
#if BDW_DEBUG
  bdw_globals = DATA(gc)->globals;
#endif
  return 1;
}

static void collect( gc_t *gc, int gen, gc_type_t type, unsigned nbytes )
{
  GC_gcollect();
}

static word *allocate( gc_t *gc, unsigned nbytes )
{
  void *p = GC_malloc( nbytes );
  if (p == 0) panic( "GC_malloc failed for %u bytes.", nbytes );
  return (word*)p;
}

static word *data_load_area( gc_t *gc, unsigned nbytes )
{
  /* The anchor allows the use of GC_malloc_ignore_off_page, which
   * is recommended by the authors (see bdw-gc/gc.h).
   */
  static void *anchor;

  anchor = GC_malloc_ignore_off_page( nbytes );
  if (anchor == 0) 
    panic( "GC_malloc failed for %u bytes.", nbytes );
  return (word*)anchor;
}

static word *text_load_area( gc_t *gc, unsigned nbytes )
{
  /* The anchor allows the use of GC_malloc_atomic_ignore_off_page, which
   * is recommended by the authors (see bdw-gc/gc.h).
   */
  static void *anchor;

  anchor = GC_malloc_atomic_ignore_off_page( nbytes );
  if (anchor == 0)
    panic( "GC_malloc_atomic failed for %u bytes.", nbytes );
  return (word*)anchor;
}

static int iflush( gc_t *gc, int generation )
{
  return (int)(DATA(gc)->globals[G_CACHE_FLUSH]);
}

static void stats( gc_t *gc, int generation, heap_stats_t *stats )
{
  /* The following functions are available:
     GC_word GC_no_gc                 -- # of collections
     size_t GC_get_heap_size()        -- bytes; heap memory (not overhead)
     size_t GC_get_bytes_since_gc()
   */
  stats->live = stats->semispace1 = GC_get_heap_size();
}

static word creg_get( gc_t *gc )
{
  word k;

  flush_stack( gc );
  k = DATA(gc)->globals[ G_CONT ];
  stack_underflow( gc );
  return k;
}

static void creg_set( gc_t *gc, word k )
{
  stk_clear( DATA(gc)->globals );
  DATA(gc)->globals[ G_CONT ] = k;
  stack_underflow( gc );
}

static void stack_underflow( gc_t *gc )
{
  if (!stk_restore_frame( DATA(gc)->globals ))
    panic( "stack_underflow" );
}

static void stack_overflow( gc_t *gc )
{
  flush_stack( gc );
  stack_underflow( gc );
}


/* Internal */

static void init_stack( gc_t *gc )
{
  static void *anchor = 0;
  word *globals = DATA(gc)->globals;

  if (anchor == 0)
    anchor = GC_malloc_ignore_off_page( STACK_CACHE_SIZE );
  if (anchor == 0)
    panic( "init_stack" );

  globals[ G_EBOT ] = globals[ G_ETOP ] = (word)anchor;
  globals[ G_ELIM ] = (word)anchor + STACK_CACHE_SIZE;
  globals[ G_STKP ] = globals[ G_ELIM ];
  globals[ G_STKUFLOW ] = 0;

  if (!stk_create( DATA(gc)->globals ))
    panic( "create_stack" );
  DATA(gc)->stacks_created++;
}

static void flush_stack( gc_t *gc )
{
  bdw_data_t *data = DATA(gc);
  unsigned frames, bytes;
  word k1, k2, first, new, last;

  /* Must flush in-place first, then walk the chain of frames and
   * copy each individually into the heap.  Can't just allocate a
   * single chunk (or we'd retain a lot of garbage, potentially).
   */
  k1 = data->globals[ G_CONT ];                /* already in heap */
  stk_flush( data->globals, &frames, &bytes );
  k2 = data->globals[ G_CONT ];                /* handle to first frame */

  first = last = 0;
  while (k2 != k1) {
    new = copy_object( gc, k2 );
    if (last != 0) {
      *(ptrof(last)+HC_DYNLINK) = new;
      last = new;
    }
    else
      first = last = new;
    k2 = *(ptrof(k2)+HC_DYNLINK);
  }
  *(ptrof(last)+HC_DYNLINK) = k1;
  data->globals[ G_CONT ] = first;

  data->frames_flushed += frames;
  data->bytes_flushed += bytes;

  init_stack( gc );
}

static void clear_stack( gc_t *gc )
{
  stk_clear( DATA(gc)->globals );
}

/* eof */
