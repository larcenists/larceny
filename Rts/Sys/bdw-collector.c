/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Interface to the Boehm-Demers-Weiser conservative garbage collector.
 *
 * Allocation:
 *
 * Allocation happens by callout to millicode.  Therefore, the millicode
 * has been hacked to call GC_malloc() to allocate memory.
 *
 * The stack cache:
 *
 * The stack cache is visible to the collector (allocated in the heap),
 * and I try to avoid retention by clearing the unused portion before 
 * every collection.  Unlike the precise collectors, the stack cache 
 * is not flushed before the collection.
 *
 * The static area (data_load_area, text_load_area):
 *
 * These are allocated in the heap as a two large objects, with
 * private (but gc-visible) anchors that ensure that none of the
 * objects in the loaded heap image will be collected.  This is not
 * ideal, but it's not unreasonable (paging from a file would have
 * the same effect).  
 *
 * It is probably important to use a split heap (where the code vectors
 * are in a separate segment) so that the collector will not get confused
 * about instructions looking like data.
 *
 * The code in this file is not reentrant.
 * The code in this file does not depend on word size.
 * The code in this file does not depend on object header size.
 */

#define BDW_CLEAR_STACK 1
#define BDW_DEBUG       1
#define USE_HR_TIMER    1	/* Nanosecond timer; SunOS 5.6 (at least) */

#define GC_INTERNAL

#define STACK_CACHE_SIZE  (1024*64)

#include <string.h>
#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "memmgr.h"
#include "gclib.h"
#include "barrier.h"
#include "stats.h"
#include "stack.h"
#include "../bdw-gc/include/gc.h"

#if defined( NO_ATOMIC_ALLOCATION )
const char *larceny_gc_technology = "conservative/noatomic";
#  define GC_malloc_atomic GC_malloc
#  define GC_malloc_atomic_ignore_off_page GC_malloc_ignore_off_page
#else
const char *larceny_gc_technology = "conservative";
#endif

static struct {
  stats_id_t  gen_self;		/* Identity */
  stats_id_t  timer;		/* For timing GC */
  word        *globals;
  gc_stats_t  gc_stats;
  gen_stats_t gen_stats;
#if USE_HR_TIMER
  hrtime_t    slowpath_timeval;
  hrtime_t    slowpath_nsec;
#else
  struct timeval slowpath_timeval;
  long        slowpath_usec;
  long        slowpath_sec;
#endif
  bool        slowpath_cancel;	/* TRUE when GC is happening */
} bdw_state;

static void init_stack( gc_t *gc );
static void flush_stack( gc_t *gc );
static gc_t *allocate_area( word *globals );
static void do_stats_after_gc( void );

extern double GC_load_factor;
extern void GC_set_min_heap_size( int min_size_bytes );
extern int GC_allow_contraction;

void bdw_before_gc( void );
void bdw_after_gc( void );
void bdw_before_gc_slowpath( void );
void bdw_after_gc_slowpath( void );

static GC_PTR bdw_out_of_memory_handler( size_t bytes_requested );

gc_t *
create_bdw_gc( gc_param_t *params, int *generations )
{
  gc_t *gc;

  *generations = 1;

  gc = allocate_area( params->globals );
  GC_INIT();
  GC_register_displacement( 0 );
  GC_register_displacement( 1 );
  GC_register_displacement( 3 );
  GC_register_displacement( 5 );
  GC_register_displacement( 7 );
  GC_oom_fn = bdw_out_of_memory_handler;

  if (params->bdw_info.divisor > 0)
    GC_free_space_divisor = params->bdw_info.divisor;
  if (params->bdw_info.dynamic_max > 0) 
    GC_set_max_heap_size( params->bdw_info.dynamic_max );
  if (params->bdw_info.dynamic_min > 0)
    GC_set_min_heap_size( params->bdw_info.dynamic_min );
  if (params->bdw_info.load_factor > 0.0) {
    GC_load_factor = params->bdw_info.load_factor;
    GC_allow_contraction = 1;
  }
  if (params->bdw_info.expansion_factor > 0.0) {
    GC_load_factor = params->bdw_info.expansion_factor;
    GC_allow_contraction = 0;
  }
  if (params->use_incremental_bdw_collector)
    GC_enable_incremental();

  init_stack( gc );
  wb_disable_barrier( params->globals );

  return gc;
}

void gc_parameters( gc_t *gc, int op, int *ans )
{
  if (op == 0) {
    ans[0] = 2;			/* Conservative */
    ans[1] = 1;			/* One generation */
  }
  else {
    ans[0] = 5;			/* Conservative mark/sweep */
    ans[1] = 0;			/* Don't know */
    ans[2] = 1;			/* Expandable */
  }
}

/* Fast hooks to run before/after lazy sweep */

void bdw_before_gc_slowpath( void )
{
#if USE_HR_TIMER
  bdw_state.slowpath_timeval = gethrtime();
#else
  gettimeofday( &bdw_state.slowpath_timeval, 0 );
#endif
}

void bdw_after_gc_slowpath( void )
{
#if USE_HR_TIMER
  hrtime_t t;

  if (bdw_state.slowpath_cancel) { 
    bdw_state.slowpath_cancel=FALSE;
    return; 
  }
  t = gethrtime();
  bdw_state.slowpath_nsec += (t - bdw_state.slowpath_timeval);
#else
  struct timeval t;
  long s, us;

  if (bdw_state.slowpath_cancel) { 
    bdw_state.slowpath_cancel=FALSE; 
    return; 
  }
  gettimeofday( &t, 0 );
  s = 0;
  us = t.tv_usec - bdw_state.slowpath_timeval.tv_usec;
  if (us < 0) {
    s--;
    us += 1000000;
  }
  s += t.tv_sec - bdw_state.slowpath_timeval.tv_sec;
  bdw_state.slowpath_usec += us;
  bdw_state.slowpath_sec += s;
#endif
}

/* Slow hooks to run before/after actual GC */

void bdw_before_gc( void )
{
#if BDW_DEBUG
  word *p = 0, *lim = 0;
  int n, s;

  /* Don't increment the count because it's a nonmoving collector! */
  /* globals[ G_GC_CNT ] += fixnum(1); */

  /* Stack sanity check */
  if (bdw_state.globals[G_ETOP] != bdw_state.globals[G_EBOT])
    panic( "Foo!  In-line allocation has taken place!  Aborting." );

# if BDW_CLEAR_STACK
  /* Clear out the unused portion of the stack cache.  Ideally,
   * it would be better to tell the collector to just ignore that
   * region.
   */
  memset( (void*)bdw_state.globals[G_ETOP], 
 	  0,
	  bdw_state.globals[G_STKP]-bdw_state.globals[G_ETOP] );

  /* Clear all the dynamic links and pad words, since they're garbage. */
  p = (word*)bdw_state.globals[ G_STKP ];
  lim = (word*)bdw_state.globals[ G_STKBOT ];
  while (p < lim) {
    p[2] = 0;			        /* Clear the dynamic link */
    s = (p[0] >> 2) + 1;
    n = roundup2( s );
    if (n != s) p[s] = 0;	        /* Clear the pad word */
    p += n;
  }

# endif
#endif

#if USE_HR_TIMER
  bdw_state.gen_stats.ms_collection +=
    (long)(bdw_state.slowpath_nsec / 1000000000)  *1000 +
    (long)((bdw_state.slowpath_nsec / 1000000) % 1000);
  bdw_state.slowpath_nsec = 0;
#else
  bdw_state.gen_stats.ms_collection +=
    bdw_state.slowpath_sec + bdw_state.slowpath_usec / 1000000 +
    bdw_state.slowpath_usec / 1000;
  bdw_state.slowpath_sec = 0;
  bdw_state.slowpath_usec = 0;
#endif
  bdw_state.gen_stats.collections += 1;
  bdw_state.slowpath_cancel = TRUE;
  bdw_state.timer = stats_start_timer();
  bdw_state.gc_stats.allocated += bytes2words(GC_get_bytes_since_gc());
}

/* Hook run after gc (if gc has been set up to do it). */

void bdw_after_gc( void )
{
  bdw_state.gc_stats.reclaimed += 0; /* FIXME -- must hack in support. */
  bdw_state.gen_stats.ms_collection += stats_stop_timer( bdw_state.timer );
  do_stats_after_gc();
}

void gclib_stats( gclib_stats_t *stats )
{
  memset( stats, 0, sizeof( gclib_stats_t ) );
  stats->heap_allocated = bytes2words( GC_get_heap_size() );
  stats->heap_allocated_max = bytes2words( GC_get_max_heap_size() );
}

static void no_op_warn()
{
  hardconsolemsg( "bdw-heap: call to unsupported operation." );
}

static int initialize( gc_t *gc )
{
  return 1;
}

static void collect( gc_t *gc, int gen, int nbytes, gc_type_t type )
{
  GC_gcollect();
}

/* We can ignore no_gc because it's a non-moving collector */
static word *allocate( gc_t *gc, int nbytes, bool no_gc, bool atomic )
{
  void *p;

  /* This can be removed if the check remains on the slow path
     in bdw-gc/malloc.c. */
  if (nbytes > LARGEST_OBJECT) {
    panic( "\nSorry, an object of size %d bytes is too much for me; max is %d."
	   "\nSee you in 64-bit-land...\n",
	   nbytes, LARGEST_OBJECT );
  }
  if (atomic)
    p = GC_malloc_atomic( nbytes );
  else
    p = GC_malloc( nbytes );

  return (word*)p;
}

static word *allocate_nonmoving( gc_t *gc, int nbytes, bool atomic ) 
{
  return allocate( gc, nbytes, 0, atomic );
}

static word *data_load_area( gc_t *gc, int nbytes )
{
  /* The anchor allows the use of GC_malloc_ignore_off_page, which
   * is recommended by the authors (see bdw-gc/gc.h).
   */
  static void *anchor = 0;

  assert( anchor == 0 );
  anchor = GC_malloc_ignore_off_page( nbytes );

  return (word*)anchor;
}

static word *text_load_area( gc_t *gc, int nbytes )
{
  /* The anchor allows the use of GC_malloc_atomic_ignore_off_page, which
   * is recommended by the authors (see bdw-gc/gc.h).
   */
  static void *anchor = 0;

  assert( anchor == 0 );
  anchor = GC_malloc_atomic_ignore_off_page( nbytes );

  return (word*)anchor;
}

static int iflush( gc_t *gc, int generation )
{
  return (int)(bdw_state.globals[G_CACHE_FLUSH]);
}

static void stats( gc_t *gc )
{
  panic( "Stats no longer in use." );
}

/* The following functions are available:
   GC_word GC_no_gc                 -- # of collections
   size_t GC_get_heap_size()        -- bytes; heap memory (not overhead)
   size_t GC_get_bytes_since_gc()
   */
static void do_stats_after_gc( void )
{
  stack_stats_t stats_stack;
  gclib_stats_t stats_gclib;

  stats_add_gc_stats( &bdw_state.gc_stats );
  memset( &bdw_state.gc_stats, 0, sizeof( gc_stats_t ) );

  bdw_state.gen_stats.target = GC_get_heap_size();
  bdw_state.gen_stats.allocated = bdw_state.gen_stats.target;
  bdw_state.gen_stats.used = bdw_state.gen_stats.target;
  stats_add_gen_stats( bdw_state.gen_self, &bdw_state.gen_stats );
  memset( &bdw_state.gen_stats, 0, sizeof( gen_stats_t ) );

  memset( &stats_stack, 0, sizeof( stack_stats_t ) );
  stk_stats( bdw_state.globals, &stats_stack );
  stats_add_stack_stats( &stats_stack );

  memset( &stats_gclib, 0, sizeof( gclib_stats_t ) );
  gclib_stats( &stats_gclib );
  stats_add_gclib_stats( &stats_gclib );

  stats_dumpstate();		/* Dumps stats state if dumping is on */
}

static void stack_underflow( gc_t *gc )
{
  if (!stk_restore_frame( bdw_state.globals ))
    panic( "stack_underflow" );
}

static void stack_overflow( gc_t *gc )
{
  flush_stack( gc );
  stack_underflow( gc );
}

static word creg_get( gc_t *gc )
{
  word k;

  flush_stack( gc );
  k = bdw_state.globals[ G_CONT ];
  stack_underflow( gc );
  return k;
}

static void creg_set( gc_t *gc, word k )
{
  stk_clear( bdw_state.globals );
  bdw_state.globals[ G_CONT ] = k;
  stack_underflow( gc );
}

/* Internal */

static GC_PTR bdw_out_of_memory_handler( size_t bytes_requested )
{
  memfail( MF_HEAP, "Heap limit exceeded." );
  /* Not reached */
  return (GC_PTR)0;
}

static void init_stack( gc_t *gc )
{
  static void *anchor = 0;
  word *globals = bdw_state.globals;

  if (anchor == 0)
    anchor = GC_malloc_ignore_off_page( STACK_CACHE_SIZE );
  if (anchor == 0)
    panic( "init_stack" );

  globals[ G_EBOT ] = globals[ G_ETOP ] = (word)anchor;
  globals[ G_ELIM ] = (word)anchor + STACK_CACHE_SIZE - 8;
  globals[ G_STKP ] = globals[ G_ELIM ];
  globals[ G_STKUFLOW ] = 0;

  if (!stk_create( bdw_state.globals ))
    panic( "create_stack" );
}

static void flush_stack( gc_t *gc )
{
  word k1, k2, first, new, last;

  /* Must flush in-place first, then walk the chain of frames and
   * copy each individually into the heap.  Can't just allocate a
   * single chunk (or we'd retain a lot of garbage, potentially).
   */
  k1 = bdw_state.globals[ G_CONT ];                /* already in heap */
  stk_flush( bdw_state.globals );
  k2 = bdw_state.globals[ G_CONT ];                /* handle to first frame */

  if (k1 != k2) {
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
    bdw_state.globals[ G_CONT ] = first;
  }
  init_stack( gc );
}

static void clear_stack( gc_t *gc )
{
  stk_clear( bdw_state.globals );
}

static gc_t *allocate_area( word *globals )
{
  bdw_state.gen_self = stats_new_generation( 0, 0 );
  bdw_state.globals = globals;
  memset( &bdw_state.gen_stats, 0, sizeof( gen_stats_t ) );
  memset( &bdw_state.gc_stats, 0, sizeof( gc_stats_t ) );

  return create_gc_t("bdw/variable", 
		     0,
		     initialize,
		     allocate,
		     allocate_nonmoving,
		     collect,
		     0,		/* permute_remembered_sets */
		     no_op_warn,     	     /* set_policy */
		     data_load_area,
		     text_load_area,
		     iflush,
		     creg_get,
		     creg_set,
		     stack_overflow,
		     stack_underflow,
		     stats,
		     (int (*)())no_op_warn,  /* compact_all_ssbs */
		     no_op_warn,             /* compact_np_ssb */
		     no_op_warn,             /* np_remset_ptrs */
		     0,                      /* load_heap */
		     0,                      /* dump_heap */
		     (word *(*)())no_op_warn, /* make_handle */
		     no_op_warn,             /* free_handle */
		     no_op_warn,             /* enumerate_roots */
		     no_op_warn              /* enumerate_remsets_older_than */
		     );
}

/* Support for SRO. */

void gclib_memory_range( caddr_t *lowest, caddr_t *highest )
{
  /* These are present at least in v4.12; see bdw-gc/gc_priv.h */
  extern caddr_t GC_least_plausible_heap_addr;
  extern caddr_t GC_greatest_plausible_heap_addr;

  *lowest = GC_least_plausible_heap_addr;
  *highest = GC_greatest_plausible_heap_addr;
}

void *gclib_alloc_rts( int nbytes, unsigned attr )
{
  return GC_malloc_atomic_uncollectable( nbytes );
}

void gclib_free( void *p, int nbytes )
{
  GC_free( p );
}

/* eof */
