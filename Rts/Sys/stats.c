/* Copyright 1998, 1999 Lars T Hansen
 *
 * $Id$
 *
 * In the data structures in this module, all "word" entries are tagged
 * Scheme data; most are nonnegative integers represented as fixnums.
 */

#include <stdio.h>
#include "larceny.h"
#include "stats.h"
#include "gc.h"

#define MAX_TIMERS     20	      /* Should be plenty */
#define LARGEST_FIXNUM (2147483644L)  /* ((2^29)-1)*4 */

typedef struct gc_memstat gc_memstat_t;
typedef struct gclib_memstat gclib_memstat_t;
typedef struct stack_memstat stack_memstat_t;
typedef struct gen_memstat gen_memstat_t;
typedef struct remset_memstat remset_memstat_t;
#if defined(SIMULATE_NEW_BARRIER)
typedef struct swb_remstat swb_remstat_t;
#endif

struct gclib_memstat {
  word heap_allocated;		/* words allocated to heap areas */
  word heap_allocated_max;	/* max words allocated to heap */
  word remset_allocated;	/* words allocated to remembered sets */
  word remset_allocated_max;	/* max words allocated to remset */
  word rts_allocated;		/* words allocated to RTS "other" */
  word rts_allocated_max;	/* max words allocated to rts */
  word heap_fragmentation;	/* words of external heap framgentation */
  word heap_fragmentation_max;	/* max words of external heap fragmentation */
};

struct gc_memstat {
  word allocated_hi;		/* total words */
  word allocated_lo;		/*  allocated */
  word reclaimed_hi;		/* total words */
  word reclaimed_lo;		/*  reclaimed */
  word objects_copied_hi;	/* by copying collection */
  word objects_copied_lo;
  word words_copied_hi;		/* ditto */
  word words_copied_lo;
  word objects_moved_hi;	/* ditto */
  word objects_moved_lo;
  word words_moved_hi;		/* ditto */
  word words_moved_lo;

  /* NP (ROF) collector, when applicable */
  word np_k;			/* Number of steps (old+young together) */
  word np_j;			/* Number of steps in young */

  /* DOF collector */
  word resets;			/* Number of resets */
  word repeats;			/* Number of repeats */

  /* Full mark/sweep collections */
  word full_collections;
  word full_ms_collection;
  word full_objects_marked_hi;
  word full_objects_marked_lo;
  word full_words_marked_hi;
  word full_words_marked_lo;
  word full_pointers_traced_hi;
  word full_pointers_traced_lo;
};

struct stack_memstat {
  word stacks_created;		/* number of stacks created */
  word words_flushed_hi;	/* words of stack */
  word words_flushed_lo;	/*   frames flushed or copied */
  word frames_flushed_hi;	/* number of stack */
  word frames_flushed_lo;	/*   frames flushed */
  word frames_restored_hi;	/* number of stack */
  word frames_restored_lo;	/*   frames restored */
};

struct gen_memstat {
  word major_id;		/* Identity of */
  word minor_id;		/*   this generation */

  word target;			/* current allocation target (upper bound) */
  word allocated;		/* current allocation */
  word used;			/* current usage */

  word promotions;		/* Promotions into this heap */
  word collections;		/* Collections in this heap */
  word ms_promotion;		/* Total promotion time in ms */
  word ms_collection;		/* Total gc time in ms (including promotion) */
};

struct remset_memstat {
  word major_id;
  word minor_id;

  /* Snapshot */
  word allocated;		/* Words allocated to remset */
  word max_allocated;		/* Max words allocated to remset */
  word used;			/* Words used in node pool */
  word live;			/* Words live in node pool */

  /* For accumulation */
  word ssb_recorded_hi;		/* SSB transactions */
  word ssb_recorded_lo;		/*   recorded */
  word scanned_hi;		/* remset table */
  word scanned_lo;		/*   entries scanned */
  word recorded_hi;		/* remset table */
  word recorded_lo;		/*   entries recorded */
  word removed_hi;		/* remset table */
  word removed_lo;		/*   entries removed */
  word words_scanned_hi;	/* Words of old */
  word words_scanned_lo;	/*   objects scanned */
  word cleared;			/* Number of times remset was cleared */
  word scanned;			/* Number of times remset was scanned */
  word compacted;		/* Number of times SSB was compacted */
};

#if defined(SIMULATE_NEW_BARRIER)
/* FIXME -- extend some of these to doublewords to avoid overflow */
struct swb_memstat {
  word total_assignments;
  word array_assignments;
  word lhs_young_or_remembered;
  word rhs_constant;
  word cross_gen_check;
  word transactions;
};
#endif

static struct {
  /* Statistics */
  gc_memstat_t     gc_stats;
  gclib_memstat_t  gclib_stats;
  gen_memstat_t    gen_stats[ MAX_GENERATIONS ];
  remset_memstat_t remset_stats[ MAX_GENERATIONS ];
  stack_memstat_t  stack_stats;
#if defined(SIMULATE_NEW_BARRIER)
  swb_remstat_t    swb_stats;
#endif
  
  /* Other state variables */
  gc_t *gc;
  int  generations;		/* Number of generation entries */
  int  remsets;			/* Number of remset entries */
  bool initialized;
  int  timers[ MAX_TIMERS ];
  FILE *dump_file;
} stats_state;

static void add( word *hi, word *lo, int x );

void stats_init( gc_t *gc )
{
  stats_state.gc = gc;
  stats_state.dump_file = 0;
  stats_state.initialized = TRUE;
}

stats_id_t stats_new_generation( int major_id, int minor_id )
{
  int i;

  assert( stats_state.generations < MAX_GENERATIONS );

  i = stats_state.generations++;
  stats_state.gen_stats[i].major_id = fixnum(major_id);
  stats_state.gen_stats[i].minor_id = fixnum(minor_id);
  return i;
}

stats_id_t stats_new_remembered_set( int major_id, int minor_id )
{
  int i;

  assert( stats_state.remsets < MAX_GENERATIONS );

  i = stats_state.remsets++;
  stats_state.remset_stats[i].major_id = fixnum(major_id);
  stats_state.remset_stats[i].minor_id = fixnum(minor_id);
  return i;
}

stats_id_t stats_start_timer( void )
{
  int i;

  for ( i=0 ; i < MAX_TIMERS && stats_state.timers[i] > 0 ; i++ )
    ;
  assert(i < MAX_TIMERS);

  stats_state.timers[i] = stats_rtclock();
  return i;
}

int stats_stop_timer( stats_id_t timer )
{
  int then;

  assert( 0 <= timer && timer < MAX_TIMERS && stats_state.timers[timer] != 0 );

  then = stats_state.timers[timer];
  stats_state.timers[timer] = 0;
  return stats_rtclock() - then;
}

void stats_add_gclib_stats( gclib_stats_t *stats )
{
  gclib_memstat_t *s = &stats_state.gclib_stats;

  s->heap_allocated = fixnum( stats->heap_allocated );
  s->heap_allocated_max = fixnum( stats->heap_allocated_max );
  s->remset_allocated = fixnum( stats->remset_allocated );
  s->remset_allocated_max = fixnum( stats->remset_allocated_max );
  s->rts_allocated = fixnum( stats->rts_allocated );
  s->rts_allocated_max = fixnum( stats->rts_allocated );
  s->heap_fragmentation = fixnum( stats->heap_fragmentation );
  s->heap_fragmentation_max = fixnum( stats->heap_fragmentation_max );
}

void stats_add_gc_stats( gc_stats_t *stats )
{
  gc_memstat_t *s = &stats_state.gc_stats;

  add( &s->allocated_hi, &s->allocated_lo, stats->allocated );
  add( &s->reclaimed_hi, &s->reclaimed_lo, stats->reclaimed );
  add( &s->objects_copied_hi, &s->objects_copied_lo, stats->objects_copied );
  add( &s->words_copied_hi, &s->words_copied_lo, stats->words_copied );
  add( &s->objects_moved_hi, &s->objects_moved_lo, stats->objects_moved );
  add( &s->words_moved_hi, &s->words_moved_lo, stats->words_moved );

  /* NP (ROF) collector */
  s->np_k = fixnum( stats->np_k );
  s->np_j = fixnum( stats->np_j );

  /* DOF collector */
  s->resets += fixnum( stats->resets );
  s->repeats += fixnum( stats->repeats );

  /* Full mark/sweep backup collector */
  s->full_collections += fixnum( stats->full_collections );
  s->full_ms_collection += fixnum( stats->full_ms_collection );
  add( &s->full_objects_marked_hi, &s->full_objects_marked_lo, 
       stats->full_objects_marked );
  add( &s->full_words_marked_hi, &s->full_words_marked_lo, 
       stats->full_words_marked );
  add( &s->full_pointers_traced_hi, &s->full_pointers_traced_lo,
       stats->full_pointers_traced );
}

void stats_add_stack_stats( stack_stats_t *stats )
{
  stack_memstat_t *s = &stats_state.stack_stats;

  s->stacks_created += fixnum( stats->stacks_created );
  add( &s->words_flushed_hi, &s->words_flushed_lo, stats->words_flushed );
  add( &s->frames_flushed_hi, &s->frames_flushed_lo, stats->frames_flushed );
  add( &s->frames_restored_hi, &s->frames_restored_lo, stats->frames_restored);
}

void stats_add_gen_stats( stats_id_t generation, gen_stats_t *stats )
{
  gen_memstat_t *s;

  assert( 0 <= generation && generation < stats_state.generations );

  s = &stats_state.gen_stats[ generation ];

  s->target = fixnum( stats->target );
  s->allocated = fixnum( stats->allocated );
  s->used = fixnum( stats->used );

  s->promotions += fixnum( stats->promotions );
  s->collections += fixnum( stats->collections );
  s->ms_promotion += fixnum( stats->ms_promotion );
  s->ms_collection += fixnum( stats->ms_collection );
}

void stats_add_remset_stats( stats_id_t remset, remset_stats_t *stats )
{
  remset_memstat_t *s;

  assert( 0 <= remset && remset < stats_state.remsets );

  s = &stats_state.remset_stats[ remset ];

  s->allocated = fixnum( stats->allocated );
  s->max_allocated = max( s->max_allocated, fixnum(stats->allocated) );
  s->used = fixnum( stats->used );
  s->live = fixnum( stats->live );

  add( &s->ssb_recorded_hi, &s->ssb_recorded_lo, stats->ssb_recorded );
  add( &s->scanned_hi, &s->scanned_lo, stats->objs_scanned );
  add( &s->recorded_hi, &s->recorded_lo, stats->recorded );
  add( &s->words_scanned_hi, &s->words_scanned_lo, stats->words_scanned );
  add( &s->removed_hi, &s->removed_lo, stats->removed );
  s->cleared += fixnum( stats->cleared );
  s->scanned += fixnum( stats->scanned );
  s->compacted += fixnum( stats->compacted );
}

#if defined(SIMULATE_NEW_BARRIER)
void stats_add_swb_stats( swb_stats_t *stats )
{
  swb_memstat_t *s = &stats_state.swb_stats;

  s->total_assignments += fixnum( stats->total_assignments );
  s->array_assignments += fixnum( stats->array_assignments );
  s->lhs_young_or_remembered += fixnum( stats->lhs_young_or_remembered );
  s->rhs_constant += fixnum( stats->rhs_constant );
  s->cross_gen_check += fixnum( stats->cross_gen_check );
  s->transactions += fixnum( stats->transactions );
}
#endif

int stats_parameter( int key )
{
  switch (key) {
    case 0  : return stats_state.generations; 
    case 1  : return stats_state.remsets;
    default : panic_abort( "Illegal parameter to stats_parameter." );
              return -1;
  }
}

static void fill_main_entries( word *vp );
static void fill_gen_vector( word *gv, gen_memstat_t *gs );
static void fill_remset_vector( word *rv, remset_memstat_t *rs );

word stats_fillvector( word w_buffer )
{
  word genv, remv;
  int i, generations, remsets;

  generations = stats_state.generations;
  remsets = stats_state.remsets;

  assert( vector_length( w_buffer ) >= STAT_VSIZE );
  assert( vector_length(vector_ref(w_buffer, STAT_GENERATIONS)) 
	  >= generations );
  assert( vector_length(vector_ref(w_buffer, STAT_REMSETS)) >= remsets );

  fill_main_entries( ptrof(w_buffer)+1 );

  genv = vector_ref( w_buffer, STAT_GENERATIONS ); /* generation metavector */
  for ( i=0 ; i < generations ; i++ ) {
    word g = vector_ref(genv,i);

    assert( vector_length(g) == STAT_G_SIZE );
    fill_gen_vector( ptrof(g)+1, &stats_state.gen_stats[i] );
  }

  remv = vector_ref( w_buffer, STAT_REMSETS ); /* remset metavector */
  for ( i = 0 ; i < remsets ; i++ ) {
    word r = vector_ref(remv,i);

    assert( vector_length(r) == STAT_R_SIZE );
    fill_remset_vector( ptrof(r)+1, &stats_state.remset_stats[i] );
  }

  return w_buffer;
}

static void fill_main_entries( word *vp )
{
  stat_time_t user, system, real;
  unsigned minflt, majflt;
  gclib_memstat_t *gclib = &stats_state.gclib_stats;
  gc_memstat_t *gc = &stats_state.gc_stats;
  stack_memstat_t *stack = &stats_state.stack_stats;
#if defined(SIMULATE_NEW_BARRIER)
  swb_memstat_t *swb = &stats_state.swb_stats;
#endif

  /* gclib */
  vp[ STAT_WORDS_HEAP ]    = gclib->heap_allocated;
  vp[ STAT_HEAP_MAX ]      = gclib->heap_allocated_max;
  vp[ STAT_WORDS_REMSET ]  = gclib->remset_allocated;
  vp[ STAT_REMSET_MAX ]    = gclib->remset_allocated_max;
  vp[ STAT_WORDS_RTS ]     = gclib->rts_allocated;
  vp[ STAT_RTS_MAX ]       = gclib->rts_allocated_max;
  vp[ STAT_WORDS_WASTAGE ] = gclib->heap_fragmentation;
  vp[ STAT_WASTAGE_MAX ]   = gclib->heap_fragmentation_max;

  /* gc */
  vp[ STAT_WALLOCATED_HI ] = gc->allocated_hi;
  vp[ STAT_WALLOCATED_LO ] = gc->allocated_lo;
  vp[ STAT_WCOLLECTED_HI ] = gc->reclaimed_hi;
  vp[ STAT_WCOLLECTED_LO ] = gc->reclaimed_lo;
  vp[ STAT_WCOPIED_HI ]    = gc->words_copied_hi;
  vp[ STAT_WCOPIED_LO ]    = gc->words_copied_lo;
  vp[ STAT_WMOVED_HI ]     = gc->words_moved_hi;
  vp[ STAT_WMOVED_LO ]     = gc->words_moved_lo;
  vp[ STAT_NP_K ]          = gc->np_k;
  vp[ STAT_NP_J ]          = gc->np_j;
  vp[ STAT_DOF_RESETS ]    = gc->resets;
  vp[ STAT_DOF_REPEATS ]   = gc->repeats;
  vp[ STAT_FULL_GCS ]      = gc->full_collections;
  vp[ STAT_FULL_GCTIME ]   = gc->full_ms_collection;
  vp[ STAT_FULL_COPIED_HI ] = 0; /* unused at present */
  vp[ STAT_FULL_COPIED_LO ] = 0;
  vp[ STAT_FULL_MOVED_HI ]  = 0;
  vp[ STAT_FULL_MOVED_LO ]  = 0;
  vp[ STAT_FULL_MARKED_HI ] = gc->full_objects_marked_hi;
  vp[ STAT_FULL_MARKED_LO ] = gc->full_objects_marked_lo;
  vp[ STAT_FULL_WMARKED_HI ] = gc->full_words_marked_hi;
  vp[ STAT_FULL_WMARKED_LO ] = gc->full_words_marked_lo;
  vp[ STAT_FULL_PTRACED_HI ] = gc->full_pointers_traced_hi;
  vp[ STAT_FULL_PTRACED_LO ] = gc->full_pointers_traced_lo;

  /* stack */
  vp[ STAT_STK_CREATED ]   = stack->stacks_created;
  vp[ STAT_FFLUSHED_HI ]   = stack->frames_flushed_hi;
  vp[ STAT_FFLUSHED_LO ]   = stack->frames_flushed_lo;
  vp[ STAT_WFLUSHED_HI ]   = stack->words_flushed_hi;
  vp[ STAT_WFLUSHED_LO ]   = stack->words_flushed_lo;
  vp[ STAT_FRESTORED_HI ]  = stack->frames_restored_hi;
  vp[ STAT_FRESTORED_LO ]  = stack->frames_restored_lo;

  /* simulated barrier */
#if defined(SIMULATE_NEW_BARRIER)
  vp[ STAT_SWB_TOTAL ] = swb->total_assignments;
  vp[ STAT_SWB_ASSIGN ] = swb->array_assignments;
  vp[ STAT_SWB_LHS_OK ] = swb->lhs_young_or_remembered;
  vp[ STAT_SWB_RHS_CONST ] = swb->rhs_constant;
  vp[ STAT_SWB_NOTXGEN ] = swb->cross_gen_check;
  vp[ STAT_SWB_TRANS ] = swb->transactions;
#endif

  /* overall system stats */
  stats_time_used( &real, &user, &system );
  stats_pagefaults( &majflt, &minflt );

  vp[ STAT_RTIME ]         = fixnum( real.sec * 1000 + real.usec / 1000 );
  vp[ STAT_STIME ]         = fixnum( system.sec * 1000 + system.usec  / 1000 );
  vp[ STAT_UTIME ]         = fixnum( user.sec * 1000 + user.usec / 1000);
  vp[ STAT_MINFAULTS ]     = fixnum( minflt );
  vp[ STAT_MAJFAULTS ]     = fixnum( majflt );
}

static void fill_gen_vector( word *gv, gen_memstat_t *gs )
{
  gv[ STAT_G_MAJOR_ID ] = gs->major_id;
  gv[ STAT_G_MINOR_ID ] = gs->minor_id;

  gv[ STAT_G_TARGET ] = gs->target;
  gv[ STAT_G_ALLOC ] = gs->allocated;
  gv[ STAT_G_WLIVE ] = gs->used;

  gv[ STAT_G_PROM_COUNT ] = gs->promotions;
  gv[ STAT_G_GC_COUNT ] = gs->collections;
  gv[ STAT_G_PROMTIME ] = gs->ms_promotion;
  gv[ STAT_G_GCTIME ] = gs->ms_collection;
}

static void fill_remset_vector( word *rv, remset_memstat_t *rs )
{
  rv[ STAT_R_MAJOR_ID ] = rs->major_id;
  rv[ STAT_R_MINOR_ID ] = rs->minor_id;

  rv[ STAT_R_ALLOC ] = rs->allocated;
  rv[ STAT_R_MAX_SIZE ] = rs->max_allocated;
  rv[ STAT_R_USED ] = rs->used;
  rv[ STAT_R_LIVE ] = rs->live;

  rv[ STAT_R_SSBREC_HI ] = rs->ssb_recorded_hi;
  rv[ STAT_R_SSBREC_LO ] = rs->ssb_recorded_lo;
  rv[ STAT_R_HSCAN_HI ] = rs->scanned_hi;
  rv[ STAT_R_HSCAN_LO ] = rs->scanned_lo;
  rv[ STAT_R_HREC_HI ] = rs->recorded_hi;
  rv[ STAT_R_HREC_LO ] = rs->recorded_lo;
  rv[ STAT_R_HREM_HI ] = rs->removed_hi;
  rv[ STAT_R_HREM_LO ] = rs->removed_lo;
  rv[ STAT_R_WSCAN_HI ] = rs->words_scanned_hi;
  rv[ STAT_R_WSCAN_LO ] = rs->words_scanned_lo;
  rv[ STAT_R_CLEARED ] = rs->cleared;
  rv[ STAT_R_SCANNED ] = rs->scanned;
  rv[ STAT_R_COMPACTED ] = rs->compacted;
}

/* Adds a word to a doubleword with carry propagation, both parts of
 * the doubleword are independently represented as fixnums.  'x' is
 * a native integer.
 */
static void add( word *hi, word *lo, int x )
{
  *lo += fixnum(x);
  if (*lo > LARGEST_FIXNUM) {
    *lo -= LARGEST_FIXNUM;
    *hi += 4;
  }
}

/* Dumping */

static const char *dump_banner; /* defined below */
static void dump_gen_stats( FILE *f, gen_memstat_t *gs );
static void dump_remset_stats( FILE *f, remset_memstat_t *rs );

bool stats_opendump( const char *filename )
{
  if (stats_state.dump_file != 0) 
    stats_closedump();

  stats_state.dump_file = fopen( filename, "w" );

  if (stats_state.dump_file) {
    fprintf( stats_state.dump_file, dump_banner,
	     larceny_major_version, 
	     larceny_minor_version,
	     larceny_version_qualifier );
    return TRUE;
  }
  else
    return FALSE;
}

void stats_closedump( void )
{
  if (stats_state.dump_file == 0) return;

  fclose( stats_state.dump_file );
  stats_state.dump_file = 0;
}

void stats_dumpstate( void )
{
  FILE *f = stats_state.dump_file;

  if (f == 0) return;

  fprintf( f, "(" );

  /* Print overall memory and GC information */
  { gc_memstat_t *s = &stats_state.gc_stats;

    fprintf( f, "%lu %lu %lu %lu %lu %lu ",
	     nativeuint( s->allocated_hi ),
	     nativeuint( s->allocated_lo ),
	     nativeuint( s->reclaimed_hi ),
	     nativeuint( s->reclaimed_lo ),
	     nativeuint( s->words_copied_hi ),
	     nativeuint( s->words_copied_lo )
	     );
  }

  /* Print generations information */
  { int i;

    fprintf( f, "(" );
    for ( i=0 ; i < stats_state.generations ; i++ )
      dump_gen_stats( f, &stats_state.gen_stats[i] );
    fprintf( f, ") " );
  }

  /* Print remembered sets information */
  { int i;

    fprintf( f, "(" );
    for ( i = 0 ; i < stats_state.remsets ; i++ )
      dump_remset_stats( f, &stats_state.remset_stats[i] );
    fprintf( f, ") " );
  }

  /* Print stack information */
  { stack_memstat_t *s = &stats_state.stack_stats;

    fprintf( f, "%lu %lu %lu %lu %lu %lu %lu ",
	     nativeuint( s->frames_flushed_hi ), 
	     nativeuint( s->frames_flushed_lo ),
	     nativeuint( s->words_flushed_hi ),
	     nativeuint( s->words_flushed_lo ),
	     nativeuint( s->stacks_created ),
	     nativeuint( s->frames_restored_hi ),
	     nativeuint( s->frames_restored_lo ) );
  }

  /* Print overall heap information */
  { gc_memstat_t *s1 = &stats_state.gc_stats;
    gclib_memstat_t *s2 = &stats_state.gclib_stats;

    fprintf( f, "%lu %lu %lu %lu %lu %lu %lu %lu %lu %lu "
	        "%lu %lu %lu %lu %lu %lu %lu %lu %lu %lu ",
	     nativeuint( s2->heap_allocated ),
	     nativeuint( s2->remset_allocated ),
	     nativeuint( s2->rts_allocated ),
	     nativeuint( s1->words_moved_hi ), 
	     nativeuint( s1->words_moved_lo ),
	     nativeuint( s2->heap_fragmentation ),
	     nativeuint( s2->heap_allocated_max ),
	     nativeuint( s2->remset_allocated_max ),
	     nativeuint( s2->rts_allocated_max ),
	     nativeuint( s2->heap_fragmentation_max ),
	     nativeuint( s1->np_k ),
	     nativeuint( s1->np_j ),
	     nativeuint( s1->resets ),
	     nativeuint( s1->repeats ),
	     nativeuint( s1->full_collections ),
	     nativeuint( s1->full_ms_collection ),
	     nativeuint( s1->full_objects_marked_hi ),
	     nativeuint( s1->full_objects_marked_lo ),
	     nativeuint( s1->full_pointers_traced_hi ),
	     nativeuint( s1->full_pointers_traced_lo ) );
  }

#if defined(SIMULATE_NEW_BARRIER)
  { swb_memstat_t *s = &stats_state.swb_stats;
    fprintf( f, "(swb . (%lu %lu %lu %lu %lu %lu))",
	     nativeuint( s->array_assignments ),
	     nativeuint( s->lhs_young_or_remembered ),
	     nativeuint( s->rhs_constant ),
	     nativeuint( s->cross_gen_check ),
	     nativeuint( s->transactions ),
	     nativeuint( s->total_assignments ) );
  }
#endif
   
  fprintf( f, ")\n" );
}

static void dump_gen_stats( FILE *f, gen_memstat_t *gs )
{
  fprintf( f, "(%lu %lu %lu %lu %lu %lu %lu %lu %lu) ",
	   nativeuint( gs->major_id ),
	   nativeuint( gs->minor_id ),
	   nativeuint( gs->target ),
	   nativeuint( gs->allocated ),
	   nativeuint( gs->used ),
	   nativeuint( gs->promotions ),
	   nativeuint( gs->collections ),
	   nativeuint( gs->ms_promotion ),
	   nativeuint( gs->ms_collection ) );
}

static void dump_remset_stats( FILE *f, remset_memstat_t *rs )
{
  fprintf( f, "(%lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu "
	      "%lu %lu %lu %lu %lu %lu) ",
	   nativeuint( rs->major_id ),
	   nativeuint( rs->minor_id ),
	   nativeuint( rs->allocated ),
	   nativeuint( rs->max_allocated ),
	   nativeuint( rs->used ),
	   nativeuint( rs->live ),
	   nativeuint( rs->recorded_hi ),
	   nativeuint( rs->recorded_lo ),
	   nativeuint( rs->removed_hi ),
	   nativeuint( rs->removed_lo ),
	   nativeuint( rs->scanned_hi ),
	   nativeuint( rs->scanned_lo ),
	   nativeuint( rs->words_scanned_hi ),
	   nativeuint( rs->words_scanned_lo ),
	   nativeuint( rs->ssb_recorded_hi ),
	   nativeuint( rs->ssb_recorded_lo ),
	   nativeuint( rs->cleared ),
	   nativeuint( rs->scanned ),
	   nativeuint( rs->compacted ) );
}

static const char *dump_banner =
"; RTS statistics, dumped by Larceny version %d.%d%s\n"
";\n"
"; The output is an s-expression, one following each GC.  When an entry\n"
"; is advertised as xxx-hi and xxx-lo, then the -hi is the high 29 bits\n"
"; and the -lo is the low 29 bits of a 58-bit unsigned integer.\n"
"; Times are in milliseconds, volumes and sizes are in words.\n"
";\n"
"; First some general data (running totals):\n"
";\n"
";  (words-allocated-hi words-allocated-lo\n"
";   words-reclaimed-hi words-reclaimed-lo\n"
";   words-copied-hi    words-copied-lo\n"
";\n"
"; Then follows a list of sublists of generation data, in no particular\n"
"; order:\n"
";\n"
";   ((major-id minor-id target-size allocated-size used-size\n"
";     promotions collections time-promotion time-collection) ...\n"
";\n"
"; Then follows a list of sublists of remembered set data, in no particular\n"
"; order:\n"
";\n"
";   ((major-id minor-id allocated-size max-size used-size live-size\n"
";     entries-recorded-hi entries-recorded-lo\n"
";     entries-removed-hi entries-removed-lo\n"
";     entries-scanned-hi entries-scanned-lo\n"
";     object-words-scanned-hi object-words-scanned-lo\n"
";     ssb-entries-recorded-hi ssb-entries-recorded-lo\n"
";     times-cleared times-scanned times-compacted) ...)\n"
";\n"
"; Then follow data about the stack cache:\n"
";\n"
";   frames-flushed-hi frames-flushed-lo words-flushed-hi words-flushed-lo\n"
";   stacks-created frames-restored-hi frames-restored-lo\n"
";\n"
"; Then there are more data about overall memory use:\n"
";\n"
";   words-allocated-heap words-allocated-remset words-allocated-rts-other\n"
";   words-moved-hi words-moved-lo words-heap-fragmentation\n"
";   words-heap-max words-remset-max words-rts-max words-fragmentation-max\n"
";   np-k np-j dof-resets dof-repeats full-collections time-full-collections\n"
";   full-objects-marked-hi full-objects-marked-lo\n"
";   full-pointers-traced-hi full-objects-traced-lo\n"
";\n"
"; The tail of the list is an association list, where the entries that\n"
"; are dumped depend on how the system was compiled.  Each entry is a list\n"
"; where the car is the tag and the cdr is a list of data, as shown.\n"
";\n"
"; Tag=`swb': simulated write barrier profile data\n;\n"
";   (swb . (array-assignments\n"
";           lhs-young-or-remembered rhs-constant not-cross-gen\n"
";           array-transactions))\n"
"\n";

/* eof */
