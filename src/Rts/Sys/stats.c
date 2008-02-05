/* Copyright 1998, 1999, 2000 Lars T Hansen    -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * In the data structures in this module, all "word" entries are tagged
 * Scheme data; most are nonnegative integers represented as fixnums.
 *
 * FIXME: There is a level of buffering going on here that isn't very
 * elegant.  The rest of the RTS operates on native data, which are
 * buffered in Scheme representations in the static variable stats_state
 * in this module, and then copied into a Scheme data structure when the
 * Scheme system makes a callout to get accounting data.  It would be
 * better if the data were placed directly into the Scheme data
 * structure as it comes in from the rest of the RTS.  All that's
 * necessary is for the Scheme library to register a buffer with this
 * module into which data can be accumulated.  On the other hand, that
 * creates a heap-allocated structure whose size varies with the collector,
 * and that introduces noise, so I've decided to hold off on it.
 */

#include <stdio.h>
#include "larceny.h"
#include "stats.h"
#include "gc.h"

#define MAX_TIMERS     20	      /* Should be plenty */
#define LARGEST_FIXNUM (2147483644L)  /* ((2^29)-1)*4 */

#define FIXNUM_MASK    536870911L     /* 2^29-1 */
#define FIXNUM_SHIFT   29

#define PASTE(name,x)  name##x
#define PASTE3(x,y,z)  x##y##z

#define DWORD(name)       \
  word PASTE(name,_hi);   \
  word PASTE(name,_lo)

#define ADD_DWORD( src, target, field ) \
  add( &target->PASTE( field, _hi ), &target->PASTE( field, _lo ), src->field )

#define ADD_WORD( src, target, field ) \
  target->field += fixnum( src->field )

#define MAX_WORD( src, target, field ) \
  target->field = max(target->field, fixnum( src->field ))

/* Put a 58-bit int into two fixnum fields */
#define PUTBIG_DWORD( src, target, field )				\
  do { target->PASTE(field,_lo) = fixnum( src->field & FIXNUM_MASK );	\
       target->PASTE(field,_hi) =					\
         fixnum( (src->field >> FIXNUM_SHIFT) & FIXNUM_MASK );		\
  } while(0) 

#define PUT_WORD( src, target, field ) 		\
  target->field = fixnum( src->field )

#define CPUTBIG_DWORD( src, target, field )                             \
  do { if (src->field != 0) {                                           \
         target->PASTE(field,_lo) = fixnum( src->field & FIXNUM_MASK ); \
         target->PASTE(field,_hi) =                                     \
           fixnum( (src->field >> FIXNUM_SHIFT) & FIXNUM_MASK );        \
       }                                                                \
  } while(0) 

#define CPUT_WORD( src, target, field ) 		\
  if (src->field != 0) target->field = fixnum( src->field )


typedef struct gc_memstat gc_memstat_t;
typedef struct gclib_memstat gclib_memstat_t;
typedef struct stack_memstat stack_memstat_t;
typedef struct gen_memstat gen_memstat_t;
typedef struct remset_memstat remset_memstat_t;
#if defined(SIMULATE_NEW_BARRIER)
typedef struct swb_remstat swb_remstat_t;
#endif
typedef struct gc_event_memstat gc_event_memstat_t;

struct gclib_memstat {
  word heap_allocated;		/* words allocated to heap areas */
  word heap_allocated_max;	/* max words allocated to heap */
  word remset_allocated;	/* words allocated to remembered sets */
  word remset_allocated_max;	/* max words allocated to remset */
  word rts_allocated;		/* words allocated to RTS "other" */
  word rts_allocated_max;	/* max words allocated to rts */
  word heap_fragmentation;	/* words of external heap framgentation */
  word heap_fragmentation_max;	/* max words of external heap fragmentation */
  word mem_allocated;		/* total words of allocation */
  word mem_allocated_max;	/* max total words of allocation */

  word max_remset_scan;
  word max_remset_scan_cpu;
  word total_remset_scan;
  word total_remset_scan_cpu;
  word remset_scan_count;
  word max_entries_remset_scan;
  word total_entries_remset_scan;

  word max_mark_pause;
  word max_mark_pause_cpu;
  word total_mark_pause;
  word total_mark_pause_cpu;
  word mark_pause_count;

  word max_ms_minor;
  word max_ms_minor_cpu;
  word total_ms_minor;
  word total_ms_minor_cpu;
  word count_minors;
  
  word max_ms_major;
  word max_ms_major_cpu;
  word total_ms_major;
  word total_ms_major_cpu;
  word count_majors;
  
  word max_build_remset_summary;
  word max_build_remset_summary_cpu;
  word total_build_remset_summary;
  word total_build_remset_summary_cpu;
  word build_remset_summary_count;
  
  word count_collect_00_10_ms;
  word count_collect_10_20_ms;
  word count_collect_20_30_ms;
  word count_collect_30_40_ms;
  word count_collect_40_50_ms;
  word count_collect_50_60_ms;
  word count_collect_60_70_ms;
  word count_collect_70_80_ms;
  word count_collect_80_90_ms;
  word count_collect_90_100_ms;
  word count_collect_100_200_ms;
  word count_collect_200_300_ms;
  word count_collect_300_400_ms;
  word count_collect_400_500_ms;
  word count_collect_500_600_ms;
  word count_collect_600_700_ms;
  word count_collect_700_800_ms;
  word count_collect_800_900_ms;
  word count_collect_900_1000_ms;
  word count_collect_1000_2000_ms;
  word count_collect_geq_2000_ms;

  word count_minorgc_00_10_ms;
  word count_minorgc_10_20_ms;
  word count_minorgc_20_30_ms;
  word count_minorgc_30_40_ms;
  word count_minorgc_40_50_ms;
  word count_minorgc_50_60_ms;
  word count_minorgc_60_70_ms;
  word count_minorgc_70_80_ms;
  word count_minorgc_80_90_ms;
  word count_minorgc_90_100_ms;
  word count_minorgc_100_200_ms;
  word count_minorgc_200_300_ms;
  word count_minorgc_300_400_ms;
  word count_minorgc_400_500_ms;
  word count_minorgc_500_600_ms;
  word count_minorgc_600_700_ms;
  word count_minorgc_700_800_ms;
  word count_minorgc_800_900_ms;
  word count_minorgc_900_1000_ms;
  word count_minorgc_1000_2000_ms;
  word count_minorgc_geq_2000_ms;

  word count_majorgc_00_10_ms;
  word count_majorgc_10_20_ms;
  word count_majorgc_20_30_ms;
  word count_majorgc_30_40_ms;
  word count_majorgc_40_50_ms;
  word count_majorgc_50_60_ms;
  word count_majorgc_60_70_ms;
  word count_majorgc_70_80_ms;
  word count_majorgc_80_90_ms;
  word count_majorgc_90_100_ms;
  word count_majorgc_100_200_ms;
  word count_majorgc_200_300_ms;
  word count_majorgc_300_400_ms;
  word count_majorgc_400_500_ms;
  word count_majorgc_500_600_ms;
  word count_majorgc_600_700_ms;
  word count_majorgc_700_800_ms;
  word count_majorgc_800_900_ms;
  word count_majorgc_900_1000_ms;
  word count_majorgc_1000_2000_ms;
  word count_majorgc_geq_2000_ms;

  word count_sumrize_00_10_ms;
  word count_sumrize_10_20_ms;
  word count_sumrize_20_30_ms;
  word count_sumrize_30_40_ms;
  word count_sumrize_40_50_ms;
  word count_sumrize_50_60_ms;
  word count_sumrize_60_70_ms;
  word count_sumrize_70_80_ms;
  word count_sumrize_80_90_ms;
  word count_sumrize_90_100_ms;
  word count_sumrize_100_200_ms;
  word count_sumrize_200_300_ms;
  word count_sumrize_300_400_ms;
  word count_sumrize_400_500_ms;
  word count_sumrize_500_600_ms;
  word count_sumrize_600_700_ms;
  word count_sumrize_700_800_ms;
  word count_sumrize_800_900_ms;
  word count_sumrize_900_1000_ms;
  word count_sumrize_1000_2000_ms;
  word count_sumrize_geq_2000_ms;

  word count_minor_00_02_runs;
  word count_minor_02_04_runs;
  word count_minor_04_06_runs;
  word count_minor_06_08_runs;
  word count_minor_08_10_runs;
  word count_minor_10_20_runs;
  word count_minor_20_30_runs;
  word count_minor_30_40_runs;
  word count_minor_40_50_runs;
  word count_minor_50_60_runs;
  word count_minor_60_70_runs;
  word count_minor_70_80_runs;
  word count_minor_80_90_runs;
  word count_minor_90_100_runs;
  word count_minor_100_200_runs;
  word count_minor_200_300_runs;
  word count_minor_300_400_runs;
  word count_minor_400_500_runs;
  word count_minor_500_600_runs;
  word count_minor_600_700_runs;
  word count_minor_700_800_runs;
  word count_minor_800_900_runs;
  word count_minor_900_1000_runs;
  word count_minor_1000_2000_runs;
  word count_minor_geq_2000_runs;
};

struct gc_memstat {
  DWORD( allocated );		/* total words allocated */
  DWORD( reclaimed );		/* total words reclaimed */
  DWORD( objects_copied );	/* by copying collection */
  DWORD( words_copied );	/* ditto */
  DWORD( objects_moved );	/* ditto */
  DWORD( words_moved );		/* ditto */

  /* NP (ROF) collector, when applicable */
  word np_k;			/* Number of steps (old+young together) */
  word np_j;			/* Number of steps in young */

  /* DOF collector */
  word resets;			/* Number of resets */
  word repeats;			/* Number of repeats */

  /* Full mark/sweep collections */
  word full_collections;
  word full_ms_collection;
  word full_ms_collection_cpu;
  DWORD( full_objects_marked );
  DWORD( full_words_marked );
  DWORD( full_pointers_traced );

  word max_ms_collection;       /* Max milliseconds collecting in an area */
  word max_ms_collection_cpu;   /* ditto, CPU time */
};

struct stack_memstat {
  word stacks_created;		/* number of stacks created */
  DWORD( words_flushed );	/* words of stack frames flushed or copied */
  DWORD( frames_flushed );	/* number of stack frames flushed */
  DWORD( frames_restored );	/* number of stack frames restored */
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
  word ms_promotion_cpu;	/* Ditto cpu time */
  word ms_collection;		/* Total gc time in ms (excluding promotion) */
  word ms_collection_cpu;	/* Ditto cpu time */
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
  DWORD( ssb_recorded );	/* SSB transactions recorded */
  DWORD( recorded );		/* remset table entries recorded */
  DWORD( objs_scanned );	/* remset table entries scanned */
  DWORD( words_scanned );	/* words of old objects scanned */
  DWORD( removed );		/* remset table entries removed */
  word max_objs_scanned;	/* max remset table entries scanned */
  word max_words_scanned;	/* max words of old objects scanned */
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

struct gc_event_memstat {
  /* Generic */
  DWORD( gctime );
  DWORD( promtime );
  DWORD( free_unused );
  DWORD( root_scan_gc );
  DWORD( root_scan_prom );
  DWORD( los_sweep_gc );
  DWORD( los_sweep_prom );
  DWORD( remset_scan_gc );
  DWORD( remset_scan_prom );
  DWORD( tospace_scan_gc );
  DWORD( tospace_scan_prom );
  /* DOF */
  DWORD( reset_after_gc );
  DWORD( decrement_after_gc );
  DWORD( dof_remset_scan );
  DWORD( sweep_shadow );
  DWORD( msgc_mark );
  DWORD( sweep_dof_sets );
  DWORD( sweep_remset );
  DWORD( sweep_los );
  DWORD( assimilate_prom );
  DWORD( assimilate_gc );

  word copied_by_gc;
  word moved_by_gc;
  word copied_by_prom;
  word moved_by_prom;
  word words_forwarded;
  word ptrs_forwarded;
  word gc_barrier_hit;
  word remset_large_objs_scanned;
  word remset_large_obj_words_scanned;
};

static struct {
  /* Statistics */
  gc_memstat_t     gc_stats;
  gclib_memstat_t  gclib_stats;
  gen_memstat_t    gen_stats[ MAX_GENERATIONS ];
  remset_memstat_t remset_stats[ MAX_REMSETS ];
  stack_memstat_t  stack_stats;
#if defined(SIMULATE_NEW_BARRIER)
  swb_remstat_t    swb_stats;
#endif
  gc_event_memstat_t gc_event_stats;
  
  /* Other state variables */
  gc_t *gc;
  int  generations;		/* Number of generation entries */
  int  remsets;			/* Number of remset entries */
  bool initialized;
  struct {
    int           timer;	/* Base measurement */
    stats_timer_t type;		/* What are we measuring? */
  } timers[ MAX_TIMERS ];
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

  assert( stats_state.remsets < MAX_REMSETS );

  i = stats_state.remsets++;
  stats_state.remset_stats[i].major_id = fixnum(major_id);
  stats_state.remset_stats[i].minor_id = fixnum(minor_id);
  return i;
}

stats_id_t stats_start_timer( stats_timer_t type )
{
  int i;

  for ( i=0 ; i < MAX_TIMERS && stats_state.timers[i].timer > 0 ; i++ )
    ;
  assert(i < MAX_TIMERS);

  stats_state.timers[i].type = type;
  switch (type) {
    case TIMER_ELAPSED : 
      stats_state.timers[i].timer = osdep_realclock();
      break;
    case TIMER_CPU : 
      stats_state.timers[i].timer = osdep_cpuclock();
      break;
    default :
      assert(0);
  }
  assert( stats_state.timers[i].timer != 0 );

  return i;
}

int stats_stop_timer( stats_id_t timer )
{
  int then;

  assert( 0 <= timer && 
	  timer < MAX_TIMERS && 
	  stats_state.timers[timer].timer != 0 );

  then = stats_state.timers[timer].timer;
  stats_state.timers[timer].timer = 0;
  switch (stats_state.timers[timer].type) {
    case TIMER_ELAPSED :
      return osdep_realclock() - then;
    case TIMER_CPU :
      return osdep_cpuclock() - then;
    default :
      assert(0);
      return 0;
  }
}

void stats_add_gclib_stats( gclib_stats_t *stats )
{
  gclib_memstat_t *s = &stats_state.gclib_stats;

  PUT_WORD( stats, s, heap_allocated );
  PUT_WORD( stats, s, heap_allocated_max );
  PUT_WORD( stats, s, remset_allocated );
  PUT_WORD( stats, s, remset_allocated_max );
  PUT_WORD( stats, s, rts_allocated );
  PUT_WORD( stats, s, rts_allocated_max );
  PUT_WORD( stats, s, heap_fragmentation );
  PUT_WORD( stats, s, heap_fragmentation_max );
  PUT_WORD( stats, s, mem_allocated );
  PUT_WORD( stats, s, mem_allocated_max );

  PUT_WORD( stats, s, max_remset_scan );
  PUT_WORD( stats, s, max_remset_scan_cpu );
  PUT_WORD( stats, s, total_remset_scan );
  PUT_WORD( stats, s, total_remset_scan_cpu );
  PUT_WORD( stats, s, remset_scan_count );
  PUT_WORD( stats, s, max_entries_remset_scan );
  PUT_WORD( stats, s, total_entries_remset_scan );

#define RANGECASE(lo, hi, recv_prefix, recv_suffix, arg)     \
  else if (lo <= arg && arg < hi)            \
    recv_prefix ## lo ## _ ## hi ## recv_suffix += fixnum(1)

#define RANGECASE_ALT(lo, hi, lo_id, hi_id, recv_prefix, recv_suffix, arg) \
  else if (lo <= arg && arg < hi)            \
    recv_prefix ## lo_id ## _ ## hi_id ## recv_suffix += fixnum(1)

  /* Note that the leading zero in 00 below is signifcant, 
   * since the token is turned into an identifier as well
   * as being used to represent zero. */
#define RANGECASES( recv_prefix, recv_suffix, arg )          \
  do {                                          \
    if (0);                                     \
    RANGECASE(   00,   10, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   10,   20, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   20,   30, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   30,   40, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   40,   50, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   50,   60, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   60,   70, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   70,   80, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   80,   90, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   90,  100, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  100,  200, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  200,  300, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  300,  400, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  400,  500, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  500,  600, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  600,  700, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  700,  800, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  800,  900, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  900, 1000, recv_prefix, recv_suffix, arg );  \
    RANGECASE( 1000, 2000, recv_prefix, recv_suffix, arg );  \
    else { assert(arg > 1000);                               \
      recv_prefix ## geq_2000 ## recv_suffix += fixnum(1);   \
    }                                                        \
  } while (0)

#define RANGECASES_FINE( recv_prefix, recv_suffix, arg )     \
  do {                                          \
    if (0);                                     \
    RANGECASE_ALT(0,2, 00,02, recv_prefix, recv_suffix, arg ); \
    RANGECASE_ALT(2,4, 02,04, recv_prefix, recv_suffix, arg ); \
    RANGECASE_ALT(4,6, 04,06, recv_prefix, recv_suffix, arg ); \
    RANGECASE_ALT(6,8, 06,08, recv_prefix, recv_suffix, arg ); \
    RANGECASE_ALT(8,10,08,10, recv_prefix, recv_suffix, arg ); \
    RANGECASE(   10,   20, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   20,   30, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   30,   40, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   40,   50, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   50,   60, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   60,   70, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   70,   80, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   80,   90, recv_prefix, recv_suffix, arg );  \
    RANGECASE(   90,  100, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  100,  200, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  200,  300, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  300,  400, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  400,  500, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  500,  600, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  600,  700, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  700,  800, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  800,  900, recv_prefix, recv_suffix, arg );  \
    RANGECASE(  900, 1000, recv_prefix, recv_suffix, arg );  \
    RANGECASE( 1000, 2000, recv_prefix, recv_suffix, arg );  \
    else { assert(arg > 1000);                               \
      recv_prefix ## geq_2000 ## recv_suffix += fixnum(1);   \
    }                                                        \
  } while (0)
  
  /* okay, now that we have the above helper macros,
   * here's the actual code to put in the values. */
  RANGECASES( s->count_collect_, _ms, stats->last_ms_gc_pause );
  if (stats->last_gc_pause_ismajor) {
    word ms_major     = stats->last_ms_gc_pause;
    word ms_major_cpu = stats->last_ms_gc_pause_cpu;
    RANGECASES( s->count_majorgc_, _ms, ms_major );
    RANGECASES_FINE( s->count_minor_, _runs, stats->length_minor_gc_run );
    s->max_ms_major        = max( fixnum(ms_major),     s->max_ms_major );
    s->max_ms_major_cpu    = max( fixnum(ms_major_cpu), s->max_ms_major_cpu );
    s->count_majors       += fixnum(1);
    s->total_ms_major     += fixnum( ms_major );
    s->total_ms_major_cpu += fixnum( ms_major_cpu );
  } else {
    word ms_minor     = stats->last_ms_gc_pause;
    word ms_minor_cpu = stats->last_ms_gc_pause_cpu;
    RANGECASES( s->count_minorgc_, _ms, ms_minor );
    s->max_ms_minor        = max( fixnum(ms_minor),     s->max_ms_minor );
    s->max_ms_minor_cpu    = max( fixnum(ms_minor_cpu), s->max_ms_minor_cpu );
    s->count_minors       += fixnum(1);
    s->total_ms_minor     += fixnum( ms_minor );
    s->total_ms_minor_cpu += fixnum( ms_minor_cpu );
  }
  if (stats->last_ms_remset_sumrize == -1) {
  } else {
    word ms     = stats->last_ms_remset_sumrize;
    word ms_cpu = stats->last_ms_remset_sumrize_cpu;
    RANGECASES( s->count_sumrize_, _ms, ms );
    s->max_build_remset_summary        = max( ms, s->max_build_remset_summary);
    s->max_build_remset_summary_cpu    = 
      max( ms_cpu, s->max_build_remset_summary_cpu);
    s->build_remset_summary_count     += fixnum(1);
    s->total_build_remset_summary     += fixnum( ms );
    s->total_build_remset_summary_cpu += fixnum( ms_cpu );
  }
  if (stats->last_ms_mark_refinement == -1) {
  } else {
    word ms     = fixnum( stats->last_ms_mark_refinement );
    word ms_cpu = fixnum( stats->last_ms_mark_refinement_cpu );
    s->max_mark_pause        = max( ms, s->max_mark_pause );
    s->max_mark_pause_cpu    = max( ms_cpu, s->max_mark_pause_cpu );
    s->mark_pause_count     += fixnum(1);
    s->total_mark_pause     += ms;
    s->total_mark_pause_cpu += ms_cpu;
  }
}

void stats_add_gc_stats( gc_stats_t *stats )
{
  gc_memstat_t *s = &stats_state.gc_stats;

  ADD_DWORD( stats, s, allocated );
  ADD_DWORD( stats, s, reclaimed );
  ADD_DWORD( stats, s, objects_copied );
  ADD_DWORD( stats, s, words_copied );
  ADD_DWORD( stats, s, objects_moved );
  ADD_DWORD( stats, s, words_moved );

  /* NP (ROF) collector */
  PUT_WORD( stats, s, np_k );
  PUT_WORD( stats, s, np_j );

  /* DOF collector */
  ADD_WORD( stats, s, resets );
  ADD_WORD( stats, s, repeats );

  /* Full mark/sweep backup collector */
  ADD_WORD( stats, s, full_collections );
  ADD_WORD( stats, s, full_ms_collection );
  ADD_WORD( stats, s, full_ms_collection_cpu );
  ADD_DWORD( stats, s, full_objects_marked );
  ADD_DWORD( stats, s, full_words_marked );
  ADD_DWORD( stats, s, full_pointers_traced );

  /* RROF collector (but why not others...) */
  MAX_WORD( stats, s, max_ms_collection );
  MAX_WORD( stats, s, max_ms_collection_cpu );
}

void stats_add_stack_stats( stack_stats_t *stats )
{
  stack_memstat_t *s = &stats_state.stack_stats;

  ADD_WORD( stats, s, stacks_created );
  ADD_DWORD( stats, s, words_flushed );
  ADD_DWORD( stats, s, frames_flushed );
  ADD_DWORD( stats, s, frames_restored );
}

void stats_add_gen_stats( stats_id_t generation, gen_stats_t *stats )
{
  gen_memstat_t *s;

  assert( 0 <= generation && generation < stats_state.generations );

  s = &stats_state.gen_stats[ generation ];

  PUT_WORD( stats, s, target );
  PUT_WORD( stats, s, allocated );
  PUT_WORD( stats, s, used );

  ADD_WORD( stats, s, promotions );
  ADD_WORD( stats, s, collections );
  ADD_WORD( stats, s, ms_promotion );
  ADD_WORD( stats, s, ms_promotion_cpu );
  ADD_WORD( stats, s, ms_collection );
  ADD_WORD( stats, s, ms_collection_cpu );
}

void stats_add_remset_stats( stats_id_t remset, remset_stats_t *stats )
{
  remset_memstat_t *s;

  assert( 0 <= remset && remset < stats_state.remsets );

  s = &stats_state.remset_stats[ remset ];

  PUT_WORD( stats, s, allocated );
  s->max_allocated = max( s->max_allocated, fixnum(stats->allocated) );
  PUT_WORD( stats, s, used );
  PUT_WORD( stats, s, live );

  ADD_DWORD( stats, s, ssb_recorded );
  ADD_DWORD( stats, s, objs_scanned );
  ADD_DWORD( stats, s, recorded );
  ADD_DWORD( stats, s, words_scanned );
  ADD_DWORD( stats, s, removed );

  ADD_WORD( stats, s, cleared );
  ADD_WORD( stats, s, scanned );
  ADD_WORD( stats, s, compacted );

  MAX_WORD( stats, s, max_objs_scanned );
  MAX_WORD( stats, s, max_words_scanned );
}

#if defined(SIMULATE_NEW_BARRIER)
void stats_add_swb_stats( swb_stats_t *stats )
{
  swb_memstat_t *s = &stats_state.swb_stats;

  ADD_WORD( stats, s, total_assignments );
  ADD_WORD( stats, s, array_assignments );
  ADD_WORD( stats, s, lhs_young_or_remembered );
  ADD_WORD( stats, s, rhs_constant );
  ADD_WORD( stats, s, cross_gen_check );
  ADD_WORD( stats, s, transactions );
}
#endif

void stats_set_gc_event_stats( gc_event_stats_t *stats )
{
  gc_event_memstat_t *s = &stats_state.gc_event_stats;

  CPUTBIG_DWORD( stats, s, gctime );
  CPUTBIG_DWORD( stats, s, promtime );
  CPUTBIG_DWORD( stats, s, free_unused );
  CPUTBIG_DWORD( stats, s, root_scan_gc );
  CPUTBIG_DWORD( stats, s, root_scan_prom );
  CPUTBIG_DWORD( stats, s, los_sweep_gc );
  CPUTBIG_DWORD( stats, s, los_sweep_prom );
  CPUTBIG_DWORD( stats, s, remset_scan_gc );
  CPUTBIG_DWORD( stats, s, remset_scan_prom );
  CPUTBIG_DWORD( stats, s, tospace_scan_gc );
  CPUTBIG_DWORD( stats, s, tospace_scan_prom );
  CPUTBIG_DWORD( stats, s, reset_after_gc );
  CPUTBIG_DWORD( stats, s, decrement_after_gc );
  CPUTBIG_DWORD( stats, s, dof_remset_scan );
  CPUTBIG_DWORD( stats, s, sweep_shadow );
  CPUTBIG_DWORD( stats, s, msgc_mark );
  CPUTBIG_DWORD( stats, s, sweep_dof_sets );
  CPUTBIG_DWORD( stats, s, sweep_remset );
  CPUTBIG_DWORD( stats, s, sweep_los );
  CPUTBIG_DWORD( stats, s, assimilate_prom );
  CPUTBIG_DWORD( stats, s, assimilate_gc );

  CPUT_WORD( stats, s, copied_by_gc );
  CPUT_WORD( stats, s, moved_by_gc );
  CPUT_WORD( stats, s, copied_by_prom );
  CPUT_WORD( stats, s, moved_by_prom );
  CPUT_WORD( stats, s, words_forwarded );
  CPUT_WORD( stats, s, ptrs_forwarded );
  CPUT_WORD( stats, s, gc_barrier_hit );
  CPUT_WORD( stats, s, remset_large_objs_scanned );
  CPUT_WORD( stats, s, remset_large_obj_words_scanned );
}

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

#define STAT_PUT_DWORD( vp, tag, src, field )			\
  do { vp[ PASTE3(STAT_,tag,_HI) ] = src->PASTE(field,_hi);	\
       vp[ PASTE3(STAT_,tag,_LO) ] = src->PASTE(field,_lo);	\
  } while(0)

#define STAT_PUT_WORD( vp, tag, src, field ) \
  vp[ PASTE(STAT_,tag) ] = src->field

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
  gc_event_memstat_t *gce = &stats_state.gc_event_stats;
  
  /* gclib */
  vp[ STAT_WORDS_HEAP ]    = gclib->heap_allocated;
  vp[ STAT_HEAP_MAX ]      = gclib->heap_allocated_max;
  vp[ STAT_WORDS_REMSET ]  = gclib->remset_allocated;
  vp[ STAT_REMSET_MAX ]    = gclib->remset_allocated_max;
  vp[ STAT_WORDS_RTS ]     = gclib->rts_allocated;
  vp[ STAT_RTS_MAX ]       = gclib->rts_allocated_max;
  vp[ STAT_WORDS_WASTAGE ] = gclib->heap_fragmentation;
  vp[ STAT_WASTAGE_MAX ]   = gclib->heap_fragmentation_max;
  vp[ STAT_WORDS_MEM ]     = gclib->mem_allocated;
  vp[ STAT_WORDS_MEM_MAX ] = gclib->mem_allocated_max;

  vp[ STAT_MAX_REMSET_SCAN ]       = gclib->max_remset_scan;
  vp[ STAT_MAX_REMSET_SCAN_CPU ]   = gclib->max_remset_scan_cpu;
  vp[ STAT_TOTAL_REMSET_SCAN ]     = gclib->total_remset_scan;
  vp[ STAT_TOTAL_REMSET_SCAN_CPU ] = gclib->total_remset_scan_cpu;
  vp[ STAT_REMSET_SCAN_COUNT ]     = gclib->remset_scan_count;

  vp[ STAT_MAX_MARK_PAUSE ]          = gclib->max_mark_pause;
  vp[ STAT_MAX_MARK_PAUSE_CPU ]      = gclib->max_mark_pause_cpu;
  vp[ STAT_TOTAL_MARK_PAUSE ]        = gclib->total_mark_pause;
  vp[ STAT_TOTAL_MARK_PAUSE_CPU ]    = gclib->total_mark_pause_cpu;
  vp[ STAT_MARK_PAUSE_COUNT ]        = gclib->mark_pause_count;

  vp[ STAT_MAX_ENTRIES_REMSET_SCAN ]   = gclib->max_entries_remset_scan;
  vp[ STAT_TOTAL_ENTRIES_REMSET_SCAN ] = gclib->total_entries_remset_scan;

  vp[ STAT_MAX_BUILD_REMSET_SUMMARY ]       = 
    gclib->max_build_remset_summary;
  vp[ STAT_MAX_BUILD_REMSET_SUMMARY_CPU ]   = 
    gclib->max_build_remset_summary_cpu;
  vp[ STAT_TOTAL_BUILD_REMSET_SUMMARY ]     = 
    gclib->total_build_remset_summary;
  vp[ STAT_TOTAL_BUILD_REMSET_SUMMARY_CPU ] = 
    gclib->total_build_remset_summary_cpu;
  vp[ STAT_BUILD_REMSET_SUMMARY_COUNT ]     =
    gclib->build_remset_summary_count;

  vp[ STAT_MAX_MAJORGC_PAUSE ]               = gclib->max_ms_major;
  vp[ STAT_MAX_MAJORGC_PAUSE_CPU ]           = gclib->max_ms_major_cpu;
  vp[ STAT_TOTAL_MAJORGC_PAUSE ]             = gclib->total_ms_major;
  vp[ STAT_TOTAL_MAJORGC_PAUSE_CPU ]         = gclib->total_ms_major_cpu;
  vp[ STAT_MAJORGC_PAUSE_COUNT ]             = gclib->count_majors;

  vp[ STAT_MAX_MINORGC_PAUSE ]               = gclib->max_ms_minor;
  vp[ STAT_MAX_MINORGC_PAUSE_CPU ]           = gclib->max_ms_minor_cpu;
  vp[ STAT_TOTAL_MINORGC_PAUSE ]             = gclib->total_ms_minor;
  vp[ STAT_TOTAL_MINORGC_PAUSE_CPU ]         = gclib->total_ms_minor_cpu;
  vp[ STAT_MINORGC_PAUSE_COUNT ]             = gclib->count_minors;

#define UPDATE_COUNT_COLLECT( lo, hi ) \
  vp[ STAT_COUNT_COLLECT_ ## lo ## _ ## hi ] = \
    gclib->count_collect_ ## lo ## _ ## hi ## _ms
  UPDATE_COUNT_COLLECT( 00, 10 );
  UPDATE_COUNT_COLLECT( 10, 20 );
  UPDATE_COUNT_COLLECT( 20, 30 );
  UPDATE_COUNT_COLLECT( 30, 40 );
  UPDATE_COUNT_COLLECT( 40, 50 );
  UPDATE_COUNT_COLLECT( 50, 60 );
  UPDATE_COUNT_COLLECT( 60, 70 );
  UPDATE_COUNT_COLLECT( 70, 80 );
  UPDATE_COUNT_COLLECT( 80, 90 );
  UPDATE_COUNT_COLLECT( 90, 100 );
  UPDATE_COUNT_COLLECT( 100, 200 );
  UPDATE_COUNT_COLLECT( 200, 300 );
  UPDATE_COUNT_COLLECT( 300, 400 );
  UPDATE_COUNT_COLLECT( 400, 500 );
  UPDATE_COUNT_COLLECT( 500, 600 );
  UPDATE_COUNT_COLLECT( 600, 700 );
  UPDATE_COUNT_COLLECT( 700, 800 );
  UPDATE_COUNT_COLLECT( 800, 900 );
  UPDATE_COUNT_COLLECT( 900, 1000 );
  UPDATE_COUNT_COLLECT( 1000, 2000 );
  vp[ STAT_COUNT_COLLECT_GEQ_2000 ] = gclib->count_collect_geq_2000_ms;

#define UPDATE_COUNT_MINORGC( lo, hi ) \
  vp[ STAT_COUNT_MINORGC_ ## lo ## _ ## hi ] = \
    gclib->count_minorgc_ ## lo ## _ ## hi ## _ms
  UPDATE_COUNT_MINORGC( 00, 10 );
  UPDATE_COUNT_MINORGC( 10, 20 );
  UPDATE_COUNT_MINORGC( 20, 30 );
  UPDATE_COUNT_MINORGC( 30, 40 );
  UPDATE_COUNT_MINORGC( 40, 50 );
  UPDATE_COUNT_MINORGC( 50, 60 );
  UPDATE_COUNT_MINORGC( 60, 70 );
  UPDATE_COUNT_MINORGC( 70, 80 );
  UPDATE_COUNT_MINORGC( 80, 90 );
  UPDATE_COUNT_MINORGC( 90, 100 );
  UPDATE_COUNT_MINORGC( 100, 200 );
  UPDATE_COUNT_MINORGC( 200, 300 );
  UPDATE_COUNT_MINORGC( 300, 400 );
  UPDATE_COUNT_MINORGC( 400, 500 );
  UPDATE_COUNT_MINORGC( 500, 600 );
  UPDATE_COUNT_MINORGC( 600, 700 );
  UPDATE_COUNT_MINORGC( 700, 800 );
  UPDATE_COUNT_MINORGC( 800, 900 );
  UPDATE_COUNT_MINORGC( 900, 1000 );
  UPDATE_COUNT_MINORGC( 1000, 2000 );
  vp[ STAT_COUNT_MINORGC_GEQ_2000 ] = gclib->count_minorgc_geq_2000_ms;

#define UPDATE_COUNT_MAJORGC( lo, hi ) \
  vp[ STAT_COUNT_MAJORGC_ ## lo ## _ ## hi ] = \
    gclib->count_majorgc_ ## lo ## _ ## hi ## _ms
  UPDATE_COUNT_MAJORGC( 00, 10 );
  UPDATE_COUNT_MAJORGC( 10, 20 );
  UPDATE_COUNT_MAJORGC( 20, 30 );
  UPDATE_COUNT_MAJORGC( 30, 40 );
  UPDATE_COUNT_MAJORGC( 40, 50 );
  UPDATE_COUNT_MAJORGC( 50, 60 );
  UPDATE_COUNT_MAJORGC( 60, 70 );
  UPDATE_COUNT_MAJORGC( 70, 80 );
  UPDATE_COUNT_MAJORGC( 80, 90 );
  UPDATE_COUNT_MAJORGC( 90, 100 );
  UPDATE_COUNT_MAJORGC( 100, 200 );
  UPDATE_COUNT_MAJORGC( 200, 300 );
  UPDATE_COUNT_MAJORGC( 300, 400 );
  UPDATE_COUNT_MAJORGC( 400, 500 );
  UPDATE_COUNT_MAJORGC( 500, 600 );
  UPDATE_COUNT_MAJORGC( 600, 700 );
  UPDATE_COUNT_MAJORGC( 700, 800 );
  UPDATE_COUNT_MAJORGC( 800, 900 );
  UPDATE_COUNT_MAJORGC( 900, 1000 );
  UPDATE_COUNT_MAJORGC( 1000, 2000 );
  vp[ STAT_COUNT_MAJORGC_GEQ_2000 ] = gclib->count_majorgc_geq_2000_ms;

#define UPDATE_COUNT_SUMMARIZE( lo, hi ) \
  vp[ STAT_COUNT_SUMMARIZE_ ## lo ## _ ## hi ] = \
    gclib->count_sumrize_ ## lo ## _ ## hi ## _ms
  UPDATE_COUNT_SUMMARIZE( 00, 10 );
  UPDATE_COUNT_SUMMARIZE( 10, 20 );
  UPDATE_COUNT_SUMMARIZE( 20, 30 );
  UPDATE_COUNT_SUMMARIZE( 30, 40 );
  UPDATE_COUNT_SUMMARIZE( 40, 50 );
  UPDATE_COUNT_SUMMARIZE( 50, 60 );
  UPDATE_COUNT_SUMMARIZE( 60, 70 );
  UPDATE_COUNT_SUMMARIZE( 70, 80 );
  UPDATE_COUNT_SUMMARIZE( 80, 90 );
  UPDATE_COUNT_SUMMARIZE( 90, 100 );
  UPDATE_COUNT_SUMMARIZE( 100, 200 );
  UPDATE_COUNT_SUMMARIZE( 200, 300 );
  UPDATE_COUNT_SUMMARIZE( 300, 400 );
  UPDATE_COUNT_SUMMARIZE( 400, 500 );
  UPDATE_COUNT_SUMMARIZE( 500, 600 );
  UPDATE_COUNT_SUMMARIZE( 600, 700 );
  UPDATE_COUNT_SUMMARIZE( 700, 800 );
  UPDATE_COUNT_SUMMARIZE( 800, 900 );
  UPDATE_COUNT_SUMMARIZE( 900, 1000 );
  UPDATE_COUNT_SUMMARIZE( 1000, 2000 );
  vp[ STAT_COUNT_SUMMARIZE_GEQ_2000 ] = gclib->count_sumrize_geq_2000_ms;

#define UPDATE_COUNT_MINOR_RUNS( lo, hi ) \
  vp[ STAT_COUNT_MINOR_RUNS_ ## lo ## _ ## hi ] = \
    gclib->count_minor_ ## lo ## _ ## hi ## _runs
  UPDATE_COUNT_MINOR_RUNS( 00, 02 );
  UPDATE_COUNT_MINOR_RUNS( 02, 04 );
  UPDATE_COUNT_MINOR_RUNS( 04, 06 );
  UPDATE_COUNT_MINOR_RUNS( 06, 08 );
  UPDATE_COUNT_MINOR_RUNS( 08, 10 );
  UPDATE_COUNT_MINOR_RUNS( 10, 20 );
  UPDATE_COUNT_MINOR_RUNS( 20, 30 );
  UPDATE_COUNT_MINOR_RUNS( 30, 40 );
  UPDATE_COUNT_MINOR_RUNS( 40, 50 );
  UPDATE_COUNT_MINOR_RUNS( 50, 60 );
  UPDATE_COUNT_MINOR_RUNS( 60, 70 );
  UPDATE_COUNT_MINOR_RUNS( 70, 80 );
  UPDATE_COUNT_MINOR_RUNS( 80, 90 );
  UPDATE_COUNT_MINOR_RUNS( 90, 100 );
  UPDATE_COUNT_MINOR_RUNS( 100, 200 );
  UPDATE_COUNT_MINOR_RUNS( 200, 300 );
  UPDATE_COUNT_MINOR_RUNS( 300, 400 );
  UPDATE_COUNT_MINOR_RUNS( 400, 500 );
  UPDATE_COUNT_MINOR_RUNS( 500, 600 );
  UPDATE_COUNT_MINOR_RUNS( 600, 700 );
  UPDATE_COUNT_MINOR_RUNS( 700, 800 );
  UPDATE_COUNT_MINOR_RUNS( 800, 900 );
  UPDATE_COUNT_MINOR_RUNS( 900, 1000 );
  UPDATE_COUNT_MINOR_RUNS( 1000, 2000 );
  vp[ STAT_COUNT_MINOR_RUNS_GEQ_2000 ] = gclib->count_minor_geq_2000_runs;

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
  vp[ STAT_FULL_GCTIME_CPU ] = gc->full_ms_collection_cpu;
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
  vp[ STAT_MAX_GCTIME ]     = gc->max_ms_collection;
  vp[ STAT_MAX_GCTIME_CPU ] = gc->max_ms_collection_cpu;

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

  /* GC event counters */
  /* FIXME: does not put moved_by_gc, moved_by_prom. */
  STAT_PUT_DWORD( vp, GCE_GCTIME, gce, gctime );
  STAT_PUT_DWORD( vp, GCE_PROMTIME, gce, promtime );
  STAT_PUT_DWORD( vp, GCE_FREE_UNUSED, gce, free_unused );
  STAT_PUT_DWORD( vp, GCE_ROOT_SCAN_GC, gce, root_scan_gc );
  STAT_PUT_DWORD( vp, GCE_ROOT_SCAN_PROM, gce, root_scan_prom );
  STAT_PUT_DWORD( vp, GCE_LOS_SWEEP_GC, gce, los_sweep_gc );
  STAT_PUT_DWORD( vp, GCE_LOS_SWEEP_PROM, gce, los_sweep_prom );
  STAT_PUT_DWORD( vp, GCE_REMSET_SCAN_GC, gce, remset_scan_gc );
  STAT_PUT_DWORD( vp, GCE_REMSET_SCAN_PROM, gce, remset_scan_prom );
  STAT_PUT_DWORD( vp, GCE_TOSPACE_SCAN_GC, gce, tospace_scan_gc );
  STAT_PUT_DWORD( vp, GCE_TOSPACE_SCAN_PROM, gce, tospace_scan_prom );
  STAT_PUT_DWORD( vp, GCE_RESET_AFTER_GC, gce, reset_after_gc );
  STAT_PUT_DWORD( vp, GCE_DECREMENT_AFTER_GC, gce, decrement_after_gc );
  STAT_PUT_DWORD( vp, GCE_DOF_REMSET_SCAN, gce, dof_remset_scan );
  STAT_PUT_DWORD( vp, GCE_SWEEP_SHADOW, gce, sweep_shadow );
  STAT_PUT_DWORD( vp, GCE_MSGC_MARK, gce, msgc_mark );
  STAT_PUT_DWORD( vp, GCE_SWEEP_DOF_SETS, gce, sweep_dof_sets );
  STAT_PUT_DWORD( vp, GCE_SWEEP_REMSET, gce, sweep_remset );
  STAT_PUT_DWORD( vp, GCE_SWEEP_LOS, gce, sweep_los );
  STAT_PUT_DWORD( vp, GCE_ASSIMILATE_PROM, gce, assimilate_prom );
  STAT_PUT_DWORD( vp, GCE_ASSIMILATE_GC, gce, assimilate_gc );
  STAT_PUT_WORD(  vp, GCE_COPIED_BY_GC, gce, copied_by_gc );
  STAT_PUT_WORD(  vp, GCE_COPIED_BY_PROM, gce, copied_by_prom );
  STAT_PUT_WORD(  vp, GCE_WORDS_FORWARDED, gce, words_forwarded );
  STAT_PUT_WORD(  vp, GCE_PTRS_FORWARDED, gce, ptrs_forwarded );
  STAT_PUT_WORD(  vp, GCE_GC_BARRIER_HIT, gce, gc_barrier_hit );
  STAT_PUT_WORD(  vp, GCE_REMSET_LO_SCANNED, gce, remset_large_objs_scanned );
  STAT_PUT_WORD(  vp, GCE_REMSET_LOW_SCANNED, gce, 
		  remset_large_obj_words_scanned );
  
  /* overall system stats */
  osdep_time_used( &real, &user, &system );
  osdep_pagefaults( &majflt, &minflt );

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
  gv[ STAT_G_PROMTIME_CPU ] = gs->ms_promotion_cpu;
  gv[ STAT_G_GCTIME ] = gs->ms_collection;
  gv[ STAT_G_GCTIME_CPU ] = gs->ms_collection_cpu;
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
  rv[ STAT_R_HSCAN_HI ] = rs->objs_scanned_hi;
  rv[ STAT_R_HSCAN_LO ] = rs->objs_scanned_lo;
  rv[ STAT_R_HREC_HI ] = rs->recorded_hi;
  rv[ STAT_R_HREC_LO ] = rs->recorded_lo;
  rv[ STAT_R_HREM_HI ] = rs->removed_hi;
  rv[ STAT_R_HREM_LO ] = rs->removed_lo;
  rv[ STAT_R_WSCAN_HI ] = rs->words_scanned_hi;
  rv[ STAT_R_WSCAN_LO ] = rs->words_scanned_lo;
  rv[ STAT_R_CLEARED ] = rs->cleared;
  rv[ STAT_R_SCANNED ] = rs->scanned;
  rv[ STAT_R_COMPACTED ] = rs->compacted;
  rv[ STAT_R_MAX_HSCAN ] = rs->max_objs_scanned;
  rv[ STAT_R_MAX_WSCAN ] = rs->max_words_scanned;
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

static void dump_gen_stats( FILE *f, gen_memstat_t *gs );
static void dump_remset_stats( FILE *f, remset_memstat_t *rs );
static void stats_dump_state_now( FILE *f );

bool stats_opendump( const char *filename )
{
  if (stats_state.dump_file != 0) 
    stats_closedump();

  stats_state.dump_file = fopen( filename, "w" );

  if (stats_state.dump_file) {
    fprintf( stats_state.dump_file, 
             "; RTS statistics, dumped by Larceny version %d.%d%s\n\n",
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
  if (stats_state.dump_file != 0) {
    stats_dump_state_now( stats_state.dump_file );
    fprintf( stats_state.dump_file, "\n" );
  }
}

void stats_dumpstate_stdout( void )
{
  stats_dump_state_now( stdout );
}

#define PRINT_DWORD( f, s, fld ) \
  fprintf( f, "%lu %lu ", \
           nativeuint( s->PASTE(fld,_hi) ), nativeuint( s->PASTE(fld,_lo) ) )

#define PRINT_WORD( f, s, fld ) \
  fprintf( f, "%lu ", nativeuint( s->fld ) )

static void stats_dump_state_now( FILE *f )
{
  assert( f != 0 );

  fprintf( f, "#(%d.%d ", larceny_major_version, larceny_minor_version );

  { stat_time_t user, system, real;
    unsigned minflt, majflt;

    osdep_time_used( &real, &user, &system );
    osdep_pagefaults( &majflt, &minflt );

    fprintf( f, "#(system_overall %lu %lu %lu %lu %lu) ",
             (unsigned long)(real.sec * 1000 + real.usec / 1000),
             (unsigned long)(system.sec * 1000 + system.usec  / 1000),
             (unsigned long)(user.sec * 1000 + user.usec / 1000),
             (unsigned long)minflt,
             (unsigned long)majflt );
  }
  
  { gc_memstat_t *s = &stats_state.gc_stats;

    fprintf( f, "#(gc_memstat_t " );
    PRINT_DWORD( f, s, allocated );
    PRINT_DWORD( f, s, reclaimed );
    PRINT_DWORD( f, s, objects_copied );
    PRINT_DWORD( f, s, words_copied );
    PRINT_DWORD( f, s, objects_moved );
    PRINT_DWORD( f, s, words_moved );
    PRINT_WORD( f, s, np_k );
    PRINT_WORD( f, s, np_j );
    PRINT_WORD( f, s, full_collections );
    PRINT_WORD( f, s, full_ms_collection );
    PRINT_WORD( f, s, full_ms_collection_cpu );
    PRINT_DWORD( f, s, full_objects_marked );
    PRINT_DWORD( f, s, full_words_marked );
    PRINT_DWORD( f, s, full_pointers_traced );
    fprintf( f, ") " );
  }

  { gclib_memstat_t *s = &stats_state.gclib_stats;

    fprintf( f, "#(gclib_memstat_t " );
    PRINT_WORD( f, s, heap_allocated );
    PRINT_WORD( f, s, heap_allocated_max );
    PRINT_WORD( f, s, remset_allocated );
    PRINT_WORD( f, s, remset_allocated_max );
    PRINT_WORD( f, s, rts_allocated );
    PRINT_WORD( f, s, rts_allocated_max );
    PRINT_WORD( f, s, heap_fragmentation );
    PRINT_WORD( f, s, heap_fragmentation_max );
    PRINT_WORD( f, s, mem_allocated );
    PRINT_WORD( f, s, mem_allocated_max );
    fprintf( f, ") " );
  }

  /* Print generations information */
  { int i;

    fprintf( f, "#(" );
    for ( i=0 ; i < stats_state.generations ; i++ )
      dump_gen_stats( f, &stats_state.gen_stats[i] );
    fprintf( f, ") " );
  }

  /* Print remembered sets information */
  { int i;

    fprintf( f, "#(" );
    for ( i = 0 ; i < stats_state.remsets ; i++ )
      dump_remset_stats( f, &stats_state.remset_stats[i] );
    fprintf( f, ") " );
  }

  { stack_memstat_t *s = &stats_state.stack_stats;

    fprintf( f, "#(stack_memstat_t " );
    PRINT_WORD( f, s, stacks_created );
    PRINT_DWORD( f, s, words_flushed );
    PRINT_DWORD( f, s, frames_flushed );
    PRINT_DWORD( f, s, frames_restored );
    fprintf( f, ") " );
  }

#if defined(SIMULATE_NEW_BARRIER)
  { swb_memstat_t *s = &stats_state.swb_stats;

    fprintf( f, "#(swb_memstat_t " );
    PRINT_WORD( f, s, total_assignments );
    PRINT_WORD( f, s, array_assignments );
    PRINT_WORD( f, s, lhs_young_or_remembered );
    PRINT_WORD( f, s, rhs_constant );
    PRINT_WORD( f, s, cross_gen_check );
    PRINT_WORD( f, s, transactions );
    fprintf( f, ") " );
  }
#else
  fprintf( f, "#(swb_memstat_t 0 0 0 0 0 0) " );
#endif
   
  { gc_event_memstat_t *s = &stats_state.gc_event_stats;
  
    fprintf( f, "#(gc_event_memstat_t " );
    PRINT_DWORD( f, s, gctime );
    PRINT_DWORD( f, s, promtime );
    PRINT_DWORD( f, s, free_unused );
    PRINT_DWORD( f, s, root_scan_gc );
    PRINT_DWORD( f, s, root_scan_prom );
    PRINT_DWORD( f, s, los_sweep_gc );
    PRINT_DWORD( f, s, los_sweep_prom );
    PRINT_DWORD( f, s, remset_scan_gc );
    PRINT_DWORD( f, s, remset_scan_prom );
    PRINT_DWORD( f, s, tospace_scan_gc );
    PRINT_DWORD( f, s, tospace_scan_prom );
    PRINT_DWORD( f, s, reset_after_gc );
    PRINT_DWORD( f, s, decrement_after_gc );
    PRINT_DWORD( f, s, dof_remset_scan );
    PRINT_DWORD( f, s, sweep_shadow );
    PRINT_DWORD( f, s, msgc_mark );
    PRINT_DWORD( f, s, sweep_dof_sets );
    PRINT_DWORD( f, s, sweep_remset );
    PRINT_DWORD( f, s, sweep_los );
    PRINT_DWORD( f, s, assimilate_prom );
    PRINT_DWORD( f, s, assimilate_gc );
    PRINT_WORD( f, s, copied_by_gc );
    PRINT_WORD( f, s, copied_by_prom );
    PRINT_WORD( f, s, words_forwarded );
    PRINT_WORD( f, s, ptrs_forwarded );
    PRINT_WORD( f, s, gc_barrier_hit );
    PRINT_WORD( f, s, remset_large_objs_scanned );
    PRINT_WORD( f, s, remset_large_obj_words_scanned );
    /* FIXME: These perhaps belong with the "copied" elements above, but
       I don't want to mess with the order because it'd break working
       code.  Perhaps fix in v0.50, when processing code can distinguish
       between the two layouts.  
       */
    PRINT_WORD( f, s, moved_by_gc );
    PRINT_WORD( f, s, moved_by_prom );
    fprintf( f, ") " );
  }
  fprintf( f, ")" );
  fflush( f );
}

static void dump_gen_stats( FILE *f, gen_memstat_t *s )
{
  fprintf( f, "#(gen_memstat_t " );
  PRINT_WORD( f, s, major_id );
  PRINT_WORD( f, s, minor_id );
  PRINT_WORD( f, s, target );
  PRINT_WORD( f, s, allocated );
  PRINT_WORD( f, s, used );
  PRINT_WORD( f, s, promotions );
  PRINT_WORD( f, s, collections );
  PRINT_WORD( f, s, ms_promotion );
  PRINT_WORD( f, s, ms_promotion_cpu );
  PRINT_WORD( f, s, ms_collection );
  PRINT_WORD( f, s, ms_collection_cpu );
  fprintf( f, ") " );
}

static void dump_remset_stats( FILE *f, remset_memstat_t *s )
{
  fprintf( f, "#(remset_memstat_t " );
  PRINT_WORD( f, s, major_id );  
  PRINT_WORD( f, s, minor_id );
  PRINT_WORD( f, s, allocated );
  PRINT_WORD( f, s, max_allocated );
  PRINT_WORD( f, s, used );
  PRINT_WORD( f, s, live );
  PRINT_DWORD( f, s, ssb_recorded );
  PRINT_DWORD( f, s, recorded );
  PRINT_DWORD( f, s, objs_scanned );
  PRINT_DWORD( f, s, words_scanned );
  PRINT_DWORD( f, s, removed );
  PRINT_WORD( f, s, cleared );
  PRINT_WORD( f, s, scanned );
  PRINT_WORD( f, s, compacted );
  fprintf( f, ") " );
}

/* eof */
