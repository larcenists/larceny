/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- run-time statistics.
 *
 * The stats module maintains run-time statistics.  Mainly, these are
 * statistics on memory use (bytes allocated and collected, amount of 
 * memory allocated to which parts the RTS, and so on) and running time
 * statistics (time spent in collector, time spent in any specific collector,
 * and so on).
 *
 * Statistics are calculated in two ways.  First, some parts of the RTS
 * are assumed to call the statistics module to signal events; for example,
 * the garbage collector calls the following callbacks:
 *
 *    stats_before_gc()      signals the start of a collection
 *    stats_gc_type()        reports the generation number and type
 *    stats_after_gc()       signals the end of a collection
 *    stats_add_gctime()     add GC time outside GC region (eg lazy sweep)
 *
 * Currently, the listed procedures are the only such callbacks.
 * Second, procedures in the stats module will call on procedures
 * in other parts of the RTS (notably gc->stats() and gclib_stats()) to
 * obtain information about those parts.
 *
 * Scheme code makes a callout to UNIX_getresourceusage().  That call 
 * eventually ends up in stats_fillvector(), which returns a vector of
 * stats info.
 *
 * There are some static data in this file, and in the longer term
 * a better solution must be found.  The statistics data don't really
 * belong with the collector, as their scope extends beyond the collector.
 * Probably, what should be done is create a 'statistics' ADT (object)
 * that can be passed around as needed, and which will contain all the
 * static data.
 *
 * Still to be done:
 * - record/report remembered set memory usage statistics
 * - record/report number of times a remset was cleared
 * - record/report overall memory usage statistics
 * - record/report low-level allocator free memory
 * - record/report peak memory usage (allocated) per generation & remset
 *
 * FIXME: this is poorly integrated with the conservative collector.
 *   Could split GC time into mark time and (lazy) sweep time.
 *
 * FIXME: the logic is too tangled.
 */

#include "config.h"

#include <sys/time.h>

#if defined(SUNOS4) || defined(SUNOS5)   /* Works in 4.x, 5.5, 5.6 */
/* For rusage() et al. */
#include <sys/resource.h>
#endif

#if defined(SUNOS5_3)                   /* Worked in 5.3 */
#include <sys/resource.h>
#include <sys/rusage.h>
#endif

#if defined(SUNOS4)
extern int gettimeofday( struct timeval *tp, struct timezone *tzp );
extern int getrusage( int who, struct rusage *rusage );
#endif

#if defined(BDW_GC)
#include "../bdw-gc/include/gc.h"
#endif

#include <stdio.h>
#include <memory.h>
#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "memmgr.h"
#include "gclib.h"
#include "heap_stats_t.h"

/* These are hardwired limits in the system. They should possibly be 
 * defined somewhere else.
 */
#define MAX_GENERATIONS 32

typedef struct {
  word hscanned_hi;      /* hash table */
  word hscanned_lo;      /*   entries scanned */
  word hrecorded_hi;     /* hash table */
  word hrecorded_lo;     /*   entries removed */
  word hremoved_hi;      /* hash table */
  word hremoved_lo;      /*   entries removed */
  word ssbrecorded_hi;   /* SSB transactions */
  word ssbrecorded_lo;   /*   recorded */
  word wscanned_hi;      /* Words of old */
  word wscanned_lo;      /*   objects scanned */
  word cleared;          /* Number of times remset was cleared */
} rem_stat_t;

typedef struct {
  word collections;      /* collections in this heap */
  word promotions;       /* promotions into this heap */
  word gctime;           /* total gc time in ms (including promotion) */
  word promtime;         /* total promotion time in ms */
  word target;           /* current allocation target (upper bound) */
  word alloc;            /* current allocation */
  unsigned np_old;       /* 1 iff NP 'old' generation */
  unsigned np_young;     /* 1 iff NP 'young' generation */
  unsigned np_k;         /* Number of steps (old+young together) */
  unsigned np_j;         /* Number of steps in young */
} gen_stat_t;

/* The structure of type sys_stat_t keeps statistics information both
 * running and at the present time.
 * All numbers in this structure are represented as fixnums.
 */

typedef struct {
  /* GC stuff - overall */
  word wallocated_hi;      /* total words */
  word wallocated_lo;      /*  allocated */
  word wcollected_hi;      /* total words */
  word wcollected_lo;      /*  reclaimed */
  word wcopied_hi;         /* total words */
  word wcopied_lo;         /*  copied */
  word wmoved_hi;          /* total words */
  word wmoved_lo;          /*  moved (LOS only, at this time) */
  word gctime;             /* total milliseconds GC time (includes promotion)*/
  word promtime;           /* total milliseconds promotion time */

  /* GC stuff - overall (preliminary) */
  gen_stat_t gen_stat[MAX_GENERATIONS]; /* Information about each generation */

  /* GC stuff - snapshot */
  word wlive;              /* current words live */
  word wmax_heap;          /* max words allocated to heap ever */
  word lastcollection_gen; /* generation of last collection */
  word lastcollection_type;/* type of last collection */

  /* Remembered sets - overall (preliminary) */
  rem_stat_t rem_stat[MAX_GENERATIONS]; /* info about each remset */
  word hscanned_hi;        /* hash table */
  word hscanned_lo;        /*   entries scanned */
  word hrecorded_hi;       /* hash table */
  word hrecorded_lo;       /*   entries recorded */
  word ssbrecorded_hi;     /* SSB entries */
  word ssbrecorded_lo;     /*   recorded */
  word remset_cleared;     /* number of times remsets cleared */
  
  /* Stacks - overall */
  word fflushed_hi;        /* number of stack */
  word fflushed_lo;        /*   frames flushed */
  word wflushed_hi;        /* words of stack */
  word wflushed_lo;        /*   frames flushed or copied */
  word stacks_created;     /* number of stacks created */
  word frestored_hi;       /* number of stack */
  word frestored_lo;       /*   frames restored */

  /* RTS memory use - snapshot */
  word wallocated_heap;    /* words allocated to heap areas */
  word wallocated_remset;  /* words allocated to remembered sets */
  word wallocated_rts;     /* words allocated to RTS "other" */

#if SIMULATE_NEW_BARRIER
  simulated_barrier_stats_t swb;   /* simulated barrier statistics */
#endif
  /* There Can Be Only One! */
  rem_stat_t np_remset;            /* Remembered-set statistics */
} sys_stat_t;


static sys_stat_t   memstats;             /* statistics */
static unsigned     rtstart;              /* time at startup (base time) */
static int          generations;          /* number of generations in gc */

static unsigned     allocated;            /* bytes alloc'd since last gc */
static unsigned     time_before_gc;       /* timestamp when starting gc */
static unsigned     live_before_gc;       /* live when starting a gc */
static heap_stats_t *heapstats_before_gc; /* stats before gc */
static heap_stats_t *heapstats_after_gc;  /* stats after last gc */
static gc_t *gc;                          /* The garbage collector */

static FILE *dumpfile = 0;                /* For stats dump */

static void current_statistics( heap_stats_t *, sys_stat_t * );
static heap_stats_t *make_heapstats( heap_stats_t *base );
static void add( unsigned *hi, unsigned *lo, unsigned x );
static void print_heapstats( heap_stats_t *stats );
static void dump_stats( heap_stats_t *stats, sys_stat_t *ms );

static void fill_main_entries( word *vp, sys_stat_t *ms );
static void fill_gen_vector( word *gv, gen_stat_t *gs, word live );
static void fill_remset_vector( word *rv, rem_stat_t *rs, int np_remset );

static void dump_gen_stats( FILE *dumpfile, gen_stat_t *gs, word live );
static void dump_remset_stats( FILE *dumpfile, rem_stat_t *rs );

/* if we're using the Boehm collector and it was compiled with 
 * REDIRECT_MALLOC (see bdw-gc/Makefile), then stats routines may
 * be called from the GC before initialization.  This flag lets us
 * ignore those calls.
 */

static int initialized = 0;

/* Initialize the stats module.
 *
 * 'generations' is the number of generations in the gc.
 * 'show_heapstats' is 1 if heap statistics should be printed at startup.
 */

void
stats_init( gc_t *collector, int gen, int show_heapstats )
{
  int i;

  assert( gen <= MAX_GENERATIONS );

  gc = collector;
  rtstart = stats_rtclock();
  generations = gen;

  heapstats_before_gc = make_heapstats( 0 );
  heapstats_after_gc  = make_heapstats( 0 );
  memset( &memstats, 0, sizeof( memstats ) );

  current_statistics( heapstats_after_gc, &memstats );

  if (show_heapstats) 
    print_heapstats( heapstats_after_gc );
  initialized = 1;
}


/* Collect statistics in anticipation of a garbage collection.
 *
 * Called by the memory manager at the very beginning of a collection.
 */

void
stats_before_gc( void )
{
  if (!initialized) return;

  time_before_gc = stats_rtclock();

  current_statistics( heapstats_before_gc, &memstats );
  live_before_gc = nativeint( memstats.wlive );

#if !defined(BDW_GC)
  allocated = heapstats_before_gc[0].live-heapstats_after_gc[0].live;
#else
  allocated = GC_get_bytes_since_gc();
#endif
}


/* Record collection type.
 *
 * Called by the actual collector code when the type and location of a 
 * collection has been determined.
 *
 * 'generation' is the generation number doing the copying.
 * 'type' is STATS_PROMOTE if it's a promotion, STATS_COLLECT if 
 * it's a collection.
 */

void
stats_gc_type( int generation, stats_gc_t type )
{
  if (!initialized) return;

  /* Just record the values; stats_gc_type may be called multiple times
     during a collection, and only the last set of values should be
     remembered.
   */
  memstats.lastcollection_gen = fixnum(generation);
  memstats.lastcollection_type = fixnum((word)type);
}


/* Wrap up statistics gathering after a garbage collection. */

void
stats_after_gc( void )
{
  unsigned gen;
  unsigned time;

  if (!initialized || memstats.lastcollection_type == fixnum(STATS_IGNORE))
    return;

  add( &memstats.wallocated_hi, &memstats.wallocated_lo,
      fixnum( allocated / sizeof(word) ) );

  if (memstats.lastcollection_type == fixnum(STATS_COLLECT))
    memstats.gen_stat[nativeint(memstats.lastcollection_gen)].collections
      += fixnum(1);
  else
    memstats.gen_stat[nativeint(memstats.lastcollection_gen)].promotions
      += fixnum(1);

  current_statistics( heapstats_after_gc, &memstats );
  time = stats_rtclock() - time_before_gc;

  add( &memstats.wcollected_hi, &memstats.wcollected_lo,
       fixnum( live_before_gc - nativeint(memstats.wlive) ) );

  gen = nativeint(memstats.lastcollection_gen);
  add( &memstats.wcopied_hi, &memstats.wcopied_lo,
      fixnum(heapstats_after_gc[gen].copied_last_gc) );

  add( &memstats.wmoved_hi, &memstats.wmoved_lo,
      fixnum(heapstats_after_gc[gen].moved_last_gc) );

  memstats.gctime += fixnum( time );
  memstats.gen_stat[gen].gctime += fixnum( time );
  if (memstats.lastcollection_type == fixnum(STATS_PROMOTE)) {
    memstats.promtime += fixnum( time );
    memstats.gen_stat[gen].promtime += fixnum( time );
  }

  dump_stats( heapstats_after_gc, &memstats );
}

void stats_add_gctime( long s, long ms )
{
  unsigned time = (unsigned)s*1000 + (unsigned)ms;

  memstats.gctime += fixnum( time );
  memstats.gen_stat[0].gctime += fixnum( time );
}

/* Get the current time in milliseconds since initialization */

unsigned
stats_rtclock( void )
{
  struct timeval t;
  struct timezone tz;

  if (gettimeofday( &t, &tz ) == -1)
    return -1;
  return (t.tv_sec * 1000 + t.tv_usec / 1000) - rtstart;
}


/* Fill stats vector with the statistics. */

word 
stats_fillvector( word w_buffer )
{
  static heap_stats_t *hs = 0;
  word allocated, genv, remv;
  sys_stat_t ms;
  int i, gen_generations, rem_remsets;

  if (hs == 0) hs = make_heapstats( 0 );
  current_statistics( hs, &memstats );

  ms = memstats;
  allocated = hs[0].live-heapstats_after_gc[0].live;
  add( &ms.wallocated_hi, &ms.wallocated_lo, fixnum(allocated/sizeof(word)));

  gen_generations = generations;  /* # of generation vectors */
  rem_remsets = generations;	  /* np remset is last elt. */

  assert( vector_length( w_buffer ) >= STAT_VSIZE );
  assert( vector_length(vector_ref(w_buffer, STAT_GENERATIONS)) 
	  >= gen_generations );
  assert( vector_length(vector_ref(w_buffer, STAT_REMSETS)) >= rem_remsets );

  fill_main_entries( ptrof(w_buffer)+1, &ms );

  /* Do generations */
  /* genv points to generation metavector */
  genv = vector_ref( w_buffer, STAT_GENERATIONS );
  for ( i=0 ; i < gen_generations ; i++ ) {
    word g = vector_ref(genv,i);

    assert( vector_length( g ) == STAT_G_SIZE );
    fill_gen_vector( ptrof(g)+1, &ms.gen_stat[i], hs[i].live);
  }

  /* Do remembered sets */
  /* remv points to remset metavector */
  remv = vector_ref( w_buffer, STAT_REMSETS );
  for ( i = 1 ; i <= rem_remsets ; i++ ) {
    int np_remset = 0;
    word r = vector_ref(remv,i-1);

    assert( vector_length(r) == STAT_R_SIZE );
    np_remset = i == rem_remsets;
    if (np_remset)
      fill_remset_vector( ptrof(r)+1, &ms.np_remset, 1 );
    else
      fill_remset_vector( ptrof(r)+1, &ms.rem_stat[i], 0 );
  }

  /* Useful as a hint only */
  vector_set( w_buffer, STAT_NPREMSET_P, TRUE_CONST );

  return w_buffer;
}

static void
fill_main_entries( word *vp, sys_stat_t *ms )
{
#if !defined(STDC_SOURCE)
  struct rusage buf;
#endif
  unsigned usertime, systime, minflt, majflt;

  vp[ STAT_WALLOCATED_HI ] = ms->wallocated_hi;
  vp[ STAT_WALLOCATED_LO ] = ms->wallocated_lo;
#if !defined(BDW_GC)
  vp[ STAT_WCOLLECTED_HI ] = ms->wcollected_hi;
  vp[ STAT_WCOLLECTED_LO ] = ms->wcollected_lo;
  vp[ STAT_WCOPIED_HI ]    = ms->wcopied_hi;
  vp[ STAT_WCOPIED_LO ]    = ms->wcopied_lo;
  vp[ STAT_WMOVED_HI ]     = ms->wmoved_hi;
  vp[ STAT_WMOVED_LO ]     = ms->wmoved_lo;
#endif
  vp[ STAT_GCTIME ]        = ms->gctime;
  vp[ STAT_PROMTIME ]      = ms->promtime;
  vp[ STAT_WLIVE ]         = ms->wlive;
  vp[ STAT_MAX_HEAP ]      = ms->wmax_heap;
  vp[ STAT_LAST_GEN ]      = ms->lastcollection_gen;
  vp[ STAT_LAST_TYPE ]     = (ms->lastcollection_type == STATS_COLLECT 
			       ? fixnum(0) 
			       : fixnum(1));
  vp[ STAT_FFLUSHED_HI ]   = ms->fflushed_hi;
  vp[ STAT_FFLUSHED_LO ]   = ms->fflushed_lo;
  vp[ STAT_WFLUSHED_HI ]   = ms->wflushed_hi;
  vp[ STAT_WFLUSHED_LO ]   = ms->wflushed_lo;
  vp[ STAT_STK_CREATED ]   = ms->stacks_created;
  vp[ STAT_FRESTORED_HI ]  = ms->frestored_hi;
  vp[ STAT_FRESTORED_LO ]  = ms->frestored_lo;
  vp[ STAT_WORDS_HEAP ]    = ms->wallocated_heap;
  vp[ STAT_WORDS_REMSET ]  = ms->wallocated_remset;
  vp[ STAT_WORDS_RTS ]     = ms->wallocated_rts;

#if SIMULATE_NEW_BARRIER
  /* If we're not simulating the new barrier, the values will be 0 */
  vp[ STAT_SWB_ASSIGN ] = ms->swb.array_assignments;
  vp[ STAT_SWB_LHS_OK ] = ms->swb.lhs_young_or_remembered;
  vp[ STAT_SWB_RHS_CONST ] = ms->swb.rhs_constant;
  vp[ STAT_SWB_NOTXGEN ] = ms->swb.cross_gen_check;
  vp[ STAT_SWB_TRANS ] = ms->swb.transactions;
#endif

#if !defined(STDC_SOURCE)
  getrusage( RUSAGE_SELF, &buf );

#if defined(SUNOS4) || defined(SUNOS5)  /* 4.x, 5.5, 5.6 */
  systime = fixnum( buf.ru_stime.tv_sec * 1000 + buf.ru_stime.tv_usec / 1000);
  usertime = fixnum( buf.ru_utime.tv_sec * 1000 + buf.ru_utime.tv_usec / 1000);
#endif

#if defined(SUNOS5_3)  /* 5.3 */
  systime = fixnum( buf.ru_stime.tv_sec*1000 + buf.ru_stime.tv_nsec/1000000);
  usertime = fixnum( buf.ru_utime.tv_sec*1000 + buf.ru_utime.tv_nsec/1000000);
#endif

  majflt = fixnum( buf.ru_majflt );
  minflt = fixnum( buf.ru_minflt );
#else
  systime = usertime = minflt = majflt = 0;
#endif

  vp[ STAT_RTIME ]         = fixnum( stats_rtclock() );
  vp[ STAT_STIME ]         = systime;
  vp[ STAT_UTIME ]         = usertime;
  vp[ STAT_MINFAULTS ]     = minflt;
  vp[ STAT_MAJFAULTS ]     = majflt;
}


static void
fill_gen_vector( word *gv, gen_stat_t *gs, word live )
{
  gv[ STAT_G_GC_COUNT ] = gs->collections;
  gv[ STAT_G_PROM_COUNT ] = gs->promotions;
  gv[ STAT_G_GCTIME ] = gs->gctime;
  gv[ STAT_G_WLIVE ] = fixnum( live/4 );
  gv[ STAT_G_NP_YOUNGP ] = gs->np_young;
  gv[ STAT_G_NP_OLDP ] = gs->np_old;
  gv[ STAT_G_NP_J ] = gs->np_j;
  gv[ STAT_G_NP_K ] = gs->np_k;
  gv[ STAT_G_ALLOC ] = gs->alloc;
  gv[ STAT_G_TARGET ] = gs->target;
  gv[ STAT_G_PROMTIME ] = gs->promtime;
}


static void
fill_remset_vector( word *rv, rem_stat_t *rs, int np_remset )
{
  rv[ STAT_R_APOOL ] = 0;  /* FIXME */
  rv[ STAT_R_UPOOL ] = 0;  /* FIXME */
  rv[ STAT_R_AHASH ] = 0;  /* FIXME */
  rv[ STAT_R_UHASH ] = 0;  /* FIXME */
  rv[ STAT_R_HREC_HI ] = rs->hrecorded_hi;
  rv[ STAT_R_HREC_LO ] = rs->hrecorded_lo;
  rv[ STAT_R_HREM_HI ] = rs->hremoved_hi;
  rv[ STAT_R_HREM_LO ] = rs->hremoved_lo;
  rv[ STAT_R_HSCAN_HI ] = rs->hscanned_hi;
  rv[ STAT_R_HSCAN_LO ] = rs->hscanned_lo;
  rv[ STAT_R_WSCAN_HI ] = rs->wscanned_hi;
  rv[ STAT_R_WSCAN_LO ] = rs->wscanned_lo;
  rv[ STAT_R_SSBREC_HI ] = rs->ssbrecorded_hi;
  rv[ STAT_R_SSBREC_LO ] = rs->ssbrecorded_lo;
  rv[ STAT_R_NP_P ] = np_remset ? TRUE_CONST : FALSE_CONST;
}

static void
print_heapstats( heap_stats_t *stats )
{
  int i;

  consolemsg( "Heap statistics:");
  for ( i=0; i < generations ; i++ ) {
    consolemsg( "Generation %d", i );
    consolemsg( "  Size of semispace 1: %lu bytes", stats[i].semispace1 );
    consolemsg( "  Size of semispace 2: %lu bytes", stats[i].semispace2 );
    consolemsg( "  Live data: %lu bytes", stats[i].live );
    if (stats[i].stack || i==0)
      consolemsg( "  Live stack: %lu bytes", stats[i].stack );
    /* FIXME: also: text/static area, if there's one. */
  }
}


/* Dump the stats to an output stream, on an s-expression format.
 * Format: see the banner string below.
 */
static char *dump_banner =
"; RTS statistics, dumped by Larceny version %d.%d\n;\n"
"; The output is an s-expression, one following each GC.  When some entry\n"
"; is advertised as xxx-hi and xxx-lo, then the -hi is the high 29 bits\n"
"; and the -lo is the low 29 bits of a 58-bit unsigned integer.\n"
"; All times are in milliseconds.\n;\n"
"; First there is some general information (running totals):\n"
";\n"
";  (words-allocated-hi words-allocated-lo\n"
";   words-reclaimed-hi words-reclaimed-lo\n"
";   words-copied-hi words-copied-lo\n"
";   gctime\n"
";\n"
"; The next entries are snapshots at the last GC; the gc type\n"
"; is a symbol, \"collect\" or \"promote\":\n"
";\n"
";   words-live generation-of-last-gc type-of-last-gc\n"
";\n"
"; Then follows a list of sublists of generation info,\n"
"; ordered from younger to older; words-live is snapshot data:\n"
";\n"
";   ((collections promotions-into gctime words-live np-young-p np-old-p\n"
";     np-j np-k) ...)\n"
";\n"
"; Then follows a list of sublists of remembered set info,\n"
"; ordered from younger to older (each remset is associated\n"
"; with one old generation).  First the snapshot entries:\n"
";\n"
";   ((words-allocated-to-pool words-used-in-pool\n"
";     hash-table-total-entries hash-table-non-null-entries\n"
";\n"
"; Then there are running totals:\n"
";\n"
";     hash-entries-recorded-hi hash-entries-recorded-lo\n"
";     hash-entries-removed-hi hash-entries-recorded-lo\n"
";     hash-entries-scanned-hi hash-entries-scanned-lo\n"
";     words-of-oldspace-scanned-hi words-of-oldspace-scanned-lo\n"
";     ssb-entries-recorded-hi ssb-entries-recorded-lo\n"
";    )...)\n"
";\n"
"; Then follows information about the stack cache (running totals):\n"
";\n"
";   frames-flushed-hi frames-flushed-lo words-flushed-hi words-flushed-lo\n"
";   stacks-created frames-restored-hi frames-restored-lo\n"
";\n"
"; Then there is information about overall memory use (snapshots):\n"
";\n"
";   words-allocated-heap words-allocated-remset words-allocated-rts-other\n"
";\n"
"; Then there are some fields added in v0.32 that belong somewhere else:\n"
";   words-moved-hi words-moved-lo\n"
";\n"
"; The tail of the list is an association list, where the entries that\n"
"; are dumped depend on how the system was compiled.  Each entry is a list\n"
"; where the car is the tag and the cdr is a list of data, as shown.\n"
";\n"
"; Tag=`swb': simulated write barrier profile data\n;\n"
";   array-assignments\n"
";   lhs-young-or-remembered rhs-constant not-cross-gen\n"
";   array-transactions\n"
";\n"
"; Tag=`np-remset': non-predictive collector's extra remembered set.\n"
";   This data is as for the remembered sets, above.\n"
";  )\n";

int
stats_opendump( const char *filename )
{
  if (dumpfile != 0) stats_closedump();

  dumpfile = fopen( filename, "w" );

  if (dumpfile)
    fprintf( dumpfile, dump_banner,
	     larceny_major_version, larceny_minor_version );
  return dumpfile != 0;
}


void
stats_closedump( void )
{
  if (dumpfile == 0) return;

  fclose( dumpfile );
  dumpfile = 0;
}


static void
dump_stats( heap_stats_t *stats, sys_stat_t *ms )
{
  int i;

  if (dumpfile == 0) return;

  fprintf( dumpfile, "(" );

  /* Print overall memory and GC information */
  fprintf( dumpfile, "%lu %lu %lu %lu %lu %lu %lu %lu %lu %s ",
	   nativeint( ms->wallocated_hi ),
	   nativeint( ms->wallocated_lo ),
	   nativeint( ms->wcollected_hi ),
	   nativeint( ms->wcollected_lo ),
	   nativeint( ms->wcopied_hi ),
	   nativeint( ms->wcopied_lo ),
	   nativeint( ms->gctime ),
	   nativeint( ms->wlive ),
	   nativeint( ms->lastcollection_gen ),
	   nativeint( ms->lastcollection_type ) == STATS_COLLECT 
	     ? "collect"
	     : "promote" );


  /* Print generations information */

  fprintf( dumpfile, "(" );
  for ( i=0 ; i < generations ; i++ )
    dump_gen_stats( dumpfile, &ms->gen_stat[i], stats[i].live );
  fprintf( dumpfile, ") " );


  /* Print remembered sets information */

  fprintf( dumpfile, "(" );
  for ( i = 1 ; i < generations ; i++ )
    dump_remset_stats( dumpfile, &ms->rem_stat[i] );
  fprintf( dumpfile, ") " );


  /* Print stack information */

  fprintf( dumpfile, "%lu %lu %lu %lu %lu %lu %lu ",
	   nativeint( ms->fflushed_hi ), 
	   nativeint( ms->fflushed_lo ),
	   nativeint( ms->wflushed_hi ),
	   nativeint( ms->wflushed_lo ),
	   nativeint( ms->stacks_created ),
	   nativeint( ms->frestored_hi ),
	   nativeint( ms->frestored_lo ) );

  /* Print overall heap information */
  fprintf( dumpfile, "%lu %lu %lu ",
	   nativeint( ms->wallocated_heap ),
	   nativeint( ms->wallocated_remset ),
	   nativeint( ms->wallocated_rts ) );

  /* New fields for v0.32 */
  fprintf( dumpfile, "%lu %lu ",
	   nativeint( ms->wmoved_hi ), nativeint( ms->wmoved_lo ) );

#if SIMULATE_NEW_BARRIER
  fprintf( dumpfile, "(swb . (%lu %lu %lu %lu %lu))",
	   nativeint( ms->swb.array_assignments ),
	   nativeint( ms->swb.lhs_young_or_remembered ),
	   nativeint( ms->swb.rhs_constant ),
	   nativeint( ms->swb.cross_gen_check ),
	   nativeint( ms->swb.transactions ) );
#endif

  /* FIXME: should depend on the presence or absence of the remset */
  fprintf( dumpfile, "(np-remset . " );
  dump_remset_stats( dumpfile, &ms->np_remset );
  fprintf( dumpfile, ")" );

  fprintf( dumpfile, ")\n" );
}

static void
dump_gen_stats( FILE *dumpfile, gen_stat_t *gs, word live )
{
  fprintf( dumpfile, "(%lu %lu %lu %lu %lu %lu %lu %lu) ",
	  nativeint( gs->collections ),
	  nativeint( gs->promotions ),
	  nativeint( gs->gctime ),
	  live/4,
	  nativeint( gs->np_young ),
	  nativeint( gs->np_old ),
	  nativeint( gs->np_j ),
	  nativeint( gs->np_k ) 
	  );
}


static void
dump_remset_stats( FILE *dumpfile, rem_stat_t *rs )
{
  fprintf( dumpfile, "(%lu %lu %lu %lu ",
	  0,			/* FIXME */
	  0,			/* FIXME */
	  0,			/* FIXME */
	  0			/* FIXME */
	  );
  fprintf( dumpfile, "%lu %lu %lu %lu %lu %lu %lu %lu %lu %lu) ",
	  nativeint( rs->hrecorded_hi ),
	  nativeint( rs->hrecorded_lo ),
	  nativeint( rs->hremoved_hi ),
	  nativeint( rs->hremoved_lo ),
	  nativeint( rs->hscanned_hi ),
	  nativeint( rs->hscanned_lo ),
	  nativeint( rs->wscanned_hi ),
	  nativeint( rs->wscanned_lo ),
	  nativeint( rs->ssbrecorded_hi ),
	  nativeint( rs->ssbrecorded_lo ) );
}


/* Get the current heap statistics.
 *
 * This is moderately hairy -- some statistics are accumulated, and some
 * are just updated.  All stats are kept as fixnums.
 */

static void
current_statistics( heap_stats_t *stats, sys_stat_t *ms )
{
  int i;
  word bytes_live = 0;
  word ssb_recorded = 0;
  word hash_recorded = 0;
  word hash_scanned = 0;
  word frames_flushed = 0;
  word frames_restored = 0;
  word bytes_flushed = 0;
  word stacks_created = 0;

  for ( i=0 ; i < generations ; i++ ) {
    gc->stats( gc, i, &stats[i] );
    bytes_live += stats[i].live;
    ssb_recorded += stats[i].ssb_recorded;
    hash_recorded += stats[i].hash_recorded;
    hash_scanned += stats[i].hash_scanned;
    frames_flushed += stats[i].frames_flushed;
    bytes_flushed += stats[i].bytes_flushed;
    stacks_created += stats[i].stacks_created;
    frames_restored += stats[i].frames_restored;
    add( &ms->rem_stat[i].hrecorded_hi, &ms->rem_stat[i].hrecorded_lo,
	 fixnum( stats[i].hash_recorded ));
    add( &ms->rem_stat[i].hscanned_hi, &ms->rem_stat[i].hscanned_lo,
	 fixnum( stats[i].hash_scanned ));
    add( &ms->rem_stat[i].hremoved_hi, &ms->rem_stat[i].hremoved_lo,
	 fixnum( stats[i].hash_removed ));
    add( &ms->rem_stat[i].ssbrecorded_hi, &ms->rem_stat[i].ssbrecorded_lo,
	 fixnum( stats[i].ssb_recorded ));
    add( &ms->rem_stat[i].wscanned_hi, &ms->rem_stat[i].wscanned_lo,
	 fixnum( stats[i].words_scanned ));
    ms->rem_stat[i].cleared += fixnum(0);  /* FIXME */

    /* Non-predictive collector */
    ms->gen_stat[i].np_young = fixnum(stats[i].np_young);
    ms->gen_stat[i].np_old = fixnum(stats[i].np_old);
    ms->gen_stat[i].np_j = fixnum(stats[i].np_j);
    ms->gen_stat[i].np_k = fixnum(stats[i].np_k);

    /* Policy & allocation */
    ms->gen_stat[i].target = fixnum(stats[i].target/sizeof(word));
    ms->gen_stat[i].alloc = fixnum((stats[i].semispace1+stats[i].semispace2)/
				   sizeof(word));

    if (stats[i].np_young) {  /* Entries valid exactly when np_young==1 */
      add( &ms->np_remset.hrecorded_hi, &ms->np_remset.hrecorded_lo,
	  fixnum( stats[i].np_hash_recorded ));
      add( &ms->np_remset.hscanned_hi, &ms->np_remset.hscanned_lo,
	  fixnum( stats[i].np_hash_scanned ));
      add( &ms->np_remset.hremoved_hi, &ms->np_remset.hremoved_lo,
	  fixnum( stats[i].np_hash_removed ));
      add( &ms->np_remset.ssbrecorded_hi, &ms->np_remset.ssbrecorded_lo,
	  fixnum( stats[i].np_ssb_recorded ));
      add( &ms->np_remset.wscanned_hi, &ms->np_remset.wscanned_lo,
	  fixnum( stats[i].np_words_scanned ));
      ms->np_remset.cleared += fixnum(0);  /* FIXME */
    }
  }

  /* Overall statistics */
  ms->wlive = fixnum(bytes_live / sizeof(word));
  add( &ms->ssbrecorded_hi, &ms->ssbrecorded_lo, fixnum( ssb_recorded ) );
  add( &ms->hrecorded_hi, &ms->hrecorded_lo, fixnum( hash_recorded ) );
  add( &ms->hscanned_hi, &ms->hscanned_lo, fixnum( hash_scanned ) );
  add( &ms->fflushed_hi, &ms->fflushed_lo, fixnum( frames_flushed ) );
  add( &ms->wflushed_hi, &ms->wflushed_lo, fixnum(bytes_flushed/sizeof(word)));
  add( &ms->frestored_hi, &ms->frestored_lo, fixnum( frames_restored ) );
  ms->stacks_created += fixnum( stacks_created );
  { unsigned heap, remset, rts, max_heap;

    gclib_stats( &heap, &remset, &rts, &max_heap );
    ms->wallocated_heap = fixnum(heap);
    ms->wallocated_remset = fixnum(remset);
    ms->wallocated_rts = fixnum(rts);
    ms->wmax_heap = fixnum(max_heap);
  }

#if SIMULATE_NEW_BARRIER
  { simulated_barrier_stats_t s;
    simulated_barrier_stats( &s );
    ms->swb.array_assignments += fixnum(s.array_assignments);
    ms->swb.lhs_young_or_remembered += fixnum(s.lhs_young_or_remembered);
    ms->swb.rhs_constant += fixnum(s.rhs_constant);
    ms->swb.cross_gen_check += fixnum(s.cross_gen_check);
    ms->swb.transactions += fixnum(s.transactions);
  }
#endif
}


static heap_stats_t *
make_heapstats( heap_stats_t *base )
{
  heap_stats_t *p;

  p = (heap_stats_t*)must_malloc( generations*sizeof(heap_stats_t) );
  if (base == 0)
    memset( p, 0, sizeof( heap_stats_t )*generations );
  else
    memcpy( p, base, sizeof( heap_stats_t )*generations );
  return p;
}


/* Adds a word to a doubleword with carry propagation, both parts of
 * the doubleword are independently represented as fixnums, as is 'x'.
 */

#define LARGEST_FIXNUM (2147483644L)  /* ((2^29)-1)*4 */

static void
add( unsigned *hi, unsigned *lo, unsigned x )
{
  assert((x & 3) == 0);
  *lo += x;
  if (*lo > LARGEST_FIXNUM) {
    *lo -= LARGEST_FIXNUM;
    *hi += 4;
  }
}

/* eof */
