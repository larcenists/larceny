/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Run-time statistics processing.
 *
 * Statistics are calculated in two ways.  First, some parts of the RTS
 * are assumed to call the statistics module to signal events.  The
 * following callbacks are used by the garbage collector:
 *
 *    stats_before_gc()      signals the start of a collection
 *    stats_gc_type()        reports the generation number and type
 *    stats_after_gc()       signals the end of a collection
 *    stats_add_gctime()     add GC time outside GC region (eg lazy sweep)
 *
 * Second, procedures in the stats module will call on procedures
 * in other parts of the RTS (notably gc_stats() and gclib_stats()) to
 * obtain information about those parts.
 *
 * Scheme code makes a callout to primitive_get_stats(), passing a Scheme
 * vector that is to be filled with resource data.  That call eventually
 * ends up in stats_fillvector(), which performs the filling.
 *
 * There are some static data in this file, and in the longer term
 * a better solution must be found.  The statistics data don't really
 * belong with the collector, as their scope extends beyond the collector.
 * Probably, what should be done is create a 'statistics' ADT (object)
 * that can be passed around as needed, and which will contain all the
 * static data.
 *
 * Still to be done:
 * - record/report peak memory usage (allocated) per generation
 * - record/report overall memory usage statistics
 * - record/report low-level allocator free memory
 *
 * FIXME: the logic is too tangled.
 * FIXME: integrate better with BDW collector.
 */

#include <stdio.h>
#include <string.h>

#include "config.h"
#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "memmgr.h"
#include "gclib.h"
#include "heap_stats_t.h"
#if defined(BDW_GC)
# include "../bdw-gc/include/gc.h"
#endif

/* All "word" entries in rem_stat_t, gen_stat_t, and sys_stat_t
 * are Scheme fixnums.
 */
typedef struct {
  word identity;         /* who am i? */
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
  word scanned;		 /* Number of times remset was scanned */
  word compacted;        /* Number of times SSB was compacted */
  word hallocated;	 /* Words allocated to hash table */
  word pallocated;       /* Words allocated to node pool */
  word pused;            /* Words used in node pool */
  word plive;		 /* Words live in node pool */
  word max_size;	 /* Max words allocated in hash+pool */
} rem_stat_t;

typedef struct {
  word collections;      /* collections in this heap */
  word promotions;       /* promotions into this heap */
  word gctime;           /* total gc time in ms (including promotion) */
  word promtime;         /* total promotion time in ms */
  word target;           /* current allocation target (upper bound) */
  word alloc;            /* current allocation */
  word np_old;           /* 1 iff NP 'old' generation */
  word np_young;         /* 1 iff NP 'young' generation */
  word np_k;             /* Number of steps (old+young together) */
  word np_j;             /* Number of steps in young */
} gen_stat_t;

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

  /* Remembered sets */
  rem_stat_t rem_stat[MAX_GENERATIONS]; /* info about each remset */
  
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

#if defined(SIMULATE_NEW_BARRIER)
  struct {
    word total_assignments;
    word array_assignments;
    word lhs_young_or_remembered;
    word rhs_constant;
    word cross_gen_check;
    word transactions;
  } swb;
#endif

  /* Deferred-oldest-first collector */
  struct {
    word promotions;
    word collections;
    word resets;
    word repeats;
    word full_collections;
    word bytes_promoted_hi;
    word bytes_promoted_lo;
    word objs_marked_hi;
    word objs_marked_lo;
    word ptrs_traced_hi;
    word ptrs_traced_lo;
    word entries_removed_hi;
    word entries_removed_lo;
  } dof;

  /* There Can Be Only One! */
  rem_stat_t np_remset;            /* Remembered-set statistics */
} sys_stat_t;

#define STATS_COLLECT   (fixnum(0)) /* Externally visible */
#define STATS_PROMOTE   (fixnum(1)) /* Externally visible */
#define STATS_FULL      (fixnum(2)) /* Externally visible */

static sys_stat_t   memstats;             /* statistics */
static int          generations;          /* number of generations in gc */

static unsigned     allocated;            /* bytes alloc'd since last gc */
static unsigned     time_before_gc;       /* timestamp when starting gc */
static unsigned     live_before_gc;       /* live when starting a gc */
static heap_stats_t *heapstats_before_gc; /* stats before gc */
static heap_stats_t *heapstats_after_gc;  /* stats after last gc */
static bool         stats_after_gc_pending = FALSE;
static gc_t *gc;                          /* The garbage collector */

static FILE *dumpfile = 0;                /* For stats dump */

static void current_statistics( heap_stats_t *, sys_stat_t * );
static heap_stats_t *make_heapstats( void );
static void add( unsigned *hi, unsigned *lo, unsigned x );
static void print_heapstats( heap_stats_t *stats );
static void dump_stats( heap_stats_t *stats, sys_stat_t *ms );

static void fill_main_entries( word *vp, sys_stat_t *ms );
static void fill_gen_vector( word *gv, gen_stat_t *gs, word live );
static void fill_remset_vector( word *rv, rem_stat_t *rs, int np_remset );

static void dump_gen_stats( FILE *dumpfile, gen_stat_t *gs, word live );
static void dump_remset_stats( FILE *dumpfile, rem_stat_t *rs );

/* If we're using the Boehm collector and it was compiled with 
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
stats_init( gc_t *collector, int gens, int show_heapstats )
{
  int i;

  gc = collector;
  generations = gens;

  heapstats_before_gc = make_heapstats();
  heapstats_after_gc  = make_heapstats();
  memset( &memstats, 0, sizeof( memstats ) );

  for ( i=0 ; i < generations ; i++ )
    memstats.rem_stat[i].identity = -1;

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

  assert( !stats_after_gc_pending ); /* Don't nest. */
  stats_after_gc_pending = TRUE;

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
 * 'generation' is the generation number doing the collection.
 */

void
stats_gc_type( int generation, gc_type_t type )
{
  if (!initialized) return;

  /* Just record the values; stats_gc_type may be called multiple times
     during a collection, and only the last set of values should be
     remembered.
   */
  memstats.lastcollection_gen = fixnum(generation);
  switch (type) {
    case GCTYPE_COLLECT : memstats.lastcollection_type = STATS_COLLECT; break;
    case GCTYPE_PROMOTE : memstats.lastcollection_type = STATS_PROMOTE; break;
    case GCTYPE_FULL    : memstats.lastcollection_type = STATS_FULL; break;
    default             : panic( "stats_gc_type: %d", type );
  }
}


/* Wrap up statistics gathering after a garbage collection. */

void
stats_after_gc( void )
{
  unsigned gen;
  unsigned time;

  if (!initialized )
    return;

  assert( stats_after_gc_pending );
  stats_after_gc_pending = FALSE;

  /* FIXME: we need to compute the slot in the memstats
     structure where the remembered set with the identity
     of this generation lives... 
     */
  gen = nativeint(memstats.lastcollection_gen);

  add( &memstats.wallocated_hi, &memstats.wallocated_lo,
      fixnum( allocated / sizeof(word) ) );

  if (memstats.lastcollection_type == STATS_PROMOTE)
    memstats.gen_stat[gen].promotions += fixnum(1);
  else
    /* Collections and full collections, FIXME */
    memstats.gen_stat[gen].collections += fixnum(1);

  current_statistics( heapstats_after_gc, &memstats );
  time = stats_rtclock() - time_before_gc;

  add( &memstats.wcollected_hi, &memstats.wcollected_lo,
       fixnum( live_before_gc - nativeint(memstats.wlive) ) );

  add( &memstats.wcopied_hi, &memstats.wcopied_lo,
      fixnum(heapstats_after_gc[gen].copied_last_gc / sizeof(word)) );

  add( &memstats.wmoved_hi, &memstats.wmoved_lo,
      fixnum(heapstats_after_gc[gen].moved_last_gc / sizeof(word)) );

  memstats.gctime += fixnum( time );
  memstats.gen_stat[gen].gctime += fixnum( time );
  if (memstats.lastcollection_type == STATS_PROMOTE) {
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

/* Fill stats vector with the statistics. */

word 
stats_fillvector( word w_buffer )
{
  static heap_stats_t *hs = 0;
  word allocated, genv, remv;
  sys_stat_t ms;
  int i, gen_generations, rem_remsets;

  if (hs == 0) hs = make_heapstats();
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

  /* FIXME: Can do better here: if gc->np_remset == -1 then #f else #t. */
  vector_set( w_buffer, STAT_NPREMSET_P, TRUE_CONST );

  return w_buffer;
}

static void
fill_main_entries( word *vp, sys_stat_t *ms )
{
  stat_time_t user, system, real;
  unsigned minflt, majflt;

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
  vp[ STAT_LAST_TYPE ]     = ms->lastcollection_type;
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

#if defined(SIMULATE_NEW_BARRIER)
  /* If we're not simulating the new barrier, the values will be 0 */
  vp[ STAT_SWB_TOTAL ] = ms->swb.total_assignments;
  vp[ STAT_SWB_ASSIGN ] = ms->swb.array_assignments;
  vp[ STAT_SWB_LHS_OK ] = ms->swb.lhs_young_or_remembered;
  vp[ STAT_SWB_RHS_CONST ] = ms->swb.rhs_constant;
  vp[ STAT_SWB_NOTXGEN ] = ms->swb.cross_gen_check;
  vp[ STAT_SWB_TRANS ] = ms->swb.transactions;
#endif

  stats_time_used( &real, &user, &system );
  stats_pagefaults( &majflt, &minflt );

  vp[ STAT_RTIME ]         = fixnum( real.sec * 1000 + real.usec / 1000 );
  vp[ STAT_STIME ]         = fixnum( system.sec * 1000 + system.usec  / 1000 );
  vp[ STAT_UTIME ]         = fixnum( user.sec * 1000 + user.usec / 1000);
  vp[ STAT_MINFAULTS ]     = fixnum( minflt );
  vp[ STAT_MAJFAULTS ]     = fixnum( majflt );
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
  rv[ STAT_R_POOL_ALLOC ] = rs->pallocated;
  rv[ STAT_R_POOL_USED ] = rs->pused;
  rv[ STAT_R_POOL_LIVE ] = rs->plive;
  rv[ STAT_R_HASH_ALLOC ] = rs->hallocated;
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
  rv[ STAT_R_CLEARED ] = rs->cleared;
  rv[ STAT_R_SCANNED ] = rs->scanned;
  rv[ STAT_R_COMPACTED ] = rs->compacted;
  rv[ STAT_R_MAX_SIZE ] = rs->max_size;
}


static void
print_heapstats( heap_stats_t *stats )
{
  int i;

  consolemsg( "Heap statistics:");
  for ( i=0; i < generations ; i++ ) {
    if (i == generations-1 && gc->static_area != 0) {
      /* Static area */
      consolemsg( "Generation %d (static)", i );
      consolemsg( "  Size of text area: %lu bytes", stats[i].semispace1 );
      consolemsg( "  Size of data area: %lu bytes", stats[i].semispace2 );
    }
    else {
      /* Any other */
      consolemsg( "Generation %d", i );
      consolemsg( "  Size of semispace 1: %lu bytes", stats[i].semispace1 );
      consolemsg( "  Size of semispace 2: %lu bytes", stats[i].semispace2 );
      if (stats[i].stack || i==0)
	consolemsg( "  Live stack: %lu bytes", stats[i].stack );
    }
    consolemsg( "  Live data: %lu bytes", stats[i].live );
  }
}


/* Dump the stats to an output stream, on an s-expression format.
 * Format: see the banner string below.
 */
static char *dump_banner =
"; RTS statistics, dumped by Larceny version %d.%d%s\n;\n"
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
";   ((pool-words-allocated pool-words-used pool-words-live\n"
";     hash-entries-allocated\n"
";\n"
"; Then there are running totals:\n"
";\n"
";     hash-entries-recorded-hi hash-entries-recorded-lo\n"
";     hash-entries-removed-hi hash-entries-recorded-lo\n"
";     hash-entries-scanned-hi hash-entries-scanned-lo\n"
";     words-of-oldspace-scanned-hi words-of-oldspace-scanned-lo\n"
";     ssb-entries-recorded-hi ssb-entries-recorded-lo\n"
";     times-cleared times-scanned times-compacted max-total-size\n"
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
";\n"
"; Tag=`dof': DOF collector's global statistics, all accumulated entries.\n"
";   dof-promotions dof-collections dof-resets dof-repeat-collections\n"
";   dof-full-collections\n"
";   dof-bytes-promoted-in-hi dof-bytes-promoted-in-lo\n"
";   dof-fullgc-objects-marked-hi dof-fullgc-objects-marked-lo\n"
";   dof-fullgc-pointers-traced-hi dof-fullgc-pointers-traced-hi\n"
";   dof-fullgc-remset-entries-removed dof-fullgc-remset-entries-removed-lo\n"
";  )\n";

int
stats_opendump( const char *filename )
{
  if (dumpfile != 0) stats_closedump();

  dumpfile = fopen( filename, "w" );

  if (dumpfile)
    fprintf( dumpfile, dump_banner,
	     larceny_major_version, 
	     larceny_minor_version,
	     larceny_version_qualifier );
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
  bool has_dofgc = FALSE;
  bool has_npgc = FALSE;
  char *type;
  int i;

  if (dumpfile == 0) return;

  fprintf( dumpfile, "(" );

  /* Print overall memory and GC information */
  switch (ms->lastcollection_type) {
    case STATS_COLLECT : type = "collect"; break;
    case STATS_PROMOTE : type = "promote"; break;
    case STATS_FULL :    type = "full"; break;
    default : panic_abort( "dump_stats: %d", ms->lastcollection_type);
  }
  fprintf( dumpfile, "%lu %lu %lu %lu %lu %lu %lu %lu %lu %s ",
	   nativeuint( ms->wallocated_hi ),
	   nativeuint( ms->wallocated_lo ),
	   nativeuint( ms->wcollected_hi ),
	   nativeuint( ms->wcollected_lo ),
	   nativeuint( ms->wcopied_hi ),
	   nativeuint( ms->wcopied_lo ),
	   nativeuint( ms->gctime ),
	   nativeuint( ms->wlive ),
	   nativeuint( ms->lastcollection_gen ),
	   type );


  /* Print generations information */

  fprintf( dumpfile, "(" );
  for ( i=0 ; i < generations ; i++ ) {
    dump_gen_stats( dumpfile, &ms->gen_stat[i], stats[i].live );
    has_dofgc = has_dofgc || stats[i].dof_generation;
    has_npgc = has_npgc || stats[i].np_young;
  }
  fprintf( dumpfile, ") " );


  /* Print remembered sets information */

  fprintf( dumpfile, "(" );
  for ( i = 1 ; i < generations ; i++ )
    dump_remset_stats( dumpfile, &ms->rem_stat[i] );
  fprintf( dumpfile, ") " );


  /* Print stack information */

  fprintf( dumpfile, "%lu %lu %lu %lu %lu %lu %lu ",
	   nativeuint( ms->fflushed_hi ), 
	   nativeuint( ms->fflushed_lo ),
	   nativeuint( ms->wflushed_hi ),
	   nativeuint( ms->wflushed_lo ),
	   nativeuint( ms->stacks_created ),
	   nativeuint( ms->frestored_hi ),
	   nativeuint( ms->frestored_lo ) );

  /* Print overall heap information */
  fprintf( dumpfile, "%lu %lu %lu ",
	   nativeuint( ms->wallocated_heap ),
	   nativeuint( ms->wallocated_remset ),
	   nativeuint( ms->wallocated_rts ) );

  /* New fields for v0.32 */
  fprintf( dumpfile, "%lu %lu ",
	   nativeuint( ms->wmoved_hi ), nativeuint( ms->wmoved_lo ) );

#if defined(SIMULATE_NEW_BARRIER)
  fprintf( dumpfile, "(swb . (%lu %lu %lu %lu %lu %lu))",
	   nativeuint( ms->swb.array_assignments ),
	   nativeuint( ms->swb.lhs_young_or_remembered ),
	   nativeuint( ms->swb.rhs_constant ),
	   nativeuint( ms->swb.cross_gen_check ),
	   nativeuint( ms->swb.transactions ),
	   nativeuint( ms->swb.total_assignments ) );
#endif

  if (has_dofgc)
    fprintf( dumpfile,
	     "(dof . (%lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu))",
	     nativeuint( ms->dof.promotions ),
	     nativeuint( ms->dof.collections ),
	     nativeuint( ms->dof.resets ),
	     nativeuint( ms->dof.repeats ),
	     nativeuint( ms->dof.full_collections ),
	     nativeuint( ms->dof.bytes_promoted_hi ),
	     nativeuint( ms->dof.bytes_promoted_lo ),
	     nativeuint( ms->dof.objs_marked_hi ),
	     nativeuint( ms->dof.objs_marked_lo ),
	     nativeuint( ms->dof.ptrs_traced_hi ),
	     nativeuint( ms->dof.ptrs_traced_lo ),
	     nativeuint( ms->dof.entries_removed_hi ),
	     nativeuint( ms->dof.entries_removed_lo ) );
	   
  if (has_npgc) {
    fprintf( dumpfile, "(np-remset . " );
    dump_remset_stats( dumpfile, &ms->np_remset );
    fprintf( dumpfile, ")" );
  }

  fprintf( dumpfile, ")\n" );
}

static void
dump_gen_stats( FILE *dumpfile, gen_stat_t *gs, word live )
{
  fprintf( dumpfile, "(%lu %lu %lu %lu %lu %lu %lu %lu) ",
	  nativeuint( gs->collections ),
	  nativeuint( gs->promotions ),
	  nativeuint( gs->gctime ),
	  live/4,
	  nativeuint( gs->np_young ),
	  nativeuint( gs->np_old ),
	  nativeuint( gs->np_j ),
	  nativeuint( gs->np_k ) 
	  );
}


static void
dump_remset_stats( FILE *dumpfile, rem_stat_t *rs )
{
  fprintf( dumpfile, "(%lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu "
	             "%lu %lu %lu %lu %lu %lu) ",
	   nativeuint( rs->pallocated ),
	   nativeuint( rs->pused ),
	   nativeuint( rs->plive ),
	   nativeuint( rs->hallocated ),
	   nativeuint( rs->hrecorded_hi ),
	   nativeuint( rs->hrecorded_lo ),
	   nativeuint( rs->hremoved_hi ),
	   nativeuint( rs->hremoved_lo ),
	   nativeuint( rs->hscanned_hi ),
	   nativeuint( rs->hscanned_lo ),
	   nativeuint( rs->wscanned_hi ),
	   nativeuint( rs->wscanned_lo ),
	   nativeuint( rs->ssbrecorded_hi ),
	   nativeuint( rs->ssbrecorded_lo ),
	   nativeuint( rs->cleared ),
	   nativeuint( rs->scanned ),
	   nativeuint( rs->compacted ),
	   nativeuint( rs->max_size ),
	   nativeuint( rs->identity ));
}


/* Get the current heap statistics.
 *
 * This is moderately hairy -- some statistics are accumulated, and some
 * are just updated.  All stats are kept as fixnums.
 */

static void do_remset( rem_stat_t *acc, remset_stats_t *in )
{
  assert( acc->identity == fixnum( in->identity ) );

  add( &acc->hrecorded_hi, &acc->hrecorded_lo, fixnum( in->hash_recorded ));
  add( &acc->hscanned_hi, &acc->hscanned_lo, fixnum( in->hash_scanned ));
  add( &acc->hremoved_hi, &acc->hremoved_lo, fixnum( in->hash_removed ));
  add( &acc->ssbrecorded_hi, &acc->ssbrecorded_lo, fixnum( in->ssb_recorded ));
  add( &acc->wscanned_hi, &acc->wscanned_lo, fixnum( in->words_scanned ));
  acc->cleared += fixnum( in->cleared );
  acc->scanned += fixnum( in->scanned );
  acc->compacted += fixnum( in->compacted );
  acc->hallocated = fixnum( in->hash_allocated );
  acc->pallocated = fixnum( in->pool_allocated );
  acc->pused = fixnum( in->pool_used );
  acc->plive = fixnum( in->pool_live );
  acc->max_size = max( acc->max_size, acc->hallocated + acc->pallocated );
}

/* We do remembered sets and generations by remset identity, which is
   the right thing except that in some collectors there can be more than
   one remembered set per generation.  Fix it when it becomes an issue
   by decoupling generations and remsets.
   */
static int find_slot( sys_stat_t *ms, int identity )
{
  int i;

  for ( i=0 ; i < generations ; i++ ) {
    if (ms->rem_stat[i].identity == fixnum(identity ))
      break;
    if (ms->rem_stat[i].identity == -1) {
      ms->rem_stat[i].identity = fixnum(identity);
      break;
    }
  }
  assert ( i < generations );
  return i;
}

static void
current_statistics( heap_stats_t *stats, sys_stat_t *ms )
{
  int i, j;
  word bytes_live = 0;
  word frames_flushed = 0;
  word frames_restored = 0;
  word bytes_flushed = 0;
  word stacks_created = 0;

  for ( j=0 ; j < generations ; j++ ) {
    stats[j].remset_data.identity = 0;
    gc_stats( gc, j, &stats[j] );

    i = find_slot( ms, stats[j].remset_data.identity );

    bytes_live += stats[j].live;

    frames_flushed += stats[j].frames_flushed;
    bytes_flushed += stats[j].bytes_flushed;
    stacks_created += stats[j].stacks_created;
    frames_restored += stats[j].frames_restored;

    do_remset( &ms->rem_stat[i], &stats[j].remset_data );

    /* Non-predictive collector */
    ms->gen_stat[i].np_young = fixnum(stats[j].np_young);
    ms->gen_stat[i].np_old = fixnum(stats[j].np_old);
    ms->gen_stat[i].np_j = fixnum(stats[j].np_j);
    ms->gen_stat[i].np_k = fixnum(stats[j].np_k);

    /* Policy & allocation */
    ms->gen_stat[i].target = fixnum(stats[j].target/sizeof(word));
    ms->gen_stat[i].alloc = fixnum((stats[j].semispace1+stats[i].semispace2)/
				   sizeof(word));

    if (stats[j].np_young)
      /* Entries valid exactly when np_young==1 */
      do_remset( &ms->np_remset, &stats[j].np_remset_data );
  }

  /* Overall statistics */
  ms->wlive = fixnum(bytes_live / sizeof(word));
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

#if defined(SIMULATE_NEW_BARRIER)
  { word total_assignments = 0,
         array_assignments = 0,
         lhs_young_or_remembered = 0,
         rhs_constant = 0,
         cross_gen_check = 0,
         transactions = 0;
    simulated_barrier_stats( &total_assignments,
			     &array_assignments, 
			     &lhs_young_or_remembered,
			     &rhs_constant,
			     &cross_gen_check,
			     &transactions );
    /* FIXME: chance that several of these fields will overflow. */
    ms->swb.total_assignments += fixnum(total_assignments);
    ms->swb.array_assignments += fixnum(array_assignments);
    ms->swb.lhs_young_or_remembered += fixnum(lhs_young_or_remembered);
    ms->swb.rhs_constant += fixnum(rhs_constant);
    ms->swb.cross_gen_check += fixnum(cross_gen_check);
    ms->swb.transactions += fixnum(transactions);
#endif
}


static heap_stats_t *make_heapstats( void )
{
  heap_stats_t *p;

  p = must_malloc( generations*sizeof(heap_stats_t) );
  memset( p, 0, sizeof( heap_stats_t )*generations );
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
