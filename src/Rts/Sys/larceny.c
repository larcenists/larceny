/* Copyright 1998 Lars T Hansen       -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- main file.
 *
 * FIXME: Over-complex.  A gazillion parameters are supported, which 
 * introduces:
 *   - parsing and error checking
 *   - text into the usage message
 *   - value printing if parameter value printing is on
 * Since various GCs have various parameters, factor this cruft out
 * into a spec that can be handled by the GC, via function pointers or
 * whatever.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "config.h"
#include "larceny.h"
#include "gc.h"
#include "stats.h"        /* for stats_init() */
#include "gc_t.h"
#include "young_heap_t.h" /* for yh_create_initial_stack() */

opt_t command_line_options;

static void param_error( char *s );
static void invalid( char *s );
static void usage( void );
static void help(int wizardp);
static void parse_options( int argc, char **argv, opt_t *opt );
static int  getsize( char *s, int *p );
static void dump_options( opt_t *o );

static bool quiet = 0;
  /* 'quiet' controls consolemsg() 
     */

static bool annoying = 0;
  /* 'annoying' controls annoying_msg() 
     */

static bool supremely_annoying = 0;
  /* 'supremely_annoying' controls supremely_annoyingmsg()
     */

#if WIN32
static char *directory_separator = ";";
#else
static char *directory_separator = ":";
#endif /* WIN32 */

static void print_banner(void) {
#ifndef PETIT_LARCENY
  consolemsg( "%s v%d.%d%s (%s, %s:%s:%s)",
              larceny_system_name,
              larceny_major_version, 
              larceny_minor_version,
              larceny_version_qualifier,
              date,
              larceny_gc_technology,
              osname, 
              (globals[ G_CACHE_FLUSH ] ? "split" : "unified") );
#else
  consolemsg( "%s v%d.%d%s (%s:%s)",
              larceny_system_name,
              larceny_major_version, 
              larceny_minor_version,
              larceny_version_qualifier,
              larceny_gc_technology,
              osname );
#endif
}

#if defined PETIT_LARCENY || defined X86_NASM
int larceny_main( int argc, char **os_argv )
#else
int main( int argc, char **os_argv )
#endif
{
  int generations;
  char **argv;
  
  /* FIXME: this allows us to (temporarily) circumvent DEP problems for the
   majority of windows users */

#if WIN32
  /* There are four possible values GetSystemDEPPolicy can return.
   * We care about AlwaysOn and OptOut.
   * If the policy is set to AlwaysOn then larceny cannot run.
   * Otherwise we can opt out of the restriction.
   * The other two cases (AlwaysOff and OptIn) have no effect on larceny.
   */
   
  switch ( GetSystemDEPPolicy() )
  {
  case 0: break; /* AlwaysOff: we don't care about this */
  case 1:  /* DEP has been set in the windows boot.ini to be always on */
    panic_exit( "Larceny cannot run with DEP set to AlwaysOn." );
    break;
  case 2: break; /* OptIn: we don't care about this either */
  case 3:  /* DEP has been set to be OptOut, which is what we do */
    if( !SetProcessDEPPolicy(0) )
      {
          consolemsg("Failed to set DEP policy");
      }
    break;
  }  
#endif /* WIN32 */

#if defined(DEC_ALPHA_32BIT)
  /* I know this looks weird.  When running Petit Larceny on the Alpha
     in 32-bit mode, pointers are 32-bit, but the interface to main() is
     64-bit (for reasons I do not understand yet).  The following
     seemingly unnecessary loop moves the argument vector into 32-bit
     space.  Presumably there's a better way, as this must be a common
     problem.
     */
  argv = (char**)malloc( sizeof( char* )*(argc+1) );
  for ( i=0 ; i < argc ; i++ ) {
    argv[i] = (char*)malloc( strlen( os_argv[i] )+1 );
    strcpy( argv[i], os_argv[i] );
  }
  argv[i] = 0;
#else
  argv = os_argv;
#endif

  osdep_init();
  cache_setup();

  memset( &command_line_options, 0, sizeof( command_line_options ) );
  command_line_options.maxheaps = MAX_GENERATIONS;
  command_line_options.timerval = 0xFFFFFFFF;
  command_line_options.heapfile = 0;
  command_line_options.enable_breakpoints = 1;
  command_line_options.restv = 0;
  command_line_options.gc_info.ephemeral_info = 0;
  command_line_options.gc_info.use_static_area = 1;
  command_line_options.gc_info.mmu_buf_size = -1;
  command_line_options.gc_info.globals = globals;
#if defined( BDW_GC )
  command_line_options.gc_info.is_conservative_system = 1;
#endif
  command_line_options.nobanner = 0;
  command_line_options.unsafe = 0;
  command_line_options.foldcase = 0;
  command_line_options.nofoldcase = 0;
  command_line_options.r5rs = 0;
  command_line_options.err5rs = 0;
  command_line_options.r7r6 = 0;
  command_line_options.r7rs = 0;
  command_line_options.r6rs = 0;
  command_line_options.ignore1 = 0;
  command_line_options.r6fast = 0;
  command_line_options.r6slow = 0;
  command_line_options.r6pedantic = 0;
  command_line_options.r6less_pedantic = 0;
  command_line_options.r6program = "";
  command_line_options.r6path = "";
  command_line_options.r6path2 = "";
  command_line_options.r7features = "";
  command_line_options.transcoder = 0;

  if (larceny_version_qualifier[0] == '.') {
    /* If we our version qualifier starts with a period, then the
     * version prints out as M.NN.XXX (a development version number).
     * On development versions, we always print the banner with
     * information about the build date, host system, and gc
     * technology. */
    print_banner();
    /* since we printed the banner here, there's no reason to print it
     * again below. */
    command_line_options.nobanner = 1;
  }

  /* FIXME: This should all be factored out as osdep_get_program_options()
     or something like that.  That requires factoring out the type of 'o'
     in a header somewhere, and exposing parse_options.
     */
#if defined(MACOS)
  { /* Look for the file "larceny.args" in the application's home directory. */
    int argc, maxargs = 100;
    char *argv[100], buf[256], *p;
    char *args_filename = "larceny.args"; /* fixme: don't hardwire file name */
    FILE *fp;

    argv[0] = "Petit Larceny";            /* fixme: get application name */
    argc = 1;
    if ((fp = fopen( args_filename, "r")) != 0) {
      while (fgets( buf, sizeof(buf), fp ) != 0) {
        p = strtok( buf, " \t\n\r" );
        while (p != 0 && argc < maxargs-1) {
          argv[argc++] = strdup( p );
          p = strtok( 0, " \t\n\r" );
        }
      }
      fclose( fp );
    }
    argv[argc] = 0;
    parse_options( argc, argv, &command_line_options );
  }
#else
  parse_options( argc, argv, &command_line_options );
#endif

  if (!command_line_options.nobanner)
    print_banner();

  osdep_poll_startup_events();

  if (command_line_options.heapfile == 0) {
    char *path = getenv(LARCENY_ROOT);
    size_t path_length = strlen(path);
    size_t base_length = strlen(larceny_heap_name);

    /* This leaks, but only once at startup.  I think it's worth it: */
    command_line_options.heapfile = malloc(path_length + base_length + 2);
    if (command_line_options.heapfile == NULL) {
      panic_exit("Cannot allocate buffer for heapfile name.");
    }

    sprintf(command_line_options.heapfile, "%s/%s", path, larceny_heap_name);
  }

  quiet = command_line_options.quiet;
  annoying = command_line_options.annoying;
  supremely_annoying = command_line_options.supremely_annoying;

  if (annoying || supremely_annoying)
    dump_options( &command_line_options );

  if (command_line_options.flush)
    globals[ G_CACHE_FLUSH ] = 1;
  else if (command_line_options.noflush)
    globals[ G_CACHE_FLUSH ] = 0;

  if (command_line_options.reorganize_and_dump &&
      !command_line_options.gc_info.is_stopcopy_system) {
    command_line_options.gc_info.is_conservative_system = 0;
    command_line_options.gc_info.is_generational_system = 0;
    command_line_options.gc_info.is_stopcopy_system = 1;
    command_line_options.gc_info.use_static_area = 1;
    command_line_options.gc_info.use_non_predictive_collector = 0;
    command_line_options.gc_info.use_incremental_bdw_collector = 0;
    command_line_options.gc_info.sc_info.size_bytes = DEFAULT_STOPCOPY_SIZE;
    command_line_options.gc_info.sc_info.load_factor = DEFAULT_LOAD_FACTOR;
  }

  if (!create_memory_manager( &command_line_options.gc_info, &generations ))
    panic_exit( "Unable to set up the garbage collector." );

  if (!load_heap_image_from_file( command_line_options.heapfile ))
    panic_exit( "Unable to load the heap image." );

  if (command_line_options.reorganize_and_dump) {
    char buf[ FILENAME_MAX ];   /* Standard C */

    sprintf( buf, "%s.split", command_line_options.heapfile );
    if (!reorganize_and_dump_static_heap( buf ))
      panic_exit( "Failed heap reorganization." );
    return 0;
  }

  /* initialize some policy globals */
  globals[ G_BREAKPT_ENABLE ] =
    (command_line_options.enable_breakpoints ? TRUE_CONST : FALSE_CONST);
  globals[ G_SINGLESTEP_ENABLE ] =
    (command_line_options.enable_singlestep ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER_ENABLE ] =
    (command_line_options.enable_timer ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER ] = 0;
  globals[ G_TIMER2 ] = command_line_options.timerval;
  globals[ G_RESULT ] = fixnum( 0 );  /* No arguments */

  setup_signal_handlers();
  stats_init( the_gc(globals) );
  scheme_init( globals );

  /* The initial stack can't be created when the garbage collector
     is created because stack creation depends on certain data
     structures allocated by scheme_init() in Petit Larceny when
     the system is compiled with USE_GOTOS_LOCALLY.  So we create
     the stack here.
     */
  yh_create_initial_stack( the_gc(globals)->young_area );

  /* Allocate vector of command line arguments and pass it as an
   * argument to the startup procedure.
   */
  { word args[1], res;

    args[0] = allocate_argument_vector( the_gc(globals),
                                        command_line_options.restc,
                                        command_line_options.restv );
    larceny_call( globals[ G_STARTUP ], 1, args, &res );
    consolemsg( "Startup procedure returned with value %08lx", (long)res );
  }

  /* Not usually reached */
  return 0;
}


/***************************************************************************
 *
 * Console messages
 *
 */

int panic_exit( const char *fmt, ... )
{
  static int in_panic = 0;
  va_list args;

  va_start( args, fmt );
  fprintf( stderr, "Larceny Panic: " );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );

  if (in_panic) abort();
  in_panic = 1;
  exit( 1 );
  /* Never returns. Return type is 'int' to facilitate an idiom. */
  return 0;
}

#ifdef __GNUC__
extern int panic_abort( const char *fmt, ... ) __attribute__ ((__noreturn__));
#endif
int panic_abort( const char *fmt, ... )
{
  static int in_panic = 0;
  va_list args;

  va_start( args, fmt );
  fprintf( stderr, "Larceny Panic: " );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );

  if (in_panic) abort();
  in_panic = 1;
  abort();
  /* Never returns. Return type is 'int' to facilitate an idiom. */
#ifndef __GNUC__
  return 0;
#endif
}

void annoyingmsg( const char *fmt, ... )
{
  va_list args;

  if (!annoying) return;

  va_start( args, fmt );
  if (!quiet) {
    vfprintf( stderr, fmt, args );
    fprintf( stderr, "\n" );
    fflush( stderr );
  }
  va_end( args );
}

void supremely_annoyingmsg( const char *fmt, ... )
{
  va_list args;

  if (!supremely_annoying) return;

  va_start( args, fmt );
  if (!quiet) {
    vfprintf( stderr, fmt, args );
    fprintf( stderr, "\n" );
    fflush( stderr );
  }
  va_end( args );
}

void consolemsg( const char *fmt, ... )
{
  va_list args;

  if (quiet) return;

  va_start( args, fmt );
  vfprintf( stdout, fmt, args );
  fprintf( stdout, "\n" );
  va_end( args );
  fflush( stdout );
}

void hardconsolemsg( const char *fmt, ... )
{
  va_list args;

  va_start( args, fmt );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );
  fflush( stderr );
}

static long long generic_event_counter = 0;
static const long long max_event_count = 0;
static const long long key_event_count = 0;
static const long long event_trace_len = 100;
int saw_event() 
{ 
  generic_event_counter++;
  if (max_event_count > 0) {
    if (generic_event_counter+1 == max_event_count)
      panic_abort( "hit max event count %lld", generic_event_counter);
  }
  return generic_event_counter;
}

void eventmsg( const char *fmt, ... )
{
  if (generic_event_counter > (key_event_count - event_trace_len)
      && (generic_event_counter < (key_event_count + event_trace_len))) {
    va_list args;
    va_start( args, fmt );
    fprintf( stderr, "(event %8lld)", generic_event_counter );
    vfprintf( stderr, fmt, args );
    va_end( args );
    fprintf( stderr, "\n" );
  }
}

/****************************************************************************
 *
 * Command line parsing.
 */

static int hstrcmp( const char *s1, const char* s2 );

/* A SizeSpec is either a Number n, or a Number n followed by the character 'M', 
 * or a Number n followed by the character 'K'
 * interpretation: [[ n ]] = n; [[ nM ]] = n*1024*1024; [[ nK ]] = n*1024
 */

/* requires: (to be determined)
 * effects: if *argv[0] matches str as an option, *argc > 1, and *argv[1] holds
 *          a SizeSpec, then *var holds the number corresponding to *argv[1],
 *          advances *argv, decrements *argc, and returns 1.  Else returns 0.
 * example: sizearg( "-option", 2, { "-option", "3K" }, receiver )
 *          returns 1, with *receiver == 3072
 */
static int sizearg( char *str, int *argc, char ***argv, int *var );

/* requires: (to be determined)
 * effects: if exists integer N such that *argv[0] matches strN as an option,
 *                    argc > 1, and *argv[1] holds a SizeSpec,
 *          then *var holds the number corresponding to *argv[1]
 *           and *loc holds (the largest such) N.
 * example: hsizearg( "-optnum", 2, { "-optnum3" "2K" }, recv1, recv2 )
 *          returns 1, with *recv1 == 2048 and *recv2 == 3
 */
static int hsizearg( char *str, int *argc, char ***argv, int *var, int *loc );
static int doublearg( char *str, int *argc, char ***argv, double *var );

/* requires: (to be determined)
 * effects: if *argv[0] matches str as an option, *argc > 1, and *argv[1] holds
 *          an integer, then *var holds the integer corresponding to *argv[1],
 *          advances *argv, decrements *argc, and returns 1.  Else returns 0.
 * example: numbarg( "-option", 2 { "-option" "4" }, receiver )
 *          returns 1, with *receiver = 4.
 */
static int numbarg( char *str, int *argc, char ***argv, int *var );
static int hnumbarg( char *str, int *argc, char ***argv, int *var, int *loc );
static void compute_np_parameters( opt_t *o, int suggested_size );

static void init_generational( opt_t *o, int areas, char *name )
{
  if (areas < 2)
    invalid( name );

  if (o->gc_info.ephemeral_info != 0) {
    consolemsg( "Error: Number of areas re-specified with '%s'", name );
    consolemsg( "Type \"larceny -help\" for help." );
    exit( 1 );
  }

  o->gc_info.is_generational_system = 1;
  o->gc_info.ephemeral_info = 
    (sc_info_t*)must_malloc( sizeof( sc_info_t )*areas-2 );
  o->gc_info.ephemeral_area_count = areas-2;
}

static void init_regional( opt_t *o, int areas, char *name )
{
  if (areas < 1)
    invalid( name );

  if (o->gc_info.ephemeral_info != 0) {
    consolemsg( "Error: Number of areas re-specified with '%s'", name );
    consolemsg( "Type \"larceny -help\" for help." );
    exit( 1 );
  }
  
  o->gc_info.is_regional_system = 1;
  o->gc_info.ephemeral_info = 
    (sc_info_t*)must_malloc( sizeof( sc_info_t)*areas );
  o->gc_info.ephemeral_area_count = areas;
}

static void
parse_options( int argc, char **argv, opt_t *o )
{
  int i, loc, prev_size, areas = DEFAULT_AREAS;
  int mmu_size;
  int mark_period;
  int oracle_countdown;
  double popular_factor = 0.0;
  double infamy_factor = 0.0;
  double refine_factor = 0.0;
  double sumz_budget = 0.0;
  double sumz_coverage = 0.0;
  int sumz_retries;
#if defined( BDW_GC )
  double load_factor = 0.0;                   /* Ignore it. */
#else
  double load_factor = DEFAULT_LOAD_FACTOR;
#endif
  double load_factor_hard = DEFAULT_LOAD_FACTOR_HARD;
  double expansion = 0.0;                     /* Ignore it. */
  int divisor = 0;                            /* Ignore it. */
  double feeling_lucky = 0.0;                 /* Not lucky at all. */
  double phase_detection = -1.0;              /* No detection. */
  int np_remset_limit = INT_MAX;              /* Infinity, or close enough. */
  int full_frequency = 0;
  double growth_divisor = 1.0;
  int dynamic_max = 0;
  int dynamic_min = 0;
  int val;                                    /* general purpose temporary */
  char* temp_string = NULL;                   /* general purpose temporary */

  while (--argc) {
    ++argv;
#if !defined( BDW_GC )
    if (hstrcmp( *argv, "-stopcopy" ) == 0)
      o->gc_info.is_stopcopy_system = 1;
    else if (numbarg( "-areas", &argc, &argv, &areas ))
      init_generational( o, areas, "-areas" );
    else if (hstrcmp( *argv, "-gen" ) == 0)
      init_generational( o, areas, "-gen" );
    else if (hstrcmp( *argv, "-nostatic" ) == 0)
      o->gc_info.use_static_area = 0;
    else if (hstrcmp( *argv, "-nocontract" ) == 0)
      o->gc_info.dont_shrink_heap = 1;
    else if (hstrcmp( *argv, "-oracle" ) == 0)
      o->gc_info.use_oracle_to_update_remsets = 1;
    else if (numbarg( "-oracle_countdown", &argc, &argv, &oracle_countdown )) {
      o->gc_info.oracle_countdown = oracle_countdown;
    } 
    else if (hsizearg( "-size", &argc, &argv, &val, &loc )) {
      if (loc > 1 && ! o->gc_info.is_regional_system) {
        /* FIXME: Maybe we shouldn't be inferring this anymore */
        o->gc_info.is_generational_system = 1; 
      }
      if (loc < 0 || loc > o->maxheaps) {
        invalid( "-size" );
      } else if (o->gc_info.is_generational_system) {
        if (loc > 0)
          o->size[loc-1] = val;
        else 
          for ( i=1 ; i < o->maxheaps ; i++ )
            if (o->size[i-1] == 0) o->size[i-1] = val;
      }

      /* FIXME:  The following could be simplified. */

      else if (o->gc_info.is_regional_system) {
        o->size[loc] = val;
      } else if (o->gc_info.is_stopcopy_system) {
        o->size[loc] = val;
      } else {
        o->size[loc] = val;
      }
    }
    else if (numbarg( "-mmusize", &argc, &argv, &mmu_size)) {
      o->gc_info.mmu_buf_size = mmu_size;
    }
    else if (numbarg( "-regions", &argc, &argv, &areas))  {
      init_regional( o, areas, "-regions" );
    } else if (hstrcmp( *argv, "-rrof" ) == 0 || 
             hstrcmp( *argv, "-regional" ) == 0) {
      init_regional( o, areas, *argv );
    }
    else if (hstrcmp( *argv, "-rrof_prefer_big_summ" ) == 0) {
      o->gc_info.rrof_prefer_big_summ = TRUE;
      o->gc_info.rrof_prefer_lil_summ = FALSE;
      o->gc_info.rrof_prefer_lat_summ = FALSE;
    }
    else if (hstrcmp( *argv, "-rrof_prefer_lil_summ" ) == 0) {
      o->gc_info.rrof_prefer_lil_summ = TRUE;
      o->gc_info.rrof_prefer_big_summ = FALSE;
      o->gc_info.rrof_prefer_lat_summ = FALSE;
    }
    else if (hstrcmp( *argv, "-rrof_prefer_late_summ" ) == 0) {
      o->gc_info.rrof_prefer_lat_summ = TRUE;
      o->gc_info.rrof_prefer_lil_summ = FALSE;
      o->gc_info.rrof_prefer_big_summ = FALSE;
    }
    else if (numbarg( "-mark_period", &argc, &argv, &mark_period)) {
      o->gc_info.mark_period = mark_period;
    }
    else if (doublearg( "-refinement", &argc, &argv, &refine_factor)) {
      o->gc_info.has_refine_factor = TRUE;
      o->gc_info.refinement_factor = refine_factor;
    }
    else if (hstrcmp( *argv, "-alloc_mark_bmp_once" ) == 0) {
      o->gc_info.alloc_mark_bmp_once = 1;
    } 
    else if (doublearg( "-sumzbudget", &argc, &argv, &sumz_budget)) {
      o->gc_info.has_sumzbudget = TRUE;
      o->gc_info.sumzbudget_inv = sumz_budget;
    }
    else if (doublearg( "-sumzcoverage", &argc, &argv, &sumz_coverage)) {
      o->gc_info.has_sumzcoverage = TRUE;
      o->gc_info.sumzcoverage_inv = sumz_coverage;
    }
    else if (numbarg( "-sumzretries", &argc, &argv, &sumz_retries )) {
      o->gc_info.has_sumz_retries = TRUE;
      o->gc_info.max_sumz_retries = sumz_retries;
    }
    else if (doublearg( "-popularity", &argc, &argv, &popular_factor)) {
      o->gc_info.has_popularity_factor = TRUE;
      o->gc_info.popularity_factor = popular_factor;
    }
    else if (doublearg( "-infamy", &argc, &argv, &infamy_factor)) {
      if (infamy_factor == 0) {
        o->gc_info.has_infamy_factor = FALSE;
      } else {
        o->gc_info.has_infamy_factor = TRUE;
        o->gc_info.infamy_factor = infamy_factor;
      }
    }
    else if (hstrcmp( *argv, "-print_float_stats_cycle" ) == 0)
      o->gc_info.print_float_stats_cycle = TRUE;
    else if (hstrcmp( *argv, "-print_float_stats_major" ) == 0)
      o->gc_info.print_float_stats_major = TRUE;
    else if (hstrcmp( *argv, "-print_float_stats_minor" ) == 0)
      o->gc_info.print_float_stats_minor = TRUE;
    else if (hstrcmp( *argv, "-print_float_stats_refine" ) == 0)
      o->gc_info.print_float_stats_refine = TRUE;
    else if (sizearg( "-rhash", &argc, &argv, (int*)&o->gc_info.rhash ))
      ;
    else if (sizearg( "-ssb", &argc, &argv, (int*)&o->gc_info.ssb ))
      ;
    else if   (hstrcmp( *argv, "-rhashrep" ) == 0) {
      o->gc_info.chose_rhashrep = TRUE;
      o->gc_info.chose_rbitsrep = FALSE;
    } else if (hstrcmp( *argv, "-rbitsrep" ) == 0) {
      o->gc_info.chose_rhashrep = FALSE;
      o->gc_info.chose_rbitsrep = TRUE;
    } else 
#endif /* !BDW_GC */
    if (numbarg( "-ticks", &argc, &argv, (int*)&o->timerval ))
      ;
    else if (doublearg( "-load", &argc, &argv, &load_factor )) {
#if defined(BDW_GC)
      if (load_factor < 1.0 && load_factor != 0.0)
        param_error( "Load factor must be at least 1.0" );
#else
      if (load_factor < 1.0)
        param_error( "Load factor must be at least 1.0" );
#endif
    }
    else if (doublearg( "-load_hard", &argc, &argv, &load_factor_hard )) {
      if (load_factor_hard < 1.0)
        param_error( "Load factor must be at least 1.0" );
    }
#if ROF_COLLECTOR
    else if (hstrcmp( *argv, "-np" ) == 0 || hstrcmp( *argv, "-rof" ) == 0) {
      o->gc_info.is_generational_system = 1;
      o->gc_info.use_non_predictive_collector = 1;
    }
    else if (numbarg( "-steps", &argc, &argv,
                     &o->gc_info.dynamic_np_info.steps )) {
      o->gc_info.is_generational_system = 1;
      o->gc_info.use_non_predictive_collector = 1;
    }
    else if (sizearg( "-stepsize", &argc, &argv,
                     &o->gc_info.dynamic_np_info.stepsize )) {
      o->gc_info.is_generational_system = 1;
      o->gc_info.use_non_predictive_collector = 1;
    }
    else if (doublearg( "-phase-detection", &argc, &argv, &phase_detection ))
      ;
    else if (numbarg( "-np-remset-limit", &argc, &argv, &np_remset_limit )) 
      ;
#endif
    else if (hstrcmp( *argv, "-nobreak" ) == 0)
      o->enable_breakpoints = 0;
    else if (hstrcmp( *argv, "-step" ) == 0)
      o->enable_singlestep = 1;
    else if (sizearg( "-min", &argc, &argv, &dynamic_min ))
      ;
    else if (sizearg( "-max", &argc, &argv, &dynamic_max ))
      ;
    else if (hstrcmp( *argv, "-help" ) == 0 || strcmp( *argv, "-h" ) == 0)
      help(0);
    else if (hstrcmp( *argv, "-wizard" ) == 0)
      help(1);
    else if (hstrcmp( *argv, "-version" ) == 0) {
      print_banner();
      exit(0);
    }
    else if (hstrcmp( *argv, "-quiet" ) == 0) 
      o->quiet = 1;
    else if (hstrcmp( *argv, "-annoy-user" ) == 0)
      o->annoying = 1;
    else if (hstrcmp( *argv, "-annoy-user-greatly" ) == 0) {
      o->annoying = 1;
      o->supremely_annoying = 1;
    }
    else if (hstrcmp( *argv, "-flush" ) == 0)
      o->flush = 1;
    else if (hstrcmp( *argv, "-noflush" ) == 0)
      o->noflush = 1;
    else if (hstrcmp( *argv, "-reorganize-and-dump" ) == 0)
      o->reorganize_and_dump = 1;
    else if (hstrcmp( *argv, "-heap" ) == 0) {
      ++argv;
      --argc;
      o->heapfile = *argv;
    }
    else if (hstrcmp( *argv, "-nobanner" ) == 0)
      o->nobanner = 1;
    else if (hstrcmp( *argv, "-foldcase" ) == 0)
      o->foldcase = 1;
    else if (hstrcmp( *argv, "-nofoldcase" ) == 0)
      o->nofoldcase = 1;
    else if ((hstrcmp( *argv, "-r5rs" ) == 0) ||
             (hstrcmp( *argv, "-r5" ) == 0)) {
      o->r5rs = 1;
      o->foldcase = 1;
    }
    else if (hstrcmp( *argv, "-err5rs" ) == 0)
      o->err5rs = 1;
    else if ((hstrcmp( *argv, "-r7rs" ) == 0) ||
             (hstrcmp( *argv, "-r7" ) == 0))
      o->r7rs = 1;
    else if (hstrcmp( *argv, "-r7r6" ) == 0)
      o->r7r6 = 1;
    else if ((hstrcmp( *argv, "-r6rs" ) == 0) ||
             (hstrcmp( *argv, "-r6" ) == 0)) {
      o->r6rs = 1;
      o->nobanner = 1;
    }
    else if (hstrcmp( *argv, "-ignore1" ) == 0) {
      o->ignore1 = 1;
    }
    else if (hstrcmp( *argv, "-unsafe" ) == 0)
      o->unsafe = 1;
    else if (hstrcmp( *argv, "-fast" ) == 0)
      o->r6fast = 1;
    else if (hstrcmp( *argv, "-slow" ) == 0)
      o->r6slow = 1;
    else if (hstrcmp( *argv, "-pedantic" ) == 0)
      o->r6pedantic = 1;
    else if (hstrcmp( *argv, "-but-not-that-pedantic" ) == 0)
      o->r6less_pedantic = 1;
    else if (hstrcmp( *argv, "-program" ) == 0) {
      if (strcmp( o->r6program, "" ) == 0) {
        ++argv;
        --argc;
        o->r6program = *argv;
        o->nobanner = 1;
      }
      else {
        consolemsg( "Error: More than one program named on command line." );
        usage();
      }
    }
    else if (strcmp( *argv, "-D" ) == 0) {
      ++argv;
      --argc;
      val = 2; /* for space and NUL */
      val += strlen( o->r7features );
      val += strlen( *argv );
      temp_string = (char*)malloc( val );
      strcpy( temp_string, o->r7features );
      if (strcmp ( o->r7features, "" ) != 0) {
        free( o->r7features );
        strcat( temp_string, " " );
      }
      strcat( temp_string, *argv );
      o->r7features = temp_string;
      temp_string = NULL;
    }
    else if ((strcmp( *argv, "-I" ) == 0) ||
             (hstrcmp( *argv, "-path" ) == 0)) {
      ++argv;
      --argc;
      val = 2; /* for colon and NUL */
      val += strlen( o->r6path );
      val += strlen( *argv );
      temp_string = (char*)malloc( val );
      strcpy( temp_string, o->r6path );
      if (strcmp ( o->r6path, "" ) != 0) {
        free( o->r6path );
        strcat( temp_string, directory_separator );
      }
      strcat( temp_string, *argv );
      o->r6path = temp_string;
      temp_string = NULL;
    }
    else if (strcmp( *argv, "-A" ) == 0) {
      ++argv;
      --argc;
      val = 2; /* for colon and NUL */
      val += strlen( o->r6path2 );
      val += strlen( *argv );
      temp_string = (char*)malloc( val );
      strcpy( temp_string, o->r6path2 );
      if (strcmp ( o->r6path2, "" ) != 0) {
        free( o->r6path2 );
        strcat( temp_string, directory_separator );
      }
      strcat( temp_string, *argv );
      o->r6path2 = temp_string;
      temp_string = NULL;
    }
    else if (numbarg( "-transcoder", &argc, &argv, &(o->transcoder) )) {
      if ((o->transcoder < 32) || (o->transcoder >= 128))
        param_error( "Illegal transcoder" );
    }
    else if (hstrcmp( *argv, "-latin1" ) == 0) {
      if (o->transcoder == 0)
        o->transcoder = 33;
      else param_error( "Only one default transcoder can be specified." );
    }
    else if (hstrcmp( *argv, "-utf8" ) == 0) {
      if (o->transcoder == 0)
        o->transcoder = 65;
      else param_error( "Only one default transcoder can be specified." );
    }
    else if (hstrcmp( *argv, "-utf16" ) == 0) {
      param_error( "UTF-16 console ports are not yet implemented." );
      if (o->transcoder == 0)
        o->transcoder = 97;
      else param_error( "Only one default transcoder can be specified." );
    }
    else if (hstrcmp( *argv, "-args" ) == 0 ||
               strcmp( *argv, "--" ) == 0) {
      o->restc = argc-1;
      o->restv = argv+1;
      break;
    }
#if defined(BDW_GC)
    else if (numbarg( "-divisor", &argc, &argv, &divisor )) {
      if (divisor < 1)
        param_error( "Divisor must be at least 1." );
    }
    else if (doublearg( "-expansion", &argc, &argv, &expansion )) {
      if (expansion < 1.0)
        param_error( "Expansion factor must be at least 1.0" );
    }
#endif
    else if (**argv == '-') {
      consolemsg( "Error: Invalid option '%s'", *argv );
      usage();
    }
    else if (strcmp( o->r6program, "" ) == 0) {
      o->r6program = *argv;
      o->nobanner = 1;
    }
    else {
      consolemsg( "Error: More than one program named on command line." );
      usage();
    }
  }

  /* Initial validation */

  if (o->foldcase && o->nofoldcase)
    param_error( "Both -foldcase and -nofoldcase selected." );

  if ((o->r5rs && (o->err5rs || o->r6rs || o->r7rs || o->r7r6)) ||
      (o->err5rs && (o->r5rs || o->r6rs || o->r7rs || o->r7r6)) ||
      (o->r6rs && (o->r5rs || o->err5rs || o->r7rs || o->r7r6)) ||
      (o->r7rs && (o->r5rs || o->err5rs || o->r6rs || o->r7r6)) ||
      (o->r7r6 && (o->r5rs || o->err5rs || o->r6rs || o->r7rs)))
    param_error( "More than one of -r5rs -r6rs -r7rs -r7r6 selected." );

  if ((o->r6slow || o->r6pedantic) &&
      ((! (o->r6rs)) || (! (o->r6slow)) ||
       (! (o->r6pedantic)) || (o->r6program == 0)))
    param_error( "Missing one of -r6rs -slow -pedantic -program options." );

  if (o->r6less_pedantic && (! (o->r6pedantic)))
    param_error( "Missing -pedantic option." );

  if (o->r6slow && (strcmp (o->r6path, "") != 0))
    param_error( "The -slow and -path options are incompatible." );

  /* If a program is specified with no mode, default to R7RS mode. */

  if ((strcmp (o->r6program, "") != 0) &&
      (! (o->r5rs)) &&
      (! (o->err5rs)) &&
      (! (o->r6rs)) &&
      (! (o->r7rs)) &&
      (! (o->r7r6)))
    o->r7rs = 1;

  if (o->ignore1 && (! (o->r6program)))
    param_error( "Missing -program option." );

  if ((((int)o->gc_info.is_conservative_system)
       + ((int)o->gc_info.is_generational_system)
       + ((int)o->gc_info.is_stopcopy_system)
       + ((int)o->gc_info.is_regional_system)) > 1) {
    param_error( "More than one kind of collector tech selected." );
  }

  if (o->gc_info.is_conservative_system &&
      (o->gc_info.is_generational_system || 
       o->gc_info.is_stopcopy_system || 
       o->gc_info.is_regional_system))
    param_error( "Both precise and conservative gc selected." );
    
  if (o->gc_info.is_generational_system && o->gc_info.is_stopcopy_system)
    param_error( "Both generational and non-generational gc selected." );

  /* TODO: double check logic in this case. */
  if (!o->gc_info.is_stopcopy_system && !o->gc_info.is_conservative_system &&
      !o->gc_info.is_regional_system
      && o->gc_info.ephemeral_info == 0)
    init_generational( o, areas, "*invalid*" );

  if (o->gc_info.is_generational_system)
    if (load_factor < 2.0)
      param_error( "Load factor must be at least 2.0" );


  if (dynamic_max && dynamic_min && dynamic_max < dynamic_min)
    param_error( "Expandable MAX is less than expandable MIN." );

  if (dynamic_max || dynamic_min) {
    int n = (o->gc_info.is_generational_system ? areas-1 : 0);

    if (o->size[n] && dynamic_max && o->size[n] > dynamic_max)
      param_error( "Size of expandable area is larger than selected max." );
    if (o->size[n] && dynamic_min && o->size[n] < dynamic_min)
      param_error( "Size of expandable area is smaller than selected min." );
  }

  /* Complete parameter structure by computing the not-specified values. */
  if (o->gc_info.is_generational_system) {
    int n = areas-1;            /* Index of dynamic generation */

    /* Nursery */
    o->gc_info.nursery_info.size_bytes =
      (o->size[0] > 0 ? o->size[0] : DEFAULT_NURSERY_SIZE);

    /* Ephemeral generations */
    prev_size = o->gc_info.nursery_info.size_bytes;

    for ( i = 1 ; i <= areas-2 ; i++ ) {
      if (o->size[i] == 0)
        o->size[i] = prev_size + DEFAULT_EPHEMERAL_INCREMENT;
      o->gc_info.ephemeral_info[i-1].size_bytes = o->size[i];
      prev_size = o->size[i];
    }

    /* Dynamic generation */
#if ROF_COLLECTOR
    if (o->gc_info.use_non_predictive_collector) {
      int size;

      o->gc_info.dynamic_np_info.load_factor = load_factor;
      o->gc_info.dynamic_np_info.dynamic_max = dynamic_max;
      o->gc_info.dynamic_np_info.dynamic_min = dynamic_min;
      if (o->size[n] != 0)
        o->gc_info.dynamic_np_info.size_bytes = o->size[n];
      size = prev_size + DEFAULT_DYNAMIC_INCREMENT;
      if (dynamic_min) size = max( dynamic_min, size );
      if (dynamic_max) size = min( dynamic_max, size );
      compute_np_parameters( o, size );
      if (feeling_lucky < 0.0 || feeling_lucky > 1.0)
        param_error( "NP luck parameter (-feeling-lucky) out of range." );
      else
        o->gc_info.dynamic_np_info.luck = feeling_lucky;
      if (phase_detection != -1.0 &&
          (phase_detection < 0.0 || phase_detection > 1.0))
        param_error( "NP phase detection paramater out of range." );
      else
        o->gc_info.dynamic_np_info.phase_detection = phase_detection;
      if (np_remset_limit < 0)
        param_error( "NP remset limit must be nonnegative." );
      else
        o->gc_info.dynamic_np_info.extra_remset_limit = np_remset_limit;
    }
#endif /* ROF_COLLECTOR */
    else {
      o->gc_info.dynamic_sc_info.load_factor = load_factor;
      o->gc_info.dynamic_sc_info.dynamic_max = dynamic_max;
      o->gc_info.dynamic_sc_info.dynamic_min = dynamic_min;
      if (o->size[n] == 0) {
        int size = prev_size + DEFAULT_DYNAMIC_INCREMENT;
        if (dynamic_min) size = max( dynamic_min, size );
        if (dynamic_max) size = min( dynamic_max, size );
        o->gc_info.dynamic_sc_info.size_bytes = size;
      }
      else
        o->gc_info.dynamic_sc_info.size_bytes = o->size[n];
    }
  }
  else if (o->gc_info.is_stopcopy_system) {
    if (o->size[0] == 0) {
      int size = DEFAULT_STOPCOPY_SIZE;

      if (dynamic_min) size = max( dynamic_min, size );
      if (dynamic_max) size = min( dynamic_max, size );
      o->gc_info.sc_info.size_bytes = size;
    }
    else
      o->gc_info.sc_info.size_bytes = o->size[0]; /* Already validated */
    o->gc_info.sc_info.load_factor = load_factor;
    o->gc_info.sc_info.dynamic_min = dynamic_min;
    o->gc_info.sc_info.dynamic_max = dynamic_max;
  }
  else if (o->gc_info.is_conservative_system) {
    if (load_factor > 0.0 && expansion > 0.0)
      param_error( "-load and -expansion are mutually exclusive." );
    o->gc_info.bdw_info.load_factor = load_factor;
    o->gc_info.bdw_info.expansion_factor = expansion;
    o->gc_info.bdw_info.divisor = divisor;
    o->gc_info.bdw_info.dynamic_min = dynamic_min;
    o->gc_info.bdw_info.dynamic_max = dynamic_max;
  }
  else if (o->gc_info.is_regional_system) {
    /* (roughly cut and pasted from is_generational_system case above) */

    /* Nursery */
    o->gc_info.nursery_info.size_bytes =
      (o->size[0] > 0 ? o->size[0] : DEFAULT_REGIONAL_NURSERY_SIZE);

    /* Ephemeral generations */
    prev_size = 5*o->gc_info.nursery_info.size_bytes;

    if (prev_size < DEFAULT_REGIONAL_REGION_SIZE)
      prev_size = DEFAULT_REGIONAL_REGION_SIZE;

    for ( i = 1 ; i <= areas ; i++ ) {
      if (o->size[i] == 0)
        o->size[i] = prev_size;
      assert( o->size[i] > 0 );
      o->gc_info.ephemeral_info[i-1].size_bytes = o->size[i];
      prev_size = o->size[i];
    }

    o->gc_info.dynamic_sc_info.load_factor = load_factor;
    o->gc_info.dynamic_sc_info.load_factor_hard = load_factor_hard;
    o->gc_info.dynamic_sc_info.dynamic_max = dynamic_max;
    o->gc_info.dynamic_sc_info.dynamic_min = dynamic_min;
  }
}

#if ROF_COLLECTOR
/* Note that by design, we do not take the load factor into account. */
static void compute_np_parameters( opt_t *o, int suggested_size )
{
  int steps = o->gc_info.dynamic_np_info.steps;
  int stepsize = o->gc_info.dynamic_np_info.stepsize;
  int size = o->gc_info.dynamic_np_info.size_bytes;

  if (steps == 0 && stepsize == 0) {
    if (size == 0)
      size = suggested_size;
    stepsize = DEFAULT_STEPSIZE;
    steps = ceildiv( size, stepsize );
  }
  else if (steps != 0 && stepsize == 0) {
    if (size == 0) {
      stepsize = DEFAULT_STEPSIZE;
      size = stepsize * steps;
    }
    else
      stepsize = size / steps;
  }
  else if (steps == 0 && stepsize != 0) {
    if (size == 0) {
      steps = DEFAULT_STEPS;
      size = stepsize * steps;
    }
    else
      steps = size / stepsize;
  }
  else 
    size = stepsize * steps;

  o->gc_info.dynamic_np_info.steps = steps;
  o->gc_info.dynamic_np_info.stepsize = stepsize;
  o->gc_info.dynamic_np_info.size_bytes = size;
}
#endif /* ROF_COLLECTOR */

/* Takes a positive integer only, suffixes K and M are accepted. */
static int sizearg( char *str, int *argc, char ***argv, int *loc ) 
{
  if (hstrcmp( **argv, str ) == 0) {
    if (*argc == 1 || !getsize( *(*argv+1), loc ) || *loc <= 0) {
      char buf[ 128 ];
      sprintf( buf, "%s requires a positive integer.", str );
      invalid( buf );
    }
    ++*argv; --*argc;
    return 1;
  }
  else
    return 0;
}

/* FIXME: doesn't allow --size0 1M etc */

static int 
hsizearg( char *str, int *argc, char ***argv, int *var, int *loc ) 
{
  int l = strlen(str);

  if (strncmp( **argv, str, l ) == 0) {
    if (*(**argv+l) != 0) {
      if (sscanf( **argv+strlen(str), "%d", loc ) != 1) invalid( str );
    }
    else
      *loc = 0;
    if (*argc == 1 || !getsize( *(*argv+1), var )) invalid( str );
    ++*argv; --*argc;
    return 1;
  }
  else
    return 0;
}

static int 
numbarg( char *str, int *argc, char ***argv, int *loc )
{
  if (hstrcmp( **argv, str ) == 0) {
    if (*argc == 1 || sscanf( *(*argv+1), "%d", loc ) != 1 ) invalid( str );
    ++*argv; --*argc;
    return 1;
  }
  else
    return 0;
}

static int 
doublearg( char *str, int *argc, char ***argv, double *loc )
{
  if (hstrcmp( **argv, str ) == 0) {
    if (*argc == 1 || sscanf( *(*argv+1), "%lf", loc ) != 1 ) invalid( str );
    ++*argv; --*argc;
    return 1;
  }
  else
    return 0;
}

static int 
hnumbarg( char *str, int *argc, char ***argv, int *var, int *loc )
{
  int l = strlen(str);

  if (strncmp( **argv, str, l ) == 0) {
    if (*(**argv+l) != 0) {
      if (sscanf( **argv+strlen(str), "%d", loc ) != 1) invalid( str );
    }
    else
      *loc = 0;
    if (*argc == 1 || sscanf( *(*argv+1), "%d", var ) != 1 ) invalid( str );
    ++*argv; --*argc;
    return 1;
  }
  else
    return 0;
}

static int getsize( char *s, int *p )
{
  int r;
  char c, d;

  r = sscanf( s, "%i%c%c", p, &c, &d );
  if (r == 0) return 0;
  if (r == 1) return 1;
  if (r == 3) return 0;
  if (c == 'M' || c == 'm') {
    *p *= 1024*1024;
    return 1;
  }
  if (c == 'K' || c == 'k') {
    *p *= 1024;
    return 1;
  }
  return 0;
}

static int hstrcmp( const char *s1, const char *s2 )
{
    /* Treat --foo as equivalent to -foo; --foo is standard (in other programs) */
    if (s1[0] == '-' && s1[1] == '-')
        return strcmp( s1+1, s2 );
    else
        return strcmp( s1, s2 );
}

static void dump_options( opt_t *o )
{
  int i;

  consolemsg( "" );
  consolemsg( "Command line parameter dump" );
  consolemsg( "---------------------------" );
  consolemsg( "R5RS: %d", o->r5rs );
  consolemsg( "R6RS: %d", o->r6rs );
  consolemsg( "R7RS: %d", o->r7rs );
  consolemsg( "R7R6: %d", o->r7r6 );
  consolemsg( "ERR5RS: %d", o->err5rs );
  consolemsg( "Ignore1: %d", o->ignore1 );
  consolemsg( "Program: %s", o->r6program );
  consolemsg( "-I path: %s", o->r6path );
  consolemsg( "-A path: %s", o->r6path2 );
  consolemsg( "Features: %s", o->r7features );
  consolemsg( "Transcoder: %d", o->transcoder );
  consolemsg( "Stepping: %d", o->enable_singlestep );
  consolemsg( "Breakpoints: %d", o->enable_breakpoints );
  consolemsg( "Timer: %d (val=%d)", o->enable_timer, o->timerval );
  consolemsg( "Heap file: %s", o->heapfile );
  consolemsg( "Quiet: %d", o->quiet );
  consolemsg( "Annoying: %d", o->annoying );
  consolemsg( "Supremely annoying: %d", o->supremely_annoying );
  consolemsg( "Flush/noflush: %d/%d", o->flush, o->noflush );
  consolemsg( "Reorganize and dump: %d", o->reorganize_and_dump );
  consolemsg( "" );
  if (o->gc_info.is_conservative_system) {
    consolemsg( "Using conservative garbage collector." );
    consolemsg( "  Incremental: %d", o->gc_info.use_incremental_bdw_collector);
    consolemsg( "  Inverse load factor: %f", o->gc_info.bdw_info.load_factor );
    consolemsg( "  Inverse expansion factor: %f", 
                o->gc_info.bdw_info.expansion_factor );
    consolemsg( "  Divisor: %d", o->gc_info.bdw_info.divisor );
    consolemsg( "  Min size: %d", o->gc_info.bdw_info.dynamic_min );
    consolemsg( "  Max size: %d", o->gc_info.bdw_info.dynamic_max );
  }
  else if (o->gc_info.is_stopcopy_system) {
    consolemsg( "Using stop-and-copy garbage collector." );
    consolemsg( "  Size (bytes): %d", o->gc_info.sc_info.size_bytes );
    consolemsg( "  Inverse load factor: %f", o->gc_info.sc_info.load_factor );
    consolemsg( "  Using static area: %d", o->gc_info.use_static_area );
    consolemsg( "  Min size: %d", o->gc_info.sc_info.dynamic_min );
    consolemsg( "  Max size: %d", o->gc_info.sc_info.dynamic_max );
  }
  else if (o->gc_info.is_regional_system) {
    consolemsg( "Using regional garbage collector." );
    consolemsg( "  Nursery" );
    consolemsg( "    Size (bytes): %d", o->gc_info.nursery_info.size_bytes );
    for ( i=1 ; i<= o->gc_info.ephemeral_area_count ; i++ ) {
      consolemsg( "  Ephemeral area %d", i );
      consolemsg( "    Size (bytes): %d",
                  o->gc_info.ephemeral_info[i-1].size_bytes );
    }    
    consolemsg( "  Using static area: %d", o->gc_info.use_static_area );
  }
  else if (o->gc_info.is_generational_system) {
    consolemsg( "Using generational garbage collector." );
    consolemsg( "  Nursery" );
    consolemsg( "    Size (bytes): %d", o->gc_info.nursery_info.size_bytes );
    for ( i=1 ; i<= o->gc_info.ephemeral_area_count ; i++ ) {
      consolemsg( "  Ephemeral area %d", i );
      consolemsg( "    Size (bytes): %d",
                  o->gc_info.ephemeral_info[i-1].size_bytes );
    }
#if ROF_COLLECTOR
    if (o->gc_info.use_non_predictive_collector ) {
      np_info_t *i = &o->gc_info.dynamic_np_info;
      consolemsg( "  Dynamic area (nonpredictive copying)" );
      consolemsg( "    Steps: %d", i->steps );
      consolemsg( "    Step size (bytes): %d", i->stepsize );
      consolemsg( "    Total size (bytes): %d", i->size_bytes );
      consolemsg( "    Inverse load factor: %f", i->load_factor );
      consolemsg( "    Min size: %d", i->dynamic_min );
      consolemsg( "    Max size: %d", i->dynamic_max );
      consolemsg( "    Luck: %f", i->luck );
    }
    else 
#endif
    {
      sc_info_t *i = &o->gc_info.dynamic_sc_info;
      consolemsg( "  Dynamic area (normal copying)" );
      consolemsg( "    Size (bytes): %d", i->size_bytes );
      consolemsg( "    Inverse load factor: %f", i->load_factor );
      consolemsg( "    Min size: %d", i->dynamic_min );
      consolemsg( "    Max size: %d", i->dynamic_max );
    }
    consolemsg( "  Using non-predictive dynamic area: %d",
               o->gc_info.use_non_predictive_collector );
    consolemsg( "  Using static area: %d", o->gc_info.use_static_area );
  }
  else {
    consolemsg( "ERROR: inconsistency: GC type not known." );
    exit( 1 );
  }
  consolemsg( "---------------------------" );
}

static void param_error( char *s )
{
  consolemsg( "Error: %s", s );
  usage();
}

static void invalid( char *s )
{
  consolemsg( "" );
  consolemsg( "Error: Invalid argument to option: %s", s );
  consolemsg( "Type \"larceny -help\" for help." );
  exit( 1 );
}

static void usage( void )
{
  consolemsg( "" );
  consolemsg( "Usage: larceny [ OPTIONS ][ PROGRAM ][-- ARGUMENTS]" );
  consolemsg( "Type \"larceny -help\" for help." );
  exit( 1 );
}


#define STR(x) STR2(x)
#define STR2(x) #x

static char *helptext[] = {
  "  -r7r6",
  "     Imports all of the standard R7RS/R6RS libraries.",
  "     If no program is specified, enters a read/eval/print loop (REPL).",
  "  -r7rs, -r7",
  "     Same as -r7r6 but imports only the (scheme base) library.",
  "  -r6rs, r6",
  "     Execute the specified R6RS program.",
  "     (An \"absolute requirement\" of the R6RS forbids REPLs.)",
  "  -r5rs, r5",
  "     Execute the program in R5RS mode, or enter R5RS-style REPL.",
  "  -D <identifier>",
  "     Declares the identifier as a supported feature for R7RS cond-expand.",
  "  -I <directories>",
  "     Search these directories first when importing libraries.",
  "  -A <directories>",
  "     Search these directories last when importing libraries.",
  "     Use colon (Unix) or semicolon (Windows) to separate directories.",
  "  -nofoldcase",
  "     Symbols are case-sensitive (the default; #!fold-case overrides).",
  "  -foldcase",
  "     Symbols are case-insensitive (#!no-fold-case overrides).",
  "  -latin1",
  "     Use Latin-1 as default for console and file io.",
  "  -utf8",
  "     Use UTF-8 as default for console and file io.",
#if 0
  "  -utf16",
  "     Use UTF-16 as default for console and file io (not yet allowed).",
#endif
  "  -quiet",
  "     Suppress nonessential messages.",
  "  -nobanner",
  "     Suppress runtime startup banner (implied by -program, -r6rs).",
  "  -- <argument> ...",
  "     Tell (command-line) to return (<larcenyname> <argument> ...)",
  "     This option, if present, must come last.",
  "     With the -r5rs option, Larceny's standard heap interprets",
  "     these command line arguments specially:",
  "         -e <expr>",
  "           Evaluate <expr> at startup.",
  "         <file>",
  "           Load the specified file (if it exists) at startup.",
  "  -help",
  "     Print this message.",
  "  -wizard",
  "     Print this message as well as help on wizard options.",
  "",
  0 };

static char *wizardhelptext[] = {
  "  (Wizard options below this point.)",
  "  -program <filename>",
  "     Execute the program found in the file; then exit.",
  "     Unless <filename> starts with a hyphen, -program may be omitted.",
  "  -err5rs",
  "     Similar to -r7rs but doesn't import any libraries at startup.",
  "  -heap <filename>",
  "     Select an initial heap image other than the default.",
  "  -transcoder nn",
  "     Use transcoder nn for console io.",
#if 0
  "  -unsafe",
  "     Crash spectacularly when errors occur.",
#endif
  "  This option may accompany the -r6rs or -r7rs option:",
  "       -ignore1",
  "          Ignore the first line of the file specified by -program.",
#if 0
  "       -fast",
  "          Execute the R6RS-style program as compiled code (the default).",
  "       -slow",
  "          Execute in Spanky mode; must be accompanied by -pedantic.",
  "       -pedantic",
  "          Execute in Spanky mode; must be accompanied by -slow.",
  "       -but-not-that-pedantic",
  "          Modifies -pedantic, which must also be specified.",
#endif
  "  -path <directories>",
  "     Same as -I <directories>.",
#if !defined(BDW_GC)
  "  -annoy-user",
  "     Print a bunch of annoying debug messages, usually about GC.",
  "  -annoy-user-greatly",
  "     Print a great many very annoying debug messages, usually about GC.",
  "  -stopcopy",
  "     Use the stop-and-copy garbage collector." ,
  "  -gen",
  "     Use the standard generational collector.  This is the default.",
#if ROF_COLLECTOR
  "  -rof",
  "     Use the hybrid renewal-oldest-first collector (experimental).",
#endif
  "  -regional",
  "     Use the bounded-latency regional collector (experimental).",
  "  -load d",
  "     Use inverse load factor d to control allocation and collection.",
  "     The garbage collector will try to resize the heap as necessary",
  "     to keep memory consumption below d*live, where live data",
  "     is computed or estimated following major collections.",
#if !defined(BDW_GC)
  "     The regional collector allows d to be less than 2.0.",
  "     All of Larceny's other collectors require d to be at least 2.0.",
  "     The default is 3.0.",
#else
  "     In the conservative collector, d must be at least 1.0; no default is",
  "     set, as the conservative collector by default manages heap growth",
  "     using the heap space divisor (see -divisor parameter).  The -load",
  "     and -expansion parameters are mutually exclusive.",
#endif
  "  -min nnnn",
  "     Set the lower limit on the size of the expandable (\"dynamic\") area.",
  "  -max nnnn",
  "     Set the upper limit on the size of the expandable (\"dynamic\") area.",
  "  -size# nnnn",
  "     Heap area number '#' is given size 'nnnn' bytes.",
  "     Area 0 is the nursery.",
  "     If # > 1, this selects the standard generational collector.",
  "  -areas n",
  "     Use the standard generational collector with n heap areas.",
  "     The default number of heap areas is "
        STR(DEFAULT_AREAS) 
        ".",
#endif
#if defined(BDW_GC)
  "  -divisor n",
  "     The divisor controls collection frequency: at least heapsize/n bytes",
  "     are allocated between one collection and the next. The default value",
  "     is 4, and n=1 effectively disables GC.  The divisor is the",
  "     conservative collector's default allocation and expansion control.",
  "  -expansion d",
  "     Use expansion factor d to control heap expansion.  Following garbage",
  "     collection the heap size is set to max(live*d,heapsize).  No",
  "     expansion factor is set by default, as the conservative collector",
  "     manages heap growth using the heap space divisor (see -divisor",
  "     parameter).  The -expansion and -load parameters are mutually",
  "     exclusive.",
#endif
  /* need double indirection to get a number in output rather than just the
   * literal "DEFAULT_MMU_BUFFER_SIZE" */
#define STRINGIZE(val) #val
#define STRINGIZE2(val) STRINGIZE(val)
#if !defined(BDW_GC)
  "  -nostatic",
  "     Do not use the static area, but load the heap image into the",
  "     garbage-collected heap." ,
  "  -nocontract",
  "     Do not allow the heap to contract according to the load factor,",
  "     but continue to use all memory that has been allocated.",
  "  -mmusize n", 
  "     Record minimum mutator utilization within a buffer of size n.",
  "     If n is 0, the default size (" STRINGIZE2(DEFAULT_MMU_BUFFER_SIZE) ") will be used.",
  /* The --regions option can cause a Larceny panic. */
#if 0
  "  -regions n",
  "     Use the regional collector with n heap areas.",
  "     The default number number of heap areas is " STR(DEFAULT_AREAS) ".",
#endif
  /* These make sense only for researchers, who can read the code base. */
#if 0
  /* FIXME
   * The -refinement switch should be exposed, but Will doesn't fully
   * understand its semantics, and there's an ad hoc adjustment in
   * memmgr.c anyway.
   */
  "  -refinement d",
  "     For the regional collector only:  Allocate d words for each",
  "     word marked.  Incompatible with -mark_period.  The default is 1.0.",
  "  -mark_period n",
  "     For the regional collector only:  Do some incremental marking",
  "     after every n minor collections.  Incompatible with -refinement.",
  "  -oracle",
  "     For the regional collector only:  Don't count time spent updating",
  "     the remembered set.  (Used to estimate the cost of updating.)",
  "  -print_float_stats_cycle",
  "  -print_float_stats_major",
  "  -print_float_stats_minor",
  "  -print_float_stats_refine",
  "     For the regional collector only:  Print information about object",
  "     float during various stages of regional collection.  The character",
  "     'Z' represents trivially collectable storage, 'R' represents ",
  "     dead storage referenced from unreachable objects in other regions",
  "     (and thus not guaranteed to be reclaimed in next major collection).",
#endif
  /* Users should probably use the regional collector instead. */
#if 0
#if ROF_COLLECTOR
  "  -steps n",
  "     Select the initial number of steps in the non-predictive collector.",
  "     This selects generational collection and the non-predictive GC.",
  "  -stepsize nnnn",
  "     Select the size of each step in the non-predictive collector.",
  "     This selects generational collection and the non-predictive GC.",
  "  -phase-detection d",
  "     A fudge factor for the non-predictive collector, 0.0 <= d <= 1.0.",
  "     If the non-predictive remembered set has grown by a factor of more ",
  "     than d for some (short) time, and then has grown by a factor of ",
  "     less than d for the same amount of time, then the collector is ",
  "     allowed to decide that a growth phase has ended and that any data",
  "     in the non-predictive young area may be shuffled into the old area",
  "     by adjusting j.  This parameter is sometimes very effective at",
  "     reducing float.  It does not select anything else, not even the",
  "     nonpredictive GC.  By default, phase detection is off.",
  "     Time is measured in the number of promotions into the young area.",
  "  -feeling-lucky d",
  "     A fudge factor for the non-predictive collector, 0.0 <= d <= 1.0.",
  "     After a non-predictive collection and selection of j and k, d*j steps",
  "     are added to k to adjust for the fact that the collector probably",
  "     over-estimates the amount of live storage.  This probably only makes",
  "     sense in a fixed-heap setting, where under-estimation may cause",
  "     failure, so you have to ask yourself, do I feel lucky?  Well, do you?",
  "     The default value is 0.0.  This does not select anything else, not",
  "     even the nonpredictive GC.",
  "  -np-remset-limit n",
  "     A fudge factor for the non-predictive collector, n >= 0.",
  "     If, after a promotion into the non-predictive young area, the number",
  "     of entries in the remembered set that tracks pointers from the",
  "     non-predictive young area to the non-predictive old area, ",
  "     extrapolated to the point when the young area is full, exceeds n, ",
  "     then the collector is allowed to shuffle the entire contents of the",
  "     young area to the old area and to clear the remembered set.  By",
  "     default, the limit is infinity.  This parameter does not select",
  "     anything else, not even the nonpredictive GC.",
#endif
#endif
  "  -rhash nnnn",
  "     Set the remembered-set hash table size, in elements.  The size must",
  "     be a power of 2.",
  "  -ssb nnnn",
  "     Set the write barrier's Sequential Store Buffer (SSB) size, in "
        "elements.",
  "  -rhashrep",
  "     Use a hashtable (array) representation of the remembered set.",
  "  -rbitsrep",
  "     Use a bitmap (tree) representation of the remembered set.",
#endif
  "  -ticks nnnn",
  "     Set the initial countdown timer interval value.",
  "  -nobreak",
  "     Disable breakpoints." ,
  "  -step",
  "     Enable MAL-level single-stepping." ,
#if !defined(BDW_GC)
  "  -reorganize-and-dump",
  "     Split a heap image into text and data, and save the split heap in a",
  "     file. If Larceny is started with foo.heap, this command will create",
  "     foo.heap.split.  The heap image is not executed.",
#endif
  "" ,
  "Values can be decimal, octal (0nnn), hex (0xnnn), or suffixed",
  "with K (for KB) or M (for MB) when that makes sense." ,
  "",
  0 };

static void help(int wizardp)
{
  int i;

  consolemsg("Usage: larceny [ OPTIONS ][ PROGRAM ][-- ARGUMENTS]");
  consolemsg("" );
  consolemsg("Options:" );
  for (i=0 ; helptext[i] != 0 ; i++ )
    consolemsg( helptext[i] );
  if (wizardp) {
      for (i=0 ; wizardhelptext[i] != 0 ; i++ )
          consolemsg( wizardhelptext[i] );
  }
  consolemsg("The Larceny User's Manual is available on the web at");
  consolemsg("  http://www.larcenists.org/doc.html/");
  exit( 0 );
}

int memfail( int code, char *fmt, ... )
{
  va_list args;
  static char *code_str[] = { "malloc", "heap", "realloc", "calloc", "rts" };

  va_start( args, fmt );
  fprintf( stderr, "Allocation failed (code '%s').\n", code_str[code] );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );
  exit( 1 );
  /* Never returns; return type is `int' to facilitate an idiom. */
  return 0;
}

void conditional_abort( void )
{
  char buf[ 10 ];

  while (1) {
    hardconsolemsg( "Abort (yes/no)?" );
    if (fgets( buf, 10, stdin ) == NULL) {
      hardconsolemsg( "EOF -- exiting." );
      exit(1);
    }
    if (strncasecmp( buf, "yes", 3 ) == 0) abort();
    if (strncasecmp( buf, "no", 2 ) == 0) return;
  }
}


/* eof */
