/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- main file.
 * On-line manual available at http://www.ccs.neu.edu/home/will/Larceny/.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "config.h"
#include "larceny.h"
#include "gc.h"
#include "millicode.h"    /* for scheme_init() */
#if defined(MACOS) && defined(CODEWARRIOR)
# include <SIOUX.h>
# include <console.h>
  long os_get_next_event( EventRecord *e );
  long os_handle_event( EventRecord *e );
#endif

/* Argument parsing structure */

typedef struct opt opt_t;
struct opt {
  int        maxheaps;		        /* length of size[] member */
  int        size[ MAX_HEAPS ];	        /* area 1 at loc 0, etc */
  gc_param_t gc_info;                   /* detailed info about areas */
  unsigned   timerval;                  /* timer value */
  bool       enable_singlestep;         /* enable/disable single stepping */
  bool       enable_breakpoints;        /* enable/disable breakpoints */
  bool       enable_timer;              /* enable/disable timer */
  bool       show_heapstats;            /* unparse this structure */
  char       *heapfile;                 /* name of heap file */
  bool       quiet;                     /* do not print informative msgs */
  bool       annoying;                  /* print many informative msgs */
  bool       supremely_annoying;        /* print massively many msgs */
  bool       flush;                     /* force icache flushing */
  bool       noflush;                   /* disable icache flushing */
  bool       reorganize_and_dump;       /* split text and data and dump */
  int        restc;                     /* number of extra arguments */
  char       **restv;                   /* vector of extra arguments */
};

static void param_error( char *s );
static void invalid( char *s );
static void usage( void );
static void help( void );
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

int main( int argc, char **os_argv )
{
  opt_t o;
  int generations;
  char **argv;

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

  memset( &o, 0, sizeof( o ) );
  o.maxheaps = MAX_HEAPS;
  o.timerval = 0xFFFFFFFF;
  o.heapfile = 0;
  o.enable_breakpoints = 1;
  o.restv = 0;
  o.gc_info.ephemeral_info = 0;
  o.gc_info.use_static_area = 1;
  o.gc_info.globals = globals;
#if defined( BDW_GC )
  o.gc_info.is_conservative_system = 1;
#endif

  cache_setup();
  consolemsg( "%s v%d.%d%s (%s:%s:%s) (%s %s)",
	      larceny_system_name,
	      larceny_major_version, 
	      larceny_minor_version,
	      larceny_version_qualifier,
	      larceny_gc_technology,
	      osname, 
	      (globals[ G_CACHE_FLUSH ] ? "split" : "unified"),
	      user, date );

#if !defined(MACOS)
  parse_options( argc, argv, &o );
#else
# if defined(CODEWARRIOR)
  SIOUXSettings.asktosaveonclose = 0;
  SIOUXSettings.autocloseonquit = 1;
  SIOUXSetTitle( "\pPetit Larceny Transcript" );
# endif  
  { /* Look for the file "larceny.args" in the application's home directory. */
    int argc, maxargs = 100;
    char *argv[100], buf[256], *p;
    char *args_filename = "larceny.args"; /* fixme: don't hardwire file name */
    FILE *fp;

    argv[0] = "Petit Larceny";  	  /* fixme: get application name */
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
    parse_options( argc, argv, &o );
  }
#endif

#if defined( MACOS )
  /* Is there a principled way to do this?
     Loop for a while to process OpenDocument Apple Event to get the 
     heap file, if any.
     */
  { EventRecord event; 
    int i;
    for ( i=0 ; i < 10 ; i++ ) {
    	os_get_next_event( &event );
    	os_handle_event( &event );
   	}
  }
#endif

  if (o.heapfile == 0)
    o.heapfile = larceny_heap_name;

  quiet = o.quiet;
  annoying = o.annoying;
  supremely_annoying = o.supremely_annoying;

  if (annoying || supremely_annoying)
    dump_options( &o );

  if (o.flush)
    globals[ G_CACHE_FLUSH ] = 1;
  else if (o.noflush)
    globals[ G_CACHE_FLUSH ] = 0;

  if (o.reorganize_and_dump && !o.gc_info.is_stopcopy_system) {
    o.gc_info.is_conservative_system = 0;
    o.gc_info.is_generational_system = 0;
    o.gc_info.is_stopcopy_system = 1;
    o.gc_info.use_static_area = 1;
    o.gc_info.use_non_predictive_collector = 0;
    o.gc_info.use_incremental_bdw_collector = 0;
    o.gc_info.sc_info.size_bytes = DEFAULT_STOPCOPY_SIZE;
    o.gc_info.sc_info.load_factor = DEFAULT_LOAD_FACTOR;
  }

  if (!create_memory_manager( &o.gc_info, &generations ))
    panic( "Unable to set up the garbage collector." );

  if (!load_heap_image_from_file( o.heapfile ))
    panic( "Unable to load the heap image." );

  if (o.reorganize_and_dump) {
    char buf[ FILENAME_MAX ];	/* Standard C */

    sprintf( buf, "%s.split", o.heapfile );
    if (!reorganize_and_dump_static_heap( buf ))
      panic( "Failed heap reorganization." );
    return 0;
  }

  /* initialize some policy globals */
  globals[ G_BREAKPT_ENABLE ] =
    (o.enable_breakpoints ? TRUE_CONST : FALSE_CONST);
  globals[ G_SINGLESTEP_ENABLE ] =
    (o.enable_singlestep ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER_ENABLE ] =
    (o.enable_timer ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER ] = 0;
  globals[ G_TIMER2 ] = o.timerval;
  globals[ G_RESULT ] = fixnum( 0 );  /* No arguments */

  setup_signal_handlers();
  osdep_init();
  stats_init( the_gc(globals), generations, o.show_heapstats );
  scheme_init( globals );

  /* Allocate vector of command line arguments and pass it as an
   * argument to the startup procedure.
   */
  { word args[1], res;

    args[0] = allocate_argument_vector( o.restc, o.restv );
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

int panic( const char *fmt, ... )
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
  return 0;
}

void annoyingmsg( const char *fmt, ... )
{
  va_list args;

  if (!annoying) return;

  va_start( args, fmt );
  if (!quiet) {
    vfprintf( stderr, fmt, args );
    fprintf( stderr, "\n" );
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
}

/****************************************************************************
 *
 * Command line parsing.
 */

static int sizearg( char *str, int *argc, char ***argv, int *var );
static int hsizearg( char *str, int *argc, char ***argv, int *var, int *loc );
static int doublearg( char *str, int *argc, char ***argv, double *var );
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

static void
parse_options( int argc, char **argv, opt_t *o )
{
  int i, loc, heaps, prev_size, areas = DEFAULT_AREAS;
#if defined( BDW_GC )
  double load_factor = 0.0;	              /* Ignore it. */
#else
  double load_factor = DEFAULT_LOAD_FACTOR;
#endif
  double expansion = 0.0;	              /* Ignore it. */
  int divisor = 0;		              /* Ignore it. */
  double feeling_lucky = 0.0;                 /* Not lucky at all. */
  double phase_detection = -1.0;              /* No detection. */
  int np_remset_limit = INT_MAX;              /* Infinity, or close enough. */
  int dynamic_max = 0;
  int dynamic_min = 0;
  int val;

  while (--argc) {
    ++argv;
#if !defined( BDW_GC )
    if (strcmp( *argv, "-stopcopy" ) == 0)
      o->gc_info.is_stopcopy_system = 1;
    else if (numbarg( "-areas", &argc, &argv, &areas ))
      init_generational( o, areas, "-areas" );
    else if (strcmp( *argv, "-gen" ) == 0)
      init_generational( o, areas, "-gen" );
    else if (strcmp( *argv, "-np" ) == 0 || strcmp( *argv, "-rof" ) == 0) {
      o->gc_info.is_generational_system = 1;
      o->gc_info.use_non_predictive_collector = 1;
    }
    else if (numbarg( "-dof", &argc, &argv, 
		      &o->gc_info.dynamic_dof_info.generations )) {
      o->gc_info.is_generational_system = 1;
      o->gc_info.use_dof_collector = 1;
    }
    else if (strcmp( *argv, "-nostatic" ) == 0)
      o->gc_info.use_static_area = 0;
    else if (strcmp( *argv, "-nocontract" ) == 0)
      o->gc_info.dont_shrink_heap = 1;
    else if (hsizearg( "-size", &argc, &argv, &val, &loc )) {
      if (loc > 1) o->gc_info.is_generational_system = 1;
      if (loc < 0 || loc > o->maxheaps)
	invalid( "-size" );
      else if (loc > 0)
	o->size[loc-1] = val;
      else 
	for ( i=1 ; i < o->maxheaps ; i++ )
	  if (o->size[i-1] == 0) o->size[i-1] = val;
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
    else if (sizearg( "-rhash", &argc, &argv, (int*)&o->gc_info.rhash ))
      ;
    else if (sizearg( "-ssb", &argc, &argv, (int*)&o->gc_info.ssb ))
      ;
    else 
#endif
    if (numbarg( "-ticks", &argc, &argv, (int*)&o->timerval ))
      ;
    else if (doublearg( "-load", &argc, &argv, &load_factor )) {
#if defined(BDW_GC)
      if (load_factor < 1.0 && load_factor != 0.0)
	param_error( "Load factor must be at least 1.0" );
#else
      if (load_factor < 2.0)
	param_error( "Load factor must be at least 2.0" );
#endif
    }
    else if (doublearg( "-feeling-lucky", &argc, &argv, &feeling_lucky ))
      ;
    else if (doublearg( "-phase-detection", &argc, &argv, &phase_detection ))
      ;
    else if (numbarg( "-np-remset-limit", &argc, &argv, &np_remset_limit )) 
      ;
    else if (sizearg( "-min", &argc, &argv, &dynamic_min ))
      ;
    else if (sizearg( "-max", &argc, &argv, &dynamic_max ))
      ;
    else if (strcmp( *argv, "-nobreak" ) == 0)
      o->enable_breakpoints = 0;
    else if (strcmp( *argv, "-step" ) == 0)
      o->enable_singlestep = 1;
    else if (strcmp( *argv, "-stats" ) == 0)
      o->show_heapstats = 1;
    else if (strcmp( *argv, "-help" ) == 0 || strcmp( *argv, "-h" ) == 0)
      help();
    else if (strcmp( *argv, "-quiet" ) == 0) 
      o->quiet = 1;
    else if (strcmp( *argv, "-annoy-user" ) == 0)
      o->annoying = 1;
    else if (strcmp( *argv, "-annoy-user-greatly" ) == 0) {
      o->annoying = 1;
      o->supremely_annoying = 1;
    }
    else if (strcmp( *argv, "-flush" ) == 0)
      o->flush = 1;
    else if (strcmp( *argv, "-noflush" ) == 0)
      o->noflush = 1;
    else if (strcmp( *argv, "-reorganize-and-dump" ) == 0)
      o->reorganize_and_dump = 1;
    else if (strcmp( *argv, "-args" ) == 0) {
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
    else if (o->heapfile != NULL) {
      consolemsg( "Error: Only one heap file allowed." );
      usage();
    }
    else
      o->heapfile = *argv;
  }

  /* Initial validation */
  if (o->gc_info.is_conservative_system &&
      (o->gc_info.is_generational_system || o->gc_info.is_stopcopy_system))
    param_error( "Both precise and conservative gc selected." );
    
  if (o->gc_info.is_generational_system && o->gc_info.is_stopcopy_system)
    param_error( "Both generational and non-generational gc selected." );

  if (!o->gc_info.is_stopcopy_system && !o->gc_info.is_conservative_system
      && o->gc_info.ephemeral_info == 0)
    init_generational( o, areas, "*invalid*" );

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
    int n = areas-1;		/* Index of dynamic generation */

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
    if (o->gc_info.use_dof_collector && 
	o->gc_info.use_non_predictive_collector) {
      consolemsg( "Error: Both nonpredictive (ROF) and DOF gc selected." );
      consolemsg( "Type \"larceny -help\" for help." );
      exit( 1 );
    }
    else if (o->gc_info.use_non_predictive_collector) {
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
    else if (o->gc_info.use_dof_collector) {
      o->gc_info.dynamic_dof_info.load_factor = load_factor;
      o->gc_info.dynamic_dof_info.dynamic_max = dynamic_max;
      o->gc_info.dynamic_dof_info.dynamic_min = dynamic_min;
      if (o->size[n] == 0) {
	int size = prev_size + DEFAULT_DYNAMIC_INCREMENT;
	if (dynamic_min) size = max( dynamic_min, size );
	if (dynamic_max) size = min( dynamic_max, size );
	o->gc_info.dynamic_dof_info.area_size = size;
      }
      else
	o->gc_info.dynamic_dof_info.area_size = o->size[n];
    }
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
}

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

static int 
sizearg( char *str, int *argc, char ***argv, int *loc ) 
{
  if (strcmp( **argv, str ) == 0) {
    if (*argc == 1 || !getsize( *(*argv+1), loc )) invalid( str );
    ++*argv; --*argc;
    return 1;
  }
  else
    return 0;
}

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
  if (strcmp( **argv, str ) == 0) {
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
  if (strcmp( **argv, str ) == 0) {
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

static void dump_options( opt_t *o )
{
  int i;

  consolemsg( "" );
  consolemsg( "Command line parameter dump" );
  consolemsg( "---------------------------" );
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
  else if (o->gc_info.is_generational_system) {
    consolemsg( "Using generational garbage collector." );
    consolemsg( "  Nursery" );
    consolemsg( "    Size (bytes): %d", o->gc_info.nursery_info.size_bytes );
    for ( i=1 ; i<= o->gc_info.ephemeral_area_count ; i++ ) {
      consolemsg( "  Ephemeral area %d", i );
      consolemsg( "    Size (bytes): %d",
		  o->gc_info.ephemeral_info[i-1].size_bytes );
    }
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
    else {
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
  consolemsg( "Error: Invalid argument to option '%s'", s );
  consolemsg( "Type \"larceny -help\" for help." );
  exit( 1 );
}

static void usage( void )
{
  consolemsg( "" );
  consolemsg( "Usage: larceny [ options ][ heapfile ][-args arguments]" );
  consolemsg( "Type \"larceny -help\" for help." );
  exit( 1 );
}


#define STR(x) STR2(x)
#define STR2(x) #x

static char *helptext[] = {
#if !defined(BDW_GC)
  "  -stopcopy",
  "     Select the stop-and-copy collector." ,
  "  -gen",
  "     Select the standard generational collector, with "
        STR(DEFAULT_AREAS) 
        " heap areas.  This is",
  "     the collection type selected by default.",
  "  -areas n",
  "     Select generational collection, with n heap areas." ,
  "  -np",
  "  -rof",
  "     Select generational collection with the renewal-oldest-first",
  "     dynamic area (radioactive decay non-predictive collection).",
  "  -dof n",
  "     Select generational collection with the deferred-oldest-first",
  "     dynamic area, using n chunks (default 4).",
  "  -size# nnnn",
  "     Heap area number '#' is given size 'nnnn' bytes.",
  "     This selects generational collection if # > 1.",
#endif
  "  -min nnnn",
  "     Set the lower limit on the size of the expandable (\"dynamic\") area.",
  "  -max nnnn",
  "     Set the upper limit on the size of the expandable (\"dynamic\") area.",
  "  -load d",
  "     Use inverse load factor d to control allocation and collection.",
  "     After a major garbage collection, the collector will resize the heap",
  "     and attempt to keep memory consumption below d*live, where live data",
  "     is computed (sometimes estimated) following the major collection.",
#if !defined(BDW_GC)
  "     In a copying collector, d must be at least 2.0; the default value",
  "     is 3.0.",
#else
  "     In the conservative collector, d must be at least 1.0; no default is",
  "     set, as the conservative collector by default manages heap growth",
  "     using the heap space divisor (see -divisor parameter).  The -load",
  "     and -expansion parameters are mutually exclusive.",
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
  "  -stats",
  "     Print some data about heap allocation at startup." ,
  "  -quiet",
  "     Suppress nonessential messages.",
  "  -help",
  "     Print this message.",
  "",
  "  (Wizard options below this point.)",
#if !defined(BDW_GC)
  "  -annoy-user",
  "     Print a bunch of annoying debug messages, usually about GC.",
  "  -annoy-user-greatly",
  "     Print a great deal of very annoying debug messages, usually about GC.",
  "  -nostatic",
  "     Do not use the static area, but load the heap image into the",
  "     garbage-collected heap." ,
  "  -nocontract",
  "     Do not contract the dynamic area according to the load factor, but",
  "     always use all the memory that has been allocated.",
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
  "     non-predictive young area to the non-predictive old area, extrapolated",
  "     to the point when the young area is full, exceeds n, then the",
  "     collector is allowed to shuffle the entire contents of the",
  "     young area to the old area and to clear the remembered set.  By",
  "     default, the limit is infinity.  This parameter does not select",
  "     anything else, not even the nonpredictive GC.",
  "  -rhash nnnn",
  "     Set the remembered-set hash table size, in elements.  The size must",
  "     be a power of 2.",
  "  -ssb nnnn",
  "     Set the remembered-set Sequential Store Buffer (SSB) size, in "
        "elements.",
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

static void help( void )
{
  int i;

  consolemsg("Usage: larceny [options][heapfile][-args arg-to-scheme ...]");
  consolemsg("" );
  consolemsg("Options:" );
  for (i=0 ; helptext[i] != 0 ; i++ )
    consolemsg( helptext[i] );
  consolemsg("The Larceny User's Manual is available on the web at");
  consolemsg("  http://www.ccs.neu.edu/home/lth/larceny/manual.html");
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
