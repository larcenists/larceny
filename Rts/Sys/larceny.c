/* Rts/Sys/larceny.c.
 * Larceny run-time system (Unix) -- main file.
 *
 * $Id: larceny.c,v 1.10 1997/02/27 16:40:26 lth Exp $
 *
 * See the manual page file ``larceny.1'' for instructions about the 
 * format of the command line.  Alternatively, read the source for 
 * parse_options() below.
 */

#include <stdio.h>
#include <signal.h>
#include <stdarg.h>
#include <memory.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

#define MAX_OLD_GENERATIONS   32

typedef struct opt opt_t;

/* Argument parsing structure */
struct opt {
  unsigned esize;
  unsigned rhash;
  unsigned ssb;
  unsigned ewatermark;
  unsigned requested_old_generations;
  old_param_t old_gen_info[ MAX_OLD_GENERATIONS ];
  int         size_explicit[ MAX_OLD_GENERATIONS ];
  int         himark_explicit[ MAX_OLD_GENERATIONS ];
  int         lomark_explicit[ MAX_OLD_GENERATIONS ];
  unsigned old_generations;
  unsigned timerval;
  unsigned enable_singlestep;
  unsigned enable_breakpoints;
  unsigned enable_timer;
  unsigned show_heapstats;
  char     *heapfile;
  int      quiet;
  int      annoying;
  int      flush;
  int      noflush;
  int      np_gc;
  int      np_steps;
  int      np_stepsize;
  int      use_static;
  int      restc;
  char     **restv;
  char     *gc_debug_file;
};

static void invalid( char *s );
static void handle_signals( int ifreq );
static void usage( void );
static void help( void );
static void parse_options( int argc, char **argv, opt_t *opt );
static int getsize( char *s, unsigned *p );

extern char *gctype();

/* 'quiet' controls consolemsg() */
static int quiet = 0;

/* 'annoying' controls annoying_msg() */
static int annoying = 0;

/* Genesis. */
int main( argc, argv )
int argc;
char **argv;
{
  unsigned ssize;
  opt_t o;

  memset( &o, 0, sizeof( o ) );
  o.timerval = 0xFFFFFFFF;
  o.heapfile = 0;
  o.restv = 0;
  o.gc_debug_file = 0;
  o.requested_old_generations = 1;
  o.use_static = 1;
  o.old_generations = MAX_OLD_GENERATIONS;  /* really determined by GC */

  cache_setup();
  consolemsg( "Larceny v%s (%s;%s) (%s/%s)",
	      version, 
	      osname, 
	      (globals[ G_CACHE_FLUSH ] ? "split" : "unified"),
	      user, date );

  parse_options( argc, argv, &o );
  if (o.heapfile == 0)
    o.heapfile = "larceny.heap";

  quiet = o.quiet;
  annoying = o.annoying;

  if (o.flush)
    globals[ G_CACHE_FLUSH ] = 1;
  else if (o.noflush)
    globals[ G_CACHE_FLUSH ] = 0;

#if 0
  /* No longer available... */
  if (o.gc_debug_file) {
    gcdebug( o.gc_debug_file );
    exit( 0 );
  }
#endif

  /* Load the heap */
  openheap( o.heapfile );

  if (o.use_static)
    ssize = heap_ssize() + heap_tsize();
  else
    ssize = 0;

  /* This is getting messy. */
  if (!allocate_heap( o.esize, o.ewatermark,
		      ssize,
		      o.rhash, o.ssb,
		      o.requested_old_generations,
		      o.old_gen_info,
		      o.np_gc, o.np_steps, o.np_stepsize ))
    panic( "Unable to allocate heap/create garbage collector." );

  load_heap();
  closeheap();

  if (o.show_heapstats)
    consolemsg( "GC type: %s\n", gctype() );

  /* initialize some policy globals */
  globals[ G_BREAKPT_ENABLE ] =
    (o.enable_breakpoints ? TRUE_CONST : FALSE_CONST);
  globals[ G_SINGLESTEP_ENABLE ] =
    (o.enable_singlestep ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER_ENABLE ] =
    (o.enable_timer ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER ] = o.timerval;
  globals[ G_RESULT ] = fixnum( 0 );  /* No arguments */

  handle_signals( 0 /* FIXME -- unused */ );
  init_stats( o.show_heapstats );

  /* Allocate vector of command line arguments and pass it as an
   * argument to the startup procedure.
   */
  globals[ G_REG1 ] = allocate_argument_vector( o.restc, o.restv );
  globals[ G_RESULT ] = fixnum( 1 );
  scheme_start();

  /* control does not usually reach this point */

  consolemsg( "Scheme_start() returned with value %08lx", globals[ G_RESULT ]);
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

  if (in_panic) _exit( 1 );
  in_panic = 1;
  exit( 1 );
  /* Never returns. Return type is 'int' to facilitate an idiom. */
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

  if (in_panic) _exit( 1 );
  in_panic = 1;
  abort();
  /* Never returns. Return type is 'int' to facilitate an idiom. */
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

void consolemsg( const char *fmt, ... )
{
  va_list args;

  if (quiet) return;

  va_start( args, fmt );
  vfprintf( stderr, fmt, args );
  fprintf( stderr, "\n" );
  va_end( args );
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
 * Signal handling.
 *
 */
static void handle_signals( int ifreq )
{
  static void inthandler(), fpehandler();

  signal( SIGINT, inthandler );
  signal( SIGQUIT, inthandler );
  signal( SIGFPE, fpehandler );
}


static void inthandler()
{
  /* Eventually this should do something intelligent to clear the timer... */
  hardconsolemsg( "Caught signal -- exiting.\n" );
  /* can't go to localdebugger because the registers are not saved */
  exit( 1 );
}

static void fpehandler()
{
  hardconsolemsg( "Arithmetic exception (SIGFPE).\n" );
  exit( 1 );
}


/****************************************************************************
 *
 * Command line parsing.
 *
 * As of 0.26, there can be multiple old generations, and each generation can
 * have its parameters specified individually or as a group.  For the time
 * being (one can make this arbitrarily complicated...) -tsize indicates
 * the size for all old generations for which an individual size has not
 * been given, and -tsizen for some 0 <= _n_ <= 31 indicates the size for
 * generation _n_.  Ditto for -thimark and -tlomark.  It is an error to
 * specify the sizes for generations that do not exist, although that error is
 * not detected.  The switch -old n indicates the desired number of old
 * generations.
 */

static int sizearg( char *str, int *argc, char ***argv, unsigned *var );
static int hsizearg( char *str, int *argc, char ***argv, unsigned *var,
		    int *loc );
static int numbarg( char *str, int *argc, char ***argv, unsigned *var );
static int hnumbarg( char *str, int *argc, char ***argv, unsigned *var,
		    int *loc );

static void
parse_options( int argc, char **argv, opt_t *o )
{
  int i, loc;
  unsigned val;

  while (--argc) {
    ++argv;
    if (sizearg( "-old", &argc, &argv, &o->requested_old_generations )) {
      if (o->requested_old_generations >= o->old_generations)
	invalid( "-old" );
    }
    else if (sizearg( "-esize", &argc, &argv, &o->esize )) ;
    else if (sizearg( "-rhash", &argc, &argv, &o->rhash )) ;
    else if (sizearg( "-ssb", &argc, &argv, &o->ssb )) ;
    else if (numbarg( "-emark", &argc, &argv, &o->ewatermark )) ;
    else if (numbarg( "-ticks", &argc, &argv, &o->timerval )) ;
    else if (hsizearg( "-tsize", &argc, &argv, &val, &loc )) {
      if (loc > 0) {
	o->old_gen_info[loc-1].size = val;
	o->size_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->old_generations ; i++ )
	  if (!o->size_explicit[i]) o->old_gen_info[i].size = val;
    }
    else if (hnumbarg( "-thimark", &argc, &argv, &val, &loc )) {
      if (loc > 0) {
	o->old_gen_info[loc-1].hiwatermark = val;
	o->himark_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->old_generations ; i++ )
	  if (!o->himark_explicit[i])
	    o->old_gen_info[i].hiwatermark = val;
    }
    else if (hnumbarg( "-tlomark", &argc, &argv, &val, &loc )) {
      if (loc > 0) {
	o->old_gen_info[loc-1].lowatermark = val;
	o->lomark_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->old_generations ; i++ )
	  if (!o->lomark_explicit[i])
	    o->old_gen_info[i].lowatermark = val;
    }
    else if (numbarg( "-np:steps", &argc, &argv, &o->np_steps ))
      o->np_gc = 1;
    else if (sizearg( "-np:size", &argc, &argv, &o->np_stepsize ))
      o->np_gc = 1;
    else if (strcmp( *argv, "-break" ) == 0)
      o->enable_breakpoints = 1;
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
    else if (strcmp( *argv, "-flush" ) == 0)
      o->flush = 1;
    else if (strcmp( *argv, "-noflush" ) == 0)
      o->noflush = 1;
    else if (strcmp( *argv, "-nostatic" ) == 0)
      o->use_static = 0;
    else if (strcmp( *argv, "-args" ) == 0) {
      o->restc = argc-1;
      o->restv = argv+1;
      break;
    }
    else if (strcmp( *argv, "-gcdebug" ) == 0) {
      if (argc == 1)
	invalid( "-gcdebug" );
      o->gc_debug_file = *(argv+1);
      ++argv;
      --argc;
    }
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
}

static int 
sizearg( char *str, int *argc, char ***argv, unsigned *loc ) 
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
hsizearg( char *str, int *argc, char ***argv, unsigned *var, int *loc ) 
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
numbarg( char *str, int *argc, char ***argv, unsigned *loc )
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
hnumbarg( char *str, int *argc, char ***argv, unsigned *var, int *loc )
{
  int l = strlen(str);

  if (strncmp( **argv, str, l ) == 0) {
    if (**argv+l != 0) {
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

static int getsize( char *s, unsigned *p )
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

static void help( void )
{
  consolemsg("Usage: larceny [ options ][ heapfile ][-args args-to-scheme]" );
  consolemsg("" );
  consolemsg("Options:" );
  consolemsg("\t-esize    nnnn  Ephemeral-area size in bytes" );
  consolemsg("\t-emark    n     Ephemeral-area watermark in percent" );
  consolemsg("\t-rhash    nnnn  Remembered-set hash table size, in elements");
  consolemsg("\t                 (The size must be a power of 2)");
  consolemsg("\t-ssb      nnnn  Remembered-set Sequential Store Buffer (SSB)");
  consolemsg("\t                 size in elements" );
  consolemsg("\t-old      n     Number of old generations." );
  consolemsg("\t-np:steps n     Number of steps in the non-predictive gc." );
  consolemsg("\t-np:size  nnnn  Size of each step in non-predictive gc." );
  consolemsg("\t-tsize    nnnn  Old-area size in bytes." );
  consolemsg("\t-thimark  n     Old-area high watermark in percent" );
  consolemsg("\t-tlomark  n     Old-area low watermark in percent" );
  consolemsg("\t-tsize#   nnnn  Old-area number '#' size" );
  consolemsg("\t-thimark# n     Old-area number '#' high watermark" );
  consolemsg("\t-tlomark# n     Old-area number '#' low watermark" );
  consolemsg("\t-nostatic       Don't use the static area" );
  consolemsg("\t-ticks    nnnn  Initial timer interval value" );
  consolemsg("\t-break          Enable breakpoints" );
  consolemsg("\t-step           Enable single-stepping" );
/*
  consolemsg( "\t-timer          Enable timer interrupts at startup" ); 
*/
  consolemsg("\t-stats          Print memory statistics" );
  consolemsg("\t-quiet          Suppress nonessential messages" );
  consolemsg("\t-annoy-user     Print annoying messages" );
  consolemsg("\t-help           Print this message" );
  consolemsg("\t-gcdebug file   GC debug mode (developers only!)" );
  consolemsg("" );
  consolemsg("Values can be decimal, octal (0nnn), hex (0xnnn), or suffixed");
  consolemsg("with K (for KB) or M (for MB) when that makes sense." );
  consolemsg("");
  consolemsg("The larceny user's manual is available on the World-Wide Web at");
  consolemsg("  http://www.ccs.neu.edu/home/lth/larceny/manual.html");
  exit( 0 );
}

int memfail( int code, char *fmt, ... )
{
  va_list args;
  static char *code_str[] = { "malloc", "heap", "realloc", "calloc", "rts" };

  va_start( args, fmt );
  fprintf( stderr, "Allocation failed (code '%s'): ", code_str[code] );
  vfprintf( stderr, fmt, args );
  va_end( args );
  abort();
}


/* eof */
