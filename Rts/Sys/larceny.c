/*
 * This is the file Rts/Sys/larceny.c.
 * $Id$
 *
 * Larceny run-time system (Unix) -- main file.
 *
 * History
 *   February 16, 1995 / lth (v0.23)
 *     Added -help option.
 *
 *   June 29 - July 1, 1994 / lth (v0.20)
 *     Cleaned up a bit.
 *
 * See the manual page file ``larceny.1'' for instructions.
 */

#include <stdio.h>
#include <signal.h>
#include <varargs.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

static void invalid();
static void handle_signals();
static void usage();
static void usage2();
static void help();
static void parse_options();
static int getsize();

/* 'quiet' controls consolemsg() */
static int quiet = 0;

/* Genesis. */
int main( argc, argv )
int argc;
char **argv;
{
  unsigned esize = 0;
  unsigned tsize = 0;
  unsigned rpool = 0;
  unsigned rhash = 0;
  unsigned ssb = 0;
  unsigned ewatermark = 0;
  unsigned thiwatermark = 0;
  unsigned tlowatermark = 0;
  unsigned timerval = 0xFFFFFFFF;
  unsigned enable_singlestep = 0;
  unsigned enable_breakpoints = 0;
  unsigned enable_timer = 0;           /* timer off at startup */
  unsigned show_heapstats = 0;
  char *heapfile = NULL;
  unsigned q = 0;
  int restc = 0;
  char **restv = 0;
  word av;

  cache_setup();
  consolemsg( "Larceny v%s (%s;%s;%s) (%s/%s)",
	      version, 
	      osname, gctype, 
	      (globals[ G_CACHE_FLUSH ] ? "flush" : "noflush"),
	      user, date );

  parse_options( argc, argv,
		 &esize,
		 &tsize,
		 &rpool,
		 &rhash,
		 &ssb,
		 &ewatermark,
		 &thiwatermark,
		 &tlowatermark,
		 &timerval,
		 &enable_singlestep,
		 &enable_breakpoints,
		 &enable_timer,
                 &show_heapstats,
                 &heapfile,
                 &q,
		 &restc,
		 &restv );

  quiet = q;

  /* create the remembered set */
  if (!create_remset( rhash, rpool, ssb ))
    panic( "Unable to create remembered set." );

  /* Load the heap */
  openheap( heapfile );

  /* The tenured area will never be smaller than a half megabyte */
  tsize = max( tsize, roundup( heap_tsize(), MEGABYTE/2 ) );
  if (!allocate_heap( esize, tsize, heap_ssize(), ewatermark,
		      thiwatermark, tlowatermark ))
    panic( "Unable to allocate heap." );

  load_heap();
  closeheap();

  /* initialize some policy globals */
  globals[ G_BREAKPT_ENABLE ] =
    (enable_breakpoints ? TRUE_CONST : FALSE_CONST);
  globals[ G_SINGLESTEP_ENABLE ] =
    (enable_singlestep ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER_ENABLE ] =
    (enable_timer ? TRUE_CONST : FALSE_CONST );
  globals[ G_TIMER ] = timerval;
  globals[ G_RESULT ] = fixnum( 0 );  /* No arguments */

  handle_signals();
  memstat_init( show_heapstats );

  /* Allocate vector of command line arguments and pass it as an
   * argument to the startup procedure.
   */
  globals[ G_REG1 ] = allocate_argument_vector( restc, restv );
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

int panic( va_alist )
va_dcl
{
  static int in_panic = 0;
  va_list args;
  char *fmt;

  va_start( args );
  fmt = va_arg( args, char * );
  fprintf( stderr, "Larceny Panic: " );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );

  if (in_panic) _exit( 1 );
  in_panic = 1;
  exit( 1 );
  /* Never returns. Return type is 'int' to facilitate an idiom. */
}

void consolemsg( va_alist )
va_dcl
{
  va_list args;
  char *fmt;

  va_start( args );
  if (!quiet) {
    fmt = va_arg( args, char * );
    vfprintf( stderr, fmt, args );
    fprintf( stderr, "\n" );
  }
  va_end( args );
}

void hardconsolemsg( va_alist )
va_dcl
{
  va_list args;
  char *fmt;

  va_start( args );
  fmt = va_arg( args, char * );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );
}


/****************************************************************************
 *
 * Signal handling.
 *
 */
static void handle_signals( ifreq )
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
 */
static 
void parse_options( argc, argv,
                   esize, tsize, rpool, rhash, ssb, emark, thimark, tlomark,
		   ticks, enable_singlestep, enable_break, 
		   enable_timer, show_heapstats, heapfile, quiet, 
		   restc, restv )
int argc, *restc;
char **argv, ***restv;
unsigned *esize, *tsize, *rpool, *rhash, *ssb, *emark, *thimark, *tlomark,
         *ticks, *enable_singlestep, *enable_break, *enable_timer, 
         *show_heapstats, *quiet;
char **heapfile;
{
  while (--argc) {
    ++argv;
    if (strcmp( *argv, "-tsize" ) == 0) {
      if (argc == 1 || !getsize( *(argv+1), tsize )) invalid( "-tsize" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-esize" ) == 0) {
      if (argc == 1 || !getsize( *(argv+1), esize )) invalid( "-esize" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-rpool" ) == 0) {
      if (argc == 1 || !getsize( *(argv+1), rpool )) invalid( "-rpool" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-rhash" ) == 0) {
      if (argc == 1 || !getsize( *(argv+1), rhash )) invalid( "-rhash" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-ssb" ) == 0) {
      if (argc == 1 || !getsize( *(argv+1), ssb )) invalid( "-ssb" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-emark" ) == 0) {
      if (argc == 1 || sscanf( *(argv+1), "%d", emark ) != 1)
	invalid( "-emark" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-thimark" ) == 0) {
      if (argc == 1 || sscanf( *(argv+1), "%d", thimark ) != 1)
	invalid( "-thimark" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-tlomark" ) == 0) {
      if (argc == 1 || sscanf( *(argv+1), "%d", tlomark ) != 1)
	invalid( "-tlomark" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-ticks" ) == 0) {
      if (argc == 1 || sscanf( *(argv+1), "%i", ticks ) != 1)
	invalid( "-ticks" );
      ++argv; --argc;
    }
    else if (strcmp( *argv, "-break" ) == 0) {
      *enable_break = 1;
    }
    else if (strcmp( *argv, "-step" ) == 0) {
      *enable_singlestep = 1;
    }
/*
    else if (strcmp( *argv, "-timer" ) == 0) {
      *enable_timer = 1;
    }
*/
    else if (strcmp( *argv, "-memstats" ) == 0) {
      *show_heapstats = 1;
    }
    else if (strcmp( *argv, "-help" ) == 0 || strcmp( *argv, "-h" ) == 0)
      help();
    else if (strcmp( *argv, "-quiet" ) == 0) 
      *quiet = 1;
    else if (strcmp( *argv, "-args" ) == 0) {
      *restc = argc-1;
      *restv = argv+1;
      argc=1;
    }
    else if (strcmp( *argv, "-heap" ) == 0) {
      if (*heapfile != 0) {
	consolemsg( "Error: Only one heap file allowed." );
	usage();
      }
      argc--;
      argv++;
      *heapfile = *argv;
    }
    else if (**argv == '-') {
      consolemsg( "Error: Invalid option '%s'", *argv );
      usage2();
    }
    else if (*heapfile != NULL) {
      consolemsg( "Error: Only one heap file allowed." );
      usage();
    }
    else
      *heapfile = *argv;
  }
  if (*heapfile == 0) {
    consolemsg( "Error: Heap file not specified." );
    usage();
  }
}

static int getsize( s, p )
char *s;
unsigned *p;
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

static void invalid( s )
char *s;
{
  consolemsg( "Error: Invalid argument to option '%s'", s );
  usage2();
}

static void usage()
{
  consolemsg( "" );
  consolemsg( "Usage: larceny [ options ] heapfile [-args arguments]" );
  usage2();
}

static void usage2()
{
  consolemsg( "Type \"larceny -help\" for help." );
  exit( 1 );
}

static void help()
{
  consolemsg( "Usage: larceny [ options ] heapfile [-args arguments]" );
  consolemsg( "" );
  consolemsg( "Options:" );
  consolemsg( "\t-tsize   nnnn   Initial tenured area size in bytes" );
  consolemsg( "\t-esize   nnnn   Ephemeral area size in bytes" );
  consolemsg( "\t-rpool   nnnn   Remembered set storage pool size in elements" );
  consolemsg( "\t-rhash   nnnn   Remembered set hash table in elements (pow2)" );
  consolemsg( "\t-ssb     nnnn   SSB size in elements" );
  consolemsg( "\t-emark   n      Ephemeral area watermark in percent" );
  consolemsg( "\t-thimark n      Tenured area high watermark in percent" );
  consolemsg( "\t-tlomark n      Tenured area low watermark in percent" );
  consolemsg( "\t-ticks   nnnn   Initial timer interval value" );
  consolemsg( "\t-break          Enable breakpoints" );
  consolemsg( "\t-step           Enable single-stepping" );
/*   consolemsg( "\t-timer          Enable timer interrupts at startup" ); */
  consolemsg( "\t-memstats       Print memory statistics" );
  consolemsg( "\t-quiet          Suppress nonessential messages" );
  consolemsg( "\t-help           Print this message" );
  consolemsg( "" );
  consolemsg( "Values can be in decimal, octal (0nnn), hex (0xnnn), or suffixed" );
  consolemsg( "by K (for KB) or M (for MB) when that makes sense." );
  exit( 0 );
}


/* eof */
