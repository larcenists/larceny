/* Rts/Sys/larceny.c.
 * Larceny run-time system (Unix) -- main file.
 *
 * $Id: larceny.c,v 1.15 1997/07/07 20:09:30 lth Exp $
 *
 * On-line manual available at http://www.ccs.neu.edu/home/lth/larceny.
 */

#include <stdio.h>
#include <signal.h>
#include <stdarg.h>
#include <memory.h>
#include <malloc.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

typedef struct opt opt_t;

/* Argument parsing structure */
struct opt {
  int        maxheaps;
  gc_param_t gc_info;
  int        size_explicit[ MAX_HEAPS ];
  int        himark_explicit[ MAX_HEAPS ];
  int        lomark_explicit[ MAX_HEAPS ];
  int        oflomark_explicit[ MAX_HEAPS ];
  unsigned   timerval;
  unsigned   enable_singlestep;
  unsigned   enable_breakpoints;
  unsigned   enable_timer;
  unsigned   show_heapstats;
  char       *heapfile;
  int        quiet;
  int        annoying;
  int        supremely_annoying;
  int        flush;
  int        noflush;
  int        reorganize_and_dump;
  int        restc;
  char       **restv;
  char       *gc_debug_file;
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
static int supremely_annoying = 0;

/* Genesis. */
int main( argc, argv )
int argc;
char **argv;
{
  opt_t o;

  memset( &o, 0, sizeof( o ) );
  o.maxheaps = MAX_HEAPS;
  o.timerval = 0xFFFFFFFF;
  o.heapfile = 0;
  o.restv = 0;

  o.gc_info.heaps = 2;   /* dual-heap generational collector */
  o.gc_info.heap_info = (heap_info_t*)malloc( sizeof(heap_info_t)*MAX_HEAPS );
  memset( o.gc_info.heap_info, 0, sizeof( heap_info_t )*MAX_HEAPS );
  o.gc_info.use_static_heap = 1;
  o.gc_info.globals = globals;

  cache_setup();
  consolemsg( "Larceny v%s/%s (%s;%s) (%s/%s)",
	      version, 
	      gc_technology,
	      osname, 
	      (globals[ G_CACHE_FLUSH ] ? "split" : "unified"),
	      user, date );

  parse_options( argc, argv, &o );
  if (o.heapfile == 0)
    o.heapfile = "larceny.heap";

  /* HACK! HACK! HACK! FIXME!
   * Static area currently disabled with stop-and-copy system.
   */
  if (o.gc_info.heaps == 1) {
    if (o.gc_info.use_static_heap) {
      consolemsg( "The static area is disabled with the stop+copy gc." );
      consolemsg( "Use -nostatic to disable this message." );
      o.gc_info.use_static_heap = 0;
    }
  }

  quiet = o.quiet;
  annoying = o.annoying;
  supremely_annoying = o.supremely_annoying;
#ifdef DEBUG
  annoying = supremely_annoying = 1;
#endif

  if (o.flush)
    globals[ G_CACHE_FLUSH ] = 1;
  else if (o.noflush)
    globals[ G_CACHE_FLUSH ] = 0;

  /* Load the heap */
  openheap( o.heapfile );

  if (o.gc_info.use_static_heap)
    o.gc_info.static_size = heap_ssize() + heap_tsize();

  if (!allocate_heap( &o.gc_info ))
    panic( "Unable to allocate heap/create garbage collector." );

  load_heap();
  closeheap();

  if (o.reorganize_and_dump) {
    char buf[ PATH_MAX ];

    if (!o.gc_info.use_static_heap)
      panic( "No static heap to reorganize!" );
    sprintf( buf, "%s.split", o.heapfile );
    if (!reorganize_and_dump_static_heap( buf ))
      panic( "Failed heap reorganization." );
    goto end;
  }

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

 end:
  exit( 0 );
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
  static void inthandler();
  static void fpehandler();

  signal( SIGINT, inthandler );
  signal( SIGQUIT, inthandler );
  signal( SIGFPE, fpehandler );
}


static void inthandler( int sig, int code, struct sigcontext *scp, char *addr )
{
  /* The commented-out code works, but the world is not quite ready for it */
#if 0
  word g;

  g = globals[ G_SIGNALS ];
  if (g != FALSE_CONST)		/* make sure it's installed */
    vector_set(gcell_ref(g), sig, vector_ref(gcell_ref(g), sig)+fixnum( 1 ));
#endif

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
    if (numbarg( "-heaps", &argc, &argv, &o->gc_info.heaps )) {
      if (o->gc_info.heaps >= o->maxheaps || o->gc_info.heaps < 1)
	invalid( "-heaps" );
    }
    else if (sizearg( "-rhash", &argc, &argv, &o->gc_info.rhash )) ;
    else if (sizearg( "-ssb", &argc, &argv, &o->gc_info.ssb )) ;
    else if (numbarg( "-ticks", &argc, &argv, &o->timerval )) ;
    else if (hsizearg( "-size", &argc, &argv, &val, &loc )) {
      if (loc < 0 || loc > o->maxheaps)
	invalid( "-size" );
      else if (loc > 0) {
	o->gc_info.heap_info[loc-1].size_bytes = val;
	o->size_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->maxheaps ; i++ )
	  if (!o->size_explicit[i]) o->gc_info.heap_info[i].size_bytes = val;
    }
    else if (hnumbarg( "-himark", &argc, &argv, &val, &loc )) {
      if (loc < 0 || loc > o->maxheaps) 
	invalid( "-himark" );
      else if (loc > 0) {
	o->gc_info.heap_info[loc-1].hi_mark = val;
	o->himark_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->maxheaps ; i++ )
	  if (!o->himark_explicit[i]) o->gc_info.heap_info[i].hi_mark = val;
    }
    else if (hnumbarg( "-lomark", &argc, &argv, &val, &loc )) {
      if (loc < 0 || loc > o->maxheaps)
	invalid( "-lomark" );
      else if (loc > 0) {
	o->gc_info.heap_info[loc-1].lo_mark = val;
	o->lomark_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->maxheaps ; i++ )
	  if (!o->lomark_explicit[i]) o->gc_info.heap_info[i].lo_mark = val;
    }
    else if (hnumbarg( "-overflowmark", &argc, &argv, &val, &loc ) ||
	     hnumbarg( "-oflomark", &argc, &argv, &val, &loc )) {
      if (loc < 0 || loc > o->maxheaps) 
	invalid( "-overflowmark" );
      else if (loc > 0) {
	o->gc_info.heap_info[loc-1].oflo_mark = val;
	o->oflomark_explicit[loc-1] = 1;
      }
      else 
	for ( i=0 ; i < o->maxheaps ; i++ )
	  if (!o->oflomark_explicit[i]) o->gc_info.heap_info[i].oflo_mark=val;
    }
    else if (numbarg( "-steps", &argc, &argv, &o->gc_info.np_steps ))
      o->gc_info.use_np_heap = 1;
    else if (sizearg( "-stepsize", &argc, &argv, &o->gc_info.np_stepsize ))
      o->gc_info.use_np_heap = 1;
    else if (strcmp( *argv, "-np" ) == 0)
      o->gc_info.use_np_heap = 1;
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
    else if (strcmp( *argv, "-annoy-user-greatly" ) == 0) {
      o->annoying = 1;
      o->supremely_annoying = 1;
    }
    else if (strcmp( *argv, "-flush" ) == 0)
      o->flush = 1;
    else if (strcmp( *argv, "-noflush" ) == 0)
      o->noflush = 1;
    else if (strcmp( *argv, "-nostatic" ) == 0)
      o->gc_info.use_static_heap = 0;
    else if (strcmp( *argv, "-reorganize-and-dump" ) == 0)
      o->reorganize_and_dump = 1;
    else if (strcmp( *argv, "-args" ) == 0) {
      o->restc = argc-1;
      o->restv = argv+1;
      break;
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
  consolemsg("Usage: larceny [ options ][ heapfile ][-args arg-to-scheme ...]");
  consolemsg("" );
  consolemsg("Options:" );
#ifndef BDW_GC
  consolemsg("\t-heaps    n     Number of non-static heaps." );
  consolemsg("\t-size     nnnn  Per-heap size in bytes." );
  consolemsg("\t-size#    nnnn  Heap number '#' size." );
  consolemsg("\t-himark   n     Per-heap expansion watermark in percent." );
  consolemsg("\t-himark#  n     Heap number '#' expansion watermark." );
  consolemsg("\t-lomark   n     Per-heap contraction watermark in percent." );
  consolemsg("\t-lomark#  n     Heap number '#' contraction watermark." );
  consolemsg("\t-oflomark n     Per-heap promotion watermark in percent." );
  consolemsg("\t-oflomark# n    Heap number '#' promotion watermark." );
  consolemsg("\t-np       n     Use the non-predictive collector." );
  consolemsg("\t-steps    n     Number of steps in the non-predictive gc." );
  consolemsg("\t-stepsize nnnn  Size of each step in non-predictive gc." );
  consolemsg("\t-nostatic       Don't use the static area." );
  consolemsg("\t-reorganize-and-dump  Split static heap." );
#endif
  consolemsg("\t-stats          Print startup memory statistics." );
  consolemsg("\t-quiet          Suppress nonessential messages." );
  consolemsg("\t-annoy-user     Print annoying messages." );
  consolemsg("\t-annoy-user-greatly  Print very annoying messages." );
  consolemsg("\t-help           Print this message." );
  consolemsg("");
#ifndef BDW_GC
  consolemsg("\t-rhash    nnnn  Remembered-set hash table size, in elements");
  consolemsg("\t                 (The size must be a power of 2)");
  consolemsg("\t-ssb      nnnn  Remembered-set Sequential Store Buffer (SSB)");
  consolemsg("\t                 size in elements" );
#endif
  consolemsg("\t-ticks    nnnn  Initial timer interval value" );
  consolemsg("\t-break          Enable breakpoints" );
  consolemsg("\t-step           Enable single-stepping" );
/*
  consolemsg( "\t-timer          Enable timer interrupts at startup" ); 
*/
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
