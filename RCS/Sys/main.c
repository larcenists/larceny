/*
 * Larceny -- A run-time system for IEEE/R4RS Scheme on the Sun Sparcstation.
 *
 * $Id: main.c,v 1.11 1992/06/10 09:06:05 lth Exp lth $
 *
 * LARCENY (1)
 *
 * NAME
 *   larceny -- local scheme system
 *
 * SYNOPSIS
 *   larceny [ options ] heapfile
 *
 * DESCRIPTION
 *   Larceny accepts zero or more options are a heap image file. It reads the
 *   heap image into memory and transfers control to the startup procedure in
 *   the heap image, which in an interactive setting is the Scheme REP-loop.
 *
 * OPTIONS
 *   Accepts the following options from the command line:
 *
 *    -t nnnn    Tenured heap size in bytes (decimal)
 *    -e nnnn    Ephemeral heap size in bytes (decimal)
 *    -l nnnn    Ephemeral->tenuring collection limit in bytes (decimal)
 *    -s nnnn    Stack cache size in bytes (decimal)
 *    -I nnnn    The initial value of the timer.
 *    -E         Load non-static part of heap into ephemeral area.
 *    -d         Raise the level of diagnostic output by one.
 *    -B         stop at every breakpoint (not much use any more).
 *    -z         Turn on single-stepping (ditto).
 *    -P         Print heap limits after initialization.
 *    -v         Do a vadvise( VA_ANOM )
 *
 * AUTHORS
 *   Lars Thomas Hansen (runtime system and some libraries);
 *   Will Clinger (compiler and many libraries).
 *
 * BUGS
 *   Lots. 
 *
 * SEE ALSO
 *   "Revised^4 Report on Algorithmic Language Scheme"
 *   Larceny Reference Manual (forthcoming).
 *   The on-line manual page (which is an expanded version of this).
 *
 * Exports the procedures C_exception() and C_panic().
 */


#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "larceny.h"
#include "machine.h"
#include "gcinterface.h"
#include "offsets.h"
#include "millicode.h"
#include "macros.h"
#include "exceptions.h"
#include "layouts.h"

static void invalid(), setup_interrupts();
void C_panic();

word globals[ GLOBALS_TABLE_SIZE ];    /* All system globals */

/*
 * Parse command line and initialize runtime system components, then
 * call Scheme initializer.
 */
int main( argc, argv, envp )
int argc;
char **argv, **envp;
{
  unsigned long tsize = 1024*1024, 
                esize = 1024*1024, 
                elimit = 1024*500,
                ssize = 1024*64, 
                Ssize = 0;
  unsigned ifreq = 0xFFFFFFFF;
  unsigned arg = 0;
  unsigned milliseconds;
  struct rusage r1, r2;
  char *heapfile = NULL;
  FILE *heap;
  int singlestep = 0;
  int which_heap = 1; /* default tenured */
  int has_user_elimit = 0;
  int print_heap_limits = 0;
  int vadv = 0;
  extern char *version, *user, *date;

  printf( "Larceny version %s (Compiled by %s on %s)\n", version, user, date );
  while (--argc) {
    ++argv;
    if (**argv == '-') {
      switch (*(*argv+1)) {
        case 'e' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &esize ) != 1)
	    invalid( "-e" );
	  if (!has_user_elimit)
	    elimit = esize / 2;
	  ++argv; --argc;
	  break;
	case 't' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &tsize ) != 1)
	    invalid( "-t" );
	  ++argv; --argc;
	  break;
	case 'l' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &elimit ) != 1)
	    invalid( "-l" );
	  has_user_elimit = 1;
	  ++argv; --argc;
	  break;
	case 's' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &ssize ) != 1)
	    invalid( "-s" );
	  ++argv; --argc;
	  break;
	case 'd' :
	  globals[ DEBUGLEVEL_OFFSET ]++;
	  break;
	case 'z' :
	  singlestep = 1;
	  break;
	case 'I' :
	  if (argc == 1 || sscanf( *(argv+1), "%u", &ifreq ) != 1)
	    invalid( "-I" );
	  ++argv; --argc;
	  break;
	case 'B' :
	  globals[ BREAKP_OFFSET ] = 1;
	  break;
	case 'E' :
	  which_heap = 0;
	  break;
	case 'P' :
	  print_heap_limits = 1;
	  break;
	case 'v' :
	  vadv = 1;
	  break;
	default :
	  fprintf( stderr, "Invalid option '%s'\n", *argv );
	  usage();
	  exit( 1 );
      }
    }
    else {
      if (heapfile != NULL) {
	fprintf( stderr, "You've already specified a heap file.\n" );
	usage();
	exit( 1 );
      }
      heapfile = *argv;
    }
  }

  /* Millicode may be used by other initializers and must be done first */
  init_millicode();

  /* Allocate memory and set up stack. */
  if (C_init_mem( esize, tsize, Ssize, ssize , elimit, vadv ) == 0)
    C_panic( "Unable to initialize memory!" );

  if (heapfile == NULL) {
    usage();
    exit( 1 );
  }

  if ((heap = fopen( heapfile, "r" )) == NULL)
    C_panic( "Unable to open heap file." );

  if (C_load_heap( heap, which_heap ) == -1)
    C_panic( "Error in loading heap file!" );

  fclose( heap );

  C_setup_resource_usage();

  if (print_heap_limits) {
    printf( "Heap statistics:\n");
    printf( "  Effective size of ephemeral area: %lu bytes\n",
	    globals[ E_LIMIT_OFFSET ] - globals[ E_BASE_OFFSET ] + 4);
    printf( "  Effective size of tenured area: %lu bytes\n",
	    globals[ T_MAX_OFFSET ] - globals[ T_BASE_OFFSET ] + 4);
    printf( "  Effective size of stack cache: %lu bytes\n",
	    globals[ STK_START_OFFSET ] - globals[ STK_LIMIT_OFFSET ] + 4 );
    printf( "  Live tenured data: %lu bytes\n",
	    globals[ T_TOP_OFFSET ] - globals[ T_BASE_OFFSET ] );
    printf( "  Live ephemeral data: %lu bytes\n",
	    globals[ E_TOP_OFFSET ] - globals[ E_BASE_OFFSET ] );
    printf( "  Tenuring limit at %ld bytes\n", globals[ E_MARK_OFFSET ] );
    printf( "\n" );
  }

  /* Catch whatever interesting interrupts there are */
  setup_interrupts();

  globals[ TIMER_OFFSET ] = globals[ INITIAL_TIMER_OFFSET ];
  globals[ INITIAL_TIMER_OFFSET ] = (ifreq == 0 ? 1 : ifreq);
  globals[ RESULT_OFFSET ] = fixnum( 0 );  /* No arguments */
  globals[ SINGLESTEP_OFFSET ] = (singlestep ? TRUE_CONST : FALSE_CONST);

  scheme_start();
  exit( 0 );
}


/*
 * Used by all sorts of run-time procedures.
 * Should make it more like printf().
 */
void C_panic( s )
char *s;
{
  fprintf( stderr, "Larceny Panic: %s\n", s );
  exit( 1 );
}


/*
 * Eventually, we will here move the first instruction of each millicode
 * procedure into the jump table and adjust the branch offset. For now,
 * do nothing.
 */
static init_millicode()
{
}


/*
 * Initialize signal handlers. We catch SIGINT and SIGQUIT.
 */
static void setup_interrupts( ifreq )
unsigned ifreq;
{
  static void inthandler(), fpehandler();

  signal( SIGINT, inthandler );
  signal( SIGQUIT, inthandler );
  signal( SIGFPE, fpehandler );
}


/*
 * Interrupts and quits abort the program.
 */
static void inthandler()
{
  fprintf( stderr, "Caught signal -- Aborted.\n" );
  exit( 1 );
}

static void fpehandler()
{
  fprintf( stderr, "Arithmetic exception (division by zero?).\n" );
  exit( 1 );
}

/*
 * Invalid command line argument error handler.
 */
static void invalid( s )
char *s;
{
  fprintf( stderr, "Invalid argument to option '%s'\n", s );
  usage();
  exit( 1 );
}

static usage()
{
  printf( "Usage: larceny [ options ] heapfilename\n\n" );
  printf( "Options:\n" );
  printf( "\t-t nnnn   Tenured heap size in bytes (decimal)\n" );
  printf( "\t-e nnnn   Ephemeral heap size in bytes (decimal)\n" );
  printf( "\t-l nnnn   Ephemeral area watermark in bytes (decimal)\n" );
  printf( "\t-s nnnn   Stack cache size in bytes (decimal)\n" );
  printf( "\t-I nnnn   Initial timer interval value (decimal)\n" );
  printf( "\t-E        Load non-static part of heap image into ephemeral area.\n" );
  printf( "\t-d        Raise the level of diagnostic output by one\n" );
  printf( "\t-B        Stop at all breakpoints\n" );
  printf( "\t-z        Turn on single-stepping\n" );
  printf( "\t-P        Print memory statistics\n" );
  printf( "\t-v        Do a vadvise( VA_ANOM )\n" );
}

/* eof */
