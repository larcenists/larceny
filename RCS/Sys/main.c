/*
 * Scheme 313 run-time system
 * C-language procedures for system initialization (Berkeley UNIX)
 *
 * $Id: main.c,v 1.3 91/07/12 03:15:49 lth Exp Locker: lth $
 *
 * Exports the procedures C_init() and panic().
 * Accepts the following options from the command line:
 *
 *    -t nnnn    Tenured heap size in bytes (decimal)
 *    -e nnnn    Ephemeral heap size in bytes (decimal)
 *    -l nnnn    Tenuring collection limit in bytes (decimal)
 *    -S nnnn    Static heap size in bytes (decimal) (?)
 *    -s nnnn    Stack cache size in bytes (decimal)
 *    -I nnnn    The initial value of the timer.
 *    -h file    Tenured-area heap file. Mandatory.
 *    -H file    Static-area heap file. Optional.
 *
 * Hack options (for benchmarks...)
 *
 *    -a nnnn    Argument for the benchmark. Supply any number of these.
 */

#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "machine.h"
#include "gcinterface.h"
#include "millicode.h"
#include "offsets.h"
#include "main.h"
#include "macros.h"
#include "exceptions.h"

static void invalid(), setup_interrupts();
void panic();

word globals[ GLOBALS_TABLE_SIZE ];

/*
 * Parse command line and initialize runtime system components, then
 * call Scheme initializer.
 */
int main( argc, argv, envp )
int argc;
char **argv, **envp;
{
  unsigned long tsize = 0, esize = 0, elimit = 0, ssize = 0, Ssize = 0;
  unsigned ifreq = 0xFFFFFFFF;
  unsigned arg = 0;
  unsigned milliseconds;
  struct rusage r1, r2;
  word args[ 10 ];
  int argcount = 0, i;
  char *heapfile = NULL;
  FILE *heap;

  while (--argc) {
    ++argv;
    if (**argv == '-') {
      switch (*(*argv+1)) {
        case 'e' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &esize ) != 1)
	    invalid( "-e" );
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
	  ++argv; --argc;
	  break;
	case 's' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &ssize ) != 1)
	    invalid( "-s" );
	  ++argv; --argc;
	  break;
	case 'S' :
	  if (argc == 1 || sscanf( *(argv+1), "%lu", &Ssize ) != 1)
	    invalid( "-S" );
	  ++argv; --argc;
	  break;
	case 'I' :
	  if (argc == 1 || sscanf( *(argv+1), "%u", &ifreq ) != 1)
	    invalid( "-I" );
	  ++argv; --argc;
	  break;
	case 'a' :
	  if (argc == 1 || sscanf( *(argv+1), "%u", &arg ) != 1)
	    invalid( "-a" );
	  ++argv; --argc;
	  args[ argcount++ ] = arg;
	  break;
	case 'h' :
	  if (argc == 1)
	    invalid( "-h" );
	  heapfile = *++argv; --argc;
	  break;
	case 'H' :
	  fprintf( stderr, "Don't know about -H yetb.\n" );
	  exit( 1 );
	default :
	  fprintf( stderr, "Invalid option '%s'\n", *argv );
	  exit( 1 );
      }
    }
    else {
      fprintf( stderr, "Invalid option '%s'\n", *argv );
      exit( 1 );
    }
  }

  /* Millicode may be used by other initializers and must be done first */
  init_millicode();

  /* Allocate memory and set up stack. Ignore command line switches for now. */
  if (init_mem( 1024*1024, 1024*1024, 0, 1024*64, 1024*700 ) == 0)
    panic( "Unable to initialize memory!" );

  if (heapfile == NULL)
    panic( "Not without a heap file, you don't." );

  if ((heap = fopen( heapfile, "r" )) == NULL)
    panic( "Unable to open heap file." );

  if (load_heap( heap ) == -1)
    panic( "Error in loading heap file!" );

  fclose( heap );

  /* Catch whatever interesting interrupts there are */
  setup_interrupts();

#if 0
  /* Setup the i/o system */
  init_iosys();
#endif

  globals[ TIMER_OFFSET ] = globals[ INITIAL_TIMER_OFFSET ];
  globals[ INITIAL_TIMER_OFFSET ] = (ifreq == 0 ? 1 : ifreq);

  getrusage( RUSAGE_SELF, &r1 );

  globals[ RESULT_OFFSET ] = fixnum( argcount );
  for (i = 0 ; i < argcount ; i++ )
    globals[ REG1_OFFSET+i ] = fixnum( args[ i ] );

  schemestart();

  getrusage( RUSAGE_SELF, &r2 );
  milliseconds = ((r2.ru_utime.tv_sec - r1.ru_utime.tv_sec)*1000
		  + (r2.ru_utime.tv_usec - r1.ru_utime.tv_usec)/1000);
  printf( "Time: %u milliseconds.\n", milliseconds );
  printf( "Result: %lx\n", globals[ RESULT_OFFSET ] );

  exit( 0 );
}


/*
 * Used by all sorts of run-time procedures.
 */
void panic( s )
char *s;
{
  fprintf( stderr, "Scheme Panic: %s\n", s );
  exit( 1 );
}


/*
 * C-language exception handler (called from exception.s)
 * This is a temporary hack; eventually this procedure will not be needed.
 */
void C_exception( i )
{
  static char *s[] = { "Timer", "Type", "Procedure", "Argument" };

  printf( "exception: %s\n.", s[ i ] );
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
  static void inthandler();

  signal( SIGINT, inthandler );
  signal( SIGQUIT, inthandler );
}


/*
 * Interrupts and quits abort the program.
 */
static void inthandler()
{
  fprintf( stderr, "Caught signal -- Aborted.\n" );
  exit( 1 );
}


/*
 * Invalid command line argument error handler.
 */
static void invalid( s )
char *s;
{
  fprintf( stderr, "Invalid argument to option '%s'\n", s );
  exit( 1 );
}
