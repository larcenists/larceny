/*
 * Larceny -- A run-time system for IEEE/R4RS Scheme on the Sun Sparcstation.
 *
 * $Id: main.c,v 1.8 92/02/23 16:56:35 lth Exp Locker: lth $
 *
 * Exports the procedures C_exception() and panic().
 * Accepts the following options from the command line:
 *
 *    -t nnnn    Tenured heap size in bytes (decimal)
 *    -e nnnn    Ephemeral heap size in bytes (decimal)
 *    -l nnnn    Ephemeral->tenuring collection limit in bytes (decimal)
 *    -s nnnn    Stack cache size in bytes (decimal)
 *    -I nnnn    The initial value of the timer.
 *    -h file    Tenured-area heap file. Mandatory.
 *    -H file    Static-area heap file. Optional. (Unimplemented)
 *    -b #       breakpoint stop number (for debugging).
 *    -B         stop at every breakpoint
 *    -d         Raise the level of debugging output by one.
 *               This has an effect only when the programming is compiled with
 *               DEBUG defined.
 *    -z         Turn on single-stepping.
 *
 * BUGS
 *  The '-h' should be implicit.
 */

#define VERSION "0.11"

#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "machine.h"
#include "gcinterface.h"
#include "offsets.h"
#include "main.h"
#include "millicode.h"
#include "macros.h"
#include "exceptions.h"
#include "layouts.h"

static void invalid(), setup_interrupts();
void panic();

word globals[ GLOBALS_TABLE_SIZE ];         /* All system globals */
int  debuglevel = 0;                        /* Except this */

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
                elimit = 1024*700, 
                ssize = 1024*64, 
                Ssize = 0;
  unsigned ifreq = 0xFFFFFFFF;
  unsigned arg = 0;
  unsigned milliseconds;
  struct rusage r1, r2;
  char *heapfile = NULL;
  FILE *heap;
  int singlestep = 0;

  printf( "Larceny version %s (Compiled by %s on %s)\n", VERSION, USER, DATE );
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
	case 'd' :
	  debuglevel++;
	  break;
	case 'z' :
	  singlestep = 1;
	  break;
	case 'I' :
	  if (argc == 1 || sscanf( *(argv+1), "%u", &ifreq ) != 1)
	    invalid( "-I" );
	  ++argv; --argc;
	  break;
	case 'h' :
	  if (argc == 1)
	    invalid( "-h" );
	  heapfile = *++argv; --argc;
	  break;
	case 'H' :
	  fprintf( stderr, "Don't know about -H yetb.\n" );
	  break;
	case 'b' :
	  { int tmp;
	    extern int break_list[], break_count;

	    if (argc == 1 || sscanf( *(argv+1), "%lu", &tmp ) != 1)
	      invalid( "-e" );
	    break_list[ break_count++ ] = tmp;
	    ++argv; --argc;
	  }
	  break;
	case 'B' :
	  { extern int break_always;
	    break_always = 1;
	  }
	  break;
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

  /* Allocate memory and set up stack. */
  if (init_mem( esize, tsize, Ssize, ssize , elimit ) == 0)
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

  globals[ TIMER_OFFSET ] = globals[ INITIAL_TIMER_OFFSET ];
  globals[ INITIAL_TIMER_OFFSET ] = (ifreq == 0 ? 1 : ifreq);
  globals[ RESULT_OFFSET ] = fixnum( 0 );  /* No arguments */
  globals[ SINGLESTEP_OFFSET ] = (singlestep ? TRUE_CONST : FALSE_CONST);

  schemestart();  /* Typically doesn't return. */
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
  static char *s[] = { "Timer Expired", 
		       "Wrong Type",
		       "Not a Procedure",
		       "Wrong Number of Arguments",
		       "Wrong arguments to arithmetic operator",
		       "Undefined global variable" };
  printf( "Scheme 313 exception (PC=0x%08x) (%d): %s.\n\n", 
	 globals[ SAVED_RETADDR_OFFSET ],
	 i,
	 s[ i ] );
  localdebugger();
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
