/*
 * Scheme run-time system
 * C-language procedures for system initialization (SunOS UNIX)
 *
 * $Id$
 *
 * Exports the procedures C_init() and panic().
 * Accepts the following options from the command line:
 *
 *    -t nnnn    Tenured heap size in bytes (decimal)
 *    -e nnnn    Ephemeral heap size in bytes (decimal)
 *    -l nnnn    Tenuring collection limit in bytes (decimal)
 *    -S nnnn    Static heap size in bytes (decimal) (?)
 *    -s nnnn    Stack cache size in bytes (decimal)
 *    -I nnnn    Interrupt frequency
 */

#include <stdio.h>
#include <sys/time.h>
#include <signal.h>
#include "memsupport.h"
#include "offsets.h"

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
  unsigned ifreq = 60;

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
 
  globals[ INITIAL_TIMER_OFFSET ] = (ifreq == 0 ? 1 : ifreq);

  init_mem( ... );
  init_iosys( ... );
  setup_interrupts();

  schemestart();

  exit( 0 );
}


/*
 * Used by all sorts of run-time procedures.
 */
void panic( s )
char *s;
{
  fprintf( stderr, "Scheme Runtime System Panic: %s\n", s );
  exit( 1 );
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
