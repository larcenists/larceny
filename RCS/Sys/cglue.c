/*
 * Scheme 313 run-time system.
 * Millicode in C.
 *
 * $Id: cglue.c,v 1.5 1992/03/31 12:31:38 lth Exp lth $
 *
 * Millicode routines which are written in C and which do not warrant 
 * their own files go in here.
 */

#include <sys/time.h>
#include <sys/resource.h>
#include "larceny.h"
#include "offsets.h"
#include "gcinterface.h"
#include "layouts.h"
#include "macros.h"


/*
 * Millicode to deal with variable-length argument lists.
 *
 * There are four cases, depending on how many arguments are passed and
 * how many that are expected. When this procedure is entered it is
 * assumed that at least the minimum number of arguments are passed; i.e.
 * that check is performed outside this procedure.
 *
 * Let R = (the # of registers), r = (R - 1).
 * Let j = %RESULT (the actual number of arguments).
 * Let n = %ARGREG2 (the number of fixed arguments).
 *
 * Since the compiler enforces that all fixed arguments are in registers,
 * we then have two easy cases (the others are not relevant):
 *
 * Case 0: n < R-2, j < r [i.e. all fixed args and varargs are in registers]
 *   (set! REGn+1 (list REGn+1 ... REGj))
 *
 * Case 1: n < R-2, j >= r [i.e. all fixed args are in registers, but
 *   all varargs are not -- some are in a list in REGr].
 *   (set! REGn+1 (append! (list REGn+1 ... REGr-1) (copylist REGr)))
 */

void C_varargs()
{
  word j = globals[ RESULT_OFFSET ] / 4;          /* Convert from fixnum */
  word n = globals[ ARGREG2_OFFSET ] / 4;         /* Ditto */
  word r = 31;			                  /* Highest register # */
  word R = 32;			                  /* # of registers */
  word *p, *q, *t;
  word k, limit;
  word bytes;

  bytes = 4*(2*(j-n));

  if (bytes == 0) {
    globals[ REG0_OFFSET + n + 1 ] = NIL_CONST;
    return;
  }

  /* At least one vararg to cons up. */

  q = p = C_alloc( bytes );         /* Allocate memory for list. */

  k = n + 1;
  limit = min( j, r-1 );

  while ( k <= limit ) {
    *p = globals[ REG0_OFFSET + k ];
    *(p+1) = (word) (p + 2)  + PAIR_TAG;
    p += 2;
    k++;
  }

  if (j >= r) {
    t = (word *) globals[ REG0_OFFSET + r ];

    /* Copy the list in t into the memory pointed to by p. */

    while ((word) t != NIL_CONST) {
      *p = *(word *)((word) t - PAIR_TAG);               /* copy the car */
      *(p+1) = (word) (p + 2) + PAIR_TAG;
      p += 2;
      t = (word *)*((word *)((word) t - PAIR_TAG)+1);    /* get the cdr */
    }
  }

  *(p-1) = NIL_CONST;
  globals[ REG0_OFFSET + n + 1 ] = (word) q + PAIR_TAG;
}


/*
 * C-language exception handler (called from exception.s)
 * This is a temporary hack; eventually this procedure will not be needed.
 */
void C_exception( i )
int i;
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
  C_localdebugger();
}



/*
 * This is for debugging the run-time system; should be replaced by a
 * more general facility which hooks into Scheme.
 */
void C_break()
{
  if (globals[ BREAKP_OFFSET ]) C_localdebugger();
}


/*
 * Single stepping is rather obsolete; not used for much except the
 * compiler class.
 */
void C_singlestep( s )
word s;
{
  word *p;
  char buf[ 300 ];
  int length;
  
  if (tagof( s ) != BVEC_TAG)
    printf( "Internal: Bad arg to C_singlestep().\n" );
  else {
    p = ptrof( s );
    length = sizefield( *p );
    if (length > 299) length = 299;
    strncpy( buf, (char *)(p+1), length );
    buf[ length ] = 0;
    printf( "Step: %s\n", buf );
  }
  C_localdebugger();
}


/*
 * Get the system time field from the rusage structure and return it as
 * a number of milliseconds.
 */
word C_getrusage()
{
  struct rusage buf;

  if (getrusage( RUSAGE_SELF, &buf ) == -1)
    return -1;  /* sort of nonsensical, since "word" is unsigned. */
  else
    return buf.ru_utime.tv_sec * 1000 + buf.ru_utime.tv_usec / 1000;
}

