/*
 * Scheme 313 run-time system.
 * Millicode in C.
 *
 * $Id: cglue.c,v 1.2 91/09/13 03:00:25 lth Exp Locker: lth $
 *
 * Millicode routines which are written in C and which do not warrant 
 * their own files go in here.
 *
 * Currently exports:
 *   void C_scheme_varargs( void )
 *   word *C_alloc( int nbytes )
 */

#include "offsets.h"
#include "gcinterface.h"
#include "macros.h"
#include "main.h"

word *C_alloc();

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

void C_scheme_varargs()
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
 * Allocate a chunk of memory. `n' is a number of bytes.
 */
static word *C_alloc( n )
{
  word p;

  n = roundup8( n );

  if (globals[ E_TOP_OFFSET ] + n > globals[ E_LIMIT_OFFSET ])
    gcstart2( n );

  p = globals[ E_TOP_OFFSET ];
  globals[ E_TOP_OFFSET ] += n;
  return (word *)p;
}

/* This is for debugging the run-time system (mostly) */

int break_counter = 0;
int break_count = 0;
int break_list[ 10 ];
int break_always = 0;

C_break()
{
  int i, do_break = 0;

  break_counter++;
  for (i = 0 ; i < break_count ; i++ )
    if (break_list[ i ] == break_counter) do_break = 1;

  if (do_break || break_always)
    localdebugger();
}
