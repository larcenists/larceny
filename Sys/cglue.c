/*
 * This is the file Sys/cglue.c
 *
 * Larceny run-time system (Unix) -- millicode-to-C interface
 *
 * History
 *   June 29 - July 13, 1994 / lth (v0.20)
 *     Cleaned up.
 *
 * All callouts from millicode to the run-time system are to C procedure
 * with names starting with C_ or UNIX_; all procedures named C_* are
 * in this file.
 */

#include <varargs.h>
#include <stdio.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

/* In general, note that a garbage collection sets up a stack and restores
 * a frame, so after a garbage collection nothing usually needs to be done.
 */


static void do_restore_frame();

/* C_garbage_collect: perform a garbage collection */
void C_garbage_collect( type, request )
word type;      /* fixnum: type requestes */
word request;   /* fixnum: words needed */
{
  garbage_collect( nativeint( type ), nativeint( request ) );
}


/* C_compact_ssb: compact SSB, garbage collect if full. */
void C_compact_ssb()
{
#ifdef DEBUG
  consolemsg( "[debug] Compacting SSB." );
#endif
  if (!compact_ssb())
    garbage_collect( TENURING_COLLECTION, 0 );
}

/* C_stack_overflow: causes a garbage collection since stkoflo => heapoflo */
void C_stack_overflow()
{
#ifdef DEBUG
  consolemsg( "[debug] Stack overflow." );
#endif
  garbage_collect( EPHEMERAL_COLLECTION, 0 );
}

/* C_creg_get: capture the current continuation. */
void C_creg_get()
{
#ifdef DEBUG
  consolemsg( "[debug] capturing continuation." );
#endif
  flush_stack();
  globals[ G_RESULT ] = globals[ G_CONT ];
  if (!create_stack())
    garbage_collect( EPHEMERAL_COLLECTION, 0 );
  else
    do_restore_frame();
}

/* C_creg_set: reinstate a continuation */
void C_creg_set()
{
#ifdef DEBUG
  consolemsg( "[debug] reinstating continuation." );
#endif
  clear_stack();
  globals[ G_CONT ] = globals[ G_RESULT ];
  do_restore_frame();
}

/* C_restore_frame: stack underflowed, restore a frame */
void C_restore_frame()
{
#ifdef DEBUG
  consolemsg( "[debug] stack cache underflow." );
#endif
  do_restore_frame();
}

static void do_restore_frame()
{
  if (!restore_frame())
    garbage_collect( EPHEMERAL_COLLECTION, 0 );
}

/* C_panic: print a message and die. */
void C_panic( va_alist )
va_dcl
{
  va_list args;
  char buf[ 128 ];
  char *msg;

  va_start( args );
  msg = va_arg( args, char * );
  vsprintf( buf, msg, args );
  va_end( args );
  panic( "%s", buf );
}

/*
 * C_varargs: Millicode to deal with variable-length argument lists.
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
  word j = nativeint( globals[ G_RESULT ] );
  word n = nativeint( globals[ G_ARGREG2 ] );
  word r = 31;			                  /* Highest register # */
  word R = 32;			                  /* # of registers */
  word *p, *q, *t;
  word k, limit;
  word bytes;

#ifdef DEBUG2
  consolemsg( "[debug] varargs given=%d, wanted=%d." j, n );
#endif
  bytes = 4*(2*(j-n));

  if (bytes == 0) {
    globals[ G_REG0 + n + 1 ] = NIL_CONST;
    return;
  }

  /* At least one vararg to cons up. */

  q = p = (word*)alloc_from_heap( bytes );  /* Allocate memory for list. */

  k = n + 1;
  limit = min( j, r-1 );

  while ( k <= limit ) {
    *p = globals[ G_REG0 + k ];
    *(p+1) = (word) (p + 2)  + PAIR_TAG;
    p += 2;
    k++;
  }

  if (j >= r) {
    t = (word *) globals[ G_REG0 + r ];

    /* Copy the list in t into the memory pointed to by p. */

    while ((word) t != NIL_CONST) {
      *p = *(word *)((word) t - PAIR_TAG);               /* copy the car */
      *(p+1) = (word) (p + 2) + PAIR_TAG;
      p += 2;
      t = (word *)*((word *)((word) t - PAIR_TAG)+1);    /* get the cdr */
    }
  }

  *(p-1) = NIL_CONST;
  globals[ G_REG0 + n + 1 ] = (word) q + PAIR_TAG;
}


/*
 * C-language exception handler (called from exception.s)
 * This code is called *only* when a Scheme exception handler is not present.
 */
void C_exception( i )
int i;
{
  hardconsolemsg( "Larceny exception at PC=0x%08x: %d.", nativeint( i ) );
  localdebugger();
}


/*
 * This is for debugging the run-time system; should be replaced by a
 * more general facility which hooks into Scheme.
 */
void C_break()
{
  localdebugger();
}


/* 
 * Single stepping. Takes a fixnum argument which is the constant vector
 * index (1-based!) at which to find a string. G_REG0 must be valid.
 */
void C_singlestep( cidx )
word cidx;
{
  char buf[ 300 ];
  int l;
  word s;


  s = *(ptrof( *(ptrof( globals[ G_REG0 ] ) + 2) ) + cidx / 4);
  if (tagof( s ) != BVEC_TAG)
    panic( "Internal: Bad arg to C_singlestep().\n" );

  l = string_length( s );
  strncpy( buf, string_data( s ), min( l, sizeof( buf )-1 ) );
  buf[ l ] = 0;
  hardconsolemsg( "Step: %s", buf );
  localdebugger();
}

/* eof */

