/* Rts/Sys/cglue.c
 * Larceny run-time system (Unix) -- millicode-to-C interface
 *
 * $Id: cglue.c,v 1.13 1997/08/25 13:07:31 lth Exp $
 *
 * All callouts from millicode to the run-time system are to C procedure
 * with names starting with C_ or UNIX_; all procedures named C_* are
 * in this file.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "larceny.h"
#include "signals.h"


/* C_allocate: allocate heap memory */
void C_allocate( word request_words )
{
  supremely_annoyingmsg( "Allocation call-out from millicode." );
  /* The assignment violates the VM invariants -- that's OK */
  globals[ G_RESULT ] =
    (word)alloc_from_heap( nativeint( request_words )*sizeof(word) );
}

/* C_garbage_collect: perform a garbage collection */
/* FIXME: SHOULD BE OBSOLETE */
void C_garbage_collect( word type, word request_words )
{
  hardconsolemsg( "Call to obsolete C_garbage_collect." );
  supremely_annoyingmsg( "Allocation exception in millicode." );
  garbage_collect3( 0, nativeint( request_words )*sizeof( word ) );
}

/* C_SRO: implements SRO operation. */
void C_SRO( word w_ptrtag, word w_hdrtag, word w_limit )
{
  int ptrtag = (int)nativeint(w_ptrtag);
  int hdrtag = (int)nativeint(w_hdrtag);
  int limit = (int)nativeint(w_limit);

  supremely_annoyingmsg( "SRO %d %d %d", ptrtag, hdrtag, limit );
  globals[ G_RESULT ] = standing_room_only( ptrtag, hdrtag, limit );
}

/* C_stack_overflow: overflow handling depends on stack */
void C_stack_overflow( void )
{
  debugmsg( "[debug] Stack overflow." );
  supremely_annoyingmsg( "Stack overflow exception in millicode." );
  stack_overflow();
}

/* C_creg_get: capture the current continuation. */
void C_creg_get( void )
{
  debugmsg( "[debug] capturing continuation." );
  supremely_annoyingmsg( "Call/cc exception in millicode." );
  globals[ G_RESULT ] = creg_get();
}

/* C_creg_set: reinstate a continuation */
void C_creg_set( void )
{
  debugmsg( "[debug] reinstating continuation." );
  supremely_annoyingmsg( "Throw exception in millicode." );
  creg_set( globals[ G_RESULT ] );
}

/* C_restore_frame: stack underflowed, restore a frame */
void C_restore_frame( void )
{
  debugmsg( "[debug] Stack underflow." );
  supremely_annoyingmsg( "Stack underflow exception in millicode." );
  stack_underflow();
}

/* C_wb_compact: some SSB filled up, and must be compacted. */

void C_wb_compact( int generation )
{ 
  debugmsg( "[debug] wb_compact." );
  supremely_annoyingmsg( "SSB exception in millicode." );
  compact_ssb();
}

/* C_panic: print a message and die. */
void C_panic( char *fmt, ... )
{
  va_list args;
  char buf[ 128 ];

  va_start( args, fmt );
  vsprintf( buf, fmt, args );
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

void C_varargs( void )
{
  word j = nativeint( globals[ G_RESULT ] );
  word n = nativeint( globals[ G_ARGREG2 ] );
  word r = 31;			                  /* Highest register # */
  word R = 32;			                  /* # of registers */
  word *p, *q, *t;
  word k, limit;
  word bytes;

#ifdef DEBUG2
  debugmsg( "[debug] varargs given=%d, wanted=%d." j, n );
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

/* C-language exception handler (called from exception.s)
 * This code is called *only* when a Scheme exception handler is not present.
 */
void C_exception( word i, word pc )
{
  hardconsolemsg( "Larceny exception at PC=0x%08x: %d.", pc, nativeint(i) );
  localdebugger();
}

/* This is for debugging the run-time system; should be replaced by a
 * more general facility which hooks into Scheme.
 */
void C_break( void )
{
  localdebugger();
}

/* Single stepping. Takes a fixnum argument which is the constant vector
 * index at which to find a string.  G_REG0 must be valid.
 */
void C_singlestep( word cidx )
{
  char buf[ 300 ];
  int l;
  word s;
  word constvec;

  constvec = *( ptrof( globals[G_REG0] ) + 2 );
  s = *( ptrof( constvec ) + VEC_HEADER_WORDS + nativeint(cidx) );
  if (tagof( s ) != BVEC_TAG)
    panic( "Internal: Bad arg to C_singlestep().\n" );

  l = string_length( s );
  strncpy( buf, string_data( s ), min( l, sizeof( buf )-1 ) );
  buf[ l ] = 0;
  hardconsolemsg( "Step: %s", buf );
  localdebugger();
}

/* Syscall primitive.
 *
 * RESULT has number of arguments.
 * R1 has index of primitive to call.
 * Arguments are in R2 .. R31.
 */
void C_syscall( void )
{
  int nargs, nproc;

  nargs = nativeint( globals[ G_RESULT ] )-1;
  nproc = nativeint( globals[ G_REG1 ] );

  larceny_syscall( nargs, nproc, &globals[ G_REG2 ] );
}

/* C_compact_ssb: compact SSB, garbage collect if full. */
void C_compact_ssb( void )
{
  debugmsg( "[debug] Warning: call to obsolete C_compact_ssb" );
  return;
}


/* eof */
