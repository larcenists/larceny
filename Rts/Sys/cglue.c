/* Rts/Sys/cglue.c
 * Larceny run-time system (Unix) -- millicode-to-C interface
 *
 * $Id: cglue.c,v 1.11 1997/07/07 20:09:30 lth Exp $
 *
 * All callouts from millicode to the run-time system are to C procedure
 * with names starting with C_ or UNIX_; all procedures named C_* are
 * in this file.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"


/* C_garbage_collect: perform a garbage collection */
void C_garbage_collect( word type, word request_words )
{
  supremely_annoyingmsg( "Allocation exception in millicode." );
  garbage_collect3( 0, 0, nativeint( request_words )*sizeof( word ) );
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
/* FIXME: this is a stopgap implementation */
/* FIXME: when the generation is no longer ignored, watch out for the
   magic generation resulting from the magic barrier */

void C_wb_compact( int generation )
{ 
  debugmsg( "[debug] wb_compact." );
  supremely_annoyingmsg( "SSB exception in millicode." );
  if (compact_ssb()) {
    /* at least one remembered set overflowed */
    supremely_annoyingmsg( "Remembered-set overflow." );
    garbage_collect3( 1, 1, 0 );
  }
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
 * index (1-based!) at which to find a string. G_REG0 must be valid.
 */
void C_singlestep( word cidx )
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

/* Syscall primitive.
 *
 * RESULT has number of arguments.
 * R1 has index of primitive to call.
 * Arguments are in R2 .. R31.
 */
void C_syscall( void )
{
  typedef void (*fptr)();

  /* Order of this table is important, see Lib/unix.sch. */
  static struct {
    fptr proc;
    int  nargs;
    int  interruptible;
  } syscall_table[] = { { (fptr)UNIX_openfile, 3, 1 },
			{ (fptr)UNIX_unlinkfile, 1, 1 },
			{ (fptr)UNIX_closefile, 1, 1  },
			{ (fptr)UNIX_readfile, 3, 1 },
			{ (fptr)UNIX_writefile, 4, 1 },
			{ (fptr)UNIX_getresourceusage, 0, 1 },
			{ (fptr)UNIX_dumpheap, 2, 1 },
			{ (fptr)UNIX_exit, 1, 0 },
			{ (fptr)UNIX_mtime, 2, 1 },
			{ (fptr)UNIX_access, 2, 1 },
			{ (fptr)UNIX_rename, 2, 1 },
			{ (fptr)UNIX_pollinput, 1, 1 },
			{ (fptr)UNIX_getenv, 1, 1 },
			{ (fptr)UNIX_garbage_collect, 2, 0 },
			{ (fptr)UNIX_flonum_log, 2, 0 },
		        { (fptr)UNIX_flonum_exp, 2, 0 },
			{ (fptr)UNIX_flonum_sin, 2, 0 },
			{ (fptr)UNIX_flonum_cos, 2, 0 },
			{ (fptr)UNIX_flonum_tan, 2, 0 },
			{ (fptr)UNIX_flonum_asin, 2, 0 },
			{ (fptr)UNIX_flonum_acos, 2, 0 },
			{ (fptr)UNIX_flonum_atan, 2, 0 },
			{ (fptr)UNIX_flonum_atan2, 3, 0 },
			{ (fptr)UNIX_flonum_sqrt, 2, 0 },
			{ (fptr)UNIX_stats_dump_on, 1, 1 },
			{ (fptr)UNIX_stats_dump_off, 0, 1 },
			{ (fptr)UNIX_iflush, 1, 0 },
			{ (fptr)UNIX_gcctl_np, 3, 0 },
			{ (fptr)UNIX_block_signals, 1, 0 },
			{ (fptr)UNIX_flonum_sinh, 2, 0 },
			{ (fptr)UNIX_flonum_cosh, 2, 0 },
		      };
  fptr proc;
  int nargs, nproc;

  nargs = nativeint( globals[ G_RESULT ] )-1;
  nproc = nativeint( globals[ G_REG1 ] );
  if (nproc < 0 || nproc >= sizeof( syscall_table )/sizeof( fptr ))
    panic( "syscall: index out of range: %d.", nproc );

  if (nargs != syscall_table[ nproc ].nargs)
    panic( "syscall: wrong number of arguments to #%d\n", nproc );

  proc = syscall_table[ nproc ].proc;

  switch (nargs) {
    case 0 : proc(); break;
    case 1 : proc( globals[ G_REG2 ] ); break;
    case 2 : proc( globals[ G_REG2 ], globals[ G_REG3 ] ); break;
    case 3 : proc( globals[ G_REG2 ], globals[ G_REG3 ], 
		   globals[ G_REG4 ] ); break;
    case 4 : proc( globals[ G_REG2 ], globals[ G_REG3 ], 
		   globals[ G_REG4 ], globals[ G_REG5 ] ); break;
    default: panic( "syscall: Too many arguments." ); break;
  }
}

#if SIMULATE_NEW_BARRIER

#include "gclib.h"

static unsigned wb_array_assignments = 0;
static unsigned wb_lhs_young_or_remembered = 0;
static unsigned wb_rhs_constant = 0;
static unsigned wb_third_check = 0;
static unsigned wb_trans_recorded = 0;

void simulated_barrier_stats( simulated_barrier_stats_t *stats )
{
  stats->array_assignments = wb_array_assignments;
  stats->lhs_young_or_remembered = wb_lhs_young_or_remembered;
  stats->rhs_constant = wb_rhs_constant;
  stats->cross_gen_check = wb_third_check;
  stats->transactions = wb_trans_recorded;
  wb_array_assignments = 0;
  wb_lhs_young_or_remembered = 0;
  wb_rhs_constant = 0;
  wb_third_check = 0;
  wb_trans_recorded = 0;
}

/* Simulation of new write barrier */
void C_simulate_new_barrier( void )
{
  word *genv = (word*)globals[G_GENV];
  word lhs = globals[G_RESULT];
  word rhs = globals[G_ARGREG2];
  unsigned gl, gr;
  word **ssbtopv, **ssblimv;

  if (tagof(lhs) == VEC_TAG) {
    wb_array_assignments++;
    if (genv[pageof(lhs)] == 0)
      wb_lhs_young_or_remembered++;
    else if (isremembered( lhs ))
      wb_lhs_young_or_remembered++;
    else if (!isptr( rhs ))
      wb_rhs_constant++;
    else 
      goto record_trans;
  }
  else {
    if (genv[pageof(lhs)] == 0)
      ;
    else if (!isptr(rhs))
      ;
    else
      goto record_trans;
  }
  return;
 record_trans:
  gl = genv[pageof(lhs)];
  gr = genv[pageof(rhs)];
  if (gl <= gr) {
    if (tagof(lhs) == VEC_TAG) wb_third_check++;
    return;
  }
  if (tagof(lhs) == VEC_TAG)
    wb_trans_recorded++;
  ssbtopv = (word**)globals[G_SSBTOPV];
  ssblimv = (word**)globals[G_SSBLIMV];
  *ssbtopv[gl] = lhs;
  ssbtopv[gl] += 1;
  if (ssbtopv[gl] == ssblimv[gl]) C_wb_compact( gl );
}

#endif  /* if SIMULATE_NEW_BARRIER */

/* OBSOLETE PROCEDURES */

/* C_compact_ssb: compact SSB, garbage collect if full. */
void C_compact_ssb( void )
{
  debugmsg( "[debug] Warning: call to obsolete C_compact_ssb" );
  return;
}


/* eof */
