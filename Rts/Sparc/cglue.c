/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Millicode-to-C interface (SPARC).
 *
 * All callouts from millicode to the run-time system are to C procedure
 * with names starting with C_, and all those procedures are in this file.
 * The procedures are flagged to the RTS as noninterruptible syscalls;
 * this for the benefit of signal handling.
 *
 * Also in this file are some support procs (scheme_init,
 * stk_initialize_underflow_frame) that are machine-dependent; it might
 * be more reasonable to move them to some other file.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "larceny.h"
#include "signals.h"
#include "gc.h"
#include "gc_t.h"
#include "young_heap_t.h"
#include "stack.h"

void scheme_init( word *globals )
{
  /* Nothing yet */
}

void stk_initialize_underflow_frame( word *stktop )
{
  extern void mem_stkuflow();

  *(stktop+0) = fixnum(3);                      /* header/size field */
  *(stktop+1) = (word)mem_stkuflow;             /* retaddr: uflow handler */
  *(stktop+2) = 0xDEADBEEF;                     /* dynamic link field */
  *(stktop+3) = 0;                              /* saved procedure */
}

/* C_allocate: allocate heap memory */
void C_allocate( word request_words )
{
  supremely_annoyingmsg( "Allocation call-out from millicode." );
  /* The assignment to G_RESULT violates the VM invariants because an
     untagged pointer to memory is being stored in a root.  That's OK,
     because the millicode will fix the problem before anyone gets to
     see the pointer.
     */
  in_noninterruptible_syscall = 1;
  globals[ G_RESULT ] =
    (word)alloc_from_heap( nativeint( request_words )*sizeof(word) );
  in_noninterruptible_syscall = 0;
}

/* C_stack_overflow: overflow handling depends on stack */
void C_stack_overflow( void )
{
  supremely_annoyingmsg( "Stack overflow exception in millicode." );
  in_noninterruptible_syscall = 1;
  gc_stack_overflow( the_gc( globals ) );
  in_noninterruptible_syscall = 0;
}

/* C_creg_get: capture the current continuation. */
void C_creg_get( void )
{
  supremely_annoyingmsg( "Call/cc exception in millicode." );
  in_noninterruptible_syscall = 1;
  globals[ G_RESULT ] = gc_creg_get( the_gc( globals ) );
  in_noninterruptible_syscall = 0;
}

/* C_creg_set: reinstate a continuation */
void C_creg_set( void )
{
  supremely_annoyingmsg( "Throw exception in millicode." );
  in_noninterruptible_syscall = 1;
  gc_creg_set( the_gc( globals ), globals[ G_RESULT ] );
  in_noninterruptible_syscall = 0;
}

#if !defined(BDW_GC)
void C_morecore( void )
{
  in_noninterruptible_syscall = 1;
  yh_make_room( (the_gc( globals ))->young_area );
  in_noninterruptible_syscall = 0;
}
#endif

/* C_restore_frame: stack underflowed, restore a frame */
void C_restore_frame( void )
{
  supremely_annoyingmsg( "Stack underflow exception in millicode." );
  in_noninterruptible_syscall = 1;
  gc_stack_underflow( the_gc( globals ) );
  in_noninterruptible_syscall = 0;
}

/* C_wb_compact: some SSB filled up, and must be compacted. */
void C_wb_compact( int generation )
{ 
  annoyingmsg( "Generation %d: SSB filled up during mutator operation.",
	       generation );
  in_noninterruptible_syscall = 1;
  gc_compact_all_ssbs( the_gc( globals ) ); /* Ignores remset overflows. */
  in_noninterruptible_syscall = 0;
}

/* C_panic: print a message and die. */
void C_panic( char *fmt, ... )
{
  va_list args;
  char buf[ 128 ];

  in_noninterruptible_syscall = 1;
  va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  panic( "%s", buf );
  in_noninterruptible_syscall = 0;
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
  word *p, *first, *prev, t;
  word k, limit;
  word bytes;
#if !defined(BDW_GC)
  word *allocptr;
#endif

  in_noninterruptible_syscall = 1;

  bytes = sizeof(word)*(2*(j-n));

  if (bytes == 0) {
    globals[ G_REG0 + n + 1 ] = NIL_CONST;
    in_noninterruptible_syscall = 0;
    return;
  }

  /* At least one vararg to cons up. */

  /* Optimized allocation for precise GC; conservative GC calls
     allocator each time.
     */
#if !defined(BDW_GC)
  allocptr = (word*)alloc_from_heap( bytes );
# define alloc_one_pair(p) (p = allocptr, allocptr+=2)
#else
# define alloc_one_pair(p) (p = (word*)alloc_from_heap(2*sizeof(word)) )
#endif
  first = prev = 0;
  k = n+1;
  limit = min( j, r-1 );

  while (k <= limit ) {
    alloc_one_pair(p);
    *p = globals[ G_REG0 + k ];
    if (prev) 
      *(prev+1) = tagptr( p, PAIR_TAG ); 
    else 
      first = p;
    prev = p;
    k++;
  }

  /* Copy the list in t into the memory pointed to by p. */

  if (j >= r) {
    t = globals[ G_REG0 + r ];

    while (t != NIL_CONST) {
      alloc_one_pair(p);
      *p = pair_car( t );
      if (prev) 
	*(prev+1) = tagptr( p, PAIR_TAG ); 
      else
	first = p;
      prev = p;
      t = pair_cdr( t );
    }
  }

  *(prev+1) = NIL_CONST;
  globals[ G_REG0+n+1 ] = tagptr( first, PAIR_TAG );

  in_noninterruptible_syscall = 0;
}

/* C-language exception handler (called from exception.s)
 * This code is called *only* when a Scheme exception handler is not present.
 */
void C_exception( word i, word pc )
{
  hardconsolemsg( "Larceny exception at PC=0x%08x: %d.", pc, nativeint(i) );
  in_noninterruptible_syscall = 1;
  localdebugger();
  in_noninterruptible_syscall = 0;
}

/* This is for debugging the run-time system. */
void C_debugvsm( void )
{
  in_noninterruptible_syscall = 1;
  localdebugger();
  in_noninterruptible_syscall = 0;
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

  in_noninterruptible_syscall = 1;
  constvec = *( ptrof( globals[G_REG0] ) + 2 );
  s = *( ptrof( constvec ) + VEC_HEADER_WORDS + nativeint(cidx) );
  if (tagof( s ) != BVEC_TAG)
    panic( "Internal: Bad arg to C_singlestep().\n" );

  l = string_length( s );
  strncpy( buf, string_data( s ), min( l, sizeof( buf )-1 ) );
  buf[ l ] = 0;
  hardconsolemsg( "Step: %s", buf );
  localdebugger();
  in_noninterruptible_syscall = 0;
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

  /* Do not set in_noninterruptible_syscall here because that is
     taken care of by the machinery in larceny_syscall.
  */
  nargs = nativeint( globals[ G_RESULT ] )-1;
  nproc = nativeint( globals[ G_REG1 ] );

  larceny_syscall( nargs, nproc, &globals[ G_REG2 ] );
}

#if defined(SIMULATE_NEW_BARRIER)

#include "gclib.h"

static word wb_total_assignments = 0;
static word wb_array_assignments = 0;
static word wb_lhs_young_or_remembered = 0;
static word wb_rhs_constant = 0;
static word wb_cross_gen_check = 0;
static word wb_trans_recorded = 0;

void simulated_barrier_stats( word *total_assignments,
			      word *array_assignments,
			      word *lhs_young_or_remembered,
			      word *rhs_constant,
			      word *cross_gen_check,
			      word *transactions )
{
  *total_assignments = wb_total_assignments;
  *array_assignments = wb_array_assignments;
  *lhs_young_or_remembered = wb_lhs_young_or_remembered;
  *rhs_constant = wb_rhs_constant;
  *cross_gen_check = wb_cross_gen_check;
  *transactions = wb_trans_recorded;
  wb_total_assignments = 0;
  wb_array_assignments = 0;
  wb_lhs_young_or_remembered = 0;
  wb_rhs_constant = 0;
  wb_cross_gen_check = 0;
  wb_trans_recorded = 0;
}

/* Simulation of new write barrier */
void C_simulate_new_barrier( void )
{
  word *genv;
  word lhs;
  word rhs;
  unsigned gl, gr;
  word **ssbtopv, **ssblimv;
  int isvec = 0;

  genv = (word*)globals[ G_GENV ];
  lhs = globals[ G_RESULT ];
  rhs = globals[ G_ARGREG2 ];

  wb_total_assignments++;
  if (tagof(lhs) == VEC_TAG || tagof(lhs) == PROC_TAG) {
    isvec = 1;
    wb_array_assignments++;

    if (genv[pageof(lhs)] == 0) {
      wb_lhs_young_or_remembered++;
      return;
    }
    else if (gc_isremembered( the_gc(globals), lhs )) {
      wb_lhs_young_or_remembered++;
      return;
    }
    else if (!isptr( rhs )) {
      wb_rhs_constant++;
      return;
    }
  }
  else if (!isptr(rhs))
    return;

 record_trans:
  gl = genv[pageof(lhs)];
  gr = genv[pageof(rhs)];
  if (gl <= gr) {
    if (isvec) wb_cross_gen_check++; /* not old->young */
    return;
  }
  if (isvec)
    wb_trans_recorded++;
  ssbtopv = (word**)globals[G_SSBTOPV];
  ssblimv = (word**)globals[G_SSBLIMV];
  *ssbtopv[gl] = lhs;
  ssbtopv[gl] += 1;
  if (ssbtopv[gl] == ssblimv[gl]) C_wb_compact( gl );
}

#endif  /* if SIMULATE_NEW_BARRIER */

/* eof */
