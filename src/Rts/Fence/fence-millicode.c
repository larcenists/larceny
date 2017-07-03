/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 *
 * $Id: millicode.c 2543 2005-07-20 21:54:03Z pnkfelix $
 *
 * Larceny -- Fence run-time support implementation, except for arithmetic.
 */

#define NOGLOBALS

#define MORECORE_ALWAYS_COLLECTS 0

#define SSB_ENQUEUE_LOUDLY 0
#define SSB_ENQUEUE_OFFSET_AS_FIXNUM 0 

/* TODO: This may only make sense once the code generator calls
 * mc_full_barrier for the write barrier.
 */
#if SSB_ENQUEUE_OFFSET_AS_FIXNUM
  #error "SSB_ENQUEUE_OFFSET_AS_FIXNUM cannot be enabled yet"
#endif

#include "larceny.h"            /* Includes config.h also */
#include "gc.h"
#include "gc_t.h"               /* For gc_allocate() macro */
#include "barrier.h"            /* For prototypes */
#include "gclib.h"              /* For pageof() */
#include "stack.h"
#include "millicode.h"
#include "regs.ch"
#include "signals.h"
#include "assert.h"
#include "young_heap_t.h"       /* For yh_make_room() */
#include "seqbuf_t.h"           /* For SSB_ENQUEUE */
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>

#if defined G_F15
#define NFPREGS 16
#elif defined G_F7
#define NFPREGS 8
#elif defined G_F3
#define NFPREGS 4
#else
#error "No floating registers visible"
#endif

static void timer_exception( word *globals, cont_t k );
static void signal_exception( word *globals, word exception, cont_t k, int preserve );
static void setup_timer( word *globals, int timer, cont_t k );
static void check_signals( word *globals, cont_t k );

extern void fence_return_from_scheme();

/* These could go in the globals vector, too */
jmp_buf *dispatch_jump_buffer;
int already_running = 0;

void my_longjmp( jmp_buf *env, int val) {
  longjmp(*env, val);
}

RTYPE EXPORT mem_stkuflow( CONT_PARAMS )
{
  /* We store a decoded return address in G_RETADDR; millicode must cope by not translating it again */
  globals[G_RETADDR] = refill_stack_cache( globals );
  /* Unclear why mem_stkuflow is not void */
  return 0;
}

/* REG0 should be sane.
   SECOND holds the instruction as a string.
*/
void EXPORT mc_singlestep(word* globals)
{
  word s;

  if (globals[G_SINGLESTEP_ENABLE] != TRUE_CONST)
    return;

  in_noninterruptible_syscall = 1;
  s = globals[G_SECOND];
  if (tagof( s ) != BVEC_TAG)
    panic_exit( "Internal: Bad arg to mc_singlestep().\n" );

  localdebugger_step(globals);
  in_noninterruptible_syscall = 0;
}

void EXPORT mc_alloc_bv( word *globals )
{
  word *etop; 
  word *elim;
  word *p;
  
  assert2( is_fixnum(globals[G_RESULT]) && (int)globals[G_RESULT] >= 0 );

#if defined( BDW_GC )
#error "This must be changed because RESULT is size in bytes"
  int nwords = (int)nativeuint( globals[ G_RESULT ] );

  nwords = roundup_walign( nwords );
  p = GC_malloc_atomic( nwords*sizeof(word) );
  assert2( p != 0 );
  globals[ G_RESULT ] = (word)p;
#else
  etop = (word*)globals[ G_ETOP ];
  elim = (word*)globals[ G_STKP ] - SCE_BUFFER;

  if ((((word)etop & 0xF) == 0x8) && /* misaligned and ... */
      (elim - etop) > 2) {           /* have room to realign */
    *etop = 0;
    etop++;
    *etop = 0;
    etop++;
    globals[ G_ETOP ] = (word) etop;
    assert( (globals[ G_ETOP ] & 0xF) == 0x0 );
  }

  globals[ G_RESULT ] = roundup4( globals[ G_RESULT ] >> 2 );
  mc_alloc( globals );
#endif
}

void EXPORT mc_alloc( word *globals )
{
#if defined( BDW_GC )
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *p;

  assert2( is_fixnum(globals[G_RESULT]) && (int)globals[G_RESULT] >= 0 );

  nwords = roundup_walign( nwords );
  if ( nwords*sizeof(word) > LARGEST_OBJECT )
    signal_exception( globals, EX_ALLOC, 0, 0 );

  p = GC_malloc( nwords*sizeof(word) );
  assert (p != 0);
  globals[ G_RESULT ] = (word)p;
#else
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *etop = (word*)globals[ G_ETOP ];
  word *elim = (word*)globals[ G_STKP ] - SCE_BUFFER;
  word *p;

  assert2( is_fixnum(globals[G_RESULT]) && (int)globals[G_RESULT] >= 0 );
  assert2(((word)etop & 7) == 0);
  assert2(((word)elim & 7) == 0);

  nwords = roundup_walign( nwords );
  if ( nwords*sizeof(word) > LARGEST_OBJECT )
    signal_exception( globals, EX_ALLOC, 0, 0 );

  p = etop;
  etop += nwords;
  if (etop <= elim) {
    globals[ G_ETOP ] = (word)etop;
    globals[ G_RESULT ] = (word)p;
  }
  else {
    globals[ G_RESULT ] =
      (word)gc_allocate( the_gc( globals ), nwords*sizeof( word ), 0, 0 );
  }
#if !GCLIB_LARGE_TABLE
  assert2( globals[ G_RESULT ] >= (word)gclib_pagebase );
#endif
#endif
}

void EXPORT mc_alloci( word *globals )
{
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *p;
  word init;

  assert2( is_fixnum(globals[G_RESULT]) && (int)globals[G_RESULT] >= 0 );

  mc_alloc( globals );
  p = (word*)globals[ G_RESULT ];
  init = globals[ G_SECOND ];
  while ( nwords-- > 0 )
    *p++ = init;
}

void EXPORT mc_morecore( word *globals )
{
  gc_make_room( the_gc(globals) );
}

void EXPORT mc_stack_overflow( word *globals )
{
  gc_stack_overflow( the_gc( globals ) );
}

void EXPORT mc_capture_continuation( word *globals )
{
  globals[ G_RESULT ] = gc_creg_get( the_gc( globals ) );
}

void EXPORT mc_restore_continuation( word *globals )
{
  gc_creg_set( the_gc( globals ), globals[ G_RESULT ] );
}

void EXPORT mc_break( word *globals )
{
  if (globals[ G_BREAKPT_ENABLE ] == TRUE_CONST)
    localdebugger();
}

void EXPORT mc_timer_exception( word *globals, cont_t k )
{
  timer_exception( globals, k );
}

static void timer_exception( word *globals, cont_t k )
{
  check_signals( globals, k );
  gc_incremental( the_gc( globals ) );

  if (globals[ G_TIMER_ENABLE ] == FALSE_CONST)
    globals[ G_TIMER ] = TEMPORARY_FUEL;         /* Run a little longer */
  else if (globals[ G_TIMER2 ] != 0) { 
    osdep_poll_events( globals );
    setup_timer( globals, globals[ G_TIMER2 ], k );
  }
  else {
    /* Timer interrupts are taken with timer interrupts disabled */
    globals[ G_TIMER_ENABLE ] = FALSE_CONST;
    signal_exception( globals, EX_TIMER, k, 1 );
  }
}

void EXPORT mc_enable_interrupts( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];

  if (is_fixnum( x ) && (int)x > 0) {
    globals[ G_TIMER_ENABLE ] = TRUE_CONST;
    setup_timer( globals, nativeuint( x ), k ); /* Checks signals */
  }
  else
    signal_exception( globals, EX_EINTR, 0, 0 ); /* Never returns */
  check_signals( globals, k );                   /* Thus, redundant */
}

void EXPORT mc_disable_interrupts( word *globals, cont_t k )
{
  if (globals[ G_TIMER_ENABLE ] == TRUE_CONST) {
    globals[ G_TIMER_ENABLE ] = FALSE_CONST;
    globals[ G_RESULT ] = fixnum( globals[ G_TIMER ] + globals[ G_TIMER2 ] );

    /* TODO: Understand this better.
     *
     * The following check is reasonable, but bugs resulting from the
     * absence of the check are so far visible only on ARM.  The logic
     * around G_TIMER and G_TIMER2 really should not allow there to be
     * an overflow, though the 0 case might occur and it's a bug that
     * that guard was not in here all along: disable_interrupts() must
     * never return 0 because enable_interrups() will not accept 0.
     *
     * The value 50000 is completely ad-hoc.
     */
    if ((s_word)globals[G_RESULT] <= 0)
      globals[G_RESULT] = fixnum(50000);
  }
  else
    globals[ G_RESULT ] = FALSE_CONST;
  check_signals( globals, k );
}

void EXPORT mc_exception( word *globals, word exception )
{
  switch (exception) {
  case EX_ARGSEQ:
  case EX_ARGSGE:
    /* Exception handler requires this, better to do it here than in-line */
    globals[G_THIRD] = globals[G_REG0];
    break;
  default:
    break;
  }
  signal_exception( globals, exception, 0, 0 );
}

void EXPORT mc_cont_exception( word *globals, word exception, cont_t k )
{
  signal_exception( globals, exception, k, 0 );
}

void EXPORT mc_apply( word *globals )
{
  word args = globals[ G_SECOND ];
  int i;

  i = 1;
  while ( i <= LASTREG-1 && args != NIL_CONST ) {
    globals[ G_REG0 + i ] = pair_car( args );
    i = i + 1;
    args = pair_cdr( args );
  }
  globals[ G_REG0 + LASTREG ] = args;
  globals[ G_REG0 ] = globals[ G_RESULT ];     /* procedure */
  globals[ G_RESULT ] = globals[ G_THIRD ];    /* argument count */
  globals[ G_RETADDR ] = 0;                    /* Invocation address (translated) */
}

/* Millicode to deal with rest arguments.

   There are four cases, depending on how many arguments are passed and
   how many that are expected.  When this procedure is entered it is
   assumed that at least the minimum number of arguments are passed; i.e.
   that check is performed outside this procedure.
 
   Let R = (the # of registers)
   Let r = R - 1
   Let j = RESULT (the actual number of arguments).
   Let n = SECOND (the number of fixed arguments).
  
   (Previously there was a comment here saying that the compiler
    enforces that all fixed arguments are in registers - allowing for
    some simplifications - but that is not true, nor would it be
    practical on systems with few registers.)
 
   Case 0: n < R-2, j < r
     (SET! REGn+1 (LIST REGn+1 ... REGj))
  
   Case 1: n < R-2, j >= r
     (SET! REGn+1 (APPEND! (LIST REGn+1 ... REGr-1) (LIST-COPY REGr)))

   Case 2: n = R-2.  Loads REGr with (LIST (LIST-COPY REGr)).

   Case 3: r <= n <= j.  Loads REGr with
            (LET ((REGr (LIST-COPY REGr)))
              (SET-CDR! (LIST-TAIL REGr (- n r))
                        (LIST (LIST-TAIL REGr (- n (- r 1)))))
              REGr)

   This is tricky to do well.  There are two problems:

    - when allocating an object, any partial results must be anchored
      in the globals array in case the collector kicks in during the
      allocation.

    - when storing values in objects, barriers are needed unless the
      stores are initializing, so SET-CDR! / APPEND! must be done
      carefully.

   Previously this code allocated an arena as a single object in the
   precise collectors and subdivided that, but that is only correct if
   the object is not allocated as a separate large-object, so we don't
   do that any more.  It would still be valid as a useful optimization
   if we can control that the object is allocated directly in the
   small-object heap.  */

static word list1(word* globals, word item)
{
  globals[G_REST0] = item;
  word* p = alloc_from_heap(2*sizeof(word));
  p[0] = globals[G_REST0];
  p[1] = NIL_CONST;
  return tagptr(p, PAIR_TAG);
}

static word list_from_registers(word* globals, word lo, word hi, word tail)
{
  word i;
  assert(lo > 0);
  globals[G_REST0] = tail;
  for ( i=hi ; i >= lo ; i-- ) {
    word *p = alloc_from_heap(2*sizeof(word));
    p[0] = globals[G_REG0+i];
    p[1] = globals[G_REST0];
    globals[G_REST0] = tagptr(p, PAIR_TAG);
  }
  return globals[G_REST0];
}

static void list_setcdr(word* globals, word pair, word value)
{
  globals[G_RESULT] = pair;
  globals[G_SECOND] = value;
  globals[G_THIRD] = (word)(ptrof(pair)+1);
  (ptrof(pair))[1] = value;
  mc_full_barrier(globals);
}

static word list_copy(word* globals, word head)
{
  globals[G_REST0] = NIL_CONST; /* Head of copy */
  globals[G_REST1] = NIL_CONST; /* Tail of copy */
  globals[G_REST2] = head;      /* Current pair of original */
  while (globals[G_REST2] != NIL_CONST) {
    word *p = alloc_from_heap(2*sizeof(word));
    p[0] = pair_car(globals[G_REST2]);
    p[1] = NIL_CONST;
    if (globals[G_REST1] == NIL_CONST)
      globals[G_REST0] = globals[G_REST1] = tagptr(p, PAIR_TAG);
    else {
      list_setcdr(globals, globals[G_REST1], tagptr(p, PAIR_TAG));
      /* p is garbage, don't use it */
      globals[G_REST1] = pair_cdr(globals[G_REST1]);
    }
    globals[G_REST2] = pair_cdr(globals[G_REST2]);
  }
  return globals[G_REST0];
}

void EXPORT mc_restargs( word *globals )
{
  const word j = nativeuint( globals[ G_RESULT ] );
  const word n = nativeuint( globals[ G_SECOND ] );
  const word r = LASTREG;
  const word R = NREGS;

  if (n < R-2 && j < r) {
    /* Case 0 */
    globals[G_REG0+n+1] = list_from_registers(globals, n+1, j, NIL_CONST);
    return;
  }

  if (n < R-2 && j >= r) {
    /* Case 1 */
    globals[G_REG0+n+1] = list_from_registers(globals, n+1, r-1, list_copy(globals, globals[G_REG0+r]));
    return;
  }
  
  if (n == R-2) {
    /* Case 2 */
    /* This test is not in the spec but I can't see how it's not required, as
       Twobit does not pass an extra null argument when there are R-2 arguments.
       Perhaps that's a bug. */
    if (n == j)
      globals[G_REG0+r] = list1(globals, NIL_CONST);
    else
      globals[G_REG0+r] = list1(globals, list_copy(globals, globals[G_REG0+r]));
    return;
  }

  if (r <= n && n <= j) {
    /* Case 3 */
    /* At the moment, REGr contains (xr ... xn ... xj)
     * We want REGr to become       (xr ... xn (... xj))
     * k is the length of (xr ... xn), which is at least 1
     * We want to skip over k elements, back up one element,
     * and then perform a set-cdr!
     */
    word k = n-(r-1);
    word l = list_copy(globals, globals[G_REG0+r]);
    word prev;
    /* REGr won't be needed below because we're using a side effect. */
    globals[G_REG0+r] = l;
    while (k > 0) {
      prev = l;
      l = pair_cdr(l);
      k--;
    }
    /* The call to list1 may move prev, so sequence carefully */
    globals[G_REST1] = prev;
    word tail = list1(globals, l);
    /* prev is now garbage, don't use it */
    list_setcdr(globals, globals[G_REST1], tail);
    return;
  }

  panic_exit("Should not happen: restargs %d %d %d %d", (int)r, (int)R, (int)n, (int)j);
}

/* Another fine mess.  The syscall primitive takes as many as six
 * arguments (RESULT holds the number of arguments, then R1..R5 holds
 * arguments, with the syscall number in R1 and the call arguments in
 * R2..R5).  On machines where the highest register is R5 then R5 will
 * hold a list containing the last argument; if the highest register
 * is R4 then the list will hold the last two arguments, etc.
 *
 * We deconstruct the list here on an ad-hoc basis.  We count on the
 * globals array being large enough to hold the arguments even if it
 * does not need to be that large to back the HW register file.  A
 * compilation error will result if that assumption does not hold
 * (some G_REGn will not be defined).
 */
void EXPORT mc_syscall( word *globals, cont_t k )
{
  int nargs = (int)nativeuint( globals[ G_RESULT ] )-1;
  int nproc = (int)nativeuint( globals[ G_REG1 ] );

#if LASTREG < 5
  #error "More code required in mc_syscall"
#endif
#if LASTREG == 5                /* True for some ARM systems */
  if (nargs >= 4)
    globals[G_REG5] = pair_car(globals[G_REG5]);
#endif

  larceny_syscall( nargs, nproc, &globals[ G_REG2 ] );
  check_signals( globals, k );
}

void EXPORT mc_typetag( word *globals )
{
  word obj = globals[ G_RESULT ];
  int t = tagof( obj );

  if (t == BVEC_TAG || t == VEC_TAG || t == PROC_TAG)
    globals[ G_RESULT ] = typetag( *ptrof( obj ) );
  else
    signal_exception( globals, EX_TYPETAG, 0, 0 );
}

void EXPORT mc_typetag_set( word *globals )
{
  word obj = globals[ G_RESULT ];
  word tag = globals[ G_SECOND ];
  word *p;
  int t = tagof( obj );
  
  if (t == BVEC_TAG || t == VEC_TAG || t == PROC_TAG) {
    /* checks if typetag is fixnum in range */
    if ((tag & (TYPETAG_MASK | 3)) == tag) {
      p = ptrof( obj );
      *p = striptypetag(*p) | tag;
      return;
    }
  }
  signal_exception( globals, EX_TYPETAGSET, 0, 0 );
}

#define compnum_real( x )  (*(double*)((word)x-BVEC_TAG+2*sizeof(word)))
#define compnum_imag( x )  (*(double*)((word)x-BVEC_TAG+4*sizeof(word)))
#define flonum_val( x )    (*(double*)((word)x-BVEC_TAG+2*sizeof(word)))

/* returns true iff x and y have the same bit-level representation. */

static int same_bits(double x, double y) {
  byte * xbits = (byte *) &x;
  byte * ybits = (byte *) &y;
  return xbits[0] == ybits[0] &&
    xbits[1] == ybits[1] &&
    xbits[2] == ybits[2] &&
    xbits[3] == ybits[3] &&
    xbits[4] == ybits[4] &&
    xbits[5] == ybits[5] &&
    xbits[6] == ybits[6] &&
    xbits[7] == ybits[7];
}

void EXPORT mc_eqv( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];

  if (x == y)                             /* eq? => eqv? */
    globals[ G_RESULT ] = TRUE_CONST;     
  else if (tagof( x ^ y ) != 0)           /* Different tags => not eqv? */
    globals[ G_RESULT ] = FALSE_CONST;
  else if (tagof( x ) == BVEC_TAG) {      /* Could be numbers */
    int t1 = *ptrof( x ) & 255;
    int t2 = *ptrof( y ) & 255;
    if (t1 == BIGNUM_HDR && t2 == BIGNUM_HDR)
      mc_equalp( globals, k );
    else if ( t1 == FLONUM_HDR && t2 == FLONUM_HDR ) {
      if ( same_bits( flonum_val(x), flonum_val(y) ) )
        globals[ G_RESULT ] = TRUE_CONST;
      else
        globals[ G_RESULT ] = FALSE_CONST;
    }
    else if ( t1 == COMPNUM_HDR && t2 == COMPNUM_HDR ) {
      if ( same_bits( compnum_real(x), compnum_real(y) ) &&
           same_bits( compnum_imag(x), compnum_imag(y) ) )
        globals[ G_RESULT ] = TRUE_CONST;
      else
        globals[ G_RESULT ] = FALSE_CONST;
    }
    else if (t1 == STR_HDR && t2 == STR_HDR &&
             (string_length(x) == 0) && (string_length(y) == 0))
      globals[ G_RESULT ] = TRUE_CONST; /* (eqv? "" "") */
    else if (t1 == USTR_HDR && t2 == USTR_HDR &&
             (string_length(x) == 0) && (string_length(y) == 0))
      globals[ G_RESULT ] = TRUE_CONST; /* (eqv? "" "") */
    else
      globals[ G_RESULT ] = FALSE_CONST;
  }
  else if (tagof( x ) == VEC_TAG) {       /* Could be numbers */
    int t1 = *ptrof( x ) & 255;
    int t2 = *ptrof( y ) & 255;
    if (t1 == RATNUM_HDR && t2 == RATNUM_HDR)
      mc_equalp( globals, k );
    else if (t1 == RECTNUM_HDR && t2 == RECTNUM_HDR)
      mc_equalp( globals, k );
    else if (t1 == VEC_HDR && t2 == VEC_HDR &&
             vector_length(x) == 0 && vector_length(y) == 0)
      globals[ G_RESULT ] = TRUE_CONST; /* (eqv? '#() '#()) */
    else
      globals[ G_RESULT ] = FALSE_CONST;
  }
  else
    globals[ G_RESULT ] = FALSE_CONST;
}

void EXPORT mc_partial_list2vector( word *globals )
{
  word x = globals[ G_RESULT ];                    /* a list */
  int y = (int) nativeuint( globals[ G_SECOND ] ); /* a fixnum (the length) */
  word *p, *dest;
  
  globals[ G_THIRD ] = x;       /* save for later */

#if VEC_HEADER_WORDS > 1
  /* May need to initialize those extra words! */
# error "Still a few bugs in the system."
#endif

  globals[ G_RESULT ] = fixnum( y + VEC_HEADER_WORDS );
  mc_alloc( globals );
  p = (word*)globals[ G_RESULT ];
  *p = mkheader( y*sizeof( word ), VECTOR_HDR );

  x = globals[ G_THIRD ];       /* restore safe value */
  dest = p + VEC_HEADER_WORDS;
  while ( y-- ) {
    *dest++ = pair_car( x );
    x = pair_cdr( x );
  }
  
  globals[ G_RESULT ] = tagptr( p, VEC_TAG );
}

void EXPORT mc_bytevector_like_fill( word *globals )
{
  word *x = ptrof( globals[ G_RESULT ] ); /* assume: bytevector-like */
  unsigned char c = (globals[ G_SECOND ] >> 2) & 255;
  int length = sizefield( *x );

  memset( x+BVEC_HEADER_WORDS, c, length );
}

void EXPORT mc_bytevector_like_compare( word *globals )
{
  word *x = ptrof( globals[ G_RESULT ] ); /* assume: bytevector-like */
  word *y = ptrof( globals[ G_SECOND ] ); /* assume: bytevector-like */
  int lx, ly, n;

  lx = sizefield( *x );
  ly = sizefield( *y );
  n = memcmp( x+BVEC_HEADER_WORDS, y+BVEC_HEADER_WORDS, min( lx, ly ) );
  if (n)
    globals[ G_RESULT ] = fixnum( n );
  else
    globals[ G_RESULT ] = fixnum( lx - ly );
}

/* Write barrier */

void wb_lowlevel_enable_barrier( word *globals )
{
  /* No-op here. */
}

void wb_lowlevel_disable_barrier( word *globals )
{
  globals[ G_GENV ] = 0;
}

void EXPORT mc_compact_ssbs( word *globals ) /* Used by inline barrier */
{
  gc_compact_all_ssbs( the_gc(globals) );
}

void EXPORT mc_partial_barrier( word *globals )
{
  unsigned *genv, gl, gr;
  word **ssbtopv, **ssblimv;
  word lhs, rhs;

  genv = (unsigned*)globals[ G_GENV ];
  if (genv == 0) return;        /* Barrier disabled */

  lhs = globals[ G_RESULT ];
  rhs = globals[ G_SECOND ];

  gl = genv[pageof(lhs)];       /* gl: generation # of lhs */
  gr = genv[pageof(rhs)];       /* gr: generation # of rhs */
  if (gl == gr) return;
  if (globals[ G_FILTER_REMSET_GEN_ORDER ] && gl <= gr) return;  
  if (globals[ G_FILTER_REMSET_RHS_NUM ] == gr) return;
  if (globals[ G_FILTER_REMSET_LHS_NUM ] == gl) return;

  ssbtopv = (word**)globals[ G_SSBTOPV ];
  ssblimv = (word**)globals[ G_SSBLIMV ];

  /* XXX should we track the most recent entry and filter repeats? */
  if (SSB_ENQUEUE_LOUDLY)
    if (*(ssbtopv[gr]-1) != lhs) 
      consolemsg("gbuf enq: 0x%08x (%d)", lhs, gen_of(lhs));
  *ssbtopv[gr] = lhs;
  assert( tagof(lhs) != 0 );
#if SSB_ENQUEUE_OFFSET_AS_FIXNUM
  assert( is_fixnum( ((word)globals[G_THIRD] - (word)ptrof(lhs)) ));
  *(ssbtopv[gr]+1) = ((word)globals[G_THIRD] - (word)ptrof(lhs));
  ssbtopv[gr] = (ssbtopv[gr])+2;
  if (ssbtopv[gr]+1 >= ssblimv[gr]) 
    gc_compact_all_ssbs( the_gc(globals) );
#else
  ssbtopv[gr] = (ssbtopv[gr])+1;
  if (ssbtopv[gr] == ssblimv[gr]) 
    gc_compact_all_ssbs( the_gc(globals) );
#endif
}

static void satb_enqueue( word *globals, word ptr, word parent_ptr ) 
{
  gc_t *gc = the_gc(globals);
  if (SSB_ENQUEUE_LOUDLY) 
    consolemsg("satb enq: 0x%08x (%d)", ptr, gen_of(ptr));
  SSB_ENQUEUE( gc, gc->satb_ssb, ptr );
}

void EXPORT mc_satb_barrier( word *globals )
{
  word rTmp = *(word*)globals[ G_THIRD ];
  word result = (word)globals[ G_RESULT ];
  if (globals[ G_CONCURRENT_MARK ] && isptr(rTmp) &&
      (gen_of(rTmp) != 0) && (gen_of(result) != 0)) {
    satb_enqueue( globals, rTmp, result );
  }
}

static void mc_gen_barrier( word *globals )
{
  if (isptr( globals[ G_SECOND ] ))
    mc_partial_barrier( globals );
}

void EXPORT mc_compact_satb_ssb_and_genb( word *globals )
{
  gc_t *gc = the_gc(globals);
  seqbuf_t *ssb = gc->satb_ssb;

  assert( *ssb->top == *ssb->lim );

  if (*ssb->top == *ssb->lim)
    process_seqbuf( (gc), ssb );

  mc_gen_barrier( globals );
}

void EXPORT mc_full_barrier( word *globals )
{
  mc_satb_barrier( globals );
  mc_gen_barrier( globals );
}

/* Stack underflow handler. */

cont_t refill_stack_cache( word *globals )
{
  word *stkp;

  gc_stack_underflow( the_gc( globals ) );
  stkp = (word*)globals[ G_STKP ];
  return stkp[ STK_RETADDR ];
}

/* Signal handling */

/* Execute_sigfpe_magic() is called from the signal handler _only_ when 
   an FPE signal was received in millicode or compiled code.  It needs 
   to call the millicode exception handler while bypassing the context 
   that's in error.
   */
void execute_sigfpe_magic( void *context )
{
  unblock_all_signals();        /* Reset signal mask, really. */
  my_longjmp( dispatch_jump_buffer, DISPATCH_SIGFPE );
}

void handle_sigfpe( word *globals )
{
  /* FPE error code is in globals[ G_FPE_CODE ]. */
  globals[ G_THIRD ] = globals[ G_FPE_CODE ];
  globals[ G_FPE_CODE ] = 0;
  signal_exception( globals, EX_FPE, 0, 0 );
}


/* Helpers */

/* 'k'          is a Scheme return address or 0.
   'preserve'   is true iff RESULT should be preserved.
   */
static void 
signal_exception( word *globals, word exception, cont_t k, int preserve )
{
  globals[ G_FOURTH ] = fixnum( exception );
  mc_scheme_callout( globals, MS_EXCEPTION_HANDLER, 4, k, preserve );
}

static void setup_timer( word *globals, int timer, cont_t k )
{
  if (timer <= TIMER_STEP) {
    globals[ G_TIMER ] = timer;
    globals[ G_TIMER2 ] = 0;
  }
  else {
    globals[ G_TIMER ] = TIMER_STEP;
    globals[ G_TIMER2 ] = timer - TIMER_STEP;
  }

  check_signals( globals, k );
}

/* A hook to set a breakpoint on... */
static void signal_keyboard_int( word *globals, cont_t k ) {
  signal_exception( globals, EX_KBDINTR, k, 1 );  
}

static void check_signals( word *globals, cont_t k )
{
  if (globals[ G_SIGNAL ] != 0 ) {
    if (globals[ G_SIGINT ] != 0) {
      globals[ G_SIGINT ] = 0;       /* Really a race condition */
      signal_keyboard_int( globals, k );
    }
    globals[ G_SIGNAL ] = 0;         /* Really a race condition */
  }
  else if (globals[ G_FPE_CODE ] != 0) {
    globals[ G_THIRD ] = globals[ G_FPE_CODE ];
    globals[ G_FPE_CODE ] = 0;       /* Really a race condition */
    signal_exception( globals, EX_FPE, k, 1 );
  }
}

/* Call Scheme when the VM is in Scheme mode already. The problem here is
   that when Scheme code calls a millicode procedure, it is not required to
   save any of its registers.  Thus, when the millicode must call out to 
   Scheme, the caller's context must be saved before the new Scheme procedure
   is invoked.  This context must also be restored before control is returned
   to the original caller, and, again, the original caller will not do this,
   so we must arrange for it to happen.

   The context comprises root registers, which are stored as is, and
   floating registers, which are split across four(!) slots and stored as
   fixnums, one halfword per slot.  (On 64-bit systems two slots would suffice.)

   To accomodate interrupts, the contents of RESULT is saved and restored iff 
   globals[ G_SCHCALL_SAVERES ] == #t on entry to this procedure.

   We create a special stack frame, shown below. The frame's return address
   points to the millicode procedure internal_scheme_return (defined below); 
   the save area contains the return offset from R0 as a fixnum and the saved
   registers.

       +--------------------------------------+  <- tos
       | (frame size)                         |
       | (retaddr - to return_from_scheme)    |
       | (dynlink - garbage)                  |
       | 0                                    |
       | (scheme return address)              |
       | (saved R0)                           |
       | (saved R1)                           |
       | ...                                  |
       | (saved Rr)                           |
       | (saved F0) first 16 bits             |
       | (saved F0)  next 16 bits             |
       | (saved F0)  next 16 bits             |
       | (saved F0)  next 16 bits             |
       | ...                                  |
       | (saved Fn)                           |
       | (saved Fn)                           |
       | (saved Fn)                           |
       | (saved Fn)                           |
       | (saved RESULT)                       |
       | (saved RESULT restore flag)          |
       +--------------------------------------+

   */

#if VEC_HEADER_WORDS != 1
  /* This changes the frame layout, and I'm too lazy to work on it now */
# error "Still a few bugs in the system"
#endif

#define S2S_REALFRAMESIZE       roundup_walign( NREGS+NFPREGS*4+7 )
  /* REALFRAMESIZE is what we bump stkp by: full header plus pad word.
     Size in _words_.
     */

#define S2S_FRAMESIZE           ((NREGS+NFPREGS*4+6)*sizeof( word ))
  /* FRAMESIZE is what we store in the frame: sans header or pad word.
     Size in _bytes_.
     */

/* 'Index' is the index in the millicode support vector of the procedure.
   'Argc' is the number of arguments.
   'K' is zero if a simple return is okay, nonzero if a longjmp is required
        (On most versions of Larceny, a nonzero value means k is a return
        address, but k is just a flag in the Fence/ARM version.)
   'Preserve' is 1 if RESULT is to be preserved across the callout.
   The procedure arguments are in RESULT, SECOND, THIRD, FOURTH.
   */

void mc_scheme_callout( word *globals, int index, int argc, cont_t k, bool preserve )
{
  word *stkp;
  word *stklim;
  word callouts;
  int i;

 start:
  stkp = (word*)globals[ G_STKP ];
  stklim = (word*)globals[ G_ETOP ] + SCE_BUFFER;

  /* Allocate frame */
  stkp = stkp - S2S_REALFRAMESIZE;
  if (stkp < stklim) {
    mc_stack_overflow( globals );
    goto start;
  }
  globals[ G_STKP ] = (word)stkp;

  /* Initialize frame */
  stkp[ STK_CONTSIZE ] = (word)S2S_FRAMESIZE;
  stkp[ STK_DYNLINK ] = 0;
  stkp[ STK_RETADDR ] = (word)fence_return_from_scheme;
  stkp[ STK_REG0 ] = 0;
  stkp[ 4 ] = globals[G_RETADDR];
  for ( i=0 ; i < NREGS ; i++ )
    stkp[ 5+i ] = globals[ G_REG0+i ];
  /* FIXME: this next part assumes 32-bit words */
  for ( i=0 ; i < NFPREGS ; i++ ) {
    stkp[ 5+NREGS+(4*i)   ] = (globals[ G_F0+(2*i) ] & 65535) << 2;
    stkp[ 5+NREGS+(4*i)+1 ] = (globals[ G_F0+(2*i) ] >> 16) << 2;
    stkp[ 5+NREGS+(4*i)+2 ] = (globals[ G_F0+(2*i)+1 ] & 65535) << 2;
    stkp[ 5+NREGS+(4*i)+3 ] = (globals[ G_F0+(2*i)+1 ] >> 16) << 2;
  }
  stkp[ 5+NREGS+(4*NFPREGS) ]   = globals[ G_RESULT ];
  stkp[ 5+NREGS+(4*NFPREGS)+1 ] = (preserve ? TRUE_CONST : FALSE_CONST );

  /* Set up procedure, arguments, and argument count */
  globals[ G_REG1 ] = globals[ G_RESULT ];
  globals[ G_REG2 ] = globals[ G_SECOND ];
  globals[ G_REG3 ] = globals[ G_THIRD ];
  globals[ G_REG4 ] = globals[ G_FOURTH ];

  callouts = global_cell_ref( globals[ G_CALLOUTS ] );
  if (callouts == UNDEFINED_CONST)
    panic_exit( "mc_scheme_callout: no callout vector present." );

  globals[ G_REG0 ] = vector_ref( callouts, index );
  globals[ G_RESULT ] = fixnum( argc );

  if (k == 0) {
    my_longjmp( dispatch_jump_buffer, DISPATCH_CALL_R0 );
  }
  else {

    /* To call the procedure, return to its offset 0. */

    globals[ G_RETADDR ] = 0;
    return;
  }
}

/* Return address for scheme-to-scheme call frame. */

RTYPE return_from_scheme( CONT_PARAMS )
{
  cont_t k = restore_context( globals );
  globals[ G_RETADDR ] = k;
  return k;
}

/* Restore all registers.
   Pop the big frame, check for underflow!
   Return continuation to dispatch loop.
   */
cont_t restore_context( CONT_PARAMS )
{
  word *stkp;
  cont_t k;
  int i;

  stkp = (word*)globals[ G_STKP ];
  
  for ( i=0 ; i < NREGS ; i++ )
    globals[ G_REG0+i ] = stkp[ 5+i ];
  for ( i=0 ; i < NFPREGS ; i++ ) {
    globals[G_F0+(2*i)]   = (stkp[5+NREGS+(4*i)+1] << 14) | (stkp[5+NREGS+(4*i)] >> 2) ;
    globals[G_F0+(2*i)+1] = (stkp[5+NREGS+(4*i)+3] << 14) | (stkp[5+NREGS+(4*i)+2] >> 2);
  }
  k = stkp[ 4 ];

  if (stkp[ 5+NREGS+(4*NFPREGS)+1 ] == TRUE_CONST)
    globals[ G_RESULT ] = stkp[ 5+NREGS+(4*NFPREGS) ];
  globals[ G_STKP ] = (word)(stkp + S2S_REALFRAMESIZE);
  if (globals[ G_STKP ] == globals[ G_STKBOT ]) {
    gc_stack_underflow( the_gc( globals ) );
  }
  return k;
}

RTYPE dispatch_loop_return(word* globals)
{
  my_longjmp( dispatch_jump_buffer, DISPATCH_EXIT );
  assert(0); /* unreachable (by design in non-Petit systems) */
}

/* eof */
