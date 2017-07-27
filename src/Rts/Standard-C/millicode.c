/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Petit Larceny -- run-time support implementation, except for arithmetic.
 */

#define NOGLOBALS

#include "larceny.h"            /* Includes config.h also */
#include "gc.h"
#include "gc_t.h"               /* For gc_allocate() macro */
#include "barrier.h"            /* For prototypes */
#include "gclib.h"              /* For pageof() */
#include "stack.h"
#include "millicode.h"
#include "petit-machine.h"
#include "signals.h"
#include "assert.h"
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>

static void timer_exception( word *globals, cont_t k );
static void signal_exception( word *globals, word exception, cont_t k, 
                              int preserve );
static void setup_timer( word *globals, int timer, cont_t k );
static void check_signals( word *globals, cont_t k );

/* Also visible to native systems based on Petit Larceny */
void handle_sigfpe( word *globals );
cont_t refill_stack_cache( word *globals );
cont_t restore_context( word *globals );
RTYPE dispatch_loop_return( CONT_PARAMS );
RTYPE return_from_scheme( CONT_PARAMS );

#ifdef X86_NASM
extern void i386_return_from_scheme();
#endif

#ifdef PETIT_LARCENY
int twobit_cache_state = 0;     /* For petit-instr.h debug code */
# if USE_LONGJUMP || USE_RETURN_WITHOUT_VALUE
cont_t twobit_cont_label = 0;   /* Label to jump to */
# endif
static int valid_datum( word x );
# if USE_GOTOS_LOCALLY
static word *make_system_procedure( gc_t *gc, codeptr_t f );
static word *dispatch_loop_return_procedure;
static word *return_from_scheme_procedure;
static word *stack_underflow_procedure;
# endif
#endif /* PETIT_LARCENY */

/* These could go in the globals vector, too */
jmp_buf *dispatch_jump_buffer;
int already_running = 0;

#ifdef PETIT_LARCENY
void scheme_init( word *globals )
{
# if USE_GOTOS_LOCALLY
  gc_t *gc = the_gc( globals );
# endif /* USE_GOTOS_LOCALLY */
  
  initialize_generic_arithmetic();

# if USE_GOTOS_LOCALLY
  /* Create some system procedures used for control flow. */
  dispatch_loop_return_procedure = 
    make_system_procedure( gc, dispatch_loop_return );
  return_from_scheme_procedure = 
    make_system_procedure( gc, return_from_scheme );
  stack_underflow_procedure = 
    make_system_procedure( gc, mem_stkuflow );
# endif /* USE_GOTOS_LOCALLY */
}

# if USE_GOTOS_LOCALLY
static word *make_system_procedure( gc_t *gc, codeptr_t f )
{
  word *p;

  p = alloc_from_heap( sizeof(word)*(PROC_HEADER_WORDS+3) );
  p[0] = mkheader( sizeof(word)*3, PROC_HDR );
  p[PROC_HEADER_WORDS+IDX_PROC_CODE] = ENCODE_CODEPTR(f);
  p[PROC_HEADER_WORDS+IDX_PROC_CONST] = FALSE_CONST;
  p[PROC_HEADER_WORDS+IDX_PROC_REG0] = 0;

  return gc_make_handle( gc, tagptr( p, PROC_TAG ) );
}
# endif /* USE_GOTOS_LOCALLY */

void scheme_start( word *globals )
{
  cont_t f = 0;
  word *stkp = (word*)globals[ G_STKP ];
  int x;

  if (already_running)
    panic_abort( "Recursive call to petit_larceny_start (FFI?)" );
  already_running = 1;

  dispatch_jump_buffer = malloc(sizeof(jmp_buf));
  if (dispatch_jump_buffer == NULL)
    panic_abort("Couldn't allocate fresh jmp_buf");

  /* Patch in bootstrap code if necessary */
  if (procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE ) == FALSE_CONST)
    procedure_set(globals[G_REG0],IDX_PROC_CODE,ENCODE_CODEPTR(twobit_start));

  /* Return address for bottom-most frame */
# if USE_GOTOS_LOCALLY
  stkp[ STK_RETADDR ] = 0;
  stkp[ STK_REG0 ] = *dispatch_loop_return_procedure;
# else
  stkp[ STK_RETADDR ] = (word)dispatch_loop_return;
  stkp[ STK_REG0 ] = 0;
# endif

# if USE_LONGJUMP
  globals[ G_TIMER ] = TIMER_STEP;
# endif

  /* The dispatch loop is a doubly-nested quasi-loop.  

     The outer loop uses setjmp/longjmp for control and is entered but 
     rarely; most of the time is spent in the inner loop.  The job of
     the outer loop is to provide the inner loop with the address of
     the first block to execute.

     The structure of the inner loop depends on the jump discipline.  
     When the jump discipline is anything but USE_LONGJUMP, the inner 
     loop is a _while_ loop that performs control transfer to the next
     block, the address of which is returned from the previously executed 
     block either as a return value or through a global variable.  When 
     the jump discipline is USE_LONGJUMP, then the inner loop simply 
     consists of a call to a block, and the transfer to the next block 
     is done in the block itself by means of a tail call.  Occasionally,
     the C stack must be pruned, and the block signals a timer interrupt
     (longjump with DISPATCH_TIMER).
     */

  /* Outer loop */
  switch (x = setjmp( *dispatch_jump_buffer )) {
  case 0 :
  case DISPATCH_CALL_R0 :
# if USE_GOTOS_LOCALLY
    f = 0;
# else
    f = DECODE_CODEPTR(procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE ));
# endif
    globals[ G_EFFECTIVE_REG0 ] = globals[G_REG0];
    break;
  case DISPATCH_CALL_AGAIN :
# if USE_LONGJUMP
    /* A longjump has pruned the stack; now continue. */
    f = twobit_cont_label;
    globals[ G_EFFECTIVE_REG0 ] = globals[G_REG0];
    break;
# else
    panic_exit( "Unexpected entry to DISPATCH_CALL_AGAIN in scheme_start()" );
# endif
  case DISPATCH_EXIT:
    already_running = 0;
    return;
  case DISPATCH_RETURN_FROM_S2S_CALL :
    f = restore_context( globals );
    globals[ G_EFFECTIVE_REG0 ] = globals[G_REG0];
    break;
  case DISPATCH_STKUFLOW :
    f = refill_stack_cache( globals );
    /* G_EFFECTIVE_REG0 set by refill_stack_cache */
    break;
  case DISPATCH_SIGFPE :
    handle_sigfpe( globals );
    panic_exit( "handle_sigfpe() returned." );
  case DISPATCH_TIMER :
# if USE_LONGJUMP
    /* The first-level timer expired.  The longjmp has pruned the stack; now
       handle the timer expiration (and re-setup the timer).  The call to 
       timer_exception returns unless TIMER2==0 or an interrupt is pending.
       */
    timer_exception( globals, twobit_cont_label );
    f = twobit_cont_label;
    globals[ G_EFFECTIVE_REG0 ] = globals[G_REG0];
    break;
# else
    panic_exit( "Unexpected entry to DISPATCH_TIMER in scheme_start()" );
# endif
  default :
    panic_exit( "Unexpected value %d from setjmp in scheme_start()", x );
  }

  /* Inner loop */
# if USE_GOTOS_LOCALLY
   /* INVARIANT: f is an entry point within the code of the procedure 
    * in G_EFFECTIVE_REG0. 
    * We use G_EFFECTIVE_REG0 instead of REG0, because the
    * VALUES code may put something else into REG0. This fix cannot be
    * localized to the inner loop, because a stack underflow will
    * cause a jump to the outer loop, and REG0 must be preserved
    * across the jump.*/
#  if USE_RETURN_WITH_VALUE
   while (1)
   {
     codeptr_t p=DECODE_CODEPTR(procedure_ref((word *)globals[ G_EFFECTIVE_REG0 ],
                                              IDX_PROC_CODE));
     f = p( globals, f );
   }
#  elif USE_RETURN_WITHOUT_VALUE
#  error must update to use G_EFFECTIVE_REG0
   twobit_cont_label = f;
   while (1) {
     codeptr_t p=DECODE_CODEPTR(procedure_ref(globals[G_REG0],IDX_PROC_CODE));
     p( globals, twobit_cont_label );
   }
#  elif USE_LONGJUMP
#  error must update to use G_EFFECTIVE_REG0
   {
     codeptr_t p=DECODE_CODEPTR(procedure_ref(globals[G_REG0],IDX_PROC_CODE));
     p( globals, f );
   }
#  endif
# else /* USE_GOTOS_LOCALLY */
#  if USE_RETURN_WITH_VALUE
   while (1)
     f = ((codeptr_t)f)( globals );
#  elif USE_RETURN_WITHOUT_VALUE
   twobit_cont_label = f;
   while (1)
     ((codeptr_t)twobit_cont_label)( globals );
#  elif USE_LONGJUMP
   ((codeptr_t)f)( globals );
#  endif
# endif /* USE_GOTOS_LOCALLY */
   panic_exit( "Unexpected return from procedure in scheme_start()" );
}

void twobit_integrity_check( word *globals, const char *name )
{
  int i;
  word *stkp, *etop, *elim, *ebot, *stkbot, *frame;

  /* Check that roots contain only valid values */
  /* Fixme: should check handles too */
  for ( i=FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    if (!valid_datum( globals[i] ))
      panic_abort( "Invalid value 0x%08x found in global %d\n", 
                  globals[i], i );

  stkp = (word*)globals[ G_STKP ];
  etop = (word*)globals[ G_ETOP ];
  ebot = (word*)globals[ G_EBOT ];
  elim = (word*)globals[ G_ELIM ];
  stkbot = (word*)globals[ G_STKBOT ];

  /* Check heap and stack pointers */
  if (stkp < etop)
    panic_abort( "Stack pointer points below heap top!\n"
                 "stkp=0x%08x, etop=0x%08x\n", (word)stkp, (word)etop );
  if (stkp > stkbot)
    panic_abort( "Stack pointer points above stack bottom!\n"
                 "stkp=0x%08x, stkbot=0x%08x\n", (word)stkp, (word)stkbot );
  if (etop < ebot || etop > elim)
    panic_abort( "Heap pointers are not ordered correctly!\n"
                "ebot=0x%08x, etop=0x%08x, elim=0x%08x",
                (word)ebot, (word)etop, (word)elim );
                 
  /* Check that all stack frames look OK
       - Size field must be fixnum and >= 12
       - Return address must be fixnum
       - All slots accounted for by size field must have data values
     */
  frame = stkp;
  while (frame < stkbot) {
    word size = frame[ STK_FRAMESIZE ];
    word retaddr = frame[ STK_RETADDR ];
    if (!is_fixnum(size) || (s_word)size < 12)
      panic_abort( "Invalid stack frame size %u\n", size );
    if (!is_fixnum(retaddr))
      panic_abort( "Invalid return address 0x%08x\n", retaddr );
    for ( i=STK_REG0 ; i <= nativeint(size) ; i++ )
      if (!valid_datum( frame[i] ))
        panic_abort( "Invalid datum in stack frame: 0x%08x\n", frame[i] );
    frame = frame + roundup_walign( size/4+1 );
  }

  /* Obviously one can also test the heap, SSBs, remembered sets, and
     so on.
     */
}

static int valid_datum( word x )
{
  return (isptr( x ) && (caddr_t)x >= gclib_pagebase) ||
         is_fixnum( x ) || 
         is_char( x ) ||
         x == UNSPECIFIED_CONST ||
         x == UNDEFINED_CONST ||
         x == NIL_CONST ||
         x == TRUE_CONST || 
         x == FALSE_CONST ||
         x == EOF_CONST;
}

void stk_initialize_underflow_frame( word *stkp )
{
# if USE_GOTOS_LOCALLY
  stkp[ STK_RETADDR ] = 0;
  stkp[ STK_REG0 ] = *stack_underflow_procedure;
# else
  stkp[ STK_RETADDR ] = ENCODE_RETURN_ADDRESS(mem_stkuflow,0);
  stkp[ STK_REG0 ] = 0;
# endif
}
#endif /* PETIT_LARCENY */

void my_longjmp( jmp_buf *env, int val) {
  longjmp(*env, val);
}

RTYPE EXPORT mem_stkuflow( CONT_PARAMS )
{
  my_longjmp( dispatch_jump_buffer, DISPATCH_STKUFLOW );
}

void EXPORT mc_alloc_bv( word *globals )
{
  assert2( is_fixnum(globals[G_RESULT]) && (int)globals[G_RESULT] >= 0 );

#if defined( BDW_GC )
#error "This must be changed because RESULT is size in bytes"
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *p;

  nwords = roundup_walign( nwords );
  p = GC_malloc_atomic( nwords*sizeof(word) );
  assert2( p != 0 );
  globals[ G_RESULT ] = (word)p;
#else
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
  word *elim = (word*)globals[ G_STKP ];
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
  gc_collect(the_gc(globals), 0, 0, GCTYPE_COLLECT);
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
#if defined PETIT_LARCENY && USE_LONGJUMP
  twobit_cont_label = k;
  my_longjmp( dispatch_jump_buffer, DISPATCH_TIMER );
#else
  timer_exception( globals, k );
#endif
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
#if defined PETIT_LARCENY && USE_LONGJUMP
  /* For now: prune the stack */
  twobit_cont_label = k;
  my_longjmp( dispatch_jump_buffer, DISPATCH_CALL_AGAIN );
#endif
}

void EXPORT mc_disable_interrupts( word *globals, cont_t k )
{
  if (globals[ G_TIMER_ENABLE ] == TRUE_CONST) {
    globals[ G_TIMER_ENABLE ] = FALSE_CONST;
    globals[ G_RESULT ] = fixnum( globals[ G_TIMER ] + globals[ G_TIMER2 ] );
  }
  else
    globals[ G_RESULT ] = FALSE_CONST;
  check_signals( globals, k );
}

void EXPORT mc_exception( word *globals, word exception )
{
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
}

/* Millicode to deal with rest arguments.

   There are four cases, depending on how many arguments are passed and
   how many that are expected. When this procedure is entered it is
   assumed that at least the minimum number of arguments are passed; i.e.
   that check is performed outside this procedure.
 
   Let R = (the # of registers), r = (R - 1).
   Let j = RESULT (the actual number of arguments).
   Let n = SECOND (the number of fixed arguments).
  
   Since the compiler enforces that all fixed arguments are in registers,
   we then have two easy cases (the others are not relevant):
 
   Case 0: n < R-2, j < r [i.e. all fixed args and varargs are in registers]
     (set! REGn+1 (list REGn+1 ... REGj))
  
   Case 1: n < R-2, j >= r [i.e. all fixed args are in registers, but
     all varargs are not -- some are in a list in REGr].
     (set! REGn+1 (append! (list REGn+1 ... REGr-1) (copylist REGr)))
   */

void EXPORT mc_restargs( word *globals )
{
  word j = nativeuint( globals[ G_RESULT ] );
  word n = nativeuint( globals[ G_SECOND ] );
  word r = LASTREG;
  word *p, *first, *prev, t;
  word k, limit;
  word words;
#if !defined(BDW_GC)
  word *allocptr;
#endif

  words = 2*(j-n);

  if (words == 0) {
    globals[ G_REG0+n+1 ] = NIL_CONST;
    return;
  }

  /* At least one vararg to cons up. */

  /* Optimized allocation for precise GC; conservative GC calls
     allocator each time.
     */
#if !defined(BDW_GC)
  globals[ G_RESULT ] = fixnum( words );
  mc_alloc( globals );
  allocptr = (word*)globals[ G_RESULT ];
# define alloc_one_pair(p) (p = allocptr, allocptr+=2)
#else
# define alloc_one_pair(p) (p = (word*)alloc_from_heap(2*sizeof(word)) )
#endif
  globals[ G_RESULT ] = FALSE_CONST;              /* Sane value */

  first = prev = 0;
  k = n + 1;
  limit = min( j, r-1 );

  while ( k <= limit ) {
    alloc_one_pair(p);
    *p = globals[ G_REG0 + k ];
    if (prev) 
      *(prev+1) = tagptr( p, PAIR_TAG ); 
    else
      first = p;
    prev = p;
    k++;
  }

  if (j >= r) {
    t = globals[ G_REG0 + r ];

    /* Copy the list in t into the memory pointed to by p. */

    while ((word) t != NIL_CONST) {
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
}

void EXPORT mc_syscall( word *globals, cont_t k )
{
  int nargs = (int)nativeuint( globals[ G_RESULT ] )-1;
  int nproc = (int)nativeuint( globals[ G_REG1 ] );

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
	     (vector_length(x) == 0) && (vector_length(y) == 0))
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

void EXPORT mc_petit_patch_boot_code( word *globals )
{
  word l;
  int i;

  for ( i=0, l=globals[ G_RESULT ] ; l != NIL_CONST ; l=pair_cdr(l), i++ ) {
    word p = pair_car( l );
    procedure_set(p,IDX_PROC_CODE,ENCODE_CODEPTR(twobit_start_procedures[i]));
  }
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
  if (gl <= gr) return;  
  
  ssbtopv = (word**)globals[ G_SSBTOPV ];
  ssblimv = (word**)globals[ G_SSBLIMV ];
  *ssbtopv[gl] = lhs;
  ssbtopv[gl] = ssbtopv[gl]+1;
  if (ssbtopv[gl] == ssblimv[gl]) 
    gc_compact_all_ssbs( the_gc(globals) );
}

void EXPORT mc_full_barrier( word *globals )
{
  if (isptr( globals[ G_SECOND ] ))
    mc_partial_barrier( globals );
}

/* Stack underflow handler. */

cont_t refill_stack_cache( word *globals )
{
  word *stkp;

  gc_stack_underflow( the_gc( globals ) );
  stkp = (word*)globals[ G_STKP ];
#if defined PETIT_LARCENY && USE_GOTOS_LOCALLY
  globals[ G_EFFECTIVE_REG0 ] = stkp[ STK_REG0 ];
#endif
  return DECODE_RETURN_ADDRESS(stkp[ STK_RETADDR ]);
}

/* Cache flushing */

void cache_setup( void )
{
  /* Not needed for Petit Larceny */
}

void mem_icache_flush( void *lo, void *limit )
{
  /* Not needed for Petit Larceny */
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

static void check_signals( word *globals, cont_t k )
{
  if (globals[ G_SIGNAL ] != 0 ) {
    if (globals[ G_SIGINT ] != 0) {
      globals[ G_SIGINT ] = 0;       /* Really a race condition */
      signal_exception( globals, EX_KBDINTR, k, 1 );  
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
       | (saved R31)                          |
       | (saved RESULT)                       |
       | (saved RESULT restore flag)          |
       +--------------------------------------+

   */

#if VEC_HEADER_WORDS != 1
  /* This changes the frame layout, and I'm too lazy to work on it now */
# error "Still a few bugs in the system"
#endif

#define S2S_REALFRAMESIZE       roundup_walign( NREGS+7 )
  /* REALFRAMESIZE is what we bump stkp by: full header plus pad word.
     Size in _words_.
     */

#define S2S_FRAMESIZE           ((NREGS+6)*sizeof( word ))
  /* FRAMESIZE is what we store in the frame: sans header or pad word.
     Size in _bytes_.
     */

/* 'Index' is the index in the millicode support vector of the procedure.
   'Argc' is the number of arguments.
   'K' is the continuation point in Scheme code (return address)
   'Preserve' is 1 if RESULT is to be preserved across the callout.
   The procedure arguments are in RESULT, SECOND, THIRD, FOURTH.
   */

void mc_scheme_callout( word *globals, int index, int argc, cont_t k, 
                       bool preserve )
{
  word *stkp;
  word *stklim;
  word callouts;
  int i;

 start:
  stkp = (word*)globals[ G_STKP ];
  stklim = (word*)globals[ G_ETOP ];

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
#if defined PETIT_LARCENY && USE_GOTOS_LOCALLY
  stkp[ STK_RETADDR ] = 0;
  stkp[ STK_REG0 ] = *return_from_scheme_procedure;
  stkp[ 4 ] = fixnum((word)k);
#elif defined X86_NASM
  stkp[ STK_RETADDR ] = (word)i386_return_from_scheme;
  stkp[ STK_REG0 ] = 0;
  stkp[ 4 ] = (word)k;
#else /* PETIT_LARCENY && !USE_GOTOS_LOCALLY */
  stkp[ STK_RETADDR ] = ENCODE_RETURN_ADDRESS(0,return_from_scheme);
  stkp[ STK_REG0 ] = 0;
  stkp[ 4 ] = (word)k;
#endif
  for ( i=0 ; i < NREGS ; i++ )
    stkp[ 5+i ] = globals[ G_REG0+i ];
  stkp[ 5+NREGS ] = globals[ G_RESULT ];
  stkp[ 5+NREGS+1 ] = (preserve ? TRUE_CONST : FALSE_CONST );

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

  my_longjmp( dispatch_jump_buffer, DISPATCH_CALL_R0 );
}

/* Return address for scheme-to-scheme call frame. 
   */
RTYPE return_from_scheme( CONT_PARAMS )
{
  my_longjmp( dispatch_jump_buffer, DISPATCH_RETURN_FROM_S2S_CALL );
}

/* Restore all registers.
   Pop the big frame, check for underflow!
   Return continuation to dispatch loop.
   */
cont_t restore_context( word *globals )
{
  word *stkp;
  cont_t k;
  int i;

  stkp = (word*)globals[ G_STKP ];
#if defined PETIT_LARCENY && USE_GOTOS_LOCALLY
  k = (cont_t)nativeint(stkp[ 4 ]);
#else
  k = (cont_t)stkp[ 4 ];
#endif
  for ( i=0 ; i < NREGS ; i++ )
    globals[ G_REG0+i ] = stkp[ 5+i ];
  if (stkp[ 5+NREGS+1 ] == TRUE_CONST)
    globals[ G_RESULT ] = stkp[ 5+NREGS ];
  globals[ G_STKP ] = (word)(stkp + S2S_REALFRAMESIZE);
  if (globals[ G_STKP ] == globals[ G_STKBOT ])
    gc_stack_underflow( the_gc( globals ) );
  return k;
}

RTYPE dispatch_loop_return( CONT_PARAMS )
{
  my_longjmp( dispatch_jump_buffer, DISPATCH_EXIT );
}

/* eof */
