/* Rts/Standard-C/millicode.c
 * Petit Larceny -- run-time support implementation, except for arithmetic.
 *
 * $Id$
 */

#define NOGLOBALS

#include <setjmp.h>
#include <stdlib.h>
#include "larceny.h"
#include "gc_t.h"		/* For gc_allocate() macro */
#include "barrier.h"		/* For prototypes */
#include "gclib.h"		/* For pageof */
#include "stack.h"
#include "millicode.h"
#include "petit-hacks.h"	/* Temporary grossness */
#include "signals.h"

int cached_state = 0;           /* HACK! For twobit.h debug code */

static void signal_exception( word *globals, word exception, cont_t k, 
			      int preserve );
static void setup_timer( word *globals, int timer, cont_t k );
static void check_signals( word *globals, cont_t k );
static cont_t restore_context( word *globals );
static void dispatch_loop_return( void );
static void handle_sigfpe( word *globals );
static cont_t refill_stack_cache( word *globals );
static int valid_datum( word x );

/* These could go in the globals vector, too */
static jmp_buf dispatch_jump_buffer;
static int already_running = 0;

#define DISPATCH_CALL_AGAIN             1
#define DISPATCH_EXIT                   2
#define DISPATCH_RETURN_FROM_S2S_CALL   3
#define DISPATCH_STKUFLOW               4
#define DISPATCH_SIGFPE                 5

typedef cont_t (*tramp_t)( CONT_PARAMS );

void scheme_init( word *globals )
{
  /* Set rounding modes etc */
  initialize_generic_arithmetic();
}

void scheme_start( word *globals )
{
  cont_t f;
  word *stkp = (word*)globals[ G_STKP ];
  int x;

  if (already_running)
    panic_abort( "Recursive call to petit_larceny_start (FFI?)" );
  already_running = 1;

  /* Patch in bootstrap code if necessary */
  if (procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE ) == FALSE_CONST)
    procedure_set( globals[ G_REG0 ], IDX_PROC_CODE, (word)twobit_start );

  /* Return address for bottom-most frame */
  stkp[ 1 ] = (word)dispatch_loop_return;

  /* Dispatch loop */
  x = setjmp( dispatch_jump_buffer );
  if (x == DISPATCH_EXIT) {
    already_running = 0;
    return;
  }
  else if (x == DISPATCH_RETURN_FROM_S2S_CALL)
    f = restore_context( globals );
  else if (x == DISPATCH_STKUFLOW)
    f = refill_stack_cache( globals );
  else if (x == DISPATCH_SIGFPE) {
    handle_sigfpe( globals );
    panic( "handle_sigfpe() returned." );
  }
  else 
    f = (cont_t)(procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE ));

  while (1)
    f = ((tramp_t)f)( CONT_ACTUALS );
}

void twobit_integrity_check( word *globals, const char *name )
{
  int i;
  word *stkp, *etop, *elim, *ebot, *stkbot, *frame;

  /* Check that roots contain only valid values */
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
    if (size % 4 != 0 || (s_word)size < 12)
      panic_abort( "Invalid stack frame size %u\n", size );
    if (retaddr % 4 != 0)
      panic_abort( "Invalid return address 0x%08x\n", retaddr );
    for ( i=STK_REG0 ; i <= size/4 ; i++ )
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

void mc_alloc_bv( word *globals )
{
#if defined( BDW_GC )
#error "This must be changed because RESULT is size in bytes"
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *p;

  nwords = roundup_walign( nwords );
  p = GC_malloc_atomic( nwords*sizeof(word) );
  if (p == 0)
    panic( "mc_alloc_bv: GC_malloc returned 0." );
  globals[ G_RESULT ] = (word)p;
#else
  globals[ G_RESULT ] = roundup4( globals[ G_RESULT ] >> 2 );
  mc_alloc( globals );
#endif
}

void mc_alloc( word *globals )
{
#if defined( BDW_GC )
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *p;

  nwords = roundup_walign( nwords );
  p = GC_malloc( nwords*sizeof(word) );
  if (p == 0)
    panic( "mc_alloc: GC_malloc returned 0." );
  globals[ G_RESULT ] = (word)p;
#else
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *etop = (word*)globals[ G_ETOP ];
  word *elim = (word*)globals[ G_STKP ];
  word *p;

  nwords = roundup_walign( nwords );
  p = etop;
  etop += nwords;
  if (etop <= elim) {
    globals[ G_ETOP ] = (word)etop;
    globals[ G_RESULT ] = (word)p;
  }
  else {
    globals[ G_RESULT ] =
      (word)gc_allocate( (gc_t*)globals[ G_GC ], nwords*sizeof( word ), 0, 0 );
  }
#endif
}

void mc_alloci( word *globals )
{
  int nwords = (int)nativeuint( globals[ G_RESULT ] );
  word *p;
  word init;

  mc_alloc( globals );
  p = (word*)globals[ G_RESULT ];
  init = globals[ G_SECOND ];
  while ( nwords-- > 0 )
    *p++ = init;
}

void mem_stkuflow( void )
{
  longjmp( dispatch_jump_buffer, DISPATCH_STKUFLOW );
}

void mc_stack_overflow( word *globals )
{
  gc_stack_overflow( (gc_t*)globals[ G_GC ] );
}

void mc_capture_continuation( word *globals )
{
  globals[ G_RESULT ] = gc_creg_get( (gc_t*)globals[ G_GC ] );
}

void mc_restore_continuation( word *globals )
{
  gc_creg_set( (gc_t*)globals[ G_GC ], globals[ G_RESULT ] );
}

void mc_full_barrier( word *globals )
{
  if (isptr( globals[ G_SECOND ] ))
    mc_partial_barrier( globals );
}

void mc_partial_barrier( word *globals )
{
  unsigned *genv, gl, gr;
  word **ssbtopv, **ssblimv;
  word lhs, rhs;

  genv = (unsigned*)globals[ G_GENV ];
  if (genv == 0) return;	/* Barrier disabled */

  lhs = globals[ G_RESULT ];
  rhs = globals[ G_SECOND ];

  gl = genv[pageof(lhs)];       /* gl: generation # of lhs */
  gr = genv[pageof(rhs)];       /* gr: generation # of rhs */
  if (gl <= gr) return;  
  
  /* Experiment */
  if (gr == gl-1 && gl == globals[ G_NP_YOUNG_GEN ]) {
    gl = globals[ G_NP_YOUNG_GEN_SSBIDX ];
  }

  ssbtopv = (word**)globals[ G_SSBTOPV ];
  ssblimv = (word**)globals[ G_SSBLIMV ];
  *ssbtopv[gl] = lhs;
  ssbtopv[gl] = ssbtopv[gl]+1;
  if (ssbtopv[gl] == ssblimv[gl]) compact_ssb();
}

void mc_break( word *globals )
{
  if (globals[ G_BREAKPT_ENABLE ] == TRUE_CONST)
    localdebugger();
}

void mc_timer_exception( word *globals, cont_t k )
{
  check_signals( globals, k );

  if (globals[ G_TIMER_ENABLE ] == FALSE_CONST)
    globals[ G_TIMER ] = TEMPORARY_FUEL;         /* Run a little longer */
  else if (globals[ G_TIMER2 ] != 0) 
    setup_timer( globals, globals[ G_TIMER2 ], k );
  else
    signal_exception( globals, EX_TIMER, k, 1 );
}

void mc_enable_interrupts( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];

  if (is_fixnum( x ) && (int)x > 0) {
    globals[ G_TIMER_ENABLE ] = TRUE_CONST;
    setup_timer( globals, nativeuint( x ), k );
  }
  else
    signal_exception( globals, EX_EINTR, 0, 0 );
  check_signals( globals, k );
}

void mc_disable_interrupts( word *globals, cont_t k )
{
  if (globals[ G_TIMER_ENABLE ] == TRUE_CONST) {
    globals[ G_TIMER_ENABLE ] = FALSE_CONST;
    globals[ G_RESULT ] = fixnum( globals[ G_TIMER ] + globals[ G_TIMER2 ] );
  }
  else
    globals[ G_RESULT ] = FALSE_CONST;
  check_signals( globals, k );
}

void mc_exception( word *globals, word exception )
{
  signal_exception( globals, exception, 0, 0 );
}

void mc_cont_exception( word *globals, word exception, cont_t k )
{
  signal_exception( globals, exception, k, 0 );
}

void mc_apply( word *globals )
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

void mc_restargs( word *globals )
{
  word j = nativeuint( globals[ G_RESULT ] );
  word n = nativeuint( globals[ G_SECOND ] );
  word r = LASTREG;
  word R = NREGS;
  word *p, *q, t;
  word k, limit;
  word words;

  words = 2*(j-n);

  if (words == 0) {
    globals[ G_REG0+n+1 ] = NIL_CONST;
    return;
  }

  /* At least one vararg to cons up. */

  globals[ G_RESULT ] = fixnum( words );
  mc_alloc( globals );
  q = p = (word*)globals[ G_RESULT ];
  globals[ G_RESULT ] = FALSE_CONST;              /* Sane value */

  k = n + 1;
  limit = min( j, r-1 );

  while ( k <= limit ) {
    *p = globals[ G_REG0 + k ];
    *(p+1) = tagptr( (p+2), PAIR_TAG );
    p += 2;
    k++;
  }

  if (j >= r) {
    t = globals[ G_REG0 + r ];

    /* Copy the list in t into the memory pointed to by p. */

    while ((word) t != NIL_CONST) {
      *p = pair_car( t );
      *(p+1) = tagptr( (p+2), PAIR_TAG );
      p += 2;
      t = pair_cdr( t );
    }
  }

  *(p-1) = NIL_CONST;
  globals[ G_REG0+n+1 ] = tagptr( q, PAIR_TAG );
}

void mc_syscall( word *globals, cont_t k )
{
  int nargs = (int)nativeuint( globals[ G_RESULT ] )-1;
  int nproc = (int)nativeuint( globals[ G_REG1 ] );

  larceny_syscall( nargs, nproc, &globals[ G_REG2 ] );
  check_signals( globals, k );
}

void mc_typetag( word *globals )
{
  word obj = globals[ G_RESULT ];
  int t = tagof( obj );

  if (t == BVEC_TAG || t == VEC_TAG || t == PROC_TAG)
    globals[ G_RESULT ] = typetag( *ptrof( obj ) );
  else
    signal_exception( globals, EX_TYPETAG, 0, 0 );
}

void mc_typetag_set( word *globals )
{
  word obj = globals[ G_RESULT ];
  word tag = globals[ G_SECOND ];
  word *p;
  int t = tagof( obj );
  
  if (t == BVEC_TAG || t == VEC_TAG || t == PROC_TAG) {
    /* checks if typetag is fixnum in range */
    if ((tag & (TYPETAG_MASK | FIXTAGMASK)) == tag) {
      p = ptrof( obj );
      *p = striptypetag(*p) | tag;
      return;
    }
  }
  signal_exception( globals, EX_TYPETAGSET, 0, 0 );
}

void mc_eqv( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];

  if (x == y)                             /* eq? => eqv? */
    globals[ G_RESULT ] = TRUE_CONST;     
  else if (tagof( x ^ y ) != 0)	          /* Different tags => not eqv? */
    globals[ G_RESULT ] = FALSE_CONST;
  else if (tagof( x ) == BVEC_TAG) {      /* Could be numbers */
    int t1 = *ptrof( x ) & 255;
    int t2 = *ptrof( y ) & 255;
    if (t1 == BIGNUM_HDR && t2 == BIGNUM_HDR)
      mc_equalp( globals, k );
    else if ((t1 == FLONUM_HDR || t1 == COMPNUM_HDR) &&
	     (t2 == FLONUM_HDR || t2 == COMPNUM_HDR))
      mc_equalp( globals, k );
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
    else
      globals[ G_RESULT ] = FALSE_CONST;
  }
  else
    globals[ G_RESULT ] = FALSE_CONST;
}

void mc_partial_list2vector( word *globals )
{
  word x = globals[ G_RESULT ];	                   /* a list */
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

  dest = p + VEC_HEADER_WORDS;
  while ( y-- ) {
    *dest++ = pair_car( x );
    x = pair_cdr( x );
  }
  
  globals[ G_RESULT ] = tagptr( p, VEC_TAG );
}

void mc_bytevector_like_fill( word *globals )
{
  word *x = ptrof( globals[ G_RESULT ] ); /* assume: bytevector-like */
  unsigned char c = (globals[ G_SECOND ] >> 2) & 255;
  int length = sizefield( *x );

  memset( x+BVEC_HEADER_WORDS, c, length );
}

void mc_bytevector_like_compare( word *globals )
{
  word *x = ptrof( globals[ G_RESULT ] ); /* assume: bytevector-like */
  word *y = ptrof( globals[ G_SECOND ] ); /* assume: bytevector-like */
  int lx, ly, l, n;

  lx = sizefield( *x );
  ly = sizefield( *y );
  n = memcmp( x+BVEC_HEADER_WORDS, y+BVEC_HEADER_WORDS, min( lx, ly ) );
  if (n)
    globals[ G_RESULT ] = fixnum( n );
  else
    globals[ G_RESULT ] = fixnum( lx - ly );
}

void mc_petit_patch_boot_code( word *globals )
{
  word l;
  int i;

  for ( i=0, l=globals[ G_RESULT ] ; l != NIL_CONST ; l=pair_cdr(l), i++ ) {
    word p = pair_car( l );
    procedure_set( p, IDX_PROC_CODE, (word)twobit_start_procedures[i] );
  }
}

/* Write barrier support */

void wb_lowlevel_enable_barrier( word *globals )
{
  /* No-op here. */
}

void wb_lowlevel_disable_barrier( word *globals )
{
  globals[ G_GENV ] = 0;
}

/* Stack underflow handler. */

static cont_t refill_stack_cache( word *globals )
{
  word *stkp;

  gc_stack_underflow( (gc_t*)globals[ G_GC ] );
  stkp = (word*)globals[ G_STKP ];
  return (cont_t)stkp[ STK_RETADDR ];
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
#if defined(SUNOS5)
  unblock_all_signals();	/* Before longjmp */
#endif
  longjmp( dispatch_jump_buffer, DISPATCH_SIGFPE );
}

static void handle_sigfpe( word *globals )
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

#define S2S_REALFRAMESIZE	roundup_walign( NREGS+7 )
  /* REALFRAMESIZE is what we bump stkp by: full header plus pad word.
     Size in _words_.
     */

#define S2S_FRAMESIZE		((NREGS+6)*sizeof( word ))
  /* FRAMESIZE is what we store in the frame: sans header or pad word.
     Size in _bytes_.
     */

/* 'Index' is the index in the millicode support vector of the procedure.
   'Argc' is the number of arguments.
   'K' is the continuation point in Scheme code (return address)
   'Preserve' is 1 if RESULT is to be preserved across the callout.
   The procedure arguments are in RESULT, SECOND, THIRD, FOURTH.
   */

static cont_t return_from_scheme( void );

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
  stkp[ 0 ] = (word)S2S_FRAMESIZE;
  stkp[ 1 ] = (word)return_from_scheme;
  stkp[ 2 ] = 0;
  stkp[ 3 ] = 0;
  stkp[ 4 ] = (word)k;
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
    panic( "mc_scheme_callout: no callout vector present." );

  globals[ G_REG0 ] = vector_ref( callouts, index );
  globals[ G_RESULT ] = fixnum( argc );

  longjmp( dispatch_jump_buffer, DISPATCH_CALL_AGAIN );
}

/* Return address for scheme-to-scheme call frame. 
   */
static cont_t return_from_scheme( void )
{
  longjmp( dispatch_jump_buffer, DISPATCH_RETURN_FROM_S2S_CALL );
}

/* Restore all registers.
   Pop the big frame, check for underflow!
   Return continuation to dispatch loop.
   */
static cont_t restore_context( word *globals )
{
  word *stkp;
  cont_t k;
  int i;

  stkp = (word*)globals[ G_STKP ];
  k = (cont_t)stkp[ 4 ];
  for ( i=0 ; i < NREGS ; i++ )
    globals[ G_REG0+i ] = stkp[ 5+i ];
  if (stkp[ 5+NREGS+1 ] == TRUE_CONST)
    globals[ G_RESULT ] = stkp[ 5+NREGS ];
  globals[ G_STKP ] = (word)(stkp + S2S_REALFRAMESIZE);
  if (globals[ G_STKP ] == globals[ G_STKBOT ])
    refill_stack_cache( globals );
  return k;
}

static void dispatch_loop_return( void )
{
  longjmp( dispatch_jump_buffer, DISPATCH_EXIT );
}

/* eof */
