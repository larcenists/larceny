/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- Foreign-function interface
 */

#include "larceny.h"

#include <assert.h>
#include <string.h>

/* larceny_C_ffi_apply()
 * This is a syscall, so the value is returned in RESULT.
 *
 * The function_address is a bytevector pointer; the bytevector contains
 *   instructions for a trampoline function created for the argument
 *   descriptor.
 * The argument_descriptor is a Scheme bytevector.
 * The return_descriptor is a Scheme fixnum.
 * The actuals is a list of actual arguments.
 *
 * The values in the argument_descriptor are the following:
 *     value   type             scheme types accepted
 *     -----   ---------------  ---------------------
 *         0   signed32         fixnum; -2^31 <= bignum < 2^31
 *         1   unsigned32       positive fixnum; 0 <= bignum < 2^32
 *         2   ieee32           flonum; compnum with 0 imag part
 *         3   ieee64           flonum; compnum with 0 imag part
 *         4   pointer          bytevector-like; vector-like; pair; 0
 *         5   signed64         fixnum; -2^63 <= bignum < 2^63
 *         6   unsigned64       positive fixnum; 0 <= bignum < 2^64
 *
 * The values for the return_descriptor are the following:
 *     value   type             C return type assumed
 *     -----   ---------------  ---------------------
 *         0   int              int
 *         1   unsigned         unsigned int
 *         2   double           double
 *         3   float            float
 *         4   void             void
 *         5   signed64         long long 
 *         6   unsigned64       unsigned long long
 *
 * Rules:
 * - The foreign code _may_not_ allocate memory from the Scheme heap.
 * - The foreign code _may_not_ assign Scheme pointers into Scheme data
 *   structures (because that would require dealing with the write barrier
 *   code, which may invoke the collector).
 */

typedef union {
  int      signed32;
  unsigned unsigned32;
  double   ieee64;
  float    ieee32;
  byte     *pointer;
  long long signed64;
  unsigned long long unsigned64;
} ffi_arg;

/* FIXME: Larceny's FFI has been unreliable when Larceny is built on a
 * Macintosh running a 64-bit OS (tickets #700, #661, and possibly #325).
 * MacOSX 10.8 seems to have been the first purely 64-bit version of MacOSX.
 * As of MacOSX 10.8, the default C compiler changed: gcc became an alias
 * for clang with the LLVM back end.  The FFI problem appears to have been
 * caused by a bug in that C compiler: the stack-allocated array declared by
 * 
 *   ffi_arg args[ 32 ];
 *
 * was not always allocated on an 4-byte boundary.  Declaring that array
 * as a global seems to work around the compiler bug.
 */

ffi_arg ffi_args[ 32 ];

/* Returns true, with the magnitude and sign, if w is a fixnum or a
 * bignum of at most 64 bits. */
static int ffi_magnitude( word w, unsigned long long *m, int *neg )
{
  if (is_fixnum( w )) {
    s_word n = (s_word)nativeint( w );
    *neg = (n < 0);
    *m = (n < 0 ? -(unsigned long long)n : (unsigned long long)n);
    return 1;
  }
  else if (tagof( w ) == BVEC_TAG
           && typetag( *ptrof( w ) ) == BIG_SUBTAG
           && bignum_length( w ) <= 2) {
    *neg = bignum_sign( w );
    *m = 0;
    if (bignum_length( w ) >= 1) *m = bignum_ref32( w, 0 );
    if (bignum_length( w ) == 2)
      *m |= (unsigned long long)bignum_ref32( w, 1 ) << 32;
    return 1;
  }
  else
    return 0;
}

#if defined(__aarch64__) || defined(__x86_64__)
#define FFI_C_CALL 1
/* On AArch64 and x86-64 there is no trampoline code: the "trampoline"
 * bytevector holds the address of the foreign function, and the call
 * is made here, through a prototype that passes every argument
 * register and some stack.  (Generated code would have to live in
 * executable memory, which MacOS keeps apart from the heap, and a C
 * call works for Petit Larceny too.)
 *
 * Integer and pointer arguments go in the integer argument registers
 * (x0-x7 on AArch64; rdi, rsi, rdx, rcx, r8, r9 on x86-64), floating-
 * point arguments in v0-v7 or xmm0-xmm7 (a float in the low 32 bits),
 * and the rest on the stack.  On MacOS/AArch64 a stack argument takes
 * its natural size and alignment; in the standard AAPCS64 and in the
 * System V x86-64 ABI each takes 8 bytes.  Callees ignore the register
 * and stack arguments they do not use.
 *
 * Variadic functions are not supported on AArch64 (MacOS passes their
 * variadic arguments on the stack).  On x86-64 the prototype is itself
 * variadic, so that the caller sets %al to the number of vector
 * registers used, as a variadic callee requires; the arguments are
 * assigned to registers and stack as for a prototyped call.
 */

#define FFI_C_STACK_WORDS 16

typedef unsigned long long ffi_gp;

#define FFI_C_STACK_ARGS \
  st[0], st[1], st[2], st[3], st[4], st[5], st[6], st[7], \
  st[8], st[9], st[10], st[11], st[12], st[13], st[14], st[15]

#if defined(__aarch64__)
#define FFI_C_GP_REGS 8

#define FFI_C_PARAMS \
  ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, \
  double, double, double, double, double, double, double, double, \
  ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, \
  ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp, ffi_gp

#define FFI_C_ARGS \
  gp[0], gp[1], gp[2], gp[3], gp[4], gp[5], gp[6], gp[7], \
  fp[0], fp[1], fp[2], fp[3], fp[4], fp[5], fp[6], fp[7], \
  FFI_C_STACK_ARGS
#else /* __x86_64__ */
#define FFI_C_GP_REGS 6

#define FFI_C_PARAMS ffi_gp, ...

#define FFI_C_ARGS \
  gp[0], gp[1], gp[2], gp[3], gp[4], gp[5], \
  fp[0], fp[1], fp[2], fp[3], fp[4], fp[5], fp[6], fp[7], \
  FFI_C_STACK_ARGS
#endif

typedef ffi_gp (*ffi_gp_fn)( FFI_C_PARAMS );
typedef double (*ffi_double_fn)( FFI_C_PARAMS );
typedef float  (*ffi_float_fn)( FFI_C_PARAMS );

/* Places an argument of the given size on the stack; returns 0 if the
 * stack area is full. */
static int ffi_c_push( byte *stack, int *offset, const void *p, int size )
{
#if defined(__aarch64__) && defined(__APPLE__)
  int align = size;
#else
  int align = 8;
#endif
  int at = (*offset + align - 1) & ~(align - 1);
  if (at + size > FFI_C_STACK_WORDS*8)
    return 0;
  memcpy( stack + at, p, size );
  *offset = at + (size < align ? align : size);
  return 1;
}

/* Calls fn with the converted arguments, and leaves the result in the
 * result location for its return descriptor.  Returns 0 if the
 * arguments do not fit. */
static int ffi_c_call( void *fn, byte *adesc, int argc, ffi_arg *args,
                       int rdesc, ffi_gp *gp_result,
                       double *d_result, float *f_result )
{
  ffi_gp gp[FFI_C_GP_REGS] = { 0 }, st[FFI_C_STACK_WORDS] = { 0 };
  double fp[8] = { 0 };
  int ngp = 0, nfp = 0, offset = 0, i;

  for ( i=0 ; i < argc ; i++ ) {
    ffi_gp g;
    int32_t i32;
    uint32_t u32;
    switch (adesc[i]) {
    case 0 : /* signed32 */
      if (ngp < FFI_C_GP_REGS) gp[ngp++] = (ffi_gp)(long long)args[i].signed32;
      else { i32 = args[i].signed32;
             if (!ffi_c_push( (byte*)st, &offset, &i32, 4 )) return 0; }
      break;
    case 1 : /* unsigned32 */
      if (ngp < FFI_C_GP_REGS) gp[ngp++] = (ffi_gp)args[i].unsigned32;
      else { u32 = args[i].unsigned32;
             if (!ffi_c_push( (byte*)st, &offset, &u32, 4 )) return 0; }
      break;
    case 2 : /* ieee32 */
      if (nfp < 8) {
        union { double d; ffi_gp bits; } u;
        uint32_t fbits;
        memcpy( &fbits, &args[i].ieee32, 4 );
        u.bits = fbits;                 /* the float in the low 32 bits */
        fp[nfp++] = u.d;
      }
      else if (!ffi_c_push( (byte*)st, &offset, &args[i].ieee32, 4 ))
        return 0;
      break;
    case 3 : /* ieee64 */
      if (nfp < 8) fp[nfp++] = args[i].ieee64;
      else if (!ffi_c_push( (byte*)st, &offset, &args[i].ieee64, 8 ))
        return 0;
      break;
    case 4 : /* pointer */
    case 5 : /* signed64 */
    case 6 : /* unsigned64 */
      g = (adesc[i] == 4 ? (ffi_gp)args[i].pointer : args[i].unsigned64);
      if (ngp < FFI_C_GP_REGS) gp[ngp++] = g;
      else if (!ffi_c_push( (byte*)st, &offset, &g, 8 )) return 0;
      break;
    default :
      return 0;
    }
  }

  switch (rdesc) {
  case 2 : *d_result = ((ffi_double_fn)fn)( FFI_C_ARGS ); break;
  case 3 : *f_result = ((ffi_float_fn)fn)( FFI_C_ARGS ); break;
  default: *gp_result = ((ffi_gp_fn)fn)( FFI_C_ARGS ); break;
  }
  return 1;
}
#endif /* __aarch64__ || __x86_64__ */

void
larceny_C_ffi_apply( word trampoline_bytevector,
		     word argument_descriptor,
		     word return_descriptor,
		     word actuals )
{
  typedef void (*tramp_double_t)( ffi_arg *, double * );
  typedef void (*tramp_float_t)( ffi_arg *, float * );
  typedef void (*tramp_word_t)( ffi_arg *, word * );
  typedef void (*tramp_void_t)( ffi_arg * );
  typedef void (*tramp_ll_t)( ffi_arg *, long long * );
  typedef void (*tramp_ull_t)( ffi_arg *, unsigned long long * );

#if 0
  ffi_arg args[ 32 ];    /*  See FIXME comment above.  */
#endif
  ffi_arg *args = ffi_args;
  int i, limit, argc;
  word w_result;
  double d_result;
  float f_result;
  long long ll_result;
  unsigned long long ull_result;

  assert( sizeof( ffi_arg ) == 8 );
  assert( (((long) args) & 3) == 0 );  /* See FIXME comment above. */

  /* Phase 1: Check the trampoline pointer */

  if (tagof(trampoline_bytevector) != BVEC_TAG 
      || typetag(*ptrof(trampoline_bytevector)) != BVEC_SUBTAG) {
    hardconsolemsg( "FFICALL failed: invalid function pointer 0x%08" PRIxWORD,
		    trampoline_bytevector );
    goto failed;
  }

  /* Phase 2: convert the arguments */

  i = 0;
  limit = bytevector_length( argument_descriptor );
  while (actuals != NIL_CONST && i < limit) {
    word arg = pair_car( actuals );
    switch (bytevector_ref( argument_descriptor, i )) {
    case 0 :
      /* signed32 */
      switch(tagof(arg)) {
      case FIX1_TAG :
      case FIX2_TAG :
	args[i].signed32 = nativeint( arg );
	break;
      case BVEC_TAG :
	if (typetag(*ptrof( arg )) == BIG_SUBTAG && bignum_length( arg ) == 1){
	  unsigned w = bignum_ref32( arg, 0 );
	  if (bignum_sign( arg ) == 0 && w < 0x80000000)
	    args[i].signed32 = (int)w;
	  else if (bignum_sign( arg ) == 1 && w <= 0x80000000)
	    args[i].signed32 = -(int)w;
	  else
	    goto badarg_s32;
	}
	else
	  goto badarg_s32;
	break;
      default :
      badarg_s32:
	hardconsolemsg( "FFICALL failed: bad arg to signed-word32, val=0x%08" PRIxWORD,
		        arg );
	goto failed;
      }
      break;
    case 1 :
      /* unsigned32 */
      switch(tagof(arg)) {
      case FIX1_TAG :
      case FIX2_TAG :
	args[i].unsigned32 = (unsigned)nativeint( arg );
	break;
      case BVEC_TAG :
	if (typetag(*ptrof( arg )) == BIG_SUBTAG
	    && bignum_length( arg ) == 1
	    && bignum_sign( arg ) == 0) {
	  args[i].unsigned32 = bignum_ref32( arg, 0 );
	}
	else
	  goto badarg_u32;
	break;
      default :
      badarg_u32:
	hardconsolemsg( "FFICALL failed: bad arg to unsigned-word32, "
		        "val=0x%08" PRIxWORD, arg );
	goto failed;
      }
      break;
    case 2 :
      /* ieee32 */
      if (tagof(arg) == BVEC_TAG) {
	if (typetag(*ptrof(arg)) == FLO_SUBTAG)
	  args[i].ieee32 = (float)real_part(arg);
	else if (typetag(*ptrof(arg) == COMP_SUBTAG) && imag_part(arg) == 0.0)
	  args[i].ieee32 = (float)real_part(arg);
	else
	  goto badarg_f32;
      }
      else {
      badarg_f32:
	hardconsolemsg( "FFICALL failed: bad arg to ieee32, val=0x%08" PRIxWORD,
		        arg );
	goto failed;
      }
      break;
    case 3 :
      /* ieee64 */
      if (tagof(arg) == BVEC_TAG) {
	if (typetag(*ptrof(arg)) == FLO_SUBTAG)
	  args[i].ieee64 = real_part(arg);
	else if (typetag(*ptrof(arg) == COMP_SUBTAG) && imag_part(arg) == 0.0)
	  args[i].ieee64 = real_part(arg);
	else
	  goto badarg_f64;
      }
      else {
      badarg_f64:
	hardconsolemsg( "FFICALL failed: bad arg to ieee64, val=0x%08" PRIxWORD,
		        arg );
	goto failed;
      }
      break;
    case 4 :
      /* pointer */
      if (arg == 0)
	args[i].pointer = (byte*)0;
      else {
	switch (tagof( arg )) {
	case PAIR_TAG :
	  args[i].pointer = (byte*)ptrof(arg);
	  break;
	case VEC_TAG :
	case BVEC_TAG :
	  args[i].pointer = (byte*)(ptrof(arg)+1);
	  break;
	default :
	  hardconsolemsg( "FFICALL failed: bad arg to pointer, val=0x%08" PRIxWORD,
			 arg );
	  goto failed;
	}
      }
      break;
    case 5 : 
      /* signed64 */
      {
        unsigned long long m;
        int neg;
        if (!ffi_magnitude( arg, &m, &neg )
            || m > (neg ? 0x8000000000000000ULL : 0x7FFFFFFFFFFFFFFFULL)) {
          hardconsolemsg( "FFICALL failed: bad arg to signed-word64, "
                          "val=0x%08" PRIxWORD, arg );
          goto failed;
        }
        args[i].signed64 = (neg ? -(long long)m : (long long)m);
      }
      break;
    case 6 : 
      /* unsigned64 */
      {
        unsigned long long m;
        int neg;
        if (!ffi_magnitude( arg, &m, &neg ) || neg) {
          hardconsolemsg( "FFICALL failed: bad arg to unsigned-word64, "
                          "val=0x%08" PRIxWORD, arg );
          goto failed;
        }
        args[i].unsigned64 = m;
      }
      break;
    default :
      hardconsolemsg( "FFICALL failed: bad argdesc value %d",
		      bytevector_ref( argument_descriptor, i ) );
      goto failed;
    }
    i++;
    actuals = pair_cdr( actuals );
  }
  argc = i;
#if !FFI_C_CALL
  FIXME_UNUSED_VARIABLE(argc);
#endif

  if ((actuals == NIL_CONST) != (i == limit)) {
    /* error -- wrong number of arguments */
    hardconsolemsg( "FFICALL failed: wrong number of arguments." );
    goto failed;
  }
    
  /* Phase 3: Invoke the function, then convert result and return. */
#if FFI_C_CALL
  {
    void *fn;
    ffi_gp g = 0;
    int rdesc = (int)nativeint(return_descriptor);

    if (bytevector_length( trampoline_bytevector ) < sizeof(void*)) {
      hardconsolemsg( "FFICALL failed: no function address" );
      goto failed;
    }
    memcpy( &fn, ptrof(trampoline_bytevector)+1, sizeof(void*) );
    if (rdesc < 0 || rdesc > 6) {
      hardconsolemsg( "FFICALL failed: bad return descriptor %d", rdesc );
      goto failed;
    }
    if (!ffi_c_call( fn, (byte*)(ptrof(argument_descriptor)+1),
                           argc, args, rdesc, &g, &d_result, &f_result )) {
      hardconsolemsg( "FFICALL failed: too many arguments on the stack" );
      goto failed;
    }
    switch (rdesc) {
    case 0 : globals[ G_RESULT ] = box_int( (int)g ); break;
    case 1 : globals[ G_RESULT ] = box_uint( (unsigned)g ); break;
    case 2 : globals[ G_RESULT ] = box_double( d_result ); break;
    case 3 : globals[ G_RESULT ] = box_double( (double)f_result ); break;
    case 4 : globals[ G_RESULT ] = UNSPECIFIED_CONST; break;
    case 5 : globals[ G_RESULT ] = box_longlong( (long long)g ); break;
    case 6 : globals[ G_RESULT ] = box_ulonglong( g ); break;
    }
    return;
  }
#endif
  switch (nativeint(return_descriptor)) {
  case 0 :  /* int */
    ((tramp_word_t)(ptrof(trampoline_bytevector)+1))( args, &w_result );
    globals[ G_RESULT ] = box_int( (int)w_result );
    return;
  case 1 :  /* unsigned */
    ((tramp_word_t)(ptrof(trampoline_bytevector)+1))( args, &w_result );
    globals[ G_RESULT ] = box_uint( (unsigned)w_result );
    return;
  case 2 :  /* double */
    ((tramp_double_t)(ptrof(trampoline_bytevector)+1))( args, &d_result );
    globals[ G_RESULT ] = box_double( d_result );
    return;
  case 3 :  /* float */
    ((tramp_float_t)(ptrof(trampoline_bytevector)+1))( args, &f_result );
    globals[ G_RESULT ] = box_double( (double)f_result );
    return;
  case 4 :  /* void */ /* FIXME: Is this broken? For stdcall? */
    ((tramp_void_t)(ptrof(trampoline_bytevector)+1))( args );
    globals[ G_RESULT ] = UNSPECIFIED_CONST;
    return;
  case 5 :  /* long long */
    ((tramp_ll_t)(ptrof(trampoline_bytevector)+1))( args, &ll_result );
    globals[ G_RESULT ] = box_longlong( ll_result );
    return;
  case 6 :  /* unsigned long long */
    ((tramp_ull_t)(ptrof(trampoline_bytevector)+1))( args, &ull_result );
    globals[ G_RESULT ] = box_ulonglong( ull_result );
    return;
  default :
    hardconsolemsg( "FFICALL failed: bad return descriptor %" PRIdWORD, 
		    return_descriptor );
    goto failed;
  }
  /*NOTREACHED*/
  panic_exit( "Fell off the end in larceny_C_ffi_apply." );

 failed:
  globals[ G_RESULT ] = UNDEFINED_CONST;
}


/* This is a syscall.
 *
 * w_path is a pointer to a bytevector containing a string.
 *
 * Returns a handle of some sort, or 0 on error.
 */
void
larceny_C_ffi_dlopen( word w_path )
{
  char *path = (char*)(ptrof(w_path)+1);
  word desc = osdep_dlopen( path );
  globals[ G_RESULT ] = box_word( desc );
}


/* This is a syscall.
 *
 * w_handle is a 32-bit unsigned integer (bignum or fixnum) representing
 * a void* returned from a previous call to larceny_C_ffi_dlopen.
 *
 * w_sym is a pointer to a bytevector containing a string.
 *
 * Returns a handle of some sort, or 0 on error.
 */
void
larceny_C_ffi_dlsym( word w_handle, word w_sym )
{
  char *sym = (char*)(ptrof(w_sym)+1);
  word handle = unbox_word( w_handle );
  word r = osdep_dlsym( handle, sym );
  globals[ G_RESULT ] = box_word( r );
}


/* This is a syscall */
void larceny_C_ffi_getaddr( word w_key )
{
  switch (nativeint(w_key)) {
  case 0 : 
    globals[ G_RESULT ] = box_word( (word)larceny_C_ffi_convert_and_call );
    break;
  default : 
    globals[ G_RESULT ] = FALSE_CONST;
    break;
  }
}


/* larceny_C_ffi_convert_and_call is called from C or from a callback
 * trampoline.
 *
 * proc points to a location that contains a Scheme procedure pointer.
 * args points to an array of arguments.
 * result points to a properly aligned result location.
 * adesc points to a location that contains a bytevector pointer, this
 *  bytevector contains argument descriptor values.
 * rdesc is a result type descriptor value.
 *
 * Convert the arguments to Scheme representations and call the Scheme
 * procedure.  Then convert the result to a C type.
 */
void
larceny_C_ffi_convert_and_call( word *proc, word **args, void *result,
			        word *adesc, int rdesc, int argc )
{
  word argv[32], *x, scheme_result, u_val, *q;
  s_word s_val;
  int i;
  byte *argp;

#if !defined(BDW_GC)
  word *allocptr;
  int bytes;
  
  bytes = 0;

  /* Make a pass over the arguments and find out if we need to do any
   * allocation.  This can't be predetermined, although the upper limit
   * can be predetermined.
   */
  for ( i=0, argp=(byte*)(ptrof(adesc)+1) ; i < argc ; i++ ) {
    x = args[i];
    switch (argp[i]) {
    case 0 : /* signed32 */
      s_val = (s_word)*x;
      if (s_val < MOST_NEGATIVE_FIXNUM || s_val >= MOST_POSITIVE_FIXNUM)
	bytes += 16;
      break;
    case 1 : /* unsigned32 */
    case 4 : /* pointer */
      if (*x >= MOST_NEGATIVE_FIXNUM)
	bytes += 16;
      break;
    case 2 : /* ieee32 */
    case 3 : /* ieee64 */
      bytes += 16;
      break;
    default: 
      panic_abort( "Illegal descriptor: %d for argument #: %d", argp[i], i );
    }
  }

  /* Allocate. */
  if (bytes > 0)
    allocptr = alloc_from_heap( bytes );
  else
    allocptr = 0;
  
# define alloc_storage( var, bytes ) \
    ( var=allocptr, allocptr+=(bytes/sizeof(word)) )
#else /* BDW_GC */
  /* FIXME: this should be atomic allocation */
# define alloc_storage( var, bytes ) var=alloc_from_heap( bytes )
#endif
  
  /* Convert. */
  for ( argp=(byte*)(ptrof(adesc)+1), i=0 ; i < argc ; i++ ) {
    x = args[i];
    switch (argp[i]) {
    case 0 : /* signed32 */
      s_val = (s_word)*x;
      if (s_val < MOST_NEGATIVE_FIXNUM || s_val >= MOST_POSITIVE_FIXNUM){
	alloc_storage( q, 16 );
	*q = mkheader( 8, BIGNUM_HDR );
	if (s_val < 0) {
	  *(q+1) = mkbignum_header( 1, 1 );
	  *(q+2) = (word)-s_val;
	}
	else {
	  *(q+1) = mkbignum_header( 0, 1 );
	  *(q+2) = s_val;
	}
	argv[i] = tagptr(q,BVEC_TAG);
      }
      else
	argv[i] = fixnum(s_val);
      break;
    case 1 : /* unsigned32 */
    case 4 : /* pointer */
      u_val = *x;
      if (u_val >= MOST_NEGATIVE_FIXNUM) {
	alloc_storage( q, 16 );
	*q = mkheader( 8, BIGNUM_HDR );
	*(q+1) = mkbignum_header( 0, 1 );
	*(q+2) = u_val;
	argv[i] = tagptr(q, BVEC_TAG);
      }
      else
	argv[i] = fixnum(u_val);
      break;
    case 2 : /* ieee32 */
    case 3 : /* ieee64 */
      alloc_storage( q, 16 );
      *q = mkheader( 12, FLONUM_HDR );
      if (*argp == 2)
	*(double*)(q+2) = (double)*(float*)x;
      else
	*(double*)(q+2) = *(double*)x;
      argv[i] = tagptr( q, BVEC_TAG );
      break;
    }
  }

  larceny_call( *proc, argc, argv, &scheme_result );

  /* Convert the result */
  switch (rdesc) {
  case 0 : /* int */
    *(int*)result = unbox_int( scheme_result );
    break;
  case 1 : /* unsigned */
    *(unsigned*)result = unbox_uint( scheme_result );
    break;
  case 2 : /* double */
    *(double*)result = *(double*)(scheme_result+8);
    break;
  case 3 : /* float */
    *(float*)result = (float)*(double*)(scheme_result+8);
    break;
  case 4 : /* void */
    break;
  default :
    panic_abort( "Invalid return code in callback: %d", rdesc );
  }
}


/* This is a syscall */
void larceny_peek_bytes( word w_addr, word w_bytevector, word w_count )
{
  int n = nativeint( w_count );
  void *dest = ptrof( w_bytevector ) + 1;
  void *src  = (void*)(unbox_word( w_addr ));

  memcpy( dest, src, n );
}


/* This is a syscall */
void larceny_poke_bytes( word w_addr, word w_bytevector, word w_count )
{
  int n = nativeint( w_count );
  void *src  = ptrof( w_bytevector ) + 1;
  void *dest = (void*)(unbox_word( w_addr ));

  memcpy( dest, src, n );
}

/* eof */
