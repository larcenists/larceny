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
    hardconsolemsg( "FFICALL failed: invalid function pointer 0x%08x",
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
	hardconsolemsg( "FFICALL failed: bad arg to signed-word32, val=0x%08x",
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
		        "val=0x%08x", arg );
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
	hardconsolemsg( "FFICALL failed: bad arg to ieee32, val=0x%08x",
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
	hardconsolemsg( "FFICALL failed: bad arg to ieee64, val=0x%08x",
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
	  hardconsolemsg( "FFICALL failed: bad arg to pointer, val=0x%08x",
			 arg );
	  goto failed;
	}
      }
      break;
    case 5 : 
      /* signed64 */
      switch(tagof(arg)) {
      case FIX1_TAG :
      case FIX2_TAG :
	args[i].signed64 = nativeint( arg );
	break;
      case BVEC_TAG :
	if (typetag(*ptrof( arg )) == BIG_SUBTAG && bignum_length( arg ) == 1){
	  unsigned w = bignum_ref32( arg, 0 );
	  if (bignum_sign( arg ) == 0)
	    args[i].signed64 = (long long)w;
	  else if (bignum_sign( arg ) == 1)
	    args[i].signed64 = -(long long)w;
	  else
	    goto badarg_s64;
	}
	else if (typetag(*ptrof( arg )) == BIG_SUBTAG && 
		 bignum_length( arg ) == 2){
	  long long val = 0;
	  unsigned w0 = bignum_ref32( arg, 0 );
	  unsigned w1 = bignum_ref32( arg, 1 );
	  val += w0;
	  val += ((long long)w1) << 32;
	  if (bignum_sign( arg ) == 0 && w1 < 0x80000000)
	    args[i].signed64 = (long long)val;
	  else if (bignum_sign( arg ) == 1 && w1 <= 0x80000000)
	    args[i].signed64 = -(long long)val;
	  else
	    goto badarg_s64;
	}
	else
	  goto badarg_s64;
	break;
      default :
      badarg_s64:
	hardconsolemsg( "FFICALL failed: bad arg to signed-word64, val=0x%08x",
		        arg );
	goto failed;
      }
      break;
    case 6 : 
      /* unsigned64 */
      switch(tagof(arg)) {
      case FIX1_TAG :
      case FIX2_TAG :
	args[i].unsigned64 = (unsigned)nativeint( arg );
	break;
      case BVEC_TAG :
	if (typetag(*ptrof( arg )) == BIG_SUBTAG
	    && bignum_length( arg ) == 1
	    && bignum_sign( arg ) == 0) {
	  args[i].unsigned64 = bignum_ref32( arg, 0 );
	}
	else if (typetag(*ptrof( arg )) == BIG_SUBTAG
		 && bignum_length( arg ) == 2
		 && bignum_sign( arg ) == 0) {
	  unsigned long long val = 0;
	  val += bignum_ref32( arg, 0 );
	  val += ((unsigned long long)bignum_ref32( arg, 1 )) << 32;
	  args[i].unsigned64 = val;
	}
	else
	  goto badarg_u64;
	break;
      default :
      badarg_u64:
	hardconsolemsg( "FFICALL failed: bad arg to unsigned-word64, "
			"val=0x%08x", arg );
      goto failed;
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
  FIXME_UNUSED_VARIABLE(argc);

  if ((actuals == NIL_CONST) != (i == limit)) {
    /* error -- wrong number of arguments */
    hardconsolemsg( "FFICALL failed: wrong number of arguments." );
    goto failed;
  }
    
  /* Phase 3: Invoke the function, then convert result and return. */
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
    hardconsolemsg( "FFICALL failed: bad return descriptor %d", 
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
  globals[ G_RESULT ] = box_uint( (unsigned)desc );
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
  word handle = (word)unbox_uint( w_handle );
  word r = osdep_dlsym( handle, sym );
  globals[ G_RESULT ] = box_uint( (unsigned)r );
}


/* This is a syscall */
void larceny_C_ffi_getaddr( word w_key )
{
  switch (nativeint(w_key)) {
  case 0 : 
    globals[ G_RESULT ] = box_uint( (unsigned)larceny_C_ffi_convert_and_call);
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
  void *src  = (void*)(unbox_uint( w_addr ));

  memcpy( dest, src, n );
}


/* This is a syscall */
void larceny_poke_bytes( word w_addr, word w_bytevector, word w_count )
{
  int n = nativeint( w_count );
  void *src  = ptrof( w_bytevector ) + 1;
  void *dest = (void*)(unbox_uint( w_addr ));

  memcpy( dest, src, n );
}

/* eof */
