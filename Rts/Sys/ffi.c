/* Rts/Sparc/ffi.c
 * Larceny -- Foreign-function interface
 *
 * $Id$
 */

#include "larceny.h"

#if defined(SUNOS4) || defined(SUNOS5)
#include <dlfcn.h>
#endif
#include <assert.h>

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
 *         4   pointer          bytevector-like; vector-like; pair
 *
 * The values for the return_descriptor are the following:
 *     value   type             C return type assumed
 *     -----   ---------------  ---------------------
 *         0   int              int
 *         1   unsigned         unsigned int
 *         2   double           double
 *         3   float            float
 *         4   void             void
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
} ffi_arg;


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

  ffi_arg args[ 32 ];
  int i, limit, argc;
  word w_result, w;
  double d_result;
  float f_result;

  assert( sizeof( ffi_arg ) == 8 );

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
  case 4 :  /* void */
    ((tramp_void_t)(ptrof(trampoline_bytevector)+1))( args );
    globals[ G_RESULT ] = UNSPECIFIED_CONST;
    return;
  default :
    hardconsolemsg( "FFICALL failed: bad return descriptor %d", 
		    return_descriptor );
    goto failed;
  }
  /*NOTREACHED*/
  panic( "Fell off the end in larceny_C_ffi_apply." );

 failed:
  globals[ G_RESULT ] = UNDEFINED_CONST;
}


/* This is a syscall.
 * w_path is a pointer to a bytevector containing a null-terminated string.
 *
 * See dlopen(3X) for details.
 */
void
larceny_C_ffi_dlopen( word w_path )
{
#if defined(SUNOS4)
  char *path;
  void *desc;

  path = (char*)(ptrof(w_path)+1);
  desc = dlopen( path, 1 );
  if (desc == 0) 
    hardconsolemsg( "dlopen error: %s", dlerror() );
  globals[ G_RESULT ] = box_uint( (unsigned)desc );
#elif defined(SUNOS5)
  char *path;
  void *desc;

  path = (char*)(ptrof(w_path)+1);
  /* One can debate whether this mode is the right one.
     Perhaps the mode should be a parameter to this function.
     */
  desc = dlopen( path, RTLD_LAZY | RTLD_LOCAL );
  if (desc == 0) 
    hardconsolemsg( "dlopen error: %s", dlerror() );
  globals[ G_RESULT ] = box_uint( (unsigned)desc );
#endif
}


/* This is a syscall.
 * w_handle is a 32-bit unsigned integer (bignum or fixnum) representing
 *  a void* returned from a previous call to larceny_C_ffi_dlopen.
 * w_sym is a pointer to a bytevector containing a null-terminated string.
 *
 * See dlsym(3X) for details.
 */
void
larceny_C_ffi_dlsym( word w_handle, word w_sym )
{
#if defined(SUNOS4) || defined(SUNOS5)
  char *sym;
  void *handle;
  void *r;

  sym = (char*)(ptrof(w_sym)+1);
  handle = (void*)unbox_uint( w_handle );
  r = dlsym( handle, sym );
  globals[ G_RESULT ] = box_uint( (unsigned)r );
#endif
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
  word argv[32], *x, scheme_result, *ptr, u_val, *q;
  s_word s_val;
  int bytes, i;
  byte *argp;

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
    }
  }

  /* Allocate. */
  if (bytes > 0)
    ptr = alloc_from_heap( bytes );

  /* Convert. */
  for ( argp=(byte*)(ptrof(adesc)+1), i=0 ; i < argc ; i++ ) {
    x = args[i];
    switch (argp[i]) {
    case 0 : /* signed32 */
      s_val = (s_word)*x;
      if (s_val < MOST_NEGATIVE_FIXNUM || s_val >= MOST_POSITIVE_FIXNUM){
	q = ptr;
	ptr += 16;
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
	q = ptr;
	ptr += 16;
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
      q = ptr;
      ptr += 16;
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


/* eof */
