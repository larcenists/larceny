
/* Rts/Standard-C/arithmetic.c
 * Petit Larceny -- generic arithmetic millicode.
 *
 * $Id: arithmetic.mac 2543 2005-07-20 21:54:03Z pnkfelix $
 *
 * WARNING: This code has been auto-generated.  Edit 
 * Rts/Standard-C/arithmetic.mac instead and rerun the expander by 
 * loading expand.sch and then evaluating
 *   (expand-file "arithmetic.mac" "arithmetic.c")
 */

#define NOGLOBALS

#include "larceny.h"			/* includes config.h also */
#include "millicode.h"
#include "macros.h"

#include <math.h>
#if defined( SUNOS4 ) || defined( SUNOS5 )
# include <sys/ieeefp.h>
#endif
#if defined( SUNOS4 )
# include <floatingpoint.h>
#endif

#define header_byte( obj, tag )  (*ptrof(obj)&255)  /* can do better */

#define setcc( c )  globals[ G_RESULT ] = ((c) ? TRUE_CONST : FALSE_CONST)

/* FIXME: these macros are not valid in a 64-bit setting! */
/* FIXME: these should be in macros.h, probably */
#if defined(BITS_64)
#  error "Must fix some macros in arithmetic.mac for 64-bit compilation."
#endif

#define compnum_real( x )  (*(double*)((word)x-BVEC_TAG+2*sizeof(word)))
#define compnum_imag( x )  (*(double*)((word)x-BVEC_TAG+4*sizeof(word)))
#define flonum_val( x )    (*(double*)((word)x-BVEC_TAG+2*sizeof(word)))

extern double rint( double x );
extern double aint( double x );

#define round_to_even( x )    rint( x )
#define round_any( x )        rint( x )
#define round_to_zero( x )    aint( x )

#define is_integer( f )       (f == round_any( f ))

static void generic_binary_operation( word *globals, int op, cont_t k );
static void generic_unary_operation( word *globals, int op, cont_t k );
static void ga_box_int( word *globals, word w );
static void ga_box_longint( word *globals, word hi, word lo );
static void ga_box_word( word *globals, word x, int sign );
static void ga_box_flonum( word *globals, double d );
static void ga_box_compnum( word *globals, double r, double i );

void initialize_generic_arithmetic( void )
{
}

#ifndef G_SECOND
# define G_SECOND G_ARGREG2
# define G_THIRD  G_ARGREG3
#endif

void EXPORT mc_add( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	ga_box_flonum(globals, flonum_val(x)+flonum_val(y));
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	ga_box_compnum(globals, compnum_real(x)+compnum_real(y), 
                      compnum_imag(x)+compnum_imag(y));
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	ga_box_flonum(globals, flonum_val(x)+flonum_val(y));
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_ADD, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    ga_box_int(globals, (int)nativeint(x)+(int)nativeint(y));
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_ADD, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        generic_binary_operation( globals, MS_RECTNUM_ADD, k );
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_add: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_ADD );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_add: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_CONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_ADD, k );
}

void EXPORT mc_sub( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	ga_box_flonum(globals, flonum_val(x)-flonum_val(y));
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	ga_box_compnum(globals, compnum_real(x)-compnum_real(y),
                      compnum_imag(x)-compnum_imag(y));
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	ga_box_flonum(globals, flonum_val(x)-flonum_val(y));
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_SUB, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    ga_box_int(globals, (int)nativeint(x)-(int)nativeint(y));
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_SUB, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        generic_binary_operation( globals, MS_RECTNUM_SUB, k );
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_sub: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_SUB );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_sub: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_CONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_SUB, k );
}

void EXPORT mc_mul( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	ga_box_flonum( globals, flonum_val(x)*flonum_val(y));
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	double xr = compnum_real( x );
double xi = compnum_imag( x );
double yr = compnum_real( y );
double yi = compnum_imag( y );
ga_box_compnum( globals, (xr * yr)-(xi * yi), (xi * yr)+(xr * yi) );
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	ga_box_flonum( globals, flonum_val(x)*flonum_val(y));
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_MUL, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    /* DEC C compiler may have problems with the #if not at SOL? */
#if defined( __GNUC__ ) && 0
  long long int res = (long long)nativeint( x ) * (long long)nativeint( y );
  ga_box_longint( globals, (int)(res >> 32), (int)res );
#else
  word res_hi, res_lo;
  mul_32x32_to_64( (word)nativeint(x), (word)nativeint(y), &res_hi, &res_lo );
  ga_box_longint( globals, res_hi, res_lo );
#endif
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_MUL, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        generic_binary_operation( globals, MS_RECTNUM_MUL, k );
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_mul: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_MUL );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_mul: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_CONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_MUL, k );
}

void EXPORT mc_div( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	ga_box_flonum( globals, flonum_val(x) / flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	double xr = compnum_real( x );
double xi = compnum_imag( x );
double yr = compnum_real( y );
double yi = compnum_imag( y );
if (fabs( yr ) >= fabs( yi )) {
  double r = yi / yr;
  double den = yr + r * yi;
  ga_box_compnum( globals, (xr + r*xi) / den, (xi - r*xr) / den );
}
else {
  double r = yr / xi;
  double den = yi + r * yr;
  ga_box_compnum( globals, (xi + r*xr) / den, (xr - r*xi) / den );
}
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	ga_box_flonum( globals, flonum_val(x) / flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_DIV, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    #if defined(EXPLICIT_DIVZ_CHECK)
if (y == 0) mc_exception( globals, EX_DIV );
#endif
if ((int)x % (int)y == 0)
  globals[ G_RESULT ] = fixnum( (int)x / (int)y );
else
  generic_binary_operation( globals, MS_FIXNUM2RATNUM_DIV, k );
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_DIV, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        generic_binary_operation( globals, MS_RECTNUM_DIV, k );
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_div: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_DIV );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_div: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_CONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_DIV, k );
}

#if BVEC_HEADER_WORDS != 1
  /* This changes the bignum layout */
# error "Still a few bugs in the system"
#endif

/* These operate on the bignum metaword (sign/length) */

#define bigmeta_length( x ) (x & 0xFFFF)
#define bigmeta_sign( x ) ((x >> 16) & 1)

void EXPORT mc_quo( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];

  if (is_both_fixnums( x, y )) {
#if defined(EXPLICIT_DIVZ_CHECK)
    if (y == 0) mc_exception( globals, EX_DIV );
#endif
    globals[ G_RESULT ] = fixnum((s_word)x / (s_word)y);
    return;
  }
  else if (tagof( x ) == BVEC_TAG && 
           header_byte(x, BVEC_TAG) == BIGNUM_HDR &&
           is_nonnegative_fixnum( y )) {
    word z = *(ptrof( x )+1);
    if (bigmeta_length(z) == 1 && bigmeta_sign(z) == 0) {
#if defined(EXPLICIT_DIVZ_CHECK)
      if (y == 0) mc_exception( globals, EX_DIV );
#endif
      z = *(ptrof( x )+2) / (y >> 2);
      ga_box_longint( globals, 0, z );
      return;
    }
  }
  generic_binary_operation( globals, MS_HEAVY_QUOTIENT, k );
}

void EXPORT mc_rem( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];

  if (is_both_fixnums( x, y )) {
#if defined(EXPLICIT_DIVZ_CHECK)
    if (y == 0) mc_exception( globals, EX_DIV );
#endif
    globals[ G_RESULT ] = fixnum((s_word)nativeint(x) % (s_word)nativeint(y));
    return;
  }
  else if (tagof( x ) == BVEC_TAG && 
           header_byte(x, BVEC_TAG) == BIGNUM_HDR &&
           is_nonnegative_fixnum( y )) {
    word z = *(ptrof( x )+1);
    if (bigmeta_length(z) == 1 && bigmeta_sign(z) == 0) {
#if defined(EXPLICIT_DIVZ_CHECK)
      if (y == 0) mc_exception( globals, EX_DIV );
#endif
      z = *(ptrof( x )+2) % (y >> 2);
      ga_box_longint( globals, 0, z );
      return;
    }
  }
  generic_binary_operation( globals, MS_HEAVY_REMAINDER, k );
}

void EXPORT mc_neg( word *globals , cont_t k )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_SECOND ] = globals[ G_RESULT ];
  globals[ G_RESULT ] = fixnum( 0 );
  mc_sub( globals, k );
  
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      ga_box_flonum( globals, -flonum_val( x ) );
    }
    else if (h == COMPNUM_HDR) {
      ga_box_compnum( globals, -compnum_real(x), -compnum_imag(x) );
    } 
    else if (h == BIGNUM_HDR) {
      generic_unary_operation( globals, MS_BIGNUM_NEGATE, k );
    }
    else if (h == RATNUM_HDR) {
      generic_unary_operation( globals, MS_RATNUM_NEGATE, k );
    } 
    else if (h == RECTNUM_HDR) {
      generic_unary_operation( globals, MS_RECTNUM_NEGATE, k );
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_NEG, k );
}

void EXPORT mc_abs( word *globals , cont_t k )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    /*nothing*/
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      ga_box_flonum( globals, fabs( flonum_val( x ) ) );
    }
    else if (h == COMPNUM_HDR) {
      mc_cont_exception( globals, EX_ABS, k );
    } 
    else if (h == BIGNUM_HDR) {
      generic_unary_operation( globals, MS_BIGNUM_ABS, k );
    }
    else if (h == RATNUM_HDR) {
      mc_cont_exception( globals, EX_ABS, k );
    } 
    else if (h == RECTNUM_HDR) {
      mc_cont_exception( globals, EX_ABS, k );
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_ABS, k );
}

void EXPORT mc_zerop( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    setcc( x == fixnum(0) );
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      setcc( flonum_val( x ) == 0.0 );
    }
    else if (h == COMPNUM_HDR) {
      setcc( compnum_real(x) == 0.0 && compnum_imag(x) == 0.0 );
    } 
    else if (h == BIGNUM_HDR) {
      word hdr = *ptrof( x );
  setcc( (hdr & 0xFFFF) == 0 );  /* 0 digits */  /* big-endian specific?? */
 
    }
    else if (h == RATNUM_HDR) {
      setcc( vector_ref( x, 0 ) == fixnum(0) );
    } 
    else if (h == RECTNUM_HDR) {
      setcc( vector_ref( x, 0 ) == fixnum(0) && vector_ref( x, 1 ) == fixnum(0) );
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_ZEROP, 0 );
}

void EXPORT mc_equalp( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	setcc( flonum_val(x) == flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	setcc( compnum_real(x)==compnum_real(y) && compnum_imag(x)==compnum_imag(y) );
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	setcc( flonum_val(x) == flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_EQUAL, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    setcc( (int)nativeint(x) == (int)nativeint(y) );
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_EQUAL, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        generic_binary_operation( globals, MS_RECTNUM_EQUAL, k );
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_equalp: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_EQUAL );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_equalp: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_ECONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_EQUALP, k );
}

void EXPORT mc_lessp( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	setcc( flonum_val(x) < flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	goto error;
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	setcc( flonum_val(x) < flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_LESS, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    setcc( (int)nativeint(x) < (int)nativeint(y) );
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_LESS, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        goto error;
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_lessp: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_LESS );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_lessp: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_PCONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_LESSP, k );
}

void EXPORT mc_less_or_equalp( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	setcc( flonum_val(x) <= flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	goto error;
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	setcc( flonum_val(x) <= flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_LESSEQ, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    setcc( (int)nativeint(x) <= (int)nativeint(y) );
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_LESSEQ, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        goto error;
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_less_or_equalp: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_LESSEQ );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_less_or_equalp: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_PCONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_LESSEQP, k );
}

void EXPORT mc_greaterp( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	setcc( flonum_val(x) > flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	goto error;
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	setcc( flonum_val(x) > flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_GREATER, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    setcc( (int)nativeint(x) > (int)nativeint(y) );
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_GREATER, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        goto error;
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_greaterp: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_GREATER );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_greaterp: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_PCONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_GREATERP, k );
}

void EXPORT mc_greater_or_equalp( word *globals, cont_t k )
{
  word x = globals[ G_RESULT ];
  word y = globals[ G_SECOND ];
  word retry;
  int t1, t2, h1, h2;

  t1 = tagof( x );
  t2 = tagof( y );
  if (t1 == BVEC_TAG) {
    if (t2 != BVEC_TAG) goto mixed;
    h1 = header_byte( x, BVEC_TAG );
    h2 = header_byte( y, BVEC_TAG );
    if (h1 == FLONUM_HDR) {
      if (h2 == FLONUM_HDR || h2 == COMPNUM_HDR && compnum_imag( y ) == 0.0) {
	setcc( flonum_val(x) >= flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == COMPNUM_HDR) {
      if (h2 == COMPNUM_HDR) {
	goto error;
      }
      else if (h2 == FLONUM_HDR && compnum_imag( x ) == 0.0) {
	setcc( flonum_val(x) >= flonum_val(y) );
      }
      else
	goto mixed;
    }
    else if (h1 == BIGNUM_HDR) {
      if (h2 == BIGNUM_HDR)
	generic_binary_operation( globals, MS_BIGNUM_GREATEREQ, k );
      else
	goto mixed;
    }
    else
      goto error;
  }
  else if (is_both_fixnums( t1, t2 )) {
    setcc( (int)nativeint(x) >= (int)nativeint(y) );
  }
  else if (t1 == VEC_TAG) {
    if (t2 != VEC_TAG) goto mixed;
    h1 = header_byte( x, VEC_TAG );
    h2 = header_byte( y, VEC_TAG );
    if (h1 == RATNUM_HDR) {
      if (h2 == RATNUM_HDR) 
	generic_binary_operation( globals, MS_RATNUM_GREATEREQ, k );
      else
	goto mixed;
    }
    else if (h1 == RECTNUM_HDR) {
      if (h2 == RECTNUM_HDR) {
        goto error;
      }
      else
	goto mixed;
    }
    else
      goto error;
  }
  else
    goto mixed;
  return;

 mixed:
  /* Call the contagion procedure.  THIRD has the retry procedure,
     which must be fetched from the callout vector.
     */
  retry = global_cell_ref( globals[ G_CALLOUTS ] );
#ifndef NDEBUG
  if (retry == UNDEFINED_CONST)
    panic_exit( "mc_greater_or_equalp: No contagion procedure defined." );
#endif
  globals[ G_THIRD ] = vector_ref( retry, MS_GENERIC_GREATEREQ );
#ifndef NDEBUG
  if (tagof( globals[ G_THIRD ] ) != PROC_TAG)
    panic_exit( "mc_greater_or_equalp: Contagion procedure is not a procedure." );
#endif
  mc_scheme_callout( globals, MS_PCONTAGION, 3, k, 0 );
  return;

 error:
  mc_cont_exception( globals, EX_GREATEREQP, k );
}

void EXPORT mc_complexp( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_RESULT ] = TRUE_CONST;
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == COMPNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    } 
    else if (h == BIGNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == RATNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else {
      globals[ G_RESULT ] = FALSE_CONST;
    }
  }
  else {
    globals[ G_RESULT ] = FALSE_CONST;
  }
  return;

 error:
  mc_cont_exception( globals, 0/* none */, 0 );
}

void EXPORT mc_rationalp( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_RESULT ] = TRUE_CONST;
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == COMPNUM_HDR) {
      if (compnum_imag( x ) == 0.0) globals[ G_RESULT ] = TRUE_CONST; else globals[ G_RESULT ] = FALSE_CONST;
    } 
    else if (h == BIGNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == RATNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    }
    else {
      globals[ G_RESULT ] = FALSE_CONST;
    }
  }
  else {
    globals[ G_RESULT ] = FALSE_CONST;
  }
  return;

 error:
  mc_cont_exception( globals, 0/* none */, 0 );
}

void EXPORT mc_integerp( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_RESULT ] = TRUE_CONST;
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      double v = flonum_val( x );
setcc( is_integer( v ) );
    }
    else if (h == COMPNUM_HDR) {
      double v = flonum_val( x );
setcc( compnum_imag(x) == 0.0 && is_integer( v ) );
    } 
    else if (h == BIGNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == RATNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    }
    else {
      globals[ G_RESULT ] = FALSE_CONST;
    }
  }
  else {
    globals[ G_RESULT ] = FALSE_CONST;
  }
  return;

 error:
  mc_cont_exception( globals, 0/* none */, 0 );
}

void EXPORT mc_exactp( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_RESULT ] = TRUE_CONST;
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    }
    else if (h == COMPNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    } 
    else if (h == BIGNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == RATNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_EXACTP, 0 );
}

void EXPORT mc_inexactp( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_RESULT ] = FALSE_CONST;
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    }
    else if (h == COMPNUM_HDR) {
      globals[ G_RESULT ] = TRUE_CONST;
    } 
    else if (h == BIGNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    }
    else if (h == RATNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = FALSE_CONST;
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_INEXACTP, 0 );
}

void EXPORT mc_exact2inexact( word *globals , cont_t k )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    ga_box_flonum( globals, (double)(int)nativeint( x ) );
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      /*nothing*/
    }
    else if (h == COMPNUM_HDR) {
      /*nothing*/
    } 
    else if (h == BIGNUM_HDR) {
      generic_unary_operation( globals, MS_GENERIC_EXACT2INEXACT, k );
    }
    else if (h == RATNUM_HDR) {
      generic_unary_operation( globals, MS_GENERIC_EXACT2INEXACT, k );
    } 
    else if (h == RECTNUM_HDR) {
      generic_unary_operation( globals, MS_GENERIC_EXACT2INEXACT, k );
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_E2I, k );
}

void EXPORT mc_inexact2exact( word *globals , cont_t k )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    /*nothing*/
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      generic_unary_operation( globals, MS_GENERIC_INEXACT2EXACT, k );
    }
    else if (h == COMPNUM_HDR) {
      generic_unary_operation( globals, MS_GENERIC_INEXACT2EXACT, k );
    } 
    else if (h == BIGNUM_HDR) {
      /*nothing*/
    }
    else if (h == RATNUM_HDR) {
      /*nothing*/
    } 
    else if (h == RECTNUM_HDR) {
      /*nothing*/
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_I2E, k );
}

void EXPORT mc_real_part( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    /*nothing*/
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      /*nothing*/
    }
    else if (h == COMPNUM_HDR) {
      ga_box_flonum( globals, compnum_real( x ) );
    } 
    else if (h == BIGNUM_HDR) {
      /*nothing*/
    }
    else if (h == RATNUM_HDR) {
      /*nothing*/
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = vector_ref( x, 0 );
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_REALPART, 0 );
}

void EXPORT mc_imag_part( word *globals  )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    globals[ G_RESULT ] = fixnum(0);
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      ga_box_flonum( globals, 0.0 );
    }
    else if (h == COMPNUM_HDR) {
      ga_box_flonum( globals, compnum_imag( x ) );
    } 
    else if (h == BIGNUM_HDR) {
      globals[ G_RESULT ] = fixnum(0);
    }
    else if (h == RATNUM_HDR) {
      globals[ G_RESULT ] = fixnum(0);
    } 
    else if (h == RECTNUM_HDR) {
      globals[ G_RESULT ] = vector_ref( x, 1 );
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_IMAGPART, 0 );
}

void EXPORT mc_round( word *globals , cont_t k )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    /*nothing*/
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      double v = flonum_val( x ); ga_box_flonum( globals, round_to_even( v ) );
    }
    else if (h == COMPNUM_HDR) {
      if (compnum_imag( x ) == 0.0) {
  double v = flonum_val( x ); 
  ga_box_flonum( globals, round_to_even( v ) );
}
else
  goto error;
    } 
    else if (h == BIGNUM_HDR) {
      /*nothing*/
    }
    else if (h == RATNUM_HDR) {
      generic_unary_operation( globals, MS_RATNUM_ROUND, k );
    } 
    else if (h == RECTNUM_HDR) {
      goto error;
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_ROUND, k );
}

void EXPORT mc_truncate( word *globals , cont_t k )
{
  word x = globals[ G_RESULT ];
  if (is_fixnum(x)) {
    /*nothing*/
  }
  else if (isptr( x )) {
    unsigned h = *ptrof( x ) & 255;
    if (h == FLONUM_HDR) {
      double v = flonum_val( x ); ga_box_flonum( globals, round_to_zero( v ) );
    }
    else if (h == COMPNUM_HDR) {
      if (compnum_imag( x ) == 0.0) {
  double v = flonum_val( x ); 
  ga_box_flonum( globals, round_to_zero( v ) );
}
else
  goto error;
    } 
    else if (h == BIGNUM_HDR) {
      /*nothing*/
    }
    else if (h == RATNUM_HDR) {
      generic_unary_operation( globals, MS_RATNUM_TRUNCATE, k );
    } 
    else if (h == RECTNUM_HDR) {
      goto error;
    }
    else {
      goto error;
    }
  }
  else {
    goto error;
  }
  return;

 error:
  mc_cont_exception( globals, EX_TRUNC, k );
}

static void generic_binary_operation( word *globals, int op, cont_t k )
{
  mc_scheme_callout( globals, op, 2, k, 0 );
}

static void generic_unary_operation( word *globals, int op, cont_t k )
{
  mc_scheme_callout( globals, op, 1, k, 0 );
}

#if !BITS_32
  /* The following procedures are 32-bit specific */
# error "Still a few bugs in the system."
#endif

/* Call this to process a one-word two's complement integer result.
   Stores a fixnum or a one-word bignum in RESULT.
   */
static void ga_box_int( word *globals, word x )
{
  if ((x & 0xe0000000) == 0 || (x & 0xe0000000) == 0xe0000000) 
    globals[ G_RESULT ] = fixnum( x );
  else if ((int)x < 0)
    ga_box_word( globals, -(int)x, 1 );
  else
    ga_box_word( globals, x, 0 );
}

/* Call this to process a two-word two's complement integer result.
   Stores a fixnum, a one-word bignum, or a two-word bignum in RESULT.
   */
static void ga_box_longint( word *globals, word hi, word lo )
{
  if (hi == 0) { 
    if ((lo & 0xe0000000) == 0) 
      globals[ G_RESULT ] = fixnum( lo );
    else
      ga_box_word( globals, lo, 0 );
  }
  else if (hi == ~0) {
    if ((lo & 0xe0000000) == 0xe0000000)
      globals[ G_RESULT ] = fixnum( lo );
    else
      ga_box_word( globals, -(int)lo, 1 );
  }
  else {
    int sign = 0;
    if ((int)hi < 0) {
      sign = 1;
      lo = ~lo + 1; hi = ~hi;
      if (lo == 0) hi += 1;
    }   
    if (hi == 0)
      ga_box_int( globals, (int)lo );
    else {
      word *p;

      globals[ G_RESULT ] = fixnum(4*sizeof(word));
      mc_alloc_bv( globals );
      p = (word*)globals[ G_RESULT ];
      p[0] = mkheader( 3*sizeof(word), BIGNUM_HDR );
      p[1] = (sign << 16) | 2;   /* sign | 2 words */
      p[2] = lo;
      p[3] = hi;
      globals[ G_RESULT ] = tagptr( p, BVEC_TAG );
    }
  }
}

/* Box an sign/magnitude integer value in a one-word bignum */
static void ga_box_word( word *globals, word x, int sign )
{
  word *p;

  globals[ G_RESULT ] = fixnum(3*sizeof(word));
  mc_alloc_bv( globals );
  p = (word*)globals[ G_RESULT ];
  p[0] = mkheader( 2*sizeof(word), BIGNUM_HDR );
  p[1] = (sign << 16) | 1;   /* sign | 1 word */
  p[2] = x;
  globals[ G_RESULT ] = tagptr( p, BVEC_TAG );
}

static void ga_box_flonum( word *globals, double d )
{
  word *p;

  globals[ G_RESULT ] = fixnum(4*sizeof(word));
  mc_alloc_bv( globals );
  p = (word*)globals[ G_RESULT ];
  *p = mkheader( 3*sizeof(word), FLONUM_HDR );
  *(double*)(p+2) = d;
  globals[ G_RESULT ] = tagptr( p, BVEC_TAG );
}

static void ga_box_compnum( word *globals, double r, double i )
{
  word *p;

  globals[ G_RESULT ] = fixnum(6*sizeof(word));
  mc_alloc_bv( globals );
  p = (word*)globals[ G_RESULT ];
  *p = mkheader( 5*sizeof(word), COMPNUM_HDR );
  *(double*)(p+2) = r;
  *(double*)(p+4) = i;
  globals[ G_RESULT ] = tagptr( p, BVEC_TAG );
}

