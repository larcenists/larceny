/* Rts/Sys/util.c
 * Larceny run-time system -- miscellaneous procedures.
 *
 * $Id: util.c,v 1.1 1997/05/23 13:50:06 lth Exp $
 */

#include <memory.h>
#include "larceny.h"
#include "gc.h"
#include "gc_t.h"

#define HDR_BYTES    4    /* Belongs in layouts.cfg */

/* Given a tagged pointer to an object, make a copy of the object in the
 * heap of the given collector.  The source object does not need to be
 * in the heap already.
 */

word copy_object( gc_t *gc, word obj )
{
  word *p;
  unsigned size;
  int t;

  t = tagof( obj );
  if (t == PAIR_TAG)
    size = 2*sizeof( word );
  else 
    size = roundup_balign( sizefield( *ptrof( obj ) )+HDR_BYTES );
  p = gc_allocate( gc, size, 0, t == BVEC_TAG );
  memcpy( p, ptrof( obj ), size );
  return tagptr( p, tagof( obj ) );
}

word box_int( int n )
{
  if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
    return fixnum(n);
  else {
    word *p = alloc_bv_from_heap( 12 );
    *p = mkheader( 8, BIGNUM_HDR );
    if (n < 0) {
      n = -n;
      *(p+1) = mkbignum_header( 1, 1 );
    }
    else
      *(p+1) = mkbignum_header( 0, 1 );
    *(p+2) = n;
    return tagptr( p, BVEC_TAG );
  }
}

word box_uint( unsigned n )
{
  if (n <= MOST_POSITIVE_FIXNUM)
    return fixnum(n);
  else {
    word *p = alloc_bv_from_heap( 12 );
    *p = mkheader( 8, BIGNUM_HDR );
    *(p+1) = mkbignum_header( 0, 1 );
    *(p+2) = n;
    return tagptr( p, BVEC_TAG );
  }
}

word box_double( double d )
{
  word *p = alloc_bv_from_heap( 16 );
  *(double*)(p+2) = d;
  *p = mkheader( 12, FLONUM_HDR );
  return tagptr(p, BVEC_TAG);
}

unsigned unbox_uint( word w )
{
  if ((w & 3) == 0)
    return nativeuint(w);
  else if (tagof(w) == BVEC_TAG
	   && typetag(*ptrof( w )) == BIG_SUBTAG
	   && bignum_length( w ) == 1
	   && bignum_sign( w ) == 0) {
    return bignum_ref32( w, 0 );
  }
  else {
    hardconsolemsg( "Illegal value in unbox_uint; returning (unsigned)-1" );
    return (unsigned)-1;
  }
}

int unbox_int( word w )
{
  if ((w & 3) == 0)
    return nativeuint(w);
  else if (tagof(w) == BVEC_TAG
	   && typetag(*ptrof( w )) == BIG_SUBTAG
	   && bignum_length( w ) == 1) {
    if (bignum_sign( w ) == 0) 
      return (int)bignum_ref32( w, 0 );
    else
      return -(int)bignum_ref32( w, 0 );
  }
  else {
    hardconsolemsg( "Illegal value in unbox_int; returning -1" );
    return -1;
  }
}

/* eof */

