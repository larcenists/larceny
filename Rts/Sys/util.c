/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- miscellaneous procedures.
 */

#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "larceny.h"		/* Includes config.h */
#include "gc.h"
#include "gc_t.h"

#define HDR_BYTES    4		/* Belongs in layouts.cfg */

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

/* To be called interactively from GDB.
 * When will the GNU project adopt a programmable debugger!?
 */
word *search_memory( word *from, word *limit, word datum )
{
  while (from < limit && *from != datum)
    from++;
  return (from < limit ? from : 0);
}

word box_int( int n )
{
  if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
    return fixnum(n);
  else {
    word *p = gc_allocate( the_gc(globals), 12, 0, 1 );
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
    word *p = gc_allocate( the_gc(globals), 12, 0, 1 );
    *p = mkheader( 8, BIGNUM_HDR );
    *(p+1) = mkbignum_header( 0, 1 );
    *(p+2) = n;
    return tagptr( p, BVEC_TAG );
  }
}

word box_double( double d )
{
  word *p = gc_allocate( the_gc(globals), 16, 0, 1 );
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

#if !defined(HAVE_RINT)
/* RINT: round double to current rounding mode.  A BSDism.
   This version always rounds to even, because that's what Larceny requires;
   the default IEEE rounding mode is round-to-even.
   */
double rint( double f )
{
  double frac, ip;

  frac = modf( fabs(f), &ip );
  if (frac == 0)
    return f;
  else if (frac > 0.5) {
    modf( f, &ip );
    if (f > 0.0)
      return ip+1.0;
    else
      return ip-1.0;
  }
  else if (frac < 0.5) {
    modf( f, &ip );
    return ip;
  }
  else if (fmod( ip, 2.0 ) == 0) { /* Even is down */
    modf( f, &ip );
    return ip;
  }
  else {			/* Even is up */
    modf( f, &ip );
    if (f > 0.0)
      return ip+1.0;
    else
      return ip-1.0;
  }
}
#endif /* !defined(HAVE_RINT) */

#if !defined(HAVE_AINT)
/* AINT: round double toward zero.  A BSDism. */
double aint( double x )
{
  return x < 0.0 ? ceil( x ) : floor( x );
}
#endif /* !defined(HAVE_AINT) */

#if !defined(HAVE_STRNCASECMP)
/* STRNCASECMP: compare string prefixes case-insensitively.  A BSDism.  */
int strncasecmp( const char *a, const char *b, size_t n )
{
  int i = 0;

  while (i < n && tolower(*a) == tolower(*b) && *a != 0) {
    i++;
    a++;
    b++;
  }
  if (i == n)
    return 0;
  else
    return tolower(*a) - tolower(*b);
}
#endif /* !defined(HAVE_STRNCASECMP) */

#if !defined(HAVE_STRDUP)
/* STRDUP: duplicate string on malloc'ed heap.  BSDism, SVIDism. */
char *strdup( const char *s )
{
  char *t = (char*)malloc( strlen(s)+1 );
  if (t != 0) 
    strcpy( t, s );
  return t;
}
#endif /* !defined(HAVE_STRDUP) */

#if !defined(HAVE_HRTIME_T)
/* The resolution of gethrtime() is nanoseconds.
   Constraint: If x = gethrtime(); y = gethrtime() then x <= y _always_.
   */
hrtime_t gethrtime( void )
{
  return 0;			/* Hey, it's portable! */
}
#endif

/* eof */
