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

#define HDR_BYTES    sizeof(word)	/* Belongs in layouts.cfg */

/* Given a tagged pointer to an object, make a copy of the object in the
 * heap of the given collector.  The source object does not need to be
 * in the heap already.
 */

word copy_object( gc_t *gc, word obj )
{
  word *p;
  word size;
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

/* Box a sign/magnitude integer as a bignum with 32-bit digits. */
static word box_magnitude( int neg, unsigned long long m )
{
  int n = ((m >> 32) != 0 ? 2 : 1);
  int bytes = 4 + 4*n;                 /* metaword + digits */
  word *p = gc_allocate( the_gc(globals), 
                         roundup_balign( sizeof(word)+bytes ), 0, 1 );
  u32 *d = (u32*)(p+1);

  *p = mkheader( bytes, BIGNUM_HDR );
  d[0] = mkbignum_header( neg ? 1 : 0, n );
  d[1] = (u32)m;
  if (n == 2) d[2] = (u32)(m >> 32);
  return tagptr( p, BVEC_TAG );
}

word box_int( int n )
{
  return box_longlong( n );
}

word box_uint( unsigned n )
{
  return box_ulonglong( n );
}

word box_longlong( long long ll )
{
  if (ll >= MOST_NEGATIVE_FIXNUM && ll <= MOST_POSITIVE_FIXNUM)
    return fixnum( (s_word)ll );
  else if (ll < 0)
    return box_magnitude( 1, -(unsigned long long)ll );
  else
    return box_magnitude( 0, (unsigned long long)ll );
}

word box_ulonglong( unsigned long long ull )
{
  if (ull <= (unsigned long long)MOST_POSITIVE_FIXNUM)
    return fixnum( (s_word)ull );
  else
    return box_magnitude( 0, ull );
}

/* A machine word (e.g. an address), treated as unsigned. */
word box_word( word w )
{
  return box_ulonglong( w );
}

word box_double( double d )
{
  word *p = gc_allocate( the_gc(globals), 
                         roundup_balign( sizeof(word)+FLONUM_DATA_BYTES ),
                         0, 1 );
  *(double*)(p+1+FLONUM_PAD_WORDS) = d;
  *p = mkheader( FLONUM_DATA_BYTES, FLONUM_HDR );
  return tagptr(p, BVEC_TAG);
}

/* Returns TRUE and the magnitude and sign if w is a fixnum or a bignum
   of at most 64 bits.
   */
static bool unbox_magnitude( word w, unsigned long long *m, int *neg )
{
  if ((w & 3) == 0) {
    s_word n = (s_word)nativeint( w );
    *neg = (n < 0);
    *m = (n < 0 ? -(unsigned long long)n : (unsigned long long)n);
    return TRUE;
  }
  else if (tagof(w) == BVEC_TAG
	   && typetag(*ptrof( w )) == BIG_SUBTAG
	   && bignum_length( w ) <= 2) {
    *neg = bignum_sign( w );
    *m = 0;
    if (bignum_length( w ) >= 1) *m = bignum_ref32( w, 0 );
    if (bignum_length( w ) == 2) 
      *m |= (unsigned long long)bignum_ref32( w, 1 ) << 32;
    return TRUE;
  }
  else
    return FALSE;
}

unsigned long long unbox_ulonglong( word w )
{
  unsigned long long m;
  int neg;

  if (unbox_magnitude( w, &m, &neg ) && !neg)
    return m;
  hardconsolemsg( "Illegal value in unbox_ulonglong; returning -1" );
  return (unsigned long long)-1;
}

long long unbox_longlong( word w )
{
  unsigned long long m;
  int neg;

  if (unbox_magnitude( w, &m, &neg ))
    return (neg ? -(long long)m : (long long)m);
  hardconsolemsg( "Illegal value in unbox_longlong; returning -1" );
  return -1;
}

word unbox_word( word w )
{
  return (word)unbox_ulonglong( w );
}

unsigned unbox_uint( word w )
{
  unsigned long long m;
  int neg;

  if (unbox_magnitude( w, &m, &neg ) && !neg && m <= 0xFFFFFFFFULL)
    return (unsigned)m;
  hardconsolemsg( "Illegal value in unbox_uint; returning (unsigned)-1" );
  return (unsigned)-1;
}

int unbox_int( word w )
{
  unsigned long long m;
  int neg;

  if (unbox_magnitude( w, &m, &neg ) && m <= 0x80000000ULL)
    return (neg ? -(int)m : (int)m);
  hardconsolemsg( "Illegal value in unbox_int: 0x%08" PRIxWORD "; returning -1", w );
  return -1;
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

/* For use when computing the maximum or minimum of two values that are
 * really unsigned values.  (Typically used with peak memory sizes.)
 */

word umax( word x, word y ) {
  return (x > y ? x : y);
}

word umin( word x, word y ) {
  return (x < y ? x : y);
}

/* eof */
