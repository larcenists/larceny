/*
 * This is the file Sys/gc.c.
 *
 * $Id: gc.c,v 1.3 1995/08/01 04:37:33 lth Exp lth $
 *
 * Larceny run-time system -- the garbage collector.
 *
 * History:
 *  December 12, 1994 / lth (v0.23)
 *    Instruction cache flush logic added.
 *
 *  June 27 - July 14, 1994 / lth (v0.20)
 *    Massively rewritten.
 *
 *  Sometime in the spring of 1991 / lth
 *    Initial version.
 *
 *
 * There are two procedures, minor_collection() and major_collection(),
 * which both take three parameters: oldlo, oldhi, and newlo. All three
 * are pointers to words.
 *
 * - Oldlo and oldhi delimit the collectable area (BOT and LIM).
 * - Newlo points to the first word in the destination space (TOP).
 *
 * Both procedures return the new value of the destination free pointer.
 *
 * Essentially, the garbage collector knows nothing about the heap,
 * about globals, or anything else. It makes one assumption: that when
 * it is given oldlo and oldhi, every live object between the two
 * pointers is collectable. Hence, the storage allocator should not
 * put non-collectable objects among the collectable ones. 
 *
 * Also, a root between oldlo and oldhi should be forwarded only once if
 * newlo is between them; this is not really a problem.
 *
 * A forwarding pointer is a two-word structure where the first word is
 * 0xFFFFFFFE (an otherwise illegal bit pattern) and the second is the
 * tagged pointer into newspace.
 */

#include <stdio.h>    /*DEBUG*/
#include <memory.h>   /* for memcpy() */
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

/* static void forw( word *p, word *oldlo, word *oldhi, word *dest ) */
/* assumes that all params are simple names! */
#define FORWARD_PTR 0xFFFEFFFE
#define forw( p, oldlo, oldhi, dest ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 ) && (word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
           word *TMP_P = ptrof( TMP2 ); \
           if (*TMP_P == FORWARD_PTR) \
	     *p = *(TMP_P+1); \
	   else if (tagof( TMP2 ) == PAIR_TAG) { \
             *dest = *TMP_P; \
	     *(dest+1) = *(TMP_P+1); \
             *TMP_P = FORWARD_PTR; \
	     *(TMP_P+1) = *p = (word)tagptr(dest, PAIR_TAG); \
             dest += 2; \
	   } \
	   else { \
	     word *TMPD = dest; \
	     *p = forward( TMP2, &TMPD ); dest = TMPD; \
	   } \
       } \
  } while( 0 )

static void remset_scanner();
static void root_scanner();
static void scan();
static word forward();

/* Some private globals to emulate closures */
static word *g_oldlo, *g_oldhi, **g_dest;

word *minor_collection( oldlo, oldhi, newlo )
word *oldlo, *oldhi, *newlo;
{
  word *dest = newlo;

#ifdef DEBUG
  consolemsg( "[debug] Minor collection %08lx %08lx %08lx.",
	     oldlo, oldhi, newlo );
#endif
  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;

  enumerate_roots( root_scanner );
  enumerate_remset( remset_scanner );
  scan( newlo, oldlo, oldhi, &dest );

  return dest;
}

word *major_collection( oldlo, oldhi, newlo )
word *oldlo, *oldhi, *newlo;
{
  word *dest = newlo;

#ifdef DEBUG
  consolemsg( "[debug] Major collection %08lx %08lx %08lx.",
	     oldlo, oldhi, newlo );
#else
  /* Way annoying in stop+copy collector */
  if (size_tspace() > 0) consolemsg( "; Major collection." );
#endif
  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;

  enumerate_roots( root_scanner );
  scan( newlo, oldlo, oldhi, &dest );

  return dest;
}

/* This procedure receives a pointer to a tagged pointer and may forward 
 * the tagged pointer.
 */
static void root_scanner( ptr )
word *ptr;
{
  word *oldlo, *oldhi, *dest;

  oldlo = g_oldlo;
  oldhi = g_oldhi;
  dest = *g_dest;

  forw( ptr, oldlo, oldhi, dest );

  *g_dest = dest;
}

/* This receives a tagged ptr to a pair, vector-like, or procedure 
 * to scan for pointers to the space between oldlo and oldhi.
 */
static void remset_scanner( ptr )
word ptr;
{
  word words, *p, *oldlo, *oldhi, *dest;

  oldlo = g_oldlo;
  oldhi = g_oldhi;
  dest = *g_dest;

  p = ptrof( ptr );
  if (tagof( ptr ) == PAIR_TAG) {
    forw( p, oldlo, oldhi, dest );
    ++p;
    forw( p, oldlo, oldhi, dest );
  }
  else {
    words = sizefield( *p ) / 4;
    while (words--) {
      ++p;
      forw( p, oldlo, oldhi, dest );
    }
  }

  *g_dest = dest;
}

static void scan( ptr, oldlo, oldhi, d )
word *ptr, *oldlo, *oldhi, **d;
{
  word w, bytes, words, h;
  word *dest;
  int iflush = (globals[ G_CACHE_FLUSH ] != 0);

  dest = *d;

  while (ptr < dest) {
    w = *ptr;
#ifdef DEBUG
    if (w == FORWARD_PTR) {
      hardconsolemsg( "INTERNAL ERROR: FORWARDING POINTER IN NEWSPACE." );
      abort();
    }
    else
#endif
    if (ishdr( w )) {
      h = header( w );
      if (h == BV_HDR) {
	/* bytevector: skip it, and flush the icache if code */
	word *oldptr = ptr;

	bytes = roundup4( sizefield( w ) );
	ptr = (word *) ((word) ptr + (bytes + 4));  /* doesn't skip padding */
	if (!(bytes & 4)) *ptr++ = 0;               /* pad. */

	/* Only code vectors typically use a plain bytevector typetag,
	 * so almost any bytevector will be a code vector which must 
         * be flushed.
	 */
	if (iflush && typetag( h ) == BVEC_SUBTAG)
	  mem_icache_flush( oldptr, ptr );
      }
      else {
	/* vector or procedure: scan in a tight loop */
	words = sizefield( w ) >> 2;
	ptr++;
	while (words--) {
	  forw( ptr, oldlo, oldhi, dest );
	  ptr++;
	}
	if (!(sizefield( w ) & 4)) *ptr++ = 0;      /* pad. */
      }
    }
    else {
      forw( ptr, oldlo, oldhi, dest );
      ptr++;
      forw( ptr, oldlo, oldhi, dest );
      ptr++;
    }
  }
  *d = dest;
}

/*
 * "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address.
 */
static word forward( p, dest )
word p, **dest;
{
  word hdr, newptr, *p1, *p2, tag, *ptr;
  unsigned bytes;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)p1;    /* really *dest, but the compiler is dumb. */
  p2 = ptr;

  hdr = *ptr;
  bytes = roundup8( sizefield( hdr ) + 4 );

#ifdef DEBUG
  if (!ishdr( hdr )) {
    hardconsolemsg( "INTERNAL ERROR: FORWARD: NOT A HEADER: %x", hdr );
    abort();
  }
#endif

  /* One might be tempted to think that the following can be speeded up by
   * either using explicit indices (e.g. *(p1+5) = *(p2+5)), introducing
   * temporaries to allow the compiler more room to schedule, by prefetching
   * the destination to prevent cache stalls, and so on. On the Sparc, 
   * however, this code is still faster.
   */
  switch (bytes >> 3) {
    case 8  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 7  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 6  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 5  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 4  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 3  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 2  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 1  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 0  : break;
    default : memcpy( p1, p2, bytes );  p1 += (bytes >> 2);
  }
  *dest = p1;
  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  *ptr = FORWARD_PTR;
  *(ptr+1) = newptr;

  return newptr;
}

/* eof */
