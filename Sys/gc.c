/*
 * This is the file Sys/gc.c.
 *
 * Larceny run-time system -- the garbage collector.
 *
 * History:
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

#include <memory.h>   /* for memcpy() */
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

#define FASTFWD 1

/* static void forw( word *, word *, word *, word ** ) */
#if FASTFWD
#define forw( p, oldlo, oldhi, dest ) \
  do { word *TMP1 = (p); \
       word TMP2 = *TMP1; \
       if (isptr( TMP2 ) && (word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
           word *TMP_P = ptrof( TMP2 ); \
           if (*TMP_P == 0xFFFFFFFE) *TMP1 = *(TMP_P+1); \
	   else if (tagof( TMP2 ) == PAIR_TAG) { \
             word *TMPQ = *dest; \
             *TMPQ = *TMP_P; *(TMPQ+1) = *(TMP_P+1); \
             *TMP_P = 0xFFFFFFFE; \
	     *(TMP_P+1) = *TMP1 = (word)tagptr(TMPQ, PAIR_TAG); \
             *dest = TMPQ+2; \
	   } else *TMP1 = forward( TMP2, dest ); \
       } \
  } while( 0 )
#else
#define forw( p, oldlo, oldhi, dest ) \
  do { word *TMP1 = (p); \
       word TMP2 = *TMP1; \
       if (isptr( TMP2 ) && (word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
           word *TMP_P = ptrof( TMP2 ); \
           if (*TMP_P == 0xFFFFFFFE) *TMP1 = *(TMP_P+1); \
	   else *TMP1 = forward( TMP2, dest ); \
       } \
  } while( 0 )
#endif

static void remset_scanner();
static void root_scanner();
static void scan();
static word forward();

word *minor_collection( oldlo, oldhi, newlo )
word *oldlo, *oldhi, *newlo;
{
  word *dest = newlo;

#ifdef DEBUG
  consolemsg( "[debug] Minor collection %08lx %08lx %08lx.",
	     oldlo, oldhi, newlo );
#endif
  enumerate_roots( root_scanner, oldlo, oldhi, &dest );
  enumerate_remset( remset_scanner, oldlo, oldhi, &dest );
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
  consolemsg( "Major collection." );
#endif
  enumerate_roots( root_scanner, oldlo, oldhi, &dest );
  scan( newlo, oldlo, oldhi, &dest );

  return dest;
}

/* This procedure receives a pointer to a tagged pointer and may forward 
 * the tagged pointer.
 */
static void root_scanner( ptr, oldlo, oldhi, dest )
word *ptr, *oldlo, *oldhi, **dest;
{
  forw( ptr, oldlo, oldhi, dest );
}

/* This receives a tagged ptr to a pair, vector-like, or procedure 
 * to scan for pointers to the space between oldlo and oldhi.
 */
static void remset_scanner( ptr, oldlo, oldhi, dest )
word ptr, *oldlo, *oldhi, **dest;
{
  word words, *p;

  p = ptrof( ptr );
  if (tagof( ptr ) == PAIR_TAG) {
    forw( p, oldlo, oldhi, dest );
    forw( p+1, oldlo, oldhi, dest );
  }
  else {
    words = sizefield( *p ) / 4;
    while (words--) {
      ++p;
      forw( p, oldlo, oldhi, dest );
    }
  }
}

static void scan( ptr, oldlo, oldhi, dest )
word *ptr, *oldlo, *oldhi, **dest;
{
  word w, bytes;

  while (ptr < *dest) {
    w = *ptr;
    if (ishdr( w ) && header( w ) == BV_HDR) {
      bytes = roundup4( sizefield( w ) );
      ptr = (word *) ((word) ptr + (bytes + 4));  /* does _not_ skip padding */
    }
    else {
      forw( ptr, oldlo, oldhi, dest );
      ptr++;
    }
  }
}

/*
 * "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr".
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

#if !FASTFWD
  if (tag == PAIR_TAG) {
    *p1++ = *p2++;
    *p1++ = *p2++;
  }
  else 
#endif
  {
    hdr = *ptr;
    bytes = roundup4( sizefield( hdr ) ) + 4;

    switch (bytes >> 2) {
    case 8 : *p1++ = *p2++;
    case 7 : *p1++ = *p2++;
    case 6 : *p1++ = *p2++;
    case 5 : *p1++ = *p2++;
    case 4 : *p1++ = *p2++; 
    case 3 : *p1++ = *p2++;
    case 2 : *p1++ = *p2++;
    case 1 : *p1++ = *p2++;
    case 0 : break;
    default: memcpy( p1, p2, bytes ); p1 += (bytes >> 2);
    }
    if (bytes & 4) *p1++ = 0;
  }    
  *dest = p1;
  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  *ptr = 0xFFFFFFFE;
  *(ptr+1) = newptr;

  return newptr;
}

/* eof */
