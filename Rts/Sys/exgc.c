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

#define FASTFWD 0

/* static void forw( word *, word *, word *, word ** ) */
#if FASTFWD
#define forw( p, oldlo, oldhi, dest ) \
ERROR!!!
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

/* Some private globals to emulate closures */
static word *g_oldlo, *g_oldhi, **g_dest;

void ephemeral_collection()
{
  word *newlo, *oldlo, *oldhi, *dest, *dest2;

#ifdef DEBUG
  consolemsg( "[debug] GC: ephemeral collection." );
#endif
  expand_heap( used_cspace() );

  memstat_before_gc( EPHEMERAL_COLLECTION );

  oldlo = (word*)globals[ G_EBOT ];
  oldhi = (word*)globals[ G_ELIM ];
  newlo = dest = dest2 = (word*)globals[ G_TTOP ];

  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;
  g_dest2 = &dest2;

  enumerate_roots( root_scanner );
  enumerate_remset( remset_scanner );
  scan( newlo, oldlo, oldhi, &dest, &dest2 );

  globals[ G_TTOP ] = (word)dest2;
  globals[ G_STKP ] = globals[ G_ELIM ];

  memstat_after_gc();
}

void tenuring_collection()
{
  word *newlo, *oldlo, *oldhi, *dest, *dest2;

#ifdef DEBUG
  consolemsg( "[debug] GC: tenuring collection." );
#endif

  esize = used_espace();
  expand_heap( esize );

  memstat_before_gc( TENURING_COLLECTION );

  oldlo = (word*)globals[ G_TBRK ];
  oldhi = (word*)globals[ G_TTOP ];
  newlo = dest = oldhi;
  dest2 = oldlo;

  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;
  g_dest2 = &dest2;

  enumerate_roots( root_scanner );
  enumerate_remset( remset_scanner );
  scan( newlo, oldlo, oldhi, &dest, &dest2 );

  memcpy( (void*)(word*)globals[ G_TBRK ],
	  (void*)newlo,
	  esize );

  globals[ G_TTOP ] = (word)dest2;

  memstat_after_gc();
}

void full_collection()
{
  word *oldlo, *oldhi, *newlo, *dest, *dest2;

#ifdef DEBUG
  consolemsg( "[debug] GC: full collection." );
#endif

  memstat_before_gc( FULL_COLLECTION );

  oldlo = (word*)globals[ G_TBOT ];
  oldhi = (word*)globals[ G_TTOP ];
  newlo = dest = oldhi;
  dest2 = oldlo;

#if 0
  madvise( MADV_WILLNEED, <tenured tospace> );
  madvise( MADV_SEQUENTIAL, <tenured tospace> ); 
  madvise( MADV_RANDOM, <tenured fromspace> );
#endif

  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;
  g_dest2 = &dest2;

  enumerate_roots( root_scanner );
  scan( newlo, oldlo, oldhi, &dest, &dest2 );

  memcpy( oldlo, oldhi, (dest - newlo) * sizeof( word ) );

  globals[ G_TTOP ] = (word)dest2;

  memstat_after_gc();

#if 0
  madvise( MADV_DONTNEED, <tenured fromspace> );
#endif
}


/* This procedure receives a pointer to a tagged pointer and may forward 
 * the tagged pointer.
 */
static void root_scanner( ptr )
word *ptr;
{
  word *oldlo, *oldhi, **dest;

  oldlo = g_oldlo;
  oldhi = g_oldhi;
  dest = g_dest;
  dest2 = g_dest2;

  forw( ptr, oldlo, oldhi, dest, dest2 );
}

/* This receives a tagged ptr to a pair, vector-like, or procedure 
 * to scan for pointers to the space between oldlo and oldhi.
 */
static void remset_scanner( ptr )
word ptr;
{
  word words, *p, *oldlo, *oldhi, **dest;

  oldlo = g_oldlo;
  oldhi = g_oldhi;
  dest = g_dest;
  dest2 = g_dest2;

  p = ptrof( ptr );
  if (tagof( ptr ) == PAIR_TAG) {
    forw( p, oldlo, oldhi, dest, dest2 );
    forw( p+1, oldlo, oldhi, dest, dest2 );
  }
  else {
    words = sizefield( *p ) / 4;
    while (words--) {
      ++p;
      forw( p, oldlo, oldhi, dest, dest2 );
    }
  }
}

static void scan( ptr, oldlo, oldhi, dest, dest2 )
word *ptr, *oldlo, *oldhi, **dest, **dest2;
{
  word w, bytes;

  while (ptr < *dest) {
    w = *ptr;
    if (ishdr( w ) && header( w ) == BV_HDR) {
      bytes = roundup4( sizefield( w ) );
      ptr = (word *) ((word) ptr + (bytes + 4));  /* does _not_ skip padding */
    }
    else {
      forw( ptr, oldlo, oldhi, dest, dest2 );
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
static word forward( p, dest, dest2 )
word p, **dest, **dest2;
{
  word hdr, newptr, *p1, *p2, tag, *ptr;
  unsigned bytes;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)*dest2;
  p2 = ptr;

#if !FASTFWD
  if (tag == PAIR_TAG) {
    *p1++ = *p2++;
    *p1++ = *p2++;
    *dest2 = (word*)(newptr + 8);
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
    *dest2 = (word*)(newptr + roundup8( bytes ));
  }    
  *dest = p1;
  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  *ptr = 0xFFFFFFFE;
  *(ptr+1) = newptr;

  return newptr;
}

/* eof */
