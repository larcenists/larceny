/*
 * This is the file Sys/expolicy.c.
 *
 * Larceny run-time system (Unix) -- experimental memory manager.
 *
 * History
 *   July 20, 1994 / lth (v0.20)
 *     Written with policy.c as a basis.
 *
 * This file contains procedures for memory allocatation and garbage 
 * collection policy. It knows a great deal about the layout of the
 * heap.
 *
 * Heap layout:
 *   All other memory must have been allocated; the heap must be at the
 *   top of allocated memory. The heap has the following layout, from
 *   low addresses toward higher addresses.
 *
 *   Static area.
 *   Creation space (one).
 *   Tenured space (one).
 *   (program break)
 *
 * All memory is allocated in the creation space. When the creation space
 * is full, live objects are copied to the high end of the tenured space.
 * (This is known as an ephemeral collection.) When "enough" objects have
 * been copied into the tenured area this way, they go through a tenuring
 * collection: dead objects are discarded, and the live objects are tenured.
 * When "enough" objects have been tenured, a full collection is performed.
 *
 * The stack lives at the high end of the creation space, growing down.
 * The only procedure in here which "knows" this is the one which uses
 * the stack pointer to calculate free ephemeral space. However, if the
 * stack lived outside of the heap and would have to be copied into it
 * on a gc, some changes would have to be made in this file.
 *
 * Some things to do:
 *   - use of madvise() is indicated but not implemented, below.
 *   - may be good to lock the ephemeral area, the ssb, and the remset
 *     in memory to avoid paging them, using mlock().
 */

#include <sys/types.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

char *gctype = "exgc";

caddr_t sbrk();
int brk();

#define FASTFWD 1

/* static void forw( word *, word *, word *, word **, word** ) */
#if FASTFWD
#define forw( p, oldlo, oldhi, dest, dest2 ) \
  do { word *TMP1 = (p); \
       word TMP2 = *TMP1; \
       if (isptr( TMP2 ) && (word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
           word *TMP_P = ptrof( TMP2 ); \
           if (*TMP_P == 0xFFFFFFFE) *TMP1 = *(TMP_P+1); \
	   else if (tagof( TMP2 ) == PAIR_TAG) { \
             word *TMPQ; \
	     TMPQ = *dest; *dest = TMPQ+2; \
             *TMPQ = *TMP_P; *(TMPQ+1) = *(TMP_P+1); \
             *TMP_P = 0xFFFFFFFE; \
	     TMPQ = *dest2; *dest2 = TMPQ+2; \
	     *(TMP_P+1) = *TMP1 = (word)tagptr(TMPQ, PAIR_TAG); \
	   } else *TMP1 = forward( TMP2, dest, dest2 ); \
       } \
  } while( 0 )
#else
#define forw( p, oldlo, oldhi, dest, dest2 ) \
  do { word *TMP1 = (p); \
       word TMP2 = *TMP1; \
       if (isptr( TMP2 ) && (word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
           word *TMP_P = ptrof( TMP2 ); \
           if (*TMP_P == 0xFFFFFFFE) *TMP1 = *(TMP_P+1); \
	   else *TMP1 = forward( TMP2, dest, dest2 ); \
       } \
  } while( 0 )
#endif

static void remset_scanner();
static void scan();
static word forward();
static void forward_roots();
static void ephemeral_collection();
static void tenuring_collection();
static void full_collection();


/* Some private globals to emulate closures */
static word *g_oldlo, *g_oldhi, **g_dest, **g_dest2;

/* _The_ heap limit (tracks the program break). */
static word *heaplimit;


/***************************************************************************
 *
 * Heap initialization and expansion.
 */

/* 
 * Procedure to intialize garbage collector.  All sizes are in bytes. 
 * Returns 1 if ok, 0 if not.
 */
int allocate_heap( ephemeral_size, tenured_size, static_size, ewatermark, thiwatermark, tlowatermark )
unsigned ephemeral_size;    /* size of espace; 0 = default */
unsigned tenured_size;      /* initial size of tspace; 0 = default */
unsigned static_size;       /* size of sspace; 0 = default */
unsigned ewatermark;        /* tenuring threshold in %, 0 = default */
unsigned thiwatermark;      /* heap expansion threshold in % */
unsigned tlowatermark;      /* heap contraction threshold in % */
{
  word *heapptr;
  word *static_bot, *static_top, *static_lim;
  word *espace_bot, *espace_lim;
  word *tspace_bot, *tspace_lim;

  if (ephemeral_size == 0) ephemeral_size = DEFAULT_ESIZE;
  if (tenured_size == 0) tenured_size = DEFAULT_TSIZE;
  if (static_size == 0) static_size = DEFAULT_SSIZE;
  if (ewatermark <= 0 || ewatermark > 100)
    ewatermark = DEFAULT_EWATERMARK;
  if (thiwatermark <= 0 || thiwatermark > 100)
    thiwatermark = DEFAULT_THIWATERMARK;
  if (tlowatermark <= 0 || tlowatermark > 100)
    tlowatermark = DEFAULT_TLOWATERMARK;

  ephemeral_size = roundup8( ephemeral_size );
  tenured_size = roundup8( tenured_size );
  static_size = roundup8( static_size );

  heapptr = (word*)sbrk(0);

  /* round up heap ptr to 8-byte boundary */
  heapptr = (word*)sbrk( roundup8( (unsigned)heapptr ) - (word)heapptr );
  if ((caddr_t)heapptr == (caddr_t)-1) return 0;

  if ((caddr_t)sbrk( static_size+tenured_size+ephemeral_size ) == (caddr_t)-1)
    return 0;

  static_bot = static_top = heapptr;
  heapptr += static_size / sizeof( word );
  static_lim = heapptr;

  espace_bot = heapptr;
  heapptr += ephemeral_size / sizeof( word );
  espace_lim = heapptr;

  tspace_bot = heapptr;
  heapptr += tenured_size / sizeof( word );
  tspace_lim = heapptr;

  globals[ G_CONT ] = FALSE_CONST;
  globals[ G_EWATERMARK ] = ewatermark;
  globals[ G_THIWATERMARK ] = thiwatermark;
  globals[ G_TLOWATERMARK ] = tlowatermark;
  globals[ G_EBOT ] = (word)espace_bot;
  globals[ G_ETOP ] = (word)espace_bot;
  globals[ G_ELIM ] = (word)espace_lim;
  globals[ G_TSPACE1_BOT ] = (word)tspace_bot;  /* tracks major collections */
  globals[ G_TBRK ] = (word)tspace_bot;
  globals[ G_TBOT ] = (word)tspace_bot;
  globals[ G_TTOP ] = (word)tspace_bot;
  globals[ G_TLIM ] = (word)tspace_lim;

  globals[ G_STKP ] = globals[ G_ELIM ];

  if (!create_stack())
    panic( "Unable to create initial stack (you miser)." );

  return 1;
}

int free_cspace()
{ return globals[ G_STKP ] - globals[ G_ETOP ]; }

int size_cspace()
{ return globals[ G_ELIM ] - globals[ G_EBOT ]; }

int used_cspace()
{ return size_cspace() - free_cspace(); }

int used_espace()
{ return globals[ G_TTOP ] - globals[ G_TBRK ]; }

int free_tspace()
{ return globals[ G_TLIM ] - globals[ G_TTOP ]; }

int size_tspace()
{ return globals[ G_TLIM ] - globals[ G_TBOT ]; }

int used_tspace()
{ return size_tspace() - free_tspace(); }

word *getheaplimit( which )
int which;
{ 
  switch (which) {
    case HL_TBOT : return (word*)globals[ G_TBOT ];
    case HL_TTOP : return (word*)globals[ G_TTOP ];
    case HL_TLIM : return (word*)globals[ G_TLIM ];
    default      : panic( "Bad argument to getheaplimit()." );
  }
  /*NOTREACHED*/
  return 0;
}

void setheaplimit( which, p )
int which;
word *p;
{
  switch (which) {
    case HL_TTOP : globals[ G_TTOP ] = (word)p;
                   globals[ G_TBRK ] = (word)p;
                   globals[ G_TSPACE1_BOT ] = (word)p; /* hack */
		   break;
    default      : panic( "Bad argument to setheaplimit()." );
  }
}

#define roundup1M( x )  (((x)+(1024*1024-1)) & ~(1024*1024-1))

static void expand_heap( accomodate )
unsigned accomodate;  /* bytes we need room for above TTOP. */
{
  unsigned free;

  free = free_tspace();
  if (free >= accomodate) return;
  
  if (sbrk( max( accomodate-free, size_cspace() ) ) == (caddr_t)-1)
    panic( "Unable to expand the heap." );
  heaplimit = (word*)sbrk( 0 );

  globals[ G_TLIM ] += max( accomodate-free, size_cspace() );

  consolemsg( "[debug] Expanding heap, new size = %dKB.",size_tspace()/1024);
}

/*
 * Contract the tenured area. The tenured area is shrunk by the size of
 * one ephemeral area.
 */
static void contract_tspace()
{
}

/***************************************************************************
 *
 * Allocation from C (sometimes necessary).
 */

/*
 * Allocate a chunk of ephemeral memory. `n' is a number of bytes.
 */
word *alloc_from_heap( n )
int n;
{
  word p;

  n = roundup8( n );
  if (globals[ G_ETOP ] + n >= globals[ G_STKP ])
    garbage_collect( EPHEMERAL_COLLECTION, n );

  p = globals[ G_ETOP ];
  globals[ G_ETOP ] += n;
  return (word *)p;
}

void garbage_collect( type, request )
int type;       /* collection type requested */
int request;    /* words requested */
{
  int must_tenure;

  request *= 4;  /* to get bytes */

  /* Always start by copying live objects out from the creation space */
  flush_stack();
  must_tenure = !compact_ssb();
  ephemeral_collection();

  /* Then, maybe we need to do more */
  if (type == EPHEMERAL_COLLECTION) {
    if (must_tenure)
      type = TENURING_COLLECTION;
    else if (used_espace() >= size_cspace()/3)
      type = TENURING_COLLECTION;
  }

  if (type == TENURING_COLLECTION) {
    tenuring_collection();
    if (globals[ G_TTOP ] - globals[ G_TSPACE1_BOT ] >= 3*size_cspace())
      type = FULL_COLLECTION;
  }

  if (type == FULL_COLLECTION)
    full_collection();

  if (!create_stack())
    panic( "Garbage collector: Failed create-stack." );

  if (!restore_frame())
    panic( "Garbage collector: Failed restore-frame." );

  if (request > free_cspace())
    panic( "Garbage collector: Object of size %d bytes too large to allocate.",
	   request );

  if (type != EPHEMERAL_COLLECTION) clear_remset();
}

static void ephemeral_collection()
{
  word *newlo, *oldlo, *oldhi, *dest, *dest2;

#ifdef DEBUG
  consolemsg( "[debug] GC: ephemeral collection." );
#endif
  expand_heap( used_cspace() );

  memstat_before_gc( EPHEMERAL_COLLECTION );

  oldlo = (word*)globals[ G_EBOT ];
  oldhi = (word*)globals[ G_ELIM ];
  newlo = (word*)globals[ G_TBRK ];
  dest = dest2 = (word*)globals[ G_TTOP ];

  forward_roots( oldlo, oldhi, &dest, &dest2 );

  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;
  g_dest2 = &dest2;
  enumerate_remset( remset_scanner );

  scan( newlo, oldlo, oldhi, &dest, &dest2 );

  globals[ G_ETOP ] = globals[ G_EBOT ];
  globals[ G_STKP ] = globals[ G_ELIM ];
  globals[ G_TTOP ] = (word)dest2;

  memstat_after_gc();
}

static void tenuring_collection()
{
  word *newlo, *oldlo, *oldhi, *dest, *dest2;

#ifdef DEBUG
  consolemsg( "[debug] GC: tenuring collection." );
#endif

  expand_heap( used_espace() );

  memstat_before_gc( TENURING_COLLECTION );

  oldlo = (word*)globals[ G_TBRK ];
  oldhi = (word*)globals[ G_TTOP ];
  newlo = dest = oldhi;
  dest2 = oldlo;

  forward_roots( oldlo, oldhi, &dest, &dest2 );

  g_oldlo = oldlo;
  g_oldhi = oldhi;
  g_dest = &dest;
  g_dest2 = &dest2;
  enumerate_remset( remset_scanner );

  scan( newlo, oldlo, oldhi, &dest, &dest2 );

  memcpy( oldlo, newlo, (dest - newlo)*sizeof( word ) );

  globals[ G_TBRK ] = globals[ G_TTOP ] = (word)dest2;

  memstat_after_gc();
}

static void full_collection()
{
  word *oldlo, *oldhi, *newlo, *dest, *dest2;

  consolemsg( "[debug] GC: major collection." );

  expand_heap( used_tspace() );

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

  forward_roots( oldlo, oldhi, &dest, &dest2 );
  scan( newlo, oldlo, oldhi, &dest, &dest2 );

  memcpy( oldlo, oldhi, (dest - newlo) * sizeof( word ) );

  globals[ G_TSPACE1_BOT ] = globals[ G_TBRK ] = globals[ G_TTOP ] = (word)dest2;

  memstat_after_gc();

#if 0
  madvise( MADV_DONTNEED, <tenured fromspace> );
#endif
}

static void forward_roots( oldlo, oldhi, dest, dest2 )
word *oldlo, *oldhi, **dest, **dest2;
{
  int i;

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    forw( &globals[ i ], oldlo, oldhi, dest, dest2 );
}

/* This receives a tagged ptr to a pair, vector-like, or procedure 
 * to scan for pointers to the space between oldlo and oldhi.
 */
static void remset_scanner( ptr )
word ptr;
{
  word words, *p, *oldlo, *oldhi, **dest, **dest2;

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
