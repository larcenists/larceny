/*
 * This is the file Sys/heapio.c.
 *
 * Larceny run-time system (Unix) -- heap I/O procedures
 *
 * History
 *   June 28 - July 5, 1994 / lth (v0.20)
 *     Moved to this file from memsupport.c.
 *
 * There are two kinds of heaps, single and split. Both have a version number
 * as the first word followed by the roots. The high 16 bits of the version
 * number is the heap type; 0=single, 1=split. The low 16 bits is the version
 * proper.
 *
 * Single heap format:
 *  - version number (1 word)
 *  - roots (n words; depends on version)
 *  - word count for the heap data
 *  - heap data
 * All pointers in the roots or in the heap are from base 0. They
 * are adjusted as the heap is read in.
 *
 * Split heap format:
 *  - version number (1 word)
 *  - roots (n words; depends on version)
 *  - word count for the static heap data
 *  - word count for the tenured or ephemeral heap data
 *  - static heap data
 *  - tenured or ephemeral data
 * Intergenerational pointers (tenured -> static) have high bit set. All 
 * pointers are adjusted relative to 0 of the heap they point to.
 *
 * All words are stored in big-endian format.
 *
 * This version does not support split heaps. Also, it knows a little bit
 * about the globals which delimit the heap areas; this should be abstracted
 * away eventually.
 */

#include <stdio.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

static FILE *fp = 0;
static word magic;

static word *loadit();
static word getword();
static word putword();

void openheap( filename )
char *filename;
{
  unsigned vno;

  fp = fopen( filename, "r" );
  if (fp == 0)
    panic( "Unable to open heap file %s.", filename );

  magic = getword();

  vno = magic & 0xFFFF;
  if (vno != HEAP_VERSION)
    panic( "Wrong version heap image; got %d, want %d\n.", vno, HEAP_VERSION );
}

void closeheap()
{
  if (fp != 0) {
    fclose( fp );
    fp = 0;
  }
}

/*
 * Return the size of the static area in the opened heap
 */
unsigned heap_ssize()
{
  return 0;
}

/*
 * Load the heap into the tenured area.
 */
void load_heap()
{
  word base, limit, scount, hcount, *p, w;
  unsigned i;

  if (fp == 0)
    panic( "load_heap(): No heap has been opened!" );

  base = (word)getheaplimit( HL_TTOP );
  limit = (word)getheaplimit( HL_TLIM );

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ ) {
    w = getword();
    globals[ i ] = (isptr( w ) ? w + base : w);
  }

  if ((magic & 0xFFFF0000) == 1)
    panic( "Can't do split heaps (yet)." );

  hcount = getword();
  p = loadit( base, limit, hcount );

  setheaplimit( HL_TTOP, p );
}

static word *loadit( base, limit, count )
word base, limit;
unsigned count;
{
  word *p, w;
  unsigned i;

  if (base + count*sizeof( word ) > limit)
    panic( "Heap image will not fit in heap.");

  if (fread( (char*)base, sizeof( word ), count, fp ) < count)
    panic( "Corrupt heap file." );

  p = (word*)base;
  while (count > 0) {
    w = *p;
    count--;
    if (isptr( w ))
      *p = w + base;
    p++;
    if (header( w ) == BV_HDR) { /* is well-defined on non-hdrs */
      i = roundup_word( sizefield( w ) ) / sizeof( word );
      p += i;
      count -= i;
    }
  }
  return p;
}


/* This procedure knows that the world is 32-bit and big-endian. */
static word getword()
{
  word a = getc( fp );
  word b = getc( fp );
  word c = getc( fp );
  word d = getc( fp );

  return (a << 24) | (b << 16) | (c << 8) | d;
}


/*
 * Dumps the tenured heap as a single image.
 *
 * The tenured heap is dumped "as is"; pointers into other areas are
 * dumped literally and should not exist. A major GC takes care of this,
 * in the absence of a static area.
 *
 * Knows too much about the globals[] table and the layout of tspace.
 */
dump_heap( filename )
char *filename;
{
  word base, top, count, w, *p, woids;
  int i, align;
  FILE *fp;

  fp = fopen( filename, "w");
  if (fp == NULL)
    return -1;

  base = (word)getheaplimit( HL_TBOT );
  top  = (word)getheaplimit( HL_TTOP );

  if (putword( HEAP_VERSION, fp ) == EOF)
    return -1;

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ ) {
    if (isptr( globals[ i ] )) {
      if (putword( globals[ i ] - base, fp ) == EOF)
	return -1;
    }
    else if (putword( globals[ i ], fp ) == EOF)
      return -1;
  }

  count = (top - base) / sizeof( word );
  if (putword( count, fp ) == EOF)
    return -1;

  p = (word *) base;
  woids = 0;
  while (count) {
    w = *p++; 
    if (putword( (isptr( w ) ? w-base : w), fp ) == EOF)
      return -1;
    count--;
    woids++;

    if (header( w ) == BV_HDR) {
      i = roundup_word( sizefield( w ) ) / sizeof( word );
      while (i--) {
	if (putword( *p++, fp ) == EOF)
	  return -1;
	count--;
	woids++;
      }
    }
  }

  fclose( fp );

  return 0;
}

/* Knows the world is 32-bit and big-endian. */
static word putword( w, fp )
word w;
FILE *fp;
{
  if (putc( (w >> 24) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 16) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 8) & 0xFF, fp ) == EOF) return EOF;
  if (putc( w & 0xFF, fp ) == EOF) return EOF;
  return 0;
}


/* eof */
