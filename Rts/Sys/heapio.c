/* Larceny run-time system (Unix) -- heap I/O procedures
 * $Id$
 *
 * History
 *   December 12, 1995 / lth (v0.25)
 *     Support for dumping split heaps.
 *
 *   August 6, 1995 / lth (v0.25)
 *     Support for loading split heaps.
 *
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
 * BUGS
 * - Assumes all words are stored in big-endian format.
 * - This version cannot dump a split heap.
 * - Knows a little bit about the globals which delimit the heap 
 *   areas; this should be abstracted away eventually.
 * - Assumes that the pad words are not garbage; this happens to be true
 *   after a collection (by design), but it means you cannot dump without
 *   collecting first.
 */

#include <stdio.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

static FILE *fp = 0;
static word magic;            /* Header word */
static int split_heap = 0;    /* 1 if the loaded heap had a static area */
static ssize = 0;             /* Size (words) of static data */
static tsize = 0;             /* Size (words) of tenured data */
static word roots[ LAST_ROOT-FIRST_ROOT+1 ];  /* Root load area */

static void load_static();
static word *load_dynamic();
static word getword();
static word putword();
static int putheader();
static int put_tagged_word();
static dump_static();
static dump_tenured();

void openheap( filename )
char *filename;
{
  unsigned vno;
  int i, j;

  fp = fopen( filename, "r" );
  if (fp == 0)
    panic( "Unable to open heap file %s.", filename );

  magic = getword();

  vno = magic & 0xFFFF;
  if (vno != HEAP_VERSION)
    panic( "Wrong version heap image; got %d, want %d\n.", vno, HEAP_VERSION );
  
  for (i = FIRST_ROOT, j=0 ; i <= LAST_ROOT ; i++,j++ ) 
    roots[j] = getword();

  if ((magic & 0xFFFF0000) == 0x00010000) {
    ssize = getword();
    split_heap = 1;
  }
  else
    split_heap = 0;
  tsize = getword();
}

void closeheap()
{
  if (fp != 0) {
    fclose( fp );
    fp = 0;
  }
}

/* Return the size in bytes of the static area in the opened heap */
unsigned heap_ssize()
{
  return ssize*sizeof(word);
}

/* Return the size in bytes of the tenured area in the opened heap */
unsigned heap_tsize()
{
  return tsize*sizeof(word);
}

/* Load the heap into the tenured area. */
void load_heap()
{
  word sbase, tbase, limit, *p, w;
  unsigned i, j;

  if (fp == 0)
    panic( "load_heap(): No heap has been opened!" );

  sbase = (word)getheaplimit( HL_SBOT );
  tbase = (word)getheaplimit( HL_TTOP );
  limit = (word)getheaplimit( HL_TLIM );

  for (i = FIRST_ROOT, j =0 ; i <= LAST_ROOT ; i++, j++ ) {
    if (isptr( roots[j] )) {
      if (roots[j] & 0x80000000)
	globals[i] = (roots[j] & ~0x80000000) + sbase;
      else
	globals[i] = roots[j] + tbase;
    }
    else
      globals[ i ] = roots[j];
  }

  if (split_heap) load_static( (word*)sbase, ssize );
  p = load_dynamic( sbase, tbase, limit, tsize );

  if (split_heap) setheaplimit( HL_STOP, (word*)sbase+ssize );
  setheaplimit( HL_TTOP, p );
}

static void load_static( sbase, count )
word *sbase;
word count;
{
  word n;

  if ((n = fread( (char*)sbase, sizeof( word ), count, fp )) < count)
    panic( "Can't load static data -- corrupt heap file?\n"
	   "Stats: ssize=%08x tsize=%08x wanted=%08x got=%08x", 
	  ssize, tsize, count, n );
}

static word *load_dynamic( sbase, tbase, limit, count )
word sbase, tbase, limit;
int count;
{
  word *p, w;
  unsigned i;

  if (tbase + count*sizeof( word ) > limit)
    panic( "Dynamic part of heap image will not fit in heap.");

  if (fread( (char*)tbase, sizeof( word ), count, fp ) < count)
    panic( "Can't load dynamic data -- corrupt heap file?" );

  p = (word*)tbase;
  while (count > 0) {
    w = *p;
    count--;
    if (isptr( w )) {
      if (w & 0x80000000)
	*p = (w & ~0x80000000) + sbase;
      else
	*p = w + tbase;
    }
    p++;
    if (header( w ) == BV_HDR) { /* is well-defined on non-hdrs */
      i = roundup_word( sizefield( w ) ) / sizeof( word );
      p += i;
      count -= i;
    }
  }
  if (count < 0) {
    hardconsolemsg( "LOAD: INCONSISTENT." );
    abort();
  }
  return p;
}


/* Dumps the heap.
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
  word tbase, ttop, tcount, scount, sbase, stop;
  FILE *fp;

  if ((fp = fopen( filename, "w")) == 0) return -1;

  tbase = (word)getheaplimit( HL_TBOT );
  ttop  = (word)getheaplimit( HL_TTOP );
  sbase = (word)getheaplimit( HL_SBOT );
  stop = (word)getheaplimit( HL_STOP );

  putheader( fp, tbase, sbase, stop );

  tcount = (ttop - tbase) / sizeof( word );
  scount = (stop - sbase) / sizeof( word );

  if (split_heap && putword( scount, fp ) == EOF) goto end;
  if (putword( tcount, fp ) == EOF) goto end;

  if (split_heap && dump_static( sbase, stop, fp ) == -1) goto end;
  if (dump_tenured( tbase, ttop, sbase, stop, fp ) == -1) goto end;

  fclose(fp);
  return 0;
 end:
  fclose(fp);
  return -1;
}

static dump_static( sbase, stop, fp )
word sbase, stop;
FILE *fp;
{
  if (fwrite( (char*)sbase, 1, stop-sbase, fp ) != stop-sbase) 
    return -1;
  return 0;
}

static dump_tenured( tbase, ttop, sbase, stop, fp )
word tbase, ttop, sbase, stop;
FILE *fp;
{
  word w, *p, woids, tcount;
  int i;

  tcount = (ttop - tbase)/sizeof(word);
  p = (word *) tbase;
  woids = 0;
  while (tcount) {
    w = *p++; 
    if (put_tagged_word( w, fp, tbase, sbase, stop ) == -1) return -1;
    tcount--;
    woids++;

    if (header( w ) == BV_HDR) {
      i = roundup4( sizefield( w ) ) / sizeof( word );
      while (i--) {
	if (putword( *p++, fp ) == EOF)
	  return -1;
	tcount--;
	woids++;
      }
    }
  }
  return 0;
}

static int putheader( fp, tbase, sbase, stop )
FILE *fp;
word tbase, sbase, stop;
{
  int i;

  if (putword( magic, fp ) == EOF)
    return -1;

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    if (put_tagged_word( globals[i], fp, tbase, sbase, stop ) == -1) return -1;
  return 0;
}

static int put_tagged_word( w, fp, tbase, sbase, stop )
word w, tbase, sbase, stop;
FILE *fp;
{
  if (isptr( w )) {
    if (w >= sbase && w < stop) {
      if (putword( (w-sbase) | 0x80000000, fp ) == EOF) return -1;
    }
    else {
      if (putword( w-tbase, fp ) == EOF) return -1;
    }
  }
  else {
    if (putword( w, fp ) == EOF) return -1;
  }
  return 0;
}

/* ********** NOTE: NOT PORTABLE **********  */
/* The following two procedures know that the heap is 32-bit and big-endian.
 * The obvious #ifdefs can be wrapped to make it more portable.
 */
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

/* This procedure knows that the world is 32-bit and big-endian. */
/* FIXME: does not check EOF */
static word getword()
{
  word a = getc( fp );
  word b = getc( fp );
  word c = getc( fp );
  word d = getc( fp );

  return (a << 24) | (b << 16) | (c << 8) | d;
}


/* eof */
