/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- heap I/O procedures
 *
 * There are three major kinds of heaps: bootstrap single, bootstrap split,
 * and dumped.
 *
 * Bootstrap single heaps are created by the development environment's heap
 * dumper only and have the following format:
 *
 *    - version number (1 word)
 *    - roots (n words; depends on version)
 *    - word count for the heap data
 *    - heap data
 *
 *   All pointers in the roots or in the heap are from base 0. They
 *   are adjusted as the heap is read in.
 *
 * Bootstrap split heaps are created from bootstrap single heaps using
 * either the external 'hsplit' utility program or the reorganize-and-dump
 * command line switch.  The split heap has two data areas, data and text,
 * where the text area holds executable code only:
 *
 *    - version number (1 word)
 *    - roots (n words; depends on version)
 *    - word count for the text area
 *    - word count for the data area
 *    - text area
 *    - data area
 *
 *   Intergenerational pointers (data -> text) have the high bit set. 
 *   All pointers are adjusted relative to 0 of the heap they point to.
 *
 * Dumped heaps are dumped interactively and preserve the data of each
 * generation individually, and in addition allows for the dumping of
 * heap metadata (e.g. free lists):
 *
 *    - version number (1 word)
 *    - roots (n words; depends on version)
 *    - word count for metadata area (1 word)
 *    - word count for page table (1 word)
 *    - word count for all data blocks (1 word)
 *    - metadata blocks (one for each generation; variable size)
 *    - page table (variable size)
 *    - data blocks (one for each generation; variable size)
 *
 *  Pointers are adjusted relative to 0, and the page table is used to
 *  map a pointer to a page number (i.e., an actual address).
 *
 *  Each metadata block starts with three words:
 *    - word count for metadata block (including length field) (1 word)
 *    - word count for the heap's data block
 *    - type word for the originating heap/gc (1 word)
 *  followed by a number of heap-specific words.
 *
 *  Each collector/heap type has its own type word, and a generation's data
 *  can only be restored to the same kind of collector/heap that created 
 *  the data block.  A dumped heap can therefore only be restored to
 *  a system with the exact same configuration that created the heap.
 *  [This is probably too restrictive; at the very least, it should be
 *  possible to restore into a system with _more_ generations.]
 *  The type encodes some standard information also: if the low bit is 1,
 *  then the data is pure text, otherwise it is standard tagged data.
 *
 * The version number has two fields: the low 16 bits is a heap version
 * number (incremented whenever the heap layout changes, for example
 * when roots are added).  The high 16 bits is the heap type: 0=single,
 * 1=split, 2=dumped.
 *
 * -----
 *
 * BUGS
 * - Assumes that the pad words are not garbage; this happens to be true
 *   after a collection (by design), but it means you cannot dump without
 *   collecting first.
 */

#define HEAPIO_INTERNAL

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "larceny.h"
#include "heapio.h"
#include "semispace_t.h"
#include "gclib.h"

typedef struct hio_range hio_range;

/* Private */
struct hio_tbl {
  hio_range *a;
  word      *lowest;
  word      *highest;
  int       next;
  int       size;
};

struct hio_range  {
  word *bot;
  word *top;
  bool is_large;
  word *real_bot;
  int  bytes;
  int  pages;
};

#define HIBIT  0x80000000U

static jmp_buf EX_heapio_ex;

#define CATCH( var )  if ((var = setjmp( EX_heapio_ex )) != 0)
#define THROW( val )  longjmp( EX_heapio_ex, val )

static int  load_text( heapio_t *h, word *base, int count );
static int  load_data( heapio_t *h, word *text, word *data, int count );
#if 0
static void putheader( FILE*, word, word, word, word*, word );
static void put_tagged_word( word, FILE*, word, word, word );
static void dump_text( word, word, FILE* );
static void dump_data( word, word, word, word, FILE* );
#else
static void put_tagged_word( word w, word *lowest, word *pagetbl, FILE *fp );
static void dump_text_block( hio_range a, word *lowest, word *pagetbl,
			     FILE *fp );
static void dump_data_block( hio_range a, word *lowest, word *pagetbl,
			     FILE *fp );
#endif
static word getword( FILE *fp );
static void putword( word, FILE* );

heapio_t *create_heapio( void )
{
  heapio_t *h;

  h = (heapio_t*)must_malloc( sizeof( heapio_t ) );

  h->fp = 0;
  h->type = 0;
  h->input = 0;
  h->output = 0;
  h->split_heap = 0;
  h->bootstrap_heap = 0;
  h->text_segments = (hio_tbl*)must_malloc( sizeof( hio_tbl ) );
  h->text_segments->a = 0;
  h->text_segments->size = 0;
  h->text_segments->next = 0;
  h->text_segments->lowest = (word*)(-1);
  h->text_segments->highest = 0;
  h->data_segments = (hio_tbl*)must_malloc( sizeof( hio_tbl ) );
  h->data_segments->a = 0;
  h->data_segments->size = 0;
  h->data_segments->next = 0;
  h->data_segments->lowest = (word*)(-1);
  h->data_segments->highest = 0;

  return h;
}

int hio_open( heapio_t *h, const char *filename )
{
  unsigned vno, metadata_size;
  int i, j, r;
  FILE *fp;

  assert( h->fp == 0 );

  fp = fopen( filename, "rb" );
  if (fp == 0)
    return HEAPIO_CANTOPEN;

  h->magic = getword( fp );

  vno = h->magic & 0xFFFF;
  if (vno != HEAP_VERSION) {
    fclose( fp );
    return HEAPIO_WRONGVERSION;
  }
  
  for (i = FIRST_ROOT, j=0 ; i <= LAST_ROOT ; i++,j++ ) 
    h->roots[j] = getword( fp );

  h->type = (h->magic >> 16) & 0xFFFF;
  switch (h->type) {
  case HEAP_SINGLE:
    h->bootstrap_heap = 1;
    h->data_size = getword( fp );
    h->text_size = 0;
    break;
  case HEAP_SPLIT:
    h->split_heap = 1;
    h->bootstrap_heap = 1;
    h->text_size = getword( fp );
    h->data_size = getword( fp );
    break;
  case HEAP_DUMPED:
    panic( "Can't open DUMPED heaps." ); 
    break;
  default:
    fclose( fp );
    return HEAPIO_WRONGTYPE;
  }
  h->fp = fp;
  h->input = 1;
  return HEAPIO_OK;
}

int hio_create( heapio_t *h, const char *filename, int type )
{
  assert( h->fp == 0 );

  h->fp = fopen( filename, "wb" );
  if (h->fp == 0)
    return HEAPIO_CANTOPEN;
  h->type = type;

  switch (type) {
  case HEAP_SINGLE :
    h->bootstrap_heap = 1;
    break;
  case HEAP_SPLIT :
    h->bootstrap_heap = 1;
    h->split_heap = 1;
    break;
  case HEAP_DUMPED :
    panic( "Can't create DUMPED heaps." );
    break;
  default :
    fclose( h->fp );
    return HEAPIO_WRONGTYPE;
  }
  h->magic = (type << 16) | HEAP_VERSION;
  h->output = 1;
  return HEAPIO_OK;
}

int hio_close( heapio_t *h )
{
  int code = HEAPIO_OK;

  if (h->fp != 0) {
    if (fclose( h->fp ) == EOF) code = HEAPIO_CANTCLOSE;
    h->fp = 0;
  }

  if (h->text_segments->a) free( h->text_segments->a );
  free( h->text_segments );
  if (h->data_segments->a) free( h->data_segments->a );
  free( h->data_segments );
  free( h );

  return code;
}

int hio_dump_initiate( heapio_t *h, word *globals )
{
  h->globals = globals;
}

int hio_dump_segment( heapio_t *h, int type, word *bot, word *top )
{
  int newsize, i, j;
  hio_tbl *tbl;
  hio_range *a;

  assert( type == TEXT_SEGMENT || type == DATA_SEGMENT );
  assert(    (word)bot % PAGESIZE == 0
	  || (gclib_desc_b[ pageof( bot ) ] & MB_LARGE_OBJECT) );
  assert( top > bot );

  tbl = (type == TEXT_SEGMENT ? h->text_segments : h->data_segments);

  if (tbl->next == tbl->size) {
    newsize = max( tbl->size*2, 10 );
    a = (hio_range*)must_malloc( sizeof( hio_range )*newsize );
    for ( i=0 ; i < tbl->size ; i++ )
      a[i] = tbl->a[i];
    tbl->size = newsize;
    if (tbl->a) free( tbl->a );
    tbl->a = a;
  }

  i = tbl->next;
  tbl->next++;
  /* Deal with large objects */
  if ((word)bot % PAGESIZE != 0) {
    tbl->a[i].is_large = 1;
    tbl->a[i].real_bot = bot;
    bot = (word*)((word)bot & ~PAGEMASK);
  }
  else
    tbl->a[i].is_large = 0;
  tbl->a[i].bot = bot;
  tbl->a[i].top = top;
  tbl->a[i].bytes = (top - bot)*sizeof( word );
  tbl->a[i].pages = roundup_page( tbl->a[i].bytes ) / PAGESIZE;
  if (bot < tbl->lowest) tbl->lowest = bot;
  if (top > tbl->highest) tbl->highest = top;
  return HEAPIO_OK;
}

int hio_dump_commit( heapio_t *h )
{
  word *lowest, *highest, *pagetbl;
  int pages, i, j, pg, r, text_size, data_size, nextpage;

  /* Compute limits */
  lowest = min( h->text_segments->lowest, h->data_segments->lowest );
  highest = max( h->text_segments->highest, h->data_segments->highest );

  /* Construct the page table and compute the data and textg size.
     The page table maps memory addresses to heap image addresses,
     where the heap image address is 0-based in each heap area,
     with the high bit set for addresses in the text area.
     */
  pages = roundup_page((highest-lowest)*sizeof(word))/PAGESIZE;
  pagetbl = (int*)must_malloc( sizeof(word)*pages );
  for ( i=0 ; i<pages ; i++ )
    pagetbl[i] = 0;

  text_size = 0;
  data_size = 0;

  nextpage = 0;
  for ( i=0 ; i < h->text_segments->next ; i++ ) {
    pg = pageof_pb( h->text_segments->a[i].bot, lowest );
    j = h->text_segments->a[i].pages;
    text_size += j*PAGESIZE;
    for ( ; j > 0 ; pg++, j--, nextpage++ )
      pagetbl[pg] = (nextpage*PAGESIZE) | HIBIT;
  }
  nextpage = 0;
  for ( i=0 ; i < h->data_segments->next ; i++ ) {
    pg = pageof_pb( h->data_segments->a[i].bot, lowest );
    j = h->data_segments->a[i].pages;
    data_size += j*PAGESIZE;
    for ( ; j > 0 ; pg++, j--, nextpage++ )
      pagetbl[pg] = nextpage*PAGESIZE;
  }

  /* Dump it! */
  CATCH( r )
    return r;

  putword( h->magic, h->fp );
  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ ) 
    put_tagged_word( h->globals[i], lowest, pagetbl, h->fp );
  putword( text_size/sizeof(word), h->fp );
  putword( data_size/sizeof(word), h->fp );
  for ( i=0 ; i < h->text_segments->next ; i++ )
    dump_text_block( h->text_segments->a[i], lowest, pagetbl, h->fp );
  for ( i=0 ; i < h->data_segments->next ; i++ )
    dump_data_block( h->data_segments->a[i], lowest, pagetbl, h->fp );
}

static void
put_tagged_word( word w, word *lowest, word *pagetbl, FILE *fp )
{
  if (isptr( w ))
    putword( pagetbl[pageof_pb(w, lowest)] | (w & PAGEMASK), fp );
  else
    putword( w, fp );
}    

static void
pad( int bytes, FILE *fp )
{
  int i;

  if (bytes % PAGESIZE != 0)
    for ( i=PAGESIZE - bytes%PAGESIZE ; i ; i-- )
      putc( 0, fp );
}

static void
dump_text_block( hio_range a, word *lowest, word *pagetbl, FILE *fp )
{
  if (fwrite( (void*)a.bot, 1, a.bytes, fp ) != a.bytes)
    THROW( HEAPIO_CANTWRITE );
  pad( a.bytes, fp );
}

static void
dump_data_block( hio_range a, word *lowest, word *pagetbl, FILE *fp )
{
  word w, *p;
  int i, data_count;

  data_count = (a.top - a.bot);
  p = a.bot;
  if (a.is_large) {
    while (p != a.real_bot) {
      putword( 0, fp );
      p++;
      data_count--;
    }
  }
  while (data_count) {
    w = *p++; 
    put_tagged_word( w, lowest, pagetbl, fp );
    data_count--;

    if (header( w ) == BV_HDR) {
      i = roundup4( sizefield( w ) ) / sizeof( word );
      while (i--) {
	putword( *p++, fp );
	data_count--;
      }
    }
  }
  pad( a.bytes, fp );
}

int hio_load_bootstrap( heapio_t *h, word *text_base, word *data_base,
		       word *globals )
{
  int i, j, r;

  if (h->fp == 0 || !h->input)
    return HEAPIO_NOTOPEN;

  if (!h->bootstrap_heap)
    return HEAPIO_WRONGTYPE;

  for ( i=FIRST_ROOT, j=0 ; i<=LAST_ROOT ; i++, j++ ) {
    if (isptr( h->roots[j] )) {
      if (h->roots[j] & HIBIT)
	globals[i] = (h->roots[j] & ~HIBIT) + (word)text_base;
      else
	globals[i] = h->roots[j] + (word)data_base;
    }
    else
      globals[ i ] = h->roots[j];
  }

  if (h->split_heap) {
    r = load_text( h, text_base, h->text_size );
    if (r < 0) return r;
  }
  return load_data( h, text_base, data_base, h->data_size );
}

static int
load_text( heapio_t *h, word *text_base, int count )
{
  word n;

  n = fread( (char*)text_base, sizeof( word ), count, h->fp );
  if (n < count)
    return HEAPIO_CANTREAD;
}

static int
load_data( heapio_t *h, word *text_base, word *data_base, int count )
{
  word *p, w;
  int i;

  if (fread( (char*)data_base, sizeof( word ), count, h->fp ) < count)
    return HEAPIO_CANTREAD;

  p = data_base;
  while (count > 0) {
    w = *p;
    count--;
    if (isptr( w )) {
      if (w & HIBIT)
	*p = (w & ~HIBIT) + (word)text_base;
      else
	*p = w + (word)data_base;
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
  return HEAPIO_OK;
}

#if 0
int hio_dump_bootstrap( heapio_t *h, semispace_t *text, semispace_t *data, 
		        word *globals )
{
  word data_base = 0, data_top = 0, data_count = 0;
  word text_count = 0, text_base = 0, text_top = 0;
  int r;

  CATCH( r ) {
    return r;
  }

  if (h->fp == 0 || !h->output)
    THROW( HEAPIO_NOTOPEN );

  /* FIXME: For now, only one chunk. */
  if (data && data->current != 0) panic( "dump_heap_image#1" );
  if (text && text->current != 0) panic( "dump_heap_image#2" );

  if (!data) 
    panic( "dump_heap_image#3" );

  data_base = (word)data->chunks[data->current].bot;
  data_top  = (word)data->chunks[data->current].top;
  if (text) {
    text_base = (word)text->chunks[text->current].bot;
    text_top  = (word)text->chunks[text->current].top;
  }
  h->magic = (( text != 0 ) << 16) | HEAP_VERSION;

  putheader( h->fp, data_base, text_base, text_top, globals, h->magic );

  data_count = (data_top - data_base) / sizeof( word );
  text_count = (text_top - text_base) / sizeof( word );

  if (text_count > 0) putword( text_count, h->fp );
  putword( data_count, h->fp );

  if (text_count > 0)
    dump_text( text_base, text_top, h->fp );
  dump_data( data_base, data_top, text_base, text_top, h->fp );

  return HEAPIO_OK;
}

static void
dump_text( word text_base, word text_top, FILE *fp )
{
  if (fwrite( (char*)text_base, 1, text_top-text_base, fp ) 
      != text_top-text_base) 
    THROW( HEAPIO_CANTWRITE );
}

static void
dump_data( word data_base, word data_top, word text_base, word text_top,
	   FILE *fp )
{
  word w, *p, words, data_count;
  int i, r;

  data_count = (data_top - data_base)/sizeof(word);
  p = (word *) data_base;
  words = 0;
  while (data_count) {
    w = *p++; 
    put_tagged_word( w, fp, data_base, text_base, text_top );
    data_count--;
    words++;

    if (header( w ) == BV_HDR) {
      i = roundup4( sizefield( w ) ) / sizeof( word );
      while (i--) {
	putword( *p++, fp );
	data_count--;
	words++;
      }
    }
  }
}

static void
putheader( FILE *fp, 
	   word data_base, word text_base, word text_top, word *globals,
	   word magic)
{
  int i;

  putword( magic, fp );
  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    put_tagged_word( globals[i], fp, data_base, text_base, text_top );
}

static void
put_tagged_word( word w, FILE *fp,
		 word data_base, word text_base, word text_top )
{
  if (isptr( w )) {
    if (w >= text_base && w < text_top) 
      putword( (w-text_base) | 0x80000000, fp );
    else 
      putword( w-data_base, fp );
  }
  else
    putword( w, fp );
}
#endif

#if defined( ENDIAN_BIG ) && defined( BITS_32 )

static void putword( word w, FILE *fp )
{
  if (putc( (w >> 24) & 0xFF, fp ) == EOF) THROW( HEAPIO_CANTWRITE );
  if (putc( (w >> 16) & 0xFF, fp ) == EOF) THROW( HEAPIO_CANTWRITE );
  if (putc( (w >> 8) & 0xFF, fp ) == EOF) THROW( HEAPIO_CANTWRITE );
  if (putc( w & 0xFF, fp ) == EOF) THROW( HEAPIO_CANTWRITE );
}

/* FIXME: does not check EOF */

static word getword( FILE *fp )
{
  word a = getc( fp );
  word b = getc( fp );
  word c = getc( fp );
  word d = getc( fp );

  return (a << 24) | (b << 16) | (c << 8) | d;
}

#else
#  error "Must write new putword() and getword()."
#endif  /* defined( ENDIAN_BIG ) && defined( BITS_32 ) */

/* eof */
