/* Rts/Sys/heapio.c
 * Larceny run-time system -- heap I/O procedures
 *
 * $Id: heapio.c,v 1.4 1997/09/17 15:17:26 lth Exp $
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
 *   Intergenerational pointers (data -> text) have high bit set. All 
 *   pointers are adjusted relative to 0 of the heap they point to.
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
 * BUGS (from the old version)
 * - Knows a little bit about the globals which delimit the heap 
 *   areas; this should be abstracted away eventually.
 * - Assumes that the pad words are not garbage; this happens to be true
 *   after a collection (by design), but it means you cannot dump without
 *   collecting first.
 */

#define HEAPIO_INTERNAL

#include <stdio.h>
#include <setjmp.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "heapio.h"

static jmp_buf EX_heapio_ex;

#define CATCH( var )  if ((var = setjmp( EX_heapio_ex )) != 0)
#define THROW( val )  longjmp( EX_heapio_ex, val )

static int openheap( heapio_t *h, const char *filename );
static int createheap( heapio_t *h, const char *filename, int type );
static int closeheap( heapio_t *h );
static int heaptype( heapio_t *h );
static int load_bootstrap( heapio_t *heap, word *text_base, word *data_base,
			   word *globals );
static int dump_bootstrap( heapio_t *h, semispace_t *text, semispace_t *data, 
			   word *globals );
static int load_roots( heapio_t *h, word *globals );

/* Internal */
static void init_heapio( heapio_t *h );

heapio_t *create_heapio( void )
{
  heapio_t *h;

  h = (heapio_t*)must_malloc( sizeof( heapio_t ) );
  h->open = openheap;
  h->create = createheap;
  h->close = closeheap;
  h->type = heaptype;
  h->load_bootstrap = load_bootstrap;
  h->dump_bootstrap = dump_bootstrap;
  h->load_roots = load_roots;

/*
  h->dump = ...;
  h->read_data = ...;
  h->read_ptr = ...;
  h->read_word = ...;
  h->write_data = ...;
  h->write_ptr = ...;
  h->write_word = ...;
*/
  
  init_heapio( h );

  return h;
}


static void
init_heapio( heapio_t *h )
{
  h->fp = 0;
  h->metadata = 0;
  h->pagetable = 0;
  h->input = 0;
  h->output = 0;
}


void delete_heapio( heapio_t *h )
{
  if (h->fp != 0)
    closeheap( h );
  free( h );
}


static metadata_block_t *read_metadata( FILE *fp, word words );
static int load_text( heapio_t *h, word *base, unsigned count );
static int load_data( heapio_t *h, word *text, word *data, unsigned count );
static void putheader( FILE*, word, word, word, word*, word );
static void put_tagged_word( word, FILE*, word, word, word );
static void dump_text( word, word, FILE* );
static void dump_data( word, word, word, word, FILE* );
static word getword( FILE *fp );
static word putword( word, FILE* );


static int
openheap( heapio_t *h, const char *filename )
{
  unsigned vno, metadata_size;
  int i, j, r;
  FILE *fp;

  if (h->fp != 0) closeheap( h );

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

  h->split_heap = 0;
  h->bootstrap_heap = 0;

  switch ((h->magic >> 16) & 0xFFFF) {
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
    metadata_size = getword( fp );
    h->data_pages = getword( fp );
    h->metadata = read_metadata( fp, metadata_size );
    if (h->metadata == 0) {
      fclose( fp );
      return HEAPIO_CANTREAD;
    }
    break;
  default:
    fclose( fp );
    return HEAPIO_WRONGTYPE;
  }
  h->fp = fp;
  h->input = 1;
  return HEAPIO_OK;
}

static int
createheap( heapio_t *h, const char *filename, int type )
{
  if (h->fp != 0) closeheap( h );

  h->fp = fopen( filename, "wb" );
  if (h->fp == 0)
    return HEAPIO_CANTOPEN;

  h->split_heap = 0;
  h->bootstrap_heap = 0;

  switch (type) {
  case HEAP_SINGLE :
    h->bootstrap_heap = 1;
    break;
  case HEAP_SPLIT :
    h->bootstrap_heap = 1;
    h->split_heap = 1;
    break;
  case HEAP_DUMPED :
    h->metadata = 0;
    h->pagetable = 0;
    break;
  default :
    fclose( h->fp );
    return HEAPIO_WRONGTYPE;
  }
  h->magic = (type << 16) | HEAP_VERSION;
  h->output = 1;
  return HEAPIO_OK;
}

static int
closeheap( heapio_t *h )
{
  if (h->fp != 0) {
    fclose( h->fp );
    h->fp = 0;
    if (!h->bootstrap_heap) {
      metadata_block_t *p = h->metadata, *q;
      while (p != 0) {
	if (p->data) free( p->data );
	q = p;
	p = p->next;
	free( q );
      }
    }
  }
  init_heapio( h );
  return HEAPIO_OK;
}

static int heaptype( heapio_t *h )
{
  if (h->bootstrap_heap)
    if (h->split_heap)
      return HEAP_SPLIT;
    else
      return HEAP_SINGLE;
  else 
    return HEAP_DUMPED;
}

static int
load_bootstrap( heapio_t *h, word *text_base, word *data_base, word *globals )
{
  unsigned i, j, r;

  if (h->fp == 0 || !h->input)
    return HEAPIO_NOTOPEN;

  if (!h->bootstrap_heap)
    return HEAPIO_WRONGTYPE;

  for ( i=FIRST_ROOT, j=0 ; i<=LAST_ROOT ; i++, j++ ) {
    if (isptr( h->roots[j] )) {
      if (h->roots[j] & 0x80000000)
	globals[i] = (h->roots[j] & ~0x80000000) + (word)text_base;
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

/* Internal */
static int
load_text( heapio_t *h, word *text_base, unsigned count )
{
  word n;

  n = fread( (char*)text_base, sizeof( word ), count, h->fp );
  if (n < count)
    return HEAPIO_CANTREAD;
}

/* Internal */
static int
load_data( heapio_t *h, word *text_base, word *data_base, unsigned count )
{
  word *p, w;
  unsigned i;

  if (fread( (char*)data_base, sizeof( word ), count, h->fp ) < count)
    return HEAPIO_CANTREAD;

  p = data_base;
  while (count > 0) {
    w = *p;
    count--;
    if (isptr( w )) {
      if (w & 0x80000000)
	*p = (w & ~0x80000000) + (word)text_base;
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

static metadata_block_t *read_metadata( FILE *fp, word words )
{
  word *p = (word *)must_malloc( words*sizeof(word) ), *wp, *limit;
  metadata_block_t *first = 0, *last = 0, *block;
  word *data;
  int r;

  r = fread( p, words, sizeof(word), fp );
  if (r < words) { free(p); return 0; }

  wp = p; 
  limit = p+words;
  while (wp < limit) {
    block = (metadata_block_t *)must_malloc( sizeof( metadata_block_t ) );
    block->metadata_length = p[0];
    block->data_pages = p[1];
    block->code = p[2];
    if (block->metadata_length > 0) {
      block->data = (word*)must_malloc( block->metadata_length*sizeof(word) );
      block->next = 0;
      memcpy( block->data, p+3, block->metadata_length*sizeof(word) );
    }
    else
      block->data = 0;
    if (first == 0)
      first = last = block;
    else
      last = last->next = block;
    wp += 3+block->metadata_length;
  }
  free( p );
  return first;
}

/* Dumped heaps */
static int load_roots( heapio_t *h, word *globals )
{
  int i, j;

#if 1
  panic( "heapio.c: load_roots" );
#else
  for ( i=FIRST_ROOT, j=0 ; i<=LAST_ROOT ; i++, j++ ) {
    if (isptr( h->roots[j] )) 
      globals[i] = adjust_ptr( h->roots[j] );
    else
      globals[ i ] = h->roots[j];
  }
#endif
  return 0;
}


static int
dump_bootstrap( heapio_t *h, semispace_t *text, semispace_t *data, 
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

/* Internal */
static void
dump_text( word text_base, word text_top, FILE *fp )
{
  if (fwrite( (char*)text_base, 1, text_top-text_base, fp ) 
      != text_top-text_base) 
    THROW( HEAPIO_CANTWRITE );
}

/* Internal */
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

/* Internal */
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

/* Internal */
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

static int
dump_dumped_image( char *filename, gc_t *gc, word *globals )
{
#if 0
  FILE *fp;
  word magic;

  fp = fopen( filename, "wb" );
  if (fp == 0) return -1;

  magic = (2 << 16) | HEAP_VERSION;
  pages = gc->live_pages( gc );      /* computes total live pages */
  <compute page table>
  gc->prepare_dump_all( gc, ... );   /* calls all metadata functions */
  
  <write header>
  <write metadata>
  <write pagetable>
  <dump data segments>
 
		      
  /*
     1. calculate the total number of pages needed
     2. allocate and initialize the page table
     3. 
     */

  
  /* ...; */
#endif
  panic( "DUMP_DUMPED_IMAGE" );
}


#if defined( ENDIAN_BIG ) && defined( BITS_32 )

static word putword( word w, FILE *fp )
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
  #error "Must write new putword() and getword()."
#endif  /* defined( ENDIAN_BIG ) && defined( BITS_32 ) */

/* eof */
