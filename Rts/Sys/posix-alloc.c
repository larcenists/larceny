/* Rts/Sys/posix-alloc.c 
 * Larceny Run-Time System  --  low-level memory allocator (Posix).
 *
 * $Id: posix-alloc.c,v 1.1.1.1 1998/11/19 21:51:42 lth Exp $
 *
 * This allocator handles memory allocation for Larceny and manages the
 * memory descriptor tables that are used by the collector and the write
 * barrier.  All allocation is done in page-sized chunks on page
 * boundaries; the allocator uses malloc()-level allocation for its 
 * low-level memory allocator for maximal portability.
 *
 * NOTE: This allocator does not provide address-order allocation of the
 * form needed for the fast write barrier used by the SPARC assembler.
 * A modified assembler must be used.
 *
 * It is expected that requests for memory will be large and infrequent; 
 * this allocator is not intended to be a replacement for malloc and friends.
 *
 * There are two descriptor tables: gclib_desc_g maps page numbers to
 * generation numbers, and gclib_desc_b maps page numbers to attribute bits.
 * The "page number" of an address is defined by the pageof() macro in 
 * gclib.h; it is the address minus the value of the global gclib_pagebase, 
 * all shifted right to throw away the low-order bits.
 *
 * There are two separate tables rather than one table of records in
 * order to simplify manipulation from assembly language (i.e., in the
 * write barrier).
 *
 * The low 16 bits of the attribute bits are reserved by the allocator;
 * the rest of the RTS can use the high bits.
 *
 * This code is not reentrant.
 */  

#define GC_INTERNAL

#include <stdlib.h>
#include "larceny.h"
#include "semispace_t.h"
#include "gclib.h"
#include "barrier.h"

#define TEST_POSIX_ALLOC  0
#define TEST_CHUNK_SIZE   (2*1024*1024)

/* Public globals */

unsigned *gclib_desc_g;              /* generation owner */
unsigned *gclib_desc_b;              /* attribute bits */
caddr_t  gclib_pagebase;             /* page address of lowest known word */

/* Private globals */

static struct {
  caddr_t    memtop;            /* address of highest known word */
  int        descriptor_slots;  /* number of allocated slots */
  unsigned   heap_bytes;        /* bytes allocated to heap */
  unsigned   max_heap_bytes;    /* max ditto */
  unsigned   remset_bytes;      /* bytes allocated to remset */
  unsigned   max_remset_bytes;  /* max ditto */
  unsigned   rts_bytes;         /* bytes allocated to RTS "other" */
  unsigned   max_rts_bytes;     /* max ditto */
} data;

#if TEST_POSIX_ALLOC
static void *test_chunk = 0;         /* anchor for low-address chunk */
#endif

static byte *gclib_alloc( unsigned bytes );
static void grow_table( byte *new_bot, byte *new_top );
static byte *alloc_aligned( unsigned bytes );
static void register_pointer( byte *derived, byte *original );
static byte *find_and_free_pointer( byte *x );

#define FREE_ALIGNED( x, bytes ) free( find_and_free_pointer( x ) )

/* Initialize descriptor tables and memory allocation pointers. */

void 
gclib_init( void ) 
{
  int i;

#if !TEST_POSIX_ALLOC
  data.descriptor_slots = 32*1024*1024 / PAGESIZE;   /* Slots to handle 32MB */
#else
  data.descriptor_slots = 2*1024*1024 / PAGESIZE;    /* Slots to handle 2 MB */
#endif

  gclib_desc_g =
    (unsigned*)must_malloc( sizeof( unsigned ) * data.descriptor_slots );
  gclib_desc_b =
    (unsigned*)must_malloc( sizeof( unsigned ) * data.descriptor_slots );

  for ( i = 0 ; i < data.descriptor_slots ; i++ )
    gclib_desc_g[i] = gclib_desc_b[i] = 0;

  /* Assume all allocation will happen following these allocations.
   * That doesn't have to be true, but it's an OK starting point.
   * gclib_alloc() will later slide the address ranges along as
   * necessary.
   */
  if ((long)gclib_desc_b > (long)gclib_desc_g)
    gclib_pagebase = data.memtop = 
      (caddr_t)roundup_page((byte*)(gclib_desc_b + data.descriptor_slots));
  else
    gclib_pagebase = data.memtop =
      (caddr_t)roundup_page((byte*)(gclib_desc_g + data.descriptor_slots));

#if TEST_POSIX_ALLOC
  test_chunk = must_malloc( TEST_CHUNK_SIZE );
#endif
}

void gclib_memory_range( caddr_t *lowest, caddr_t *highest )
{
  *lowest = gclib_pagebase;
  *highest = data.memtop;
}

void *gclib_alloc_heap( int bytes, int gen_no )
{
  byte *ptr;
  int i;

  bytes = roundup_page( bytes );
  ptr = gclib_alloc( bytes );

  for ( i = pageof( ptr ) ; i < pageof( ptr+bytes ) ; i++ ) {
    gclib_desc_g[i] = gen_no;
    gclib_desc_b[i] = MB_ALLOCATED | MB_HEAP_MEMORY;
  }
  data.heap_bytes += bytes;
  data.max_heap_bytes = max( data.max_heap_bytes, data.heap_bytes );

  supremely_annoyingmsg( "Allocated heap memory gen=%d bytes=%d addr=%p",
			 gen_no, bytes, (void*)ptr );

#if TEST_POSIX_ALLOC
  if (test_chunk != 0) {
    free( test_chunk );
    test_chunk = 0;
  }
#endif
  return (void*)ptr;
}

void *gclib_alloc_rts( int bytes, unsigned attribute )
{
  byte *ptr;
  int i;

  bytes = roundup_page( bytes );
  ptr = gclib_alloc( bytes );

  for ( i = pageof( ptr ) ; i < pageof( ptr+bytes ) ; i++ ) {
    gclib_desc_g[i] = RTS_OWNED_PAGE;
    gclib_desc_b[i] = MB_ALLOCATED | MB_RTS_MEMORY | attribute;
  }
  if (attribute & MB_REMSET) {
    data.remset_bytes += bytes;
    data.max_remset_bytes = max( data.max_remset_bytes, data.remset_bytes );
  }
  else {
    data.rts_bytes += bytes;
    data.max_rts_bytes = max( data.max_rts_bytes, data.rts_bytes );
  }
  return (void*)ptr;
}

/* The descriptor tables have to be expanded only when the allocated
 * address range cannot be captured by the descriptor table at its
 * current size.  Other times, the descriptor table can just be slid
 * over the active region.  We slide the table down only when a chunk
 * is allocated below the current pagebase.
 */
static byte *gclib_alloc( unsigned bytes )
{
  byte *ptr, *top;
  int i;
  caddr_t old_pagebase, old_memtop;

  assert( ( bytes % PAGESIZE) == 0 );
  ptr = alloc_aligned( bytes );
  top = ptr+bytes;

  if ((caddr_t)ptr < gclib_pagebase) {   /* new allocation below pagebase */
    old_pagebase = gclib_pagebase;

    if (pageof_pb( top-1, ptr ) >= data.descriptor_slots)
      grow_table( ptr, data.memtop );    /* changes gclib_pagebase */
    else {
      int diff;

      /* Slide the table down, i.e., move the entries up */

      annoyingmsg( "Low-level allocator: Sliding page table by %u entries.",
		   diff );

      diff = pageof_pb( gclib_pagebase, ptr );
      for ( i = data.descriptor_slots-1 ; i >= diff ; i-- ) {
	gclib_desc_g[i] = gclib_desc_g[i-diff];
	gclib_desc_b[i] = gclib_desc_b[i-diff];
      }

      gclib_pagebase = ptr;
    }

    /* Fill out table */
    for ( i = pageof( top ) ; i < pageof( old_pagebase ) ; i++ ) {
      gclib_desc_g[i] = FOREIGN_PAGE;
      gclib_desc_b[i] = MB_FOREIGN;
    }

    wb_re_setup( gclib_pagebase, gclib_desc_g );
  }
  else if ((caddr_t)top > data.memtop) {  /* new allocation above memtop */
    
    old_memtop = data.memtop;

    if (pageof( top-1 ) >= data.descriptor_slots) {
      grow_table( gclib_pagebase, top );  /* changes memtop */
      wb_re_setup( gclib_pagebase, gclib_desc_g );
    }
    else
      data.memtop = top;

    /* Fill out table */
    for ( i = pageof( old_memtop ) ; i < pageof( ptr ) ; i++ ) {
      gclib_desc_g[i] = FOREIGN_PAGE;
      gclib_desc_b[i] = MB_FOREIGN;
    }
  }

  return ptr;
}

static void grow_table( byte *new_bot, byte *new_top )
{
  unsigned *desc_g, *desc_b;
  int slots = max( pageof_pb( new_top, new_bot )-1, data.descriptor_slots*2 );
  int dest, i;

  assert( ((word)new_bot % PAGESIZE) == 0 );
  assert( ((word)new_top % PAGESIZE) == 0 );
  assert( (caddr_t)new_bot <= gclib_pagebase );
  assert( (caddr_t)new_top >= data.memtop );
  annoyingmsg( "Low-level allocator: Growing page tables; new slots=%u.",
	       slots );

  desc_g = (unsigned*)must_malloc( sizeof( unsigned ) * slots );
  desc_b = (unsigned*)must_malloc( sizeof( unsigned ) * slots );

  dest = pageof_pb( gclib_pagebase, new_bot );

  assert( dest < slots );
  assert( dest+data.descriptor_slots < slots );

  for ( i=0 ; i < dest ; i++ )
    desc_b[i] = desc_g[i] = 0;

  for ( i=0 ; i < data.descriptor_slots ; i++, dest++ ) {
    desc_b[dest] = gclib_desc_b[i];
    desc_g[dest] = gclib_desc_g[i];
  }

  for ( i=data.descriptor_slots ; i < slots ; i++ )
    desc_b[i] = desc_g[i] = 0;

  data.descriptor_slots = slots;
  gclib_pagebase = new_bot;
  data.memtop = new_top;

  free( gclib_desc_g );
  free( gclib_desc_b );
  gclib_desc_g = desc_g;
  gclib_desc_b = desc_b;
}

static byte *alloc_aligned( unsigned bytes )
{
  byte *p, *q;
  double wastage;

  p = (byte*)must_malloc( bytes+PAGESIZE );
  q = (byte*)roundup_page( p );
  wastage = (double)(q-p);
  register_pointer( q, p );
  return q;
}

void gclib_free( void *addr, int bytes )
{
  unsigned pages;
  unsigned pageno;

  FREE_ALIGNED( addr, bytes );

  pages = bytes/PAGESIZE;
  pageno = pageof( addr );

  /* This assumes that all pages being freed have the same major attributes.
   * That is a reasonable assumption.
   */
  if (pages > 0) {
    if (gclib_desc_b[pageno] & MB_HEAP_MEMORY)
      data.heap_bytes -= bytes;
    else if (gclib_desc_b[pageno] & MB_REMSET)
      data.remset_bytes -= bytes;
    else
      data.rts_bytes -= bytes;
  }

  while (pages > 0) {
    assert( (gclib_desc_b[pageno] & MB_ALLOCATED ) &&
	    !(gclib_desc_b[pageno] & MB_FOREIGN ) );
    gclib_desc_g[pageno] = UNALLOCATED_PAGE;
    gclib_desc_b[pageno] = MB_FOREIGN;
    pageno++;
    pages--;
  }
}

void gclib_shrink_block( void *p, int oldsize, int newsize )
{
  assert( oldsize >= newsize );

  /* This does not make sense in a malloc-based implementation */
}

void gclib_set_generation( void *address, int nbytes, int generation )
{
  int p;

  for ( p = pageof( address ) ; nbytes > 0 ; nbytes -= PAGESIZE, p++ )
    gclib_desc_g[p] = generation;
}

void gclib_add_attribute( void *address, int nbytes, unsigned attr )
{
  int p;

  for ( p = pageof( address ) ; nbytes > 0 ; nbytes -= PAGESIZE, p++ )
    gclib_desc_b[p] |= attr;
}

void gclib_stats( word *wheap, word *wremset, word *wrts, word *wmax_heap )
{
  *wheap = data.heap_bytes/sizeof(word);
  *wremset = data.remset_bytes/sizeof(word);
  *wrts = data.rts_bytes/sizeof(word);
  *wmax_heap = data.max_heap_bytes/sizeof(word);
}


/* Pointer registry for mapping pointers returned from malloc to pointers
   to page boundaries, and back.
   */

/* This is probably far too slow, but it's OK for now. */

struct regentry {
  byte *original;
  byte *derived;
};
static struct regentry *registry = 0;
static int reg_next = 0;
static int reg_size = 0;

static void register_pointer( byte *derived, byte *original )
{
  int i, j;

  if (reg_next == reg_size) {
    /* It's full, so compact it and see what happens */
    j = 0;
    for ( i=0 ; i < reg_size; i++ ) {
      if (registry[i].original != 0) {
	registry[j] = registry[i];
	j++;
      }
    }

    if (j < reg_size) {
      /* Compaction succeeded */
      reg_next = j;
    }
    else {
      /* Compaction failed: registry is full, so double its size. */
      struct regentry *new_reg;
      int k;

      k = max( 256, reg_size * 2 );
      new_reg = (struct regentry *)must_malloc( k*sizeof( struct regentry ) );
      for ( i=0 ; i < reg_size ; i++ )
	new_reg[i] = registry[i];
      if (registry != 0) free( registry );
      registry = new_reg;
      reg_size = k;
    }
  }
	
  registry[reg_next].original = original;
  registry[reg_next].derived = derived;
  reg_next++;
}

static byte *find_and_free_pointer( byte *derived )
{
  int i;
  byte *p;

  for ( i=0 ; i < reg_next && registry[i].derived != derived ; i++ )
    ;
  if (i == reg_next)
    panic_abort( "posix_alloc: pointer %p not found in registry.",
		 (void*)derived );
  p = registry[i].original;
  registry[i].original = registry[i].derived = 0;
  return p;
}

/* eof */
