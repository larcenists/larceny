/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny Run-Time System  --  low-level memory allocator (Posix).
 *
 * This allocator handles memory allocation for Larceny and manages the
 * memory descriptor tables that are used by the collector and the write
 * barrier.  All allocation is done in page-sized chunks on page
 * boundaries; the allocator uses malloc()-level allocation for its 
 * low-level memory allocator for maximal portability.
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
 *
 * FIXME: uses of caddr_t should be replaced by uses of byte*, but the
 *   effects are nonlocal so don't do it yet.
 */  

#define GC_INTERNAL

#include <stdlib.h>
#include <string.h>
#include "larceny.h"
#include "stats.h"
#include "gclib.h"
#include "barrier.h"

/* Public globals */

gclib_desc_t *gclib_desc_g;	/* generation owner */
#if !GCLIB_LARGE_TABLE
gclib_desc_t *gclib_desc_b;	/* attribute bits */
caddr_t  gclib_pagebase;	/* page address of lowest known word */
#endif

/* Private globals */

static struct {
  caddr_t membot;		/* address of lowest known word */
  caddr_t memtop;		/* address of highest known word */
  int     descriptor_slots;	/* number of allocated slots */
  int     heap_bytes_limit;	/* Maximum allowed heap allocation */
  int     heap_bytes;		/* bytes allocated to heap */
  int     max_heap_bytes;	/* max ditto */
  int     remset_bytes;		/* bytes allocated to remset */
  int     max_remset_bytes;	/* max ditto */
  int     rts_bytes;		/* bytes allocated to RTS "other" */
  int     max_rts_bytes;	/* max ditto */
  int     wastage_bytes;	/* amount of wasted space */
  int     max_wastage_bytes;	/* max ditto */
} data;

static byte *gclib_alloc( unsigned bytes );
#if !GCLIB_LARGE_TABLE
static void grow_table( byte *new_bot, byte *new_top );
#endif
static byte *alloc_aligned( unsigned bytes );
static void register_pointer( byte *derived, byte *original );
static byte *find_and_free_pointer( byte *x );

#define FREE_ALIGNED( x, bytes ) free( find_and_free_pointer( x ) )

/* Initialize descriptor tables and memory allocation pointers. */

void 
gclib_init( void ) 
{
  int i;

#if GCLIB_LARGE_TABLE
  data.descriptor_slots = 4096*(1024*1024 / PAGESIZE);/* Slots to handle 4GB */
#else
  data.descriptor_slots = 32*1024*1024 / PAGESIZE;   /* Slots to handle 32MB */
#endif
  
  gclib_desc_g =
    (gclib_desc_t*)must_malloc( sizeof(gclib_desc_t) * data.descriptor_slots );
#if !GCLIB_LARGE_TABLE
  gclib_desc_b =
    (gclib_desc_t*)must_malloc( sizeof(gclib_desc_t) * data.descriptor_slots );
#endif
  
  for ( i = 0 ; i < data.descriptor_slots ; i++ ) {
    gclib_desc_g[i] = 0;
#if !GCLIB_LARGE_TABLE
    gclib_desc_b[i] = 0;
#endif
  }

  /* Assume all allocation will happen following these allocations.
   * That doesn't have to be true; gclib_alloc() will later slide the
   * address ranges as necessary.
   */
#if !GCLIB_LARGE_TABLE
  if ((long)gclib_desc_b > (long)gclib_desc_g)
    data.membot = data.memtop = 
      (caddr_t)roundup_page((byte*)(gclib_desc_b + data.descriptor_slots));
  else
#endif
    data.membot = data.memtop =
      (caddr_t)roundup_page((byte*)(gclib_desc_g + data.descriptor_slots));
#if !GCLIB_LARGE_TABLE
  gclib_pagebase = data.memtop;
#endif
}

/* This needs to be somewhat accurate -- returning the entire address
   space would be bad.
   */
void gclib_memory_range( caddr_t *lowest, caddr_t *highest )
{
  *lowest = data.membot;
  *highest = data.memtop;
}

void gclib_set_heap_limit( int bytes )
{
  data.heap_bytes_limit = bytes;
}

void *gclib_alloc_heap( int bytes, int gen_no )
{
  byte *ptr;
  int i;

  bytes = roundup_page( bytes );

  if (data.heap_bytes_limit > 0 &&
      data.heap_bytes + bytes > data.heap_bytes_limit) {
    memfail( MF_HEAP, "Hard heap limit exceeded by request for %d bytes.\n"
	    "Current size is %d bytes.", 
	     bytes, data.heap_bytes );
  }

  ptr = gclib_alloc( bytes );

  for ( i = pageof( ptr ) ; i < pageof( ptr+bytes ) ; i++ ) {
#if GCLIB_LARGE_TABLE
    gclib_desc_g[i] = gen_no;
#else
    gclib_desc_g[i] = gen_no;
    gclib_desc_b[i] = MB_ALLOCATED | MB_HEAP_MEMORY;
#endif
  }
  data.heap_bytes += bytes;
  data.max_heap_bytes = max( data.max_heap_bytes, data.heap_bytes );

  supremely_annoyingmsg( "Allocated heap memory gen=%d bytes=%d addr=%p",
			 gen_no, bytes, (void*)ptr );

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
#if !GCLIB_LARGE_TABLE
    gclib_desc_b[i] = MB_ALLOCATED | MB_RTS_MEMORY | attribute;
#endif
  }
#if !GCLIB_LARGE_TABLE
  if (attribute & MB_REMSET) {
    data.remset_bytes += bytes;
    data.max_remset_bytes = max( data.max_remset_bytes, data.remset_bytes );
  }
  else 
#endif
  {
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

#if GCLIB_LARGE_TABLE
  if ((caddr_t)ptr < data.membot) data.membot = (caddr_t)ptr;
  if ((caddr_t)top > data.memtop) data.memtop = (caddr_t)top;
#else
  if ((caddr_t)ptr < gclib_pagebase) {   /* new allocation below pagebase */
    old_pagebase = gclib_pagebase;

    if (pageof_pb( top-1, ptr ) >= data.descriptor_slots)
      grow_table( ptr, (byte*)data.memtop );    /* changes gclib_pagebase */
    else {
      /* Slide the table down, i.e., move the entries up */
      int diff = pageof_pb( gclib_pagebase, ptr );

      annoyingmsg( "Low-level allocator: Sliding page table by %d entries.",
		   diff );

      for ( i = data.descriptor_slots-1 ; i >= diff ; i-- ) {
	gclib_desc_g[i] = gclib_desc_g[i-diff];
	gclib_desc_b[i] = gclib_desc_b[i-diff];
      }
      gclib_pagebase = (caddr_t)ptr;
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
      grow_table( (byte*)gclib_pagebase, top );  /* changes memtop */
      wb_re_setup( gclib_pagebase, gclib_desc_g );
    }
    else
      data.memtop = (caddr_t)top;

    /* Fill out table */
    for ( i = pageof( old_memtop ) ; i < pageof( ptr ) ; i++ ) {
      gclib_desc_g[i] = FOREIGN_PAGE;
      gclib_desc_b[i] = MB_FOREIGN;
    }
  }
#endif
  
  return ptr;
}

#if !GCLIB_LARGE_TABLE
static void grow_table( byte *new_bot, byte *new_top )
{
  unsigned *desc_g, *desc_b;
  int slots, dest;

  assert( (word)new_bot % PAGESIZE == 0 &&
          (word)new_top % PAGESIZE == 0 &&
          (caddr_t)new_bot <= gclib_pagebase &&
          (caddr_t)new_top >= data.memtop );

  slots = max( pageof_pb( new_top, new_bot ), data.descriptor_slots*2 );
  annoyingmsg( "Growing page tables -- new slots=%d.", slots );

  desc_g = (unsigned*)must_malloc( sizeof( unsigned ) * slots );
  desc_b = (unsigned*)must_malloc( sizeof( unsigned ) * slots );

  /* The slot in the new table at which to start copying the old table */
  dest = pageof_pb( gclib_pagebase, new_bot );

  assert( dest < slots && 
          dest+data.descriptor_slots-1 < slots );

  memset( desc_b, 0, sizeof(unsigned)*slots );
  memcpy( desc_b+dest, gclib_desc_b, sizeof(unsigned)*data.descriptor_slots );
  free( gclib_desc_b );
  gclib_desc_b = desc_b;

  memset( desc_g, 0, sizeof(unsigned)*slots );
  memcpy( desc_g+dest, gclib_desc_g, sizeof(unsigned)*data.descriptor_slots );
  free( gclib_desc_g );
  gclib_desc_g = desc_g;

  data.descriptor_slots = slots;
  gclib_pagebase = (caddr_t)new_bot;
  data.memtop = (caddr_t)new_top;
}
#endif

static byte *alloc_aligned( unsigned bytes )
{
  byte *p, *q;

  p = (byte*)must_malloc( bytes+PAGESIZE );
  q = (byte*)roundup_page( p );
  data.wastage_bytes += PAGESIZE;
  data.max_wastage_bytes = max( data.max_wastage_bytes, data.wastage_bytes );
  register_pointer( q, p );
  return q;
}

void gclib_free( void *addr, int bytes )
{
  unsigned pages;
  unsigned pageno;

  assert( (word)addr % PAGESIZE == 0 );

  bytes = roundup_page( bytes );

  supremely_annoyingmsg( "Freeing: bytes=%d addr=%p", bytes, (void*)addr );
  
  FREE_ALIGNED( addr, bytes );
  data.wastage_bytes -= PAGESIZE;

  pages = bytes/PAGESIZE;
  pageno = pageof( addr );

  /* This assumes that all pages being freed have the same major attributes.
   * That is a reasonable assumption.
   */
  if (pages > 0) {
#if GCLIB_LARGE_TABLE
    if (gclib_desc_g[pageno] == RTS_OWNED_PAGE)
      data.rts_bytes -= bytes;
    else
      data.heap_bytes -= bytes;
#else
    if (gclib_desc_b[pageno] & MB_HEAP_MEMORY)
      data.heap_bytes -= bytes;
    else if (gclib_desc_b[pageno] & MB_REMSET)
      data.remset_bytes -= bytes;
    else
      data.rts_bytes -= bytes;
#endif
  }

  while (pages > 0) {
#if !GCLIB_LARGE_TABLE
    assert( (gclib_desc_b[pageno] & MB_ALLOCATED ) &&
	    !(gclib_desc_b[pageno] & MB_FOREIGN ) );
    gclib_desc_b[pageno] = MB_FOREIGN;
#endif
    gclib_desc_g[pageno] = UNALLOCATED_PAGE;
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

  for ( p = pageof( address ) ; nbytes > 0 ; nbytes -= PAGESIZE, p++ ) {
#if GCLIB_LARGE_TABLE
    gclib_desc_g[p] = (gclib_desc_g[p] & MB_LARGE_OBJECT) | generation;
#else
    gclib_desc_g[p] = generation;
#endif
  }
}

void gclib_add_attribute( void *address, int nbytes, unsigned attr )
{
  int p;

#if GCLIB_LARGE_TABLE
  attr = attr & MB_LARGE_OBJECT;
#endif
  
  for ( p = pageof( address ) ; nbytes > 0 ; nbytes -= PAGESIZE, p++ ) {
#if GCLIB_LARGE_TABLE
    gclib_desc_g[p] |= attr;
#else
    gclib_desc_b[p] |= attr;
#endif
  }
}

void gclib_stats( gclib_stats_t *stats )
{
  stats->heap_allocated         = bytes2words( data.heap_bytes );
  stats->heap_allocated_max     = bytes2words( data.max_heap_bytes );
  stats->remset_allocated       = bytes2words( data.remset_bytes );
  stats->remset_allocated_max   = bytes2words( data.max_remset_bytes );
  stats->rts_allocated          = bytes2words( data.rts_bytes );
  stats->rts_allocated_max      = bytes2words( data.max_rts_bytes );
  stats->heap_fragmentation     = bytes2words( data.wastage_bytes );
  stats->heap_fragmentation_max = bytes2words( data.max_wastage_bytes );
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
