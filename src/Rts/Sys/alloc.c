/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Low-level memory allocator (portable).
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
 * The allocator can be compiled in one of two modes.  
 *
 * If the preprocessor macro GCLIB_LARGE_TABLE is 0, then two page
 * tables that cover 32MB, each with word entries, are allocated at the
 * outset and grown as necessary.  The table gclib_desc_g maps page
 * numbers to generation numbers; the table gclib_desc_b maps page
 * numbers to attribute bits.  The "page number" of an address is
 * defined by the pageof() macro in gclib.h; it is the address minus the
 * value of the global gclib_pagebase, all shifted right to throw away
 * the low-order bits.
 *
 * There are two separate tables rather than one table of records in
 * order to simplify manipulation from assembly language (i.e., in the
 * write barrier).
 *
 * The low 16 bits of the attribute bits are reserved by the allocator;
 * the rest of the RTS can use the high bits.
 *
 * If the preprocessor macro GCLIB_LARGE_TABLE is not 0, then one page
 * table that covers the entire 4GB address range, with one byte entry
 * for each page, is allocated at the outset and is never grown or moved.
 * The table is called gclib_desc_g.  The six low bits map the page number
 * to a generation number, and the two high bits are attribute bit: 128
 * means that the page is allocated to the large-object space, and 64 means
 * that the page is allocated to the remembered set.  Generation numbers
 * above 60 are used to store additional information.
 *
 * The single large table is expected to reduce the cost of the GC-time write
 * barrier; it is being evaluated.  By dispensing with gclib_pagebase and 
 * by never changing the value of gclib_desc_g, the page number computation
 * can be sped up.  This also improves the normal write barrier slightly.
 *
 * Client should  use the gen_of() and attr_of() macros to access the tables.
 *
 * The value of GCLIB_LARGE_OBJECT is selected in Sys/config.h.
 *
 * FIXME: This code is not reentrant.
 *
 * FIXME: Uses of caddr_t should be replaced by uses of byte*, but the
 *   effects are nonlocal so don't do it yet.  
 */

#define GC_INTERNAL

/* Set this to 1 to set every word produced by the low level allocator
 * to the bit pattern 0x01010101, which will look to the runtime like
 * a pointer to a (probably) non-existant pair
 */
#define DEMONIC_PREINIT 0

#include <stdlib.h>
#include <string.h>
#include "larceny.h"
#include "stats.h"
#include "gclib.h"
#include "barrier.h"

#if GCLIB_LARGE_TABLE
# define FOREIGN_PAGE       60
# define UNALLOCATED_PAGE   61
# define RTS_OWNED_PAGE     62
#else
# define FOREIGN_PAGE       ((gclib_desc_t)-1)    /* Unknown owner */
# define UNALLOCATED_PAGE   ((gclib_desc_t)-2)    /* Larceny owns it */
# define RTS_OWNED_PAGE     ((gclib_desc_t)-3)    /* Larceny owns it */
#endif

/* Public globals */

gclib_desc_t *gclib_desc_g;	/* generation owner */
#if !GCLIB_LARGE_TABLE
gclib_desc_t *gclib_desc_b;	/* attribute bits */
caddr_t      gclib_pagebase;	/* page address of lowest known word */
#endif

/* Private globals */

static struct {
  byte *membot;		/* address of lowest known word */
  byte *memtop;		/* address of highest known word */
  byte *heapbot;
  byte *heaplim;
  
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
  int     mem_bytes;		/* amount of heap + remset + RTS + frag */
  int     max_mem_bytes;	/* max ditto */
} data;

static byte *gclib_alloc( unsigned bytes );
#if !GCLIB_LARGE_TABLE
static void allocation_below_membot( byte *ptr, int bytes );
static void allocation_above_memtop( byte *ptr, int bytes );
static void grow_table( byte *new_bot, byte *new_top );
#endif
static byte *alloc_aligned( unsigned bytes );
static void free_aligned( byte *p, unsigned bytes );
static void update_mem_bytes( void );

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
  data.rts_bytes += sizeof(gclib_desc_t)*data.descriptor_slots;
#if !GCLIB_LARGE_TABLE
  gclib_desc_b =
    (gclib_desc_t*)must_malloc( sizeof(gclib_desc_t) * data.descriptor_slots );
  data.rts_bytes += sizeof(gclib_desc_t)*data.descriptor_slots;
#endif
  
  for ( i = 0 ; i < data.descriptor_slots ; i++ ) {
    gclib_desc_g[i] = FOREIGN_PAGE;
#if !GCLIB_LARGE_TABLE
    gclib_desc_b[i] = MB_FOREIGN;
#endif
  }

  /* Leave these explicitly uninitialized until first allocation. */
  data.memtop = data.membot = 0;
  data.heapbot = data.heaplim = 0;
#if !GCLIB_LARGE_TABLE
  gclib_pagebase = 0;
#endif
}

/* This needs to be somewhat accurate -- returning the entire address
   space would be bad.
   */
void gclib_memory_range( caddr_t *lowest, caddr_t *highest )
{
#if 0
  *lowest = data.membot;
  *highest = data.memtop;
#else
  *lowest = (caddr_t)data.heapbot;
  *highest = (caddr_t)data.heaplim;
#endif
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
    gclib_desc_g[i] = gen_no;
#if !GCLIB_LARGE_TABLE
    gclib_desc_b[i] = MB_ALLOCATED | MB_HEAP_MEMORY;
#endif
  }
  data.heap_bytes += bytes;
  data.max_heap_bytes = max( data.max_heap_bytes, data.heap_bytes );

  supremely_annoyingmsg( "Allocated heap memory gen=%d bytes=%d addr=%p",
			 gen_no, bytes, (void*)ptr );

  update_mem_bytes();

  if (data.heapbot == 0 || ptr < data.heapbot) data.heapbot = ptr;
  if (data.heaplim == 0 || ptr+bytes > data.heaplim) data.heaplim = ptr+bytes;
  return (void*)ptr;
}

void *gclib_alloc_rts( int bytes, unsigned attribute )
{
  byte *ptr;
  int i;

  bytes = roundup_page( bytes );
  ptr = gclib_alloc( bytes );

  for ( i = pageof( ptr ) ; i < pageof( ptr+bytes ) ; i++ ) {
#if GCLIB_LARGE_TABLE
    gclib_desc_g[i] = (MB_MASK & attribute) | RTS_OWNED_PAGE;
#else
    gclib_desc_g[i] = RTS_OWNED_PAGE;
    gclib_desc_b[i] = MB_ALLOCATED | MB_RTS_MEMORY | attribute;
#endif
  }
  if (attribute & MB_REMSET) {
    data.remset_bytes += bytes;
    data.max_remset_bytes = max( data.max_remset_bytes, data.remset_bytes );
  }
  else {
    data.rts_bytes += bytes;
    data.max_rts_bytes = max( data.max_rts_bytes, data.rts_bytes );
  }

  update_mem_bytes();
  return (void*)ptr;
}

/* Approximate but very close */
static void update_mem_bytes( void )
{
  data.mem_bytes = 
    data.heap_bytes + data.remset_bytes + data.rts_bytes + data.wastage_bytes;
  data.max_mem_bytes = max( data.max_mem_bytes, data.mem_bytes );
}

/* The descriptor tables have to be expanded only when the allocated
 * address range cannot be captured by the descriptor table at its
 * current size.  Other times, the descriptor table can just be slid
 * over the active region.  We slide the table down only when a chunk
 * is allocated below the current pagebase.  On system where the heap
 * grows down (MacOS, Solaris with mmap()) it would be better to slide
 * more aggressively.  FIXME.
 */
static byte *gclib_alloc( unsigned bytes )
{
  byte *ptr, *top;
  
  assert( ( bytes % PAGESIZE) == 0 );
  ptr = alloc_aligned( bytes );
  top = ptr+bytes;

#if GCLIB_LARGE_TABLE
  if (data.membot == 0 || ptr < data.membot) data.membot = ptr;
  if (data.memtop == 0 || top > data.memtop) data.memtop = top;
#else
  assert( gclib_pagebase == 0 || (byte*)gclib_pagebase == data.membot );
  
  if (gclib_pagebase == 0) {
    gclib_pagebase = (caddr_t)ptr;
    data.membot = ptr;
    data.memtop = top;
  }
  else if (ptr < data.membot)
    allocation_below_membot( ptr, bytes );
  else if (top > data.memtop)
    allocation_above_memtop( ptr, bytes );
#endif
  
  return ptr;
}

#if !GCLIB_LARGE_TABLE
static void allocation_below_membot( byte *ptr, int bytes )
{
  int i;
  byte *old_pagebase;
  byte *top = ptr + bytes;
  
  old_pagebase = (byte*)gclib_pagebase;

  if (pageof_pb( data.memtop-1, ptr ) >= data.descriptor_slots)
    grow_table( ptr, data.memtop );    /* changes gclib_pagebase 
					         and data.membot */
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
    data.membot = (byte*)ptr;
  }

  /* Fill out table */
  for ( i = pageof( top ) ; i < pageof( old_pagebase ) ; i++ ) {
    gclib_desc_g[i] = FOREIGN_PAGE;
    gclib_desc_b[i] = MB_FOREIGN;
  }

  wb_re_setup( (byte*)gclib_pagebase, gclib_desc_g );
}

static void allocation_above_memtop( byte *ptr, int bytes )
{
  int i;
  byte *old_memtop;
  byte *top;
  
  top = ptr + bytes;
  old_memtop = data.memtop;

  if (pageof( top-1 ) >= data.descriptor_slots) {
    grow_table( (byte*)gclib_pagebase, top );  /* changes data.memtop */
    wb_re_setup( (byte*)gclib_pagebase, gclib_desc_g );
  }
  else
    data.memtop = top;

  /* Fill out table */
  for ( i = pageof( old_memtop ) ; i < pageof( ptr ) ; i++ ) {
    gclib_desc_g[i] = FOREIGN_PAGE;
    gclib_desc_b[i] = MB_FOREIGN;
  }
}

static void grow_table( byte *new_bot, byte *new_top )
{
  gclib_desc_t *desc_g, *desc_b;
  int slots, dest;

  assert( (word)new_bot % PAGESIZE == 0 &&
          (word)new_top % PAGESIZE == 0 &&
          new_bot <= (byte*)gclib_pagebase &&
          new_top >= data.memtop );

  slots = max( pageof_pb( new_top, new_bot ), data.descriptor_slots*2 );
  annoyingmsg( "Growing page tables -- new slots=%d.", slots );

  desc_g = (gclib_desc_t*)must_malloc( sizeof( gclib_desc_t ) * slots );
  desc_b = (gclib_desc_t*)must_malloc( sizeof( gclib_desc_t ) * slots );
  data.mem_bytes += sizeof(gclib_desc_t)*slots*2;
  data.rts_bytes += sizeof(gclib_desc_t)*slots*2;
  
  /* The slot in the new table at which to start copying the old table */
  dest = pageof_pb( gclib_pagebase, new_bot );

  assert( dest < slots && 
          dest+data.descriptor_slots-1 < slots );

  memset( desc_b, 0, sizeof(gclib_desc_t)*slots );
  memcpy( desc_b+dest, gclib_desc_b, 
	  sizeof(gclib_desc_t)*data.descriptor_slots );
  free( gclib_desc_b );
  data.mem_bytes -= sizeof(gclib_desc_t)*data.descriptor_slots;
  data.rts_bytes -= sizeof(gclib_desc_t)*data.descriptor_slots;
  gclib_desc_b = desc_b;

  memset( desc_g, 0, sizeof(gclib_desc_t)*slots );
  memcpy( desc_g+dest, gclib_desc_g,
	  sizeof(gclib_desc_t)*data.descriptor_slots );
  free( gclib_desc_g );
  data.mem_bytes -= sizeof(gclib_desc_t)*data.descriptor_slots;
  data.rts_bytes -= sizeof(gclib_desc_t)*data.descriptor_slots;
  gclib_desc_g = desc_g;

  data.descriptor_slots = slots;
  gclib_pagebase = (caddr_t)new_bot;
  data.membot = new_bot;
  data.memtop = (byte*)new_top;
}
#endif

void gclib_free( void *addr, int bytes )
{
  unsigned pages;
  unsigned pageno;

  assert( (word)addr % PAGESIZE == 0 );

  bytes = roundup_page( bytes );

  supremely_annoyingmsg( "Freeing: bytes=%d addr=%p", bytes, (void*)addr );
  
  free_aligned( addr, bytes );
  data.wastage_bytes -= PAGESIZE;

  pages = bytes/PAGESIZE;
  pageno = pageof( addr );

  /* This assumes that all pages being freed have the same major attributes.
   * That is a reasonable assumption.
   */
  if (pages > 0) {
#if GCLIB_LARGE_TABLE
    if ((gclib_desc_g[pageno] & ~MB_MASK) == RTS_OWNED_PAGE)
      if (gclib_desc_g[pageno] & MB_REMSET)
	data.remset_bytes -= bytes;
      else
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

  update_mem_bytes();
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
    gclib_desc_g[p] = (gclib_desc_g[p] & MB_MASK) | generation;
#else
    gclib_desc_g[p] = generation;
#endif
  }
}

void gclib_add_attribute( void *address, int nbytes, unsigned attr )
{
  int p;

#if GCLIB_LARGE_TABLE
  attr = attr & MB_MASK;
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
  stats->mem_allocated          = bytes2words( data.mem_bytes );
  stats->mem_allocated_max      = bytes2words( data.max_mem_bytes );
  stats->heap_limit             = bytes2words( data.heap_bytes_limit );
}


/* Very lowest level allocator */

static byte *alloc_aligned( unsigned bytes )
{
  byte *p;
  
  assert( bytes % PAGESIZE == 0 );

  p = (byte*)osdep_alloc_aligned( bytes );
  if (DEMONIC_PREINIT) { int i; for(i=0; i<bytes; i++) p[i] = 1; }
  data.wastage_bytes = osdep_fragmentation();
  data.max_wastage_bytes = max( data.max_wastage_bytes, data.wastage_bytes );
  return p;
}

static void free_aligned( byte *p, unsigned bytes )
{
  assert( bytes % PAGESIZE == 0 );

  osdep_free_aligned( (void*)p, bytes );
  data.wastage_bytes = osdep_fragmentation();
}

/* eof */
