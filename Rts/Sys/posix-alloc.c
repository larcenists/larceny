/* Rts/Sys/posix-alloc.c 
 * Larceny Run-Time System  --  low-level memory allocator (Posix).
 *
 * $Id: posix-alloc.c,v 1.1 1997/09/23 19:57:44 lth Exp lth $
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
#include "macros.h"
#include "gclib.h"
#include "assert.h"
#include "barrier.h"

#define TEST_POSIX_ALLOC  0
#define TEST_CHUNK_SIZE   (2*1024*1024)

/* Public globals */

unsigned *gclib_desc_g;              /* generation owner */
unsigned *gclib_desc_b;              /* attribute bits */
caddr_t  gclib_pagebase;             /* page address of lowest known word */

/* Private globals */

static caddr_t    memtop;            /* address of highest known word */
static unsigned   descriptor_slots;  /* number of allocated slots */
static unsigned   heap_bytes;        /* bytes allocated to heap */
static unsigned   max_heap_bytes;    /* max ditto */
static unsigned   remset_bytes;      /* bytes allocated to remset */
static unsigned   max_remset_bytes;  /* max ditto */
static unsigned   rts_bytes;         /* bytes allocated to RTS "other" */
static unsigned   max_rts_bytes;     /* max ditto */
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
  descriptor_slots = 32*1024*1024 / PAGESIZE;   /* Slots to handle 32 MB */
#else
  descriptor_slots = 2*1024*1024 / PAGESIZE;    /* Slots to handle 2 MB */
#endif

  gclib_desc_g =
    (unsigned*)must_malloc( sizeof( unsigned ) * descriptor_slots );
  gclib_desc_b =
    (unsigned*)must_malloc( sizeof( unsigned ) * descriptor_slots );

  for ( i = 0 ; i < descriptor_slots ; i++ )
    gclib_desc_g[i] = gclib_desc_b[i] = 0;

  /* Assume all allocation will happen following these allocations.
   * That doesn't have to be true, but it's an OK starting point.
   * gclib_alloc() will later slide the address ranges along as
   * necessary.
   */
  if ((long)gclib_desc_b > (long)gclib_desc_g)
    gclib_pagebase = memtop = 
      (caddr_t)roundup_page((byte*)(gclib_desc_b + descriptor_slots));
  else
    gclib_pagebase = memtop =
      (caddr_t)roundup_page((byte*)(gclib_desc_g + descriptor_slots));

#if TEST_POSIX_ALLOC
  test_chunk = must_malloc( TEST_CHUNK_SIZE );
#endif
}


/* Allocate memory for the garbage-collected heap.
 *
 * Allocate the requested number of bytes, rounded up to an integral
 * number of pages, and return a pointer to the memory.  Set the
 * heap ownership and generation ownership as indicated.  Pointer is
 * aligned to the beginning of a page.
 */
void *gclib_alloc_heap( unsigned bytes, unsigned heap_no, unsigned gen_no )
{
  byte *ptr;
  int i;

  bytes = roundup_page( bytes );
  ptr = gclib_alloc( bytes );

  for ( i = pageof( ptr ) ; i < pageof( ptr+bytes ) ; i++ ) {
    gclib_desc_g[i] = gen_no;
    gclib_desc_b[i] = MB_ALLOCATED | MB_HEAP_MEMORY;
  }
  heap_bytes += bytes;
  if (heap_bytes > max_heap_bytes) max_heap_bytes = heap_bytes;

  annoyingmsg( "Allocated heap memory gen=%d heap=%d bytes=%u addr=%p",
	       gen_no, heap_no, bytes, (void*)ptr );

#if TEST_POSIX_ALLOC
  if (test_chunk != 0) {
    free( test_chunk );
    test_chunk = 0;
  }
#endif
  return (void*)ptr;
}


/* Allocate memory for the run-time system */

void *gclib_alloc_rts( unsigned bytes, unsigned attribute )
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
    remset_bytes += bytes;
    if (remset_bytes > max_remset_bytes) max_remset_bytes = remset_bytes;
  }
  else {
    rts_bytes += bytes;
    if (rts_bytes > max_rts_bytes) max_rts_bytes = rts_bytes;
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

    if (pageof_pb( top-1, ptr ) >= descriptor_slots)
      grow_table( ptr, memtop );      /* changes gclib_pagebase */
    else {
      unsigned diff;

      /* Slide the table down, i.e., move the entries up */

      annoyingmsg( "Sliding page table by %u entries.", diff );

      diff = pageof_pb( gclib_pagebase, ptr );
      for ( i = descriptor_slots-1 ; i >= diff ; i-- ) {
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
  else if ((caddr_t)top > memtop) {	   /* new allocation above memtop */
    
    old_memtop = memtop;

    if (pageof( top-1 ) >= descriptor_slots) {
      grow_table( gclib_pagebase, top );  /* changes memtop */
      wb_re_setup( gclib_pagebase, gclib_desc_g );
    }
    else
      memtop = top;

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
  unsigned slots = max( pageof_pb( new_top, new_bot ) - 1,
		        descriptor_slots * 2 );
  unsigned *desc_g, *desc_b;
  unsigned dest;
  int i;

  assert( ((word)new_bot % PAGESIZE) == 0 );
  assert( ((word)new_top % PAGESIZE) == 0 );
  assert( (caddr_t)new_bot <= gclib_pagebase );
  assert( (caddr_t)new_top >= memtop );
  annoyingmsg( "Growing page tables; new slots=%u.", slots );

  desc_g = (unsigned*)must_malloc( sizeof( unsigned ) * slots );
  desc_b = (unsigned*)must_malloc( sizeof( unsigned ) * slots );

  dest = pageof_pb( gclib_pagebase, new_bot );

  assert( dest < slots );
  assert( dest+descriptor_slots < slots );

  for ( i=0 ; i < dest ; i++ )
    desc_b[i] = desc_g[i] = 0;

  for ( i=0 ; i < descriptor_slots ; i++, dest++ ) {
    desc_b[dest] = gclib_desc_b[i];
    desc_g[dest] = gclib_desc_g[i];
  }

  for ( i=descriptor_slots ; i < slots ; i++ )
    desc_b[i] = desc_g[i] = 0;

  descriptor_slots = slots;
  gclib_pagebase = new_bot;
  memtop = new_top;

  free( gclib_desc_g );
  free( gclib_desc_b );
  gclib_desc_g = desc_g;
  gclib_desc_b = desc_b;
}


static byte *alloc_aligned( unsigned bytes )
{
  byte *p, *q;
  int wastage;

  p = (byte*)must_malloc( bytes+PAGESIZE );
  q = (byte*)roundup_page( p );
  wastage = q-p;
  annoyingmsg( "Allocation alignment wastage: %d (%2.1f%%)",
	       wastage, (double)wastage*100/(double)bytes  );
  register_pointer( q, p );
  return q;
}


/* This is probably far too slow, but it's OK for now. */

static struct regentry {
  byte *original;
  byte *derived;
} registry[256];
static int reg_next = 0;

static void register_pointer( byte *derived, byte *original )
{
  int i, j;

  annoyingmsg( "registering: orig=%p, derived=%p",
	      (void*)original, (void*)derived );
  if (reg_next == sizeof(registry)/sizeof(struct regentry)) {
    j = 0;
    for (i=0 ; i<reg_next; i++ ) {
      if (registry[i].original != 0) {
	registry[j] = registry[i];
	j++;
      }
    }
    if (j == reg_next)
      panic( "posix_alloc: pointer registry full." );
    reg_next = j;
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
    panic( "posix_alloc: pointer %p not found in registry.", (void*)derived );
  p = registry[i].original;
  registry[i].original = registry[i].derived = 0;
  return p;
}


/*
 * Free the memory (rounded up to an integral number of bytes), and
 * clear the ownership bits.
 */
void gclib_free( void *addr, unsigned bytes )
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
      heap_bytes -= bytes;
    else if (gclib_desc_b[pageno] & MB_REMSET)
      remset_bytes -= bytes;
    else
      rts_bytes -= bytes;
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


/* Given a semispace, set the generation number for all memory allocated
 * to the semispace to the given generation number.
 */
void gclib_set_gen_no( semispace_t *s, int gen_no )
{
  int i;
  word *bot, *lim;

  s->gen_no = gen_no;
  for ( i = 0 ; i < s->n ; i++ ) {
    if (s->chunks[i].bytes == 0) continue;
    bot = s->chunks[i].bot;
    lim = s->chunks[i].lim;
    while (bot < lim) {
      gclib_desc_g[pageof(bot)] = gen_no;
      bot += PAGESIZE/sizeof(word);
    }
  }
}


/* Return information about memory use */
void gclib_stats( word *wheap, word *wremset, word *wrts, word *wmax_heap )
{
  *wheap = heap_bytes/sizeof(word);
  *wremset = remset_bytes/sizeof(word);
  *wrts = rts_bytes/sizeof(word);
  *wmax_heap = max_heap_bytes/sizeof(word);
}

/* eof */
