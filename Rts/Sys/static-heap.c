/* Rts/Sys/static-heap.c
 * Larceny run-time system -- static heap.
 *
 * $Id$
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gc.h"			/* For heap_stats_t */
#include "memmgr.h"
#include "static_heap_t.h"
#include "semispace_t.h"
#include "gclib.h"
#include "heap_stats_t.h"

typedef struct static_data static_data_t;

struct static_data {
  int gen_no;		/* Generation number. */
};

#define DATA(heap)   ((static_data_t*)(heap->data))


static word *allocate( static_heap_t *heap, int nbytes )
{
  word *p;
  int nwords;
  static_data_t *data = DATA(heap);
  semispace_t *ss;

  nbytes = roundup_balign( nbytes );
  nwords = nbytes / sizeof( word );
  if (!heap->data_area)
    heap->data_area = create_semispace( nbytes, data->gen_no, data->gen_no );
  ss = heap->data_area;
  if (ss->chunks[ ss->current ].top + nwords >= ss->chunks[ ss->current ].lim)
    ss_expand( ss, max( nbytes, GC_CHUNK_SIZE ) );
  p = ss->chunks[ ss->current ].top;
  ss->chunks[ ss->current ].top += nwords;
  return p;
}

static void reorganize( static_heap_t *heap )
{
  static_data_t *s_data = DATA(heap);
  semispace_t *text, *data;
  unsigned textsize, datasize;

  /* HACK!  We create both semispaces large enough to receive all of
     the data from the current static area.  When just doing a static
     area reorg, each piece will have only one chunk.  We can then
     use existing (May '97) heap dumping code to dump a split heap.
     This is only a quick fix to level the field for the Boehm collector.
   */
  textsize = datasize = heap->data_area->allocated;
  text = create_semispace( textsize, s_data->gen_no, s_data->gen_no);
  data = create_semispace( datasize, s_data->gen_no, s_data->gen_no);
  gclib_stopcopy_split_heap( heap->collector, data, text );
  if (heap->text_area) ss_free( heap->text_area );
  if (heap->data_area) ss_free( heap->data_area );
  heap->text_area = heap->data_area = 0;
  ss_shrinkwrap( text ); ss_sync( text );
  ss_shrinkwrap( data ); ss_sync( data );
  if (text->used > 0) heap->text_area = text; else ss_free( text );
  if (data->used > 0) heap->data_area = data; else ss_free( data );
}

static void stats( static_heap_t *heap, heap_stats_t *s )
{
  s->target = s->semispace1 = s->live = heap->allocated;
  s->semispace2 = 0;
}

static word *allocate_chunk( semispace_t **space, int nbytes, int gen_no )
{
  assert( nbytes > 0 );
  assert( nbytes % BYTE_ALIGNMENT == 0 );

  if (*space == 0)
    *space = create_semispace( nbytes, gen_no, gen_no );
  else
    ss_expand( *space, nbytes ); 
  (*space)->chunks[ (*space)->current ].top += nbytes/sizeof(word);
  return (*space)->chunks[ (*space)->current ].bot;
}

static word *data_load_area( static_heap_t *heap, int nbytes )
{
  heap->allocated += nbytes;
  return allocate_chunk( &heap->data_area, nbytes, DATA(heap)->gen_no );
}

static word *text_load_area( static_heap_t *heap, int nbytes )
{
  heap->allocated += nbytes;
  return allocate_chunk( &heap->text_area, nbytes, DATA(heap)->gen_no );
}

static_heap_t *create_static_area( int gen_no, gc_t *gc )
{
  static_heap_t *heap;
  static_data_t *data;
  
  heap = (static_heap_t*)must_malloc( sizeof( static_heap_t ) );
  data = (static_data_t*)must_malloc( sizeof( static_data_t ) );

  heap->collector = gc;
  heap->id = "static";
  heap->code = HEAPCODE_STATIC_2SPACE;
  heap->data_area = 0;
  heap->text_area = 0;
  heap->allocated = 0;

  heap->data = data;

  heap->initialize = 0;
  heap->allocate = allocate;
  heap->stats = stats;
  heap->data_load_area = data_load_area;
  heap->text_load_area = text_load_area;
  heap->reorganize = reorganize;
  heap->load_prepare = 0;
  heap->load_data = 0;

  data->gen_no = gen_no;

  return heap;
}

#if defined(SUNOS4)

/* Debug code: write-protect the text area. */

#include <sys/mman.h>
extern int mprotect( caddr_t addr, int len, int prot );

void protect_static( static_heap_t *heap )
{
  semispace_t *ss = heap->text_area;
  int i;

  if (ss == 0) return;
  for ( i=0 ; i <= ss->current ; i++ ) {
    if (mprotect( (void*)(ss->chunks[i].bot),
		  ss->chunks[i].bytes,
		  PROT_READ | PROT_EXEC ) == -1)
      consolemsg( "mprotect failed." );
  }
}

#endif  /* defined(SUNOS4) */

/* eof */
