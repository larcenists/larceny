/* Rts/Sys/static-heap.c
 * Larceny run-time system -- static heap.
 *
 * $Id: static-heap.c,v 1.6 1997/09/17 15:17:26 lth Exp $
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "memmgr.h"
#include "gclib.h"
#include "assert.h"

#define OLD_STATIC  0

typedef struct static_data static_data_t;

struct static_data {
  int      gen_no;
  int      heap_no;
#if OLD_STATIC
  word     *bot;
  word     *top;
  word     *lim;
#else
  semispace_t *text;
  semispace_t *data;
#endif
  unsigned size;         /* bytes allocated */
};

#define DATA(heap)   ((static_data_t*)(heap->data))

static static_heap_t *allocate_static_heap( int gen_no, int heap_no );

static_heap_t *
create_static_heap( int heap_no,          /* given */
		    int gen_no,           /* given */
		    unsigned size_bytes   /* ignored under new regime */
		   )
{
  static_heap_t *heap;
  static_data_t *data;

  heap = allocate_static_heap( gen_no, heap_no );
  data = DATA(heap);

#if OLD_STATIC
  size_bytes = roundup_page( size_bytes );

 again:
  data->bot = gclib_alloc_heap( size_bytes, gen_no, heap_no );
  if (data->bot == 0) {
    memfail( MF_HEAP, "Could not allocate static heap." );
    goto again;
  }

  data->size = size_bytes;
  data->top = data->bot;
  data->lim = data->bot + size_bytes/sizeof(word);
#endif

  return heap;
}

#if defined(SUNOS)
/* Debug code, for the time being */
#include <sys/mman.h>

void protect_static( static_heap_t *heap )
{
  static_data_t *data = DATA(heap);
  int i;

  for ( i=0 ; i <= data->text->current ; i++ ) {
    if (mprotect( (void*)(data->text->chunks[i].bot),
		 data->text->chunks[i].bytes,
		 PROT_READ | PROT_EXEC ) == -1)
      consolemsg( "mprotect failed." );
  }
}
#endif

static int  initialize( static_heap_t *heap );
static void reorganize( static_heap_t *heap );
static void stats( static_heap_t *heap, heap_stats_t *s );
static word *data_load_area( static_heap_t *heap, unsigned nbytes );
static word *text_load_area( static_heap_t *heap, unsigned nbytes );
static void get_data_areas( static_heap_t *, semispace_t **, semispace_t ** );

static static_heap_t *
allocate_static_heap( int gen_no, int heap_no )
{
  static_heap_t *heap;
  static_data_t *data;
  
  heap = (static_heap_t*)must_malloc( sizeof( static_heap_t ) );
  data = (static_data_t*)must_malloc( sizeof( static_data_t ) );

  heap->id = "static";
  heap->code = HEAPCODE_STATIC_2SPACE;

  heap->data = data;
  heap->initialize = initialize;
  heap->stats = stats;
  heap->data_load_area = data_load_area;
  heap->text_load_area = text_load_area;
  heap->reorganize = reorganize;
  heap->get_data_areas = get_data_areas;

  data->gen_no = gen_no;
  data->heap_no = heap_no;

  data->size = 0;
#if OLD_STATIC
  data->top = data->lim = 0;
#else
  data->data = data->text = 0;
#endif

  return heap;
}


static int initialize( static_heap_t *heap )
{
  /* Nothing to be done */
  return 1;
}


static void get_data_areas( static_heap_t *heap,
			    semispace_t **data, semispace_t **text )
{
#if OLD_STATIC
  panic( "get_data_areas" );
#else
  *data = DATA(heap)->data;
  *text = DATA(heap)->text;
#endif
}


/* This procedure copies all live data into two parts, _text_ (for non-pointer
 * containing) and _data_ (pointer containing), and then make the static
 * area consist of those two.
 */

static void reorganize( static_heap_t *heap )
{
  static_data_t *s_data = DATA(heap);
  semispace_t *text, *data;
  unsigned textsize, datasize;

#if OLD_STATIC
  panic( "reorganize" );
#else
  /* HACK!  We create both semispaces large enough to receive all of
     the data from the current static area.  When just doing a static
     area reorg, each piece will have only one chunk.  We can then
     use existing (May '97) heap dumping code to dump a split heap.
     This is only a quick fix to level the field for the Boehm collector.
   */
  textsize = datasize = s_data->size;
  text = create_semispace( textsize, s_data->heap_no, s_data->gen_no);
  data = create_semispace( datasize, s_data->heap_no, s_data->gen_no);
  gclib_stopcopy_split_heap( heap->collector, data, text );
  if (s_data->text) ss_free( s_data->text );
  if (s_data->data) ss_free( s_data->data );
  s_data->text = s_data->data = 0;
  ss_shrinkwrap( text ); ss_sync( text );
  ss_shrinkwrap( data ); ss_sync( data );
  if (text->used > 0) s_data->text = text; else ss_free( text );
  if (data->used > 0) s_data->data = data; else ss_free( data );
#endif
}

static void stats( static_heap_t *heap, heap_stats_t *s )
{
  s->target = s->semispace1 = s->live = DATA(heap)->size;
  s->semispace2 = 0;
}

static word *data_load_area( static_heap_t *heap, unsigned nbytes )
{
  static_data_t *data = DATA(heap);
#if OLD_STATIC
  word *p;
  
  if ((data->lim - data->top)*sizeof(word) < nbytes)
    return 0;
  p = data->top;
  data->top += nbytes/sizeof(word);
  return p;
#else
  if (data->data)
    ss_expand( data->data, nbytes ); 
  else
    data->data = create_semispace( nbytes, data->heap_no, data->gen_no );
  data->data->chunks[ data->data->current ].top += nbytes;
  data->size += nbytes;
  return data->data->chunks[ data->data->current ].bot;
#endif
}

static word *text_load_area( static_heap_t *heap, unsigned nbytes )
{
  static_data_t *data = DATA(heap);

#if OLD_STATIC
  panic( "text_load_area" ); return 0;
#else
  if (data->text)
    ss_expand( data->text, nbytes ); 
  else
    data->text = create_semispace( nbytes, data->heap_no, data->gen_no);
  data->text->chunks[ data->text->current ].top += nbytes;
  data->size += nbytes;
  return data->text->chunks[ data->text->current ].bot;
#endif
}

/* eof */
