/* Rts/Sys/gc.c
 * Larceny -- RTS/GC glue code ("temporary" since the 0.26 rewrite (sigh))
 * 
 * $Id: gc.c,v 1.19 1997/09/17 15:17:26 lth Exp $
 *
 * The code in this file presents an interface to the new GC that looks
 * mostly like the interface to the old GC.  The purpose of the deception
 * is to allow more gradual changes to the RTS overall.  Eventually, the
 * RTS will be modified to deal with the new GC, and much or all of this file
 * will go away.
 */

#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "static_heap_t.h"
#include "heapio.h"

static gc_t *gc;
static int  generations;

int create_memory_manager( gc_param_t *params )
{
#if !defined( BDW_GC )
  gc = create_gc( params, &generations );
#else
  gc = create_bdw_gc( params, &generations );
#endif /* BDW_GC */
  gc_initialize( gc );
  return 1;
}

char *gctype( void )
{
  return gc->id;
}

void policy_control( int heap, int op, unsigned arg )
{
  gc_set_policy( gc, heap, op, arg );
}

void init_stats( int show_stats )
{
  stats_init( gc, generations, show_stats );
}

word *alloc_from_heap( int bytes )
{
  return gc_allocate( gc, bytes, 0, 0 );
}

word *alloc_bv_from_heap( int bytes )
{
  return gc_allocate( gc, bytes, 0, 1 );
}

word standing_room_only( int p_tag, int h_tag, int limit )
{
  return sro( gc, globals, p_tag, h_tag, limit );
}

word allocate_nonmoving( int length, int tag )
{
  int i;
  word *obj;

  switch( tag ) {
  case PAIR_TAG :
  case VEC_TAG :
    length = sizeof(word)*length;
    break;
  case BVEC_TAG :
    break;
  default :
    panic( "Bad case in UNIX_allocate_nonmoving: %d", tag );
  }

  obj = gc_allocate_nonmoving( gc, length, tag == BVEC_TAG );
  switch (tag) {
  case PAIR_TAG :
    obj[0] = FALSE_CONST;
    obj[1] = FALSE_CONST;
    return tagptr( obj, PAIR_TAG );
  case VEC_TAG :
    obj[0] = mkheader( length, VECTOR_HDR );
    for ( i=1 ; i < length/sizeof(word) ; i++ )
      obj[i] = FALSE_CONST;
    return tagptr( obj, VEC_TAG );
  case BVEC_TAG :
    obj[0] = mkheader( length, BYTEVECTOR_HDR );
    return tagptr( obj, BVEC_TAG );
  }
}

void garbage_collect3( int gen, int request_bytes )
{
  gc_collect( gc, gen, request_bytes );
}

word creg_get( void )        { return gc_creg_get( gc ); }
void creg_set( word c )      { gc_creg_set( gc, c ); }
void stack_underflow( void ) { gc_stack_underflow( gc ); }
void stack_overflow( void )  { gc_stack_overflow( gc ); }

void compact_ssb( void )     
{ 
  if (gc_compact_all_ssbs( gc )) {
    /* At least one remembered set overflowed. */
    /* FIXME: this probably should be under direct memmgr control */
    supremely_annoyingmsg( "Remembered-set overflow." );
    garbage_collect3( 1, 0 );
  }
}

static char *heapio_msg[] =
{ "OK", "Wrong type", "Wrong version", "Can't read", "Can't open",
  "Heap not open", "Can't write", "Unmatched heap code", "Can't close" };

/* Load_heap_image_from_file() supports single and split bootstrap 
   heaps directly; other types must be loaded by the garbage collector.
   */
int load_heap_image_from_file( const char *filename )
{
  heapio_t *heap;
  int text_size, data_size, r;
  word *sbase, *tbase;

  heap = create_heapio();
  if ((r = hio_open( heap, filename )) < 0)
    goto fail;

  sbase = 0;
  tbase = 0;

  if (heap->type == HEAP_SINGLE || heap->type == HEAP_SPLIT) {
    text_size = heap->text_size * sizeof(word); 
    data_size = heap->data_size * sizeof(word); 

    if (text_size > 0)
      sbase = gc_text_load_area( gc, text_size );
    tbase = gc_data_load_area( gc, data_size );
    if ((r = hio_load_bootstrap( heap, sbase, tbase, globals )) < 0)
      goto fail;
  }
  else if (!gc_load_heap( gc, heap ))
    goto fail2;

  if ((r = hio_close( heap )) < 0)
    goto fail;

  return 1;

 fail:
  hardconsolemsg( "Heap open failure: %s.", heapio_msg[-r] );
 fail2:
  hio_close( heap );
  return 0;
}

/* Dump_heap_image_to_file() just defers to the collector, because different
   collector types dump different heap images.
   */
int dump_heap_image_to_file( const char *filename )
{
  int r;

  if ((r = gc_dump_heap( gc, filename )) < 0) {
    hardconsolemsg( "Heap create failure: file %s, reason '%s'.", 
		    filename, heapio_msg[ -r ] );
    return 0;
  }
  return 1;
}

int reorganize_and_dump_static_heap( const char *filename )
{
#if defined( BDW_GC )
  panic( "reorganize_and_dump_static_heap: not in bdwlarceny!" );
  return 0;
#else
  semispace_t *data, *text;
  heapio_t *heap;
  int r;

  if (gc->static_area == 0)
    panic( "reorganize_and_dump_static_heap: no static heap." );
  sh_reorganize( gc->static_area );
  data = gc->static_area->data_area;
  text = gc->static_area->text_area;

  return dump_heap_image_to_file( filename );
#endif
}

/* eof */
