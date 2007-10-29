/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- RTS/GC glue code ("temporary" since the 0.26 rewrite (sigh))
 * 
 * The code in this file presents an interface to the new GC that looks
 * mostly like the interface to the old GC.  The purpose of the deception
 * is to allow more gradual changes to the RTS overall.  Eventually, the
 * RTS will be modified to deal with the new GC, and much or all of this file
 * will go away.
 *
 * (Much of this file _has_ gone away.  990608 / lth)
 */

#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "static_heap_t.h"
#include "semispace_t.h"
#include "heapio.h"

static gc_t *gc;
static int  generations;

int create_memory_manager( gc_param_t *params, int *gens )
{
#if !defined( BDW_GC )
  gc = create_gc( params, &generations );
#else
  gc = create_bdw_gc( params, &generations );
#endif /* BDW_GC */
  gc_initialize( gc );
  globals[ G_GC ] = (word)gc;
  globals[ G_GC_CNT ] = fixnum(0);
  globals[ G_MAJORGC_CNT ] = fixnum(0);
  *gens = generations;
  return 1;
}

word *alloc_from_heap( int bytes )
{
  return gc_allocate( gc, bytes, 0, 0 );
}

/* For vectors and pairs, the length is number of words.
   For bytevectors, the length is number of bytes.
   The length is the number of data fields in the structure and does
   not include header or padding.
   */
word allocate_nonmoving( int length, int tag )
{
  int i, bytes = 0;
  word *obj;

  switch( tag ) {
  case PAIR_TAG :
  case VEC_TAG :
    bytes = sizeof(word)*length + 1; /* header */
    break;
  case BVEC_TAG :
    bytes = length + sizeof(word);   /* header */
    break;
  default :
    panic_exit( "Bad case in UNIX_allocate_nonmoving: %d", tag );
  }

  obj = gc_allocate_nonmoving( gc, bytes, tag == BVEC_TAG );
  switch (tag) {
  case PAIR_TAG :
    obj[0] = FALSE_CONST;
    obj[1] = FALSE_CONST;
    return tagptr( obj, PAIR_TAG );
  case VEC_TAG :
    obj[0] = mkheader( length*sizeof(word), VECTOR_HDR );
    for ( i=1 ; i < length ; i++ )
      obj[i] = FALSE_CONST;
    return tagptr( obj, VEC_TAG );
  case BVEC_TAG :
    obj[0] = mkheader( length, BYTEVECTOR_HDR );
    return tagptr( obj, BVEC_TAG );
  }
  /*NOTREACHED*/
  return 0;
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
  int text_size, data_size, r, i;
  word *sbase, *tbase, tmp1, tmp2;

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

  /* Clear the globals as necessary */
  tmp1 = globals[ G_STARTUP ];
  tmp2 = globals[ G_CALLOUTS ];
  for ( i=FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    globals[ i ] = FALSE_CONST;
  globals[ G_STARTUP ] = tmp1;
  globals[ G_CALLOUTS ] = tmp2;

  return 1;

 fail:
  hardconsolemsg( "Heap open failure: %s: %s.", filename, heapio_msg[-r] );
 fail2:
  hio_close( heap );
  return 0;
}

/* Dump_heap_image_to_file() just defers to the collector, because different
   collector types dump different heap images.
   
   FIXME: referenced only from primitive.c; could move this function to that 
   file.  That would allow error reporting to be consistent with the other
   primitives.
   */
int dump_heap_image_to_file( const char *filename )
{
  int r;

  if ((r = gc_dump_heap( gc, filename, 1 )) < 0) {
    hardconsolemsg( "Heap create failure: file %s, reason '%s'.", 
		    filename, heapio_msg[ -r ] );
    return 0;
  }
  return 1;
}

int reorganize_and_dump_static_heap( const char *filename )
{
#if defined( BDW_GC )
  /* Shouldn't happen -- the command line switch should be disabled. */
  panic_abort( "Cannot reorganize the heap in bdwlarceny." );
  return 0;
#else
  semispace_t *data, *text;
  int r;

  consolemsg( "Reorganizing." );
  if (gc->static_area == 0)
    panic_exit( "reorganize_and_dump_static_heap: no static heap." );
  sh_reorganize( gc->static_area );
  data = gc->static_area->data_area;
  text = gc->static_area->text_area;
  consolemsg( "Data: %d bytes, Text: %d bytes",
	      data->allocated, text->allocated );

  consolemsg( "Dumping \"%s\".", filename );
  if ((r = gc_dump_heap( gc, filename, 0 )) < 0) {
    hardconsolemsg( "Heap create failure: file %s, reason '%s'.", 
		    filename, heapio_msg[ -r ] );
    return 0;
  }
  return 1;
#endif
}

/* eof */
