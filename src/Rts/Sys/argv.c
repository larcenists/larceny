/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system (Unix) -- argument vector handling.
 *
 * The code in this file is reentrant.
 * The code in this file does not depend on word size.
 * The code in this file does not depend on header size.
 */

#include <stdlib.h>
#include <string.h>
#include "larceny.h"
#include "gc_t.h"

/* Return a vector of length argc containing strings which represent
 * the command line arguments passed after the -args argument.
 */
word allocate_argument_vector( gc_t *gc, int argc, char **argv )
{
  int i, l;
  word w, *p, *argvec;
  
  /* Allocate outer vector */
  p = alloc_from_heap( (argc+VEC_HEADER_WORDS)*sizeof(word) );
  *p = mkheader( argc*sizeof(word), VECTOR_HDR );
  for ( i=1 ; i < argc+VEC_HEADER_WORDS ; i++ )
    p[i] = 0;
  argvec = gc_make_handle(gc, tagptr(p,VEC_TAG));

  /* Allocate strings for arguments */
  for ( i=0 ; i < argc ; i++ ) {
    l = strlen( argv[i] );
    p = alloc_from_heap( l + BVEC_HEADER_BYTES );
    *p = mkheader( l, STR_HDR );
    memcpy( p + BVEC_HEADER_WORDS, argv[i], l );
    ptrof(*argvec)[ i+VEC_HEADER_WORDS ] = (word)tagptr( p, BVEC_TAG );
  }

  /* Clear any pad words (shouldn't have to do this...) */
  p = ptrof(*argvec);
  for ( i=argc+VEC_HEADER_WORDS; i<roundup_walign(argc+VEC_HEADER_WORDS); i++ )
    p[i] = 0;

  w = *argvec;
  gc_free_handle( gc, argvec );
  return w;
}

/* eof */
