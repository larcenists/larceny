/* Rts/Sys/argv.c
 * Larceny run-time system (Unix) -- argument vector handling.
 *
 * $Id$
 *
 * The code in this file is reentrant.
 * The code in this file does not depend on word size.
 * The code in this file does not depend on header size.
 */

#include <stdlib.h>
#include "larceny.h"

/* Return a vector of length argc containing strings which represent
 * the command line arguments passed after the -args argument.
 */
word allocate_argument_vector( int argc, char **argv )
{
  int bytes, i, l;
  word *w, *p;

  bytes = roundup_balign( (argc+VEC_HEADER_WORDS)*sizeof(word) );
  for ( i = 0 ; i < argc ; i++ )
    bytes += roundup_balign(  strlen( argv[i] ) + BVEC_HEADER_BYTES );
  w = alloc_from_heap( bytes );

  *w = mkheader( argc*sizeof(word), VECTOR_HDR );
  p = w + roundup_walign( argc + VEC_HEADER_WORDS );
  for ( i=0 ; i < argc ; i++ ) {
    l = strlen( argv[i] );
    *p = mkheader( l, STR_HDR );
    memcpy( p + BVEC_HEADER_WORDS, argv[i], l );
    w[ i+VEC_HEADER_WORDS ] = (word)tagptr( p, BVEC_TAG );
    p = (word*)roundup_balign( (word)p + BVEC_HEADER_BYTES + l );
  }

  /* Clear any pad words (shouldn't have to do this...) */
  i=argc+VEC_HEADER_WORDS;
  while ( i < roundup_walign( argc+VEC_HEADER_WORDS ) ) {
    w[i] = 0;
    i++;
  }

  return (word)tagptr( w, VEC_TAG );
}

/* eof */
