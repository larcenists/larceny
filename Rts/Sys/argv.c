/* Rts/Sys/argv.c
 * Larceny run-time system (Unix) -- argument vector handling.
 *
 * $Id: argv.c,v 1.2 1997/02/06 20:01:41 lth Exp $
 */

#include <stdlib.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

/* Return a vector of length argc containing strings which represent
 * the command line arguments passed after the -args argument.
 *
 * I belive this procedure does not depend on the word size of the
 * architecture on which Larceny is running.
 */
word allocate_argument_vector( int argc, char **argv )
{
  int n, i, l;
  word *w, *p;

  n = roundup_balign( (argc+1)*sizeof(word) );
  for ( i = 0 ; i < argc ; i++ )
    n += roundup_balign( strlen( argv[i] ) + sizeof( word ) );
  w = alloc_from_heap( n );

  *w = mkheader( argc*sizeof( word ), VECTOR_HDR );
  p = w + roundup_walign( argc + 1 );
  for ( i = 0 ; i < argc ; i++ ) {
    l = strlen( argv[i] );
    *p = mkheader( l, STR_HDR );
    memcpy( p+1, argv[i], l );
    w[i+1] = (word)tagptr( p, BVEC_TAG );
    p = (word*)roundup_balign( (word)p+l+sizeof(word) );
  }
  /* Get any pad words */
  for ( ; i < roundup_walign( argc+1 )-1 ; i++ )
    w[i+1] = 0;
  return (word)tagptr( w, VEC_TAG );
}

/* eof */
