/* Rts/Sys/malloc.c
 * Larceny run-time system -- malloc wrappers
 *
 * $Id: malloc.c,v 1.2 1997/05/15 00:58:49 lth Exp lth $
 */

#include <stdlib.h>
#include "larceny.h"

void *must_malloc( unsigned bytes )
{
  void *p;

 again:
  p = malloc( bytes );
  if (p == 0) {
    memfail( MF_MALLOC, "Could not allocate RTS-internal data." );
    goto again;
  }
  return p;
}


void *must_realloc( void *ptr, unsigned bytes )
{
  void *p;

 again:
  p = realloc( ptr, bytes );
  if (p == 0) {
    memfail( MF_REALLOC, "Could not allocate RTS-internal data." );
    goto again;
  }
  return p;
}

/* eof */
