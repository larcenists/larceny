/* Rts/Sys/util.c
 * Larceny run-time system -- miscellaneous procedures.
 *
 * $Id: util.c,v 1.1 1997/05/23 13:50:06 lth Exp $
 */

#include <memory.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "gc.h"


#define HDR_BYTES    4    /* Belongs in layouts.cfg */


/* Given a tagged pointer to an object, make a copy of the object in the
 * heap of the given collector.  The source object does not need to be
 * in the heap already.
 */

word copy_object( gc_t *gc, word obj )
{
  word *p;
  unsigned size;

  if (tagof( obj ) == PAIR_TAG)
    size = 2*sizeof( word );
  else 
    size = roundup_balign( sizefield( *ptrof( obj ) )+HDR_BYTES );
  p = gc->allocate( gc, size );
  memcpy( p, ptrof( obj ), size );
  return tagptr( p, tagof( obj ) );
}

/* eof */

