NOT CURRENTLY IN USE

/* static area */
/* Note! this should maintain two spaces: text and static.  As it is,
 * it only maintains a text area and the non-code static stuff must be
 * loaded somewhere else. 
 */

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "gc-interface.h"
#include "gclib.h"

static int allocate_heap( /* size */ );

static_heap_t gc_static_heap =
  { /* allocate_heap = */ allocate_heap,
  };

static int allocate_heap( size )
unsigned size;
{
  word *heapptr;

  if (static_size == 0) static_size = DEFAULT_SSIZE;
  static_size = roundup8( static_size );

  if ((heapptr = gclib_alloc( static_size, 32767 )) == 0)
    return 0;

  globals[ G_STATIC_BOT ] = (word)heapptr;
  globals[ G_STATIC_TOP ] = (word)heapptr;
  heapptr += static_size / sizeof( word );
  globals[ G_STATIC_LIM ] = (word)heapptr;

  return 1;
}

int size_sspace()
{ return globals[ G_STATIC_TOP ] - globals[ G_STATIC_BOT ]; }


/* eof */
