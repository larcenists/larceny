/* Larceny RTS -- cache management for the SPARC
 *
 * lth@cs.uoregon.edu / August 24, 1995
 * $Id: cache.c,v 1.1 1997/01/21 20:03:54 lth Exp lth $
 */

#include "larceny.h"
#include "cdefs.h"

/* Configure Larceny's cache logic */
void cache_setup()
{
#if defined( FLUSH_ALWAYS )
  globals[ G_CACHE_FLUSH ] = 1;
#elif defined( FLUSH_NEVER )
  globals[ G_CACHE_FLUSH ] = 0;
#else
  globals[ G_CACHE_FLUSH ] = (test_cache() > 0);
#endif
}

/* eof */
