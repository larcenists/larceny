/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Cache management for the SPARC
 */

#include "larceny.h"

/* Configure Larceny's cache logic */
void cache_setup( void )
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
