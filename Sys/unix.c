/*
 * This is the file Sys/unix.c.
 *
 * Larceny Runtime System (Unix) -- Unix and related services.
 *
 * These procedures are the upper-level counterparts of the assembly
 * procedures in ../Sparc/unix.s.
 *
 * History
 *   June 26, 1994 / lth
 *     Created, along with unix.s.
 *
 * Input arguments come in RESULT, ARGREG2, and ARGREG3, and leave
 * in RESULT (one result only). Very little magic.
 */

static char *getfilename();

#include <limits.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

int UNIX_openfile()
{
  char *fn = getfilename( G_RESULT );
  if (fn == 0) 
    globals[ G_RESULT ] = fixnum( -1 );
  else
    globals[ G_RESULT ] = fixnum( open( fn,
				       nativeint( globals[ G_ARGREG2 ] ),
				       nativeint( globals[ G_ARGREG3 ] ) ) );
}

int UNIX_unlinkfile()
{
  char *fn = getfilename( G_RESULT );
  if (fn == 0) 
    globals[ G_RESULT ] = fixnum( -1 );
  else
    globals[ G_RESULT ] = fixnum( unlink( fn ) );
}

int UNIX_closefile()
{
  globals[ G_RESULT ] = fixnum( close( nativeint( globals[ G_RESULT ] ) ) );
}

int UNIX_readfile()
{
  globals[ G_RESULT ] = fixnum( read( nativeint( globals[ G_RESULT ] ),
				    string_data( globals[ G_ARGREG2 ] ),
				    nativeint( globals[ G_ARGREG3 ] ) ) );
}

int UNIX_writefile()
{
  globals[ G_RESULT ] = fixnum( write( nativeint( globals[ G_RESULT ] ),
				     string_data( globals[ G_ARGREG2 ] ),
				     nativeint( globals[ G_ARGREG3 ] ) ));
}

int UNIX_getresourceusage()
{
  /* call on the procedure defined in memstats.c */
  memstat_fillvector( ptrof( globals[ G_RESULT ] )+1 );
}

int UNIX_dumpheap()
{
  char *fn = getfilename( G_RESULT );

  /* call on the procedure defined in heapio.c */
  garbage_collect( FULL_COLLECTION, 0 );
  if (fn == 0 || dump_heap( fn ) == -1)
    globals[ G_RESULT ] = FALSE_CONST;
  else 
    globals[ G_RESULT ] = TRUE_CONST;
}

/* 
 * Utility procedure to copy a file name from a Scheme string to a
 * C string.
 */
static char *getfilename( offs )
int offs;
{
  static char fnbuf[ 1024 ];
  int l;

  l = string_length( globals[ offs ] );
  if (l >= sizeof( fnbuf )) return 0;
  strncpy( fnbuf, string_data( globals[ offs ] ), l );
  fnbuf[ l ] = 0;
  return fnbuf;
}

/* eof */
