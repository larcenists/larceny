/* This is the file Sys/unix.c.
 * Larceny Runtime System (Unix) -- Unix and related services.
 *
 * $Id: unix.c,v 1.2 1995/08/01 04:37:33 lth Exp lth $
 *
 * History
 *   June 27, 1995 / lth
 *     Rewritten to be used with the new syscall, getting rid of some
 *     assembly code.
 *
 *   June 26, 1994 / lth
 *     Created, along with unix.s.
 */

static char *getfilename();

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>
#include <poll.h>
#include <stdlib.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

void UNIX_openfile( w_fn, w_flags, w_mode )
word w_fn, w_flags, w_mode;
{
  char *fn = getfilename( w_fn );
  int flags = nativeint( w_flags );
  int mode = nativeint( w_mode );

  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  globals[ G_RESULT ] = fixnum( open( fn, flags, mode ) );
}

void UNIX_unlinkfile( w_fn )
word w_fn;
{
  char *fn = getfilename( w_fn );
  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  globals[ G_RESULT ] = fixnum( unlink( fn ) );
}

void UNIX_closefile( w_fd )
word w_fd;
{
  globals[ G_RESULT ] = fixnum( close( nativeint( w_fd ) ) );
}

void UNIX_readfile( w_fd, w_buf, w_cnt )
word w_fd, w_buf, w_cnt;
{
  globals[ G_RESULT ] = fixnum( read( nativeint( w_fd ),
				    string_data( w_buf ),
				    nativeint( w_cnt ) ) );
}

void UNIX_writefile( w_fd, w_buf, w_cnt )
word w_fd, w_buf, w_cnt;
{
  globals[ G_RESULT ] = fixnum( write( nativeint( w_fd ),
				     string_data( w_buf ),
				     nativeint( w_cnt ) ) );
}

void UNIX_getresourceusage( w_vec )
word w_vec;
{
  /* call on the procedure defined in memstats.c */
  memstat_fillvector( ptrof( w_vec )+1 );
}

void UNIX_dumpheap( w_fn, w_proc )
word w_fn, w_proc;
{
  char *fn;

  fn = getfilename( w_fn );                      /* heap file name */
  globals[ G_STARTUP ] = w_proc;                 /* startup procedure */

  garbage_collect( FULL_COLLECTION, 0 );

  if (fn == 0 || dump_heap( fn ) == -1)
    globals[ G_RESULT ] = FALSE_CONST;
  else 
    globals[ G_RESULT ] = TRUE_CONST;
}

/* File modification time as six-element vector */
void UNIX_mtime( w_fn, w_buf )
word w_fn, w_buf;
{
  struct stat buf;
  struct tm *tm;

  if (stat( getfilename( w_fn ), &buf ) == -1) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  /* NOTES
   * -This is not right -- it produces GMT, for some reason.
   * -If the TZ variable is broken, localtime() may segfault.
   */
  tm = localtime( &buf.st_mtime );
  vector_set( w_buf, 0, fixnum( tm->tm_year + 1900 ) );
  vector_set( w_buf, 1, fixnum( tm->tm_mon + 1 ) );
  vector_set( w_buf, 2, fixnum( tm->tm_mday ) );
  vector_set( w_buf, 3, fixnum( tm->tm_hour ) );
  vector_set( w_buf, 4, fixnum( tm->tm_min ) );
  vector_set( w_buf, 5, fixnum( tm->tm_sec ) );
  globals[ G_RESULT ] = fixnum( 0 );
}

void UNIX_access( w_fn, w_bits )
word w_fn, w_bits;
{
  globals[ G_RESULT ] = fixnum(access(getfilename(w_fn), nativeint(w_bits)));
}

void UNIX_rename( w_from, w_to )
word w_from, w_to;
{
  char fnbuf[ 1024 ];

  strcpy( fnbuf, getfilename( w_from ) );
  globals[ G_RESULT ] = fixnum( rename( fnbuf, getfilename( w_to ) ) );
}

void UNIX_pollinput( w_fd )
word w_fd;
{
  /* One could also use select() */
  int r;
  struct pollfd fd[1];

  fd[0].fd = nativeint( w_fd );
  fd[0].events = POLLIN;
  r = poll( fd, 1, 0 );
  if (r == 1)
    globals[ G_RESULT ] = fixnum( fd[0].revents & (POLLIN|POLLHUP|POLLERR) );
  else
    globals[ G_RESULT ] = fixnum( r );
}

void UNIX_getenv( w_envvar )
word w_envvar;
{
  char *p;
  word *q;
  int l;

  p = getenv( getfilename( w_envvar ) );
  if (p == 0) {
    globals[ G_RESULT ] = FALSE_CONST;
    return;
  }
  l = strlen( p );
  q = (word*)alloc_from_heap( 4 + l );
  *q = mkheader( l, (BV_HDR | STR_SUBTAG) );
  memcpy( string_data( q ), p, l );
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

void UNIX_garbage_collect( w_type )
word w_type;      /* fixnum: type requested */
{
  garbage_collect( nativeint( w_type ), 0 );
}

/* Copy a file name from a Scheme string to a C string. */
static char *getfilename( w_str )
word w_str;
{
  static char fnbuf[ 1024 ];
  int l;

  l = string_length( w_str );
  if (l >= sizeof( fnbuf )) return 0;
  strncpy( fnbuf, string_data( w_str ), l );
  fnbuf[ l ] = 0;
  return fnbuf;
}

/* eof */
