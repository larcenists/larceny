/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny Runtime System -- operating system specific services: Unix.
 *
 * RTS call-outs, for Unix.
 */

/* Welcome to Unix */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>
#include <poll.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>
#include <signal.h>
#include <poll.h>
#include <string.h>
#include <stdio.h>

#include "larceny.h"
#include "signals.h"
#include "gc_t.h"

/* Why these aren't in the system's header files I don't know */
#if defined(SUNOS4)
extern int rename( const char *oldname, const char *newname );
extern int poll( struct pollfd *fds, unsigned long nfds, int timeout );
#endif

extern void mem_icache_flush( void *lo, void *limit );

static char *getstring( word w );

void UNIX_openfile( w_fn, w_flags, w_mode )
word w_fn, w_flags, w_mode;
{
  char *fn = getstring( w_fn );
  int flags = nativeint( w_flags );
  int mode = nativeint( w_mode );
  int newflags = 0;

  if (flags & 0x01) newflags |= O_RDONLY;
  if (flags & 0x02) newflags |= O_WRONLY;
  if (flags & 0x04) newflags |= O_APPEND;
  if (flags & 0x08) newflags |= O_CREAT;
  if (flags & 0x10) newflags |= O_TRUNC;

  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  globals[ G_RESULT ] = fixnum( open( fn, newflags, mode ) );
}

void UNIX_unlinkfile( w_fn )
word w_fn;
{
  char *fn = getstring( w_fn );
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

void UNIX_writefile( w_fd, w_buf, w_cnt, w_offset )
word w_fd, w_buf, w_cnt, w_offset;
{
  globals[ G_RESULT ] = fixnum( write( nativeint( w_fd ),
				     string_data(w_buf)+nativeint(w_offset),
				     nativeint( w_cnt ) ) );
}

void UNIX_getresourceusage( void )
{
  /* call on the procedure defined in Rts/Sys/stats.c */
  globals[ G_RESULT ] = stats_fillvector();
}

void UNIX_dumpheap( w_fn, w_proc )
word w_fn, w_proc;
{
  char *fn;

  fn = getstring( w_fn );                      /* heap file name */
  globals[ G_STARTUP ] = w_proc;                 /* startup procedure */

  if (fn == 0 || dump_heap_image_to_file( fn ) == -1)
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

  if (stat( getstring( w_fn ), &buf ) == -1) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
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
  int bits = nativeint( w_bits );
  int newbits = 0;

  if (bits & 0x01) newbits |= F_OK;
  if (bits & 0x02) newbits |= R_OK;
  if (bits & 0x04) newbits |= W_OK;
  if (bits & 0x08) newbits |= X_OK;
  globals[ G_RESULT ] = fixnum(access(getstring(w_fn), newbits ));
}

void UNIX_rename( w_from, w_to )
word w_from, w_to;
{
  char fnbuf[ 1024 ];

  strcpy( fnbuf, getstring( w_from ) );
  globals[ G_RESULT ] = fixnum( rename( fnbuf, getstring( w_to ) ) );
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

  p = getenv( getstring( w_envvar ) );
  if (p == 0) {
    globals[ G_RESULT ] = FALSE_CONST;
    return;
  }
  l = strlen( p );
  q = (word*)alloc_bv_from_heap( 4 + l );
  *q = mkheader( l, (BV_HDR | STR_SUBTAG) );
  memcpy( string_data( q ), p, l );
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

void UNIX_garbage_collect( w_gen, w_type )
word w_gen;
word w_type;      /* fixnum: type requested */
{
  /* Type is ignored */
  garbage_collect3( nativeint( w_gen ), 0 );
}

void UNIX_iflush( w_bv )
word w_bv;
{
  mem_icache_flush( ptrof( w_bv )+1, 
		    ptrof( w_bv )+roundup4(sizefield(*ptrof(w_bv)))/4 );
}

/* Copy a file name from a Scheme string to a C string. */
static char *getstring( word w_str )
{
  static char *fnbuf = 0;
  static int fnbuf_length = 0;
  size_t l;

  l = string_length( w_str );
  if (l >= fnbuf_length) {
    if (fnbuf != 0) free( fnbuf );
    fnbuf = malloc( l+1 );
    fnbuf_length = l+1;
  }
  strncpy( fnbuf, string_data( w_str ), l );
  fnbuf[ l ] = 0;
  return fnbuf;
}

/* Floating-point operations */

#define flonum_val( p )    (*(double*)((char*)(p)-5+8))
#define box_flonum( p, v ) (*(double*)((char*)(p)-5+8) = (v))

/* One-argument math operations */
#define numeric_onearg( name, op ) \
  void name( w_flonum, w_result ) \
  word w_flonum, w_result; \
  { \
    box_flonum( w_result, op( flonum_val( w_flonum ) ) ); \
    globals[ G_RESULT ] = w_result; \
  }

numeric_onearg( UNIX_flonum_log, log )
numeric_onearg( UNIX_flonum_exp, exp )
numeric_onearg( UNIX_flonum_sin, sin )
numeric_onearg( UNIX_flonum_cos, cos )
numeric_onearg( UNIX_flonum_tan, tan )
numeric_onearg( UNIX_flonum_asin, asin )
numeric_onearg( UNIX_flonum_acos, acos )
numeric_onearg( UNIX_flonum_atan, atan )
numeric_onearg( UNIX_flonum_sinh, sinh )
numeric_onearg( UNIX_flonum_cosh, cosh )

void UNIX_flonum_atan2( w_flonum1, w_flonum2, w_result )
word w_flonum1, w_flonum2, w_result;
{
  box_flonum( w_result, atan2(flonum_val(w_flonum1), flonum_val(w_flonum2)) );
  globals[ G_RESULT ] = w_result;
}

numeric_onearg( UNIX_flonum_sqrt, sqrt )

/* Statistics dump interface */

void UNIX_stats_dump_on( w_fn )
word w_fn;
{
  char *fn = getstring( w_fn );
  globals[ G_RESULT ] = fixnum( stats_opendump( fn ) );
}

void UNIX_stats_dump_off( void )
{
  stats_closedump();
}

void UNIX_gcctl_np( word heap, word rator, word rand )
{
  /* Heap# comes in as 1..n, but RTS uses 0..n-1 */
  policy_control( nativeint( heap )-1,
		 nativeint( rator ), 
		 (unsigned)nativeint( rand ) );
}

void UNIX_exit( word code )
{
  exit( nativeint( code ) );
}

void UNIX_block_signals( word code )
{
  static signal_set_t old_mask;

  if (code == fixnum(1))
    block_all_signals( &old_mask );
  else if (code == fixnum(0))
    unblock_signals( &old_mask );
}

void UNIX_system( word w_cmd )
{
  char *cmd = getstring( w_cmd );
  globals[ G_RESULT ] = fixnum(system( cmd ));
}

void UNIX_allocate_nonmoving( word w_length, word w_tag )
{
  globals[ G_RESULT ] = 
    allocate_nonmoving( nativeint( w_length ), nativeint( w_tag ) );
}

void UNIX_object_to_address( word w_obj )
{
  /* Invariant: the pointer _must_ point to nonrelocatable memory,
   * or the result may be completely invalid.
   */
  if (isptr(w_obj))
    globals[ G_RESULT ] = box_uint( (word)ptrof( w_obj ) );
  else
    globals[ G_RESULT ] = w_obj;
}

void UNIX_sysfeature( word v /* a vector of sufficient length */ )
{
  struct utsname name;
  int major, minor, ans[4];

  switch (nativeint(vector_ref( v, 0 ))) {
  case 0  : /* larceny-major */
    vector_set( v, 0, fixnum( larceny_major_version ) );
    break;
  case 1  : /* larceny-minor */
    vector_set( v, 0, fixnum( larceny_minor_version ) );
    break;
  case 2  : /* os-major */
    uname( &name );
    sscanf( name.release, "%d.%d", &major, &minor );
    vector_set( v, 0, fixnum( major ) );
    break;
  case 3  : /* os-minor */
    uname( &name );
    sscanf( name.release, "%d.%d", &major, &minor );
    vector_set( v, 0, fixnum( minor ) );
    break;
  case 4  : /* gc-info */
    gc_parameters( the_gc( globals ), 0, ans );
    vector_set( v, 0, fixnum(ans[0]) );	/* technology */
    vector_set( v, 1, fixnum(ans[1]) );	/* generations */
    break;
  case 5  : /* gen-info, generations numbered 1..n */
    ans[0] = 0; ans[1] = 0; ans[2] = 0; ans[3] = 0;
    gc_parameters( the_gc( globals ), nativeint(vector_ref( v, 1 )), ans );
    vector_set( v, 0, fixnum(ans[0]) ); /* type */
    vector_set( v, 1, fixnum(ans[1]) ); /* size */
    vector_set( v, 2, fixnum(ans[2]) ); /* parameter (maybe) */
    vector_set( v, 3, fixnum(ans[3]) ); /* parameter (maybe) */
    break;
  case 6 : /* arch-name */
    if (strcmp( larceny_architecture, "SPARC" ) == 0)
      vector_set( v, 0, fixnum(0) );
    else if (strcmp( larceny_architecture, "Standard-C" ) == 0)
      vector_set( v, 0, fixnum(1) );
    else
      vector_set( v, 0, fixnum(-1) );
    break;
  case 7 : /* os-name */
#if defined(SUNOS4) || defined(SUNOS5)
    vector_set( v, 0, fixnum(0) );
#elif defined(LINUX)
    vector_set( v, 0, fixnum(1) );
#else
    vector_set( v, 0, fixnum(-1) );
#endif
    break;
  case 8 : /* endianness */
#if defined(ENDIAN_BIG)
    vector_set( v, 0, fixnum( 0 ) );
#elif defined(ENDIAN_LITTLE)
    vector_set( v, 0, fixnum( 1 ) );
#else
#error "No endianness."
#endif
    break;
  default : panic( "Unknown code %d passed to UNIX_sysfeature",
		   nativeint( vector_ref( v, 0 ) ) );
  }
}

/* UNIX_sro: implements SRO operation. */
void UNIX_sro( word w_ptrtag, word w_hdrtag, word w_limit )
{
  int ptrtag = (int)nativeint(w_ptrtag);
  int hdrtag = (int)nativeint(w_hdrtag);
  int limit = (int)nativeint(w_limit);

  supremely_annoyingmsg( "SRO %d %d %d", ptrtag, hdrtag, limit );
  globals[ G_RESULT ] = standing_room_only( ptrtag, hdrtag, limit );
}


/* eof */
