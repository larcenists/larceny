/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Operating-system dependent functionality -- generic operating system.
 *
 * The goal for this file is that it should compile and work whereever
 * there is an ANSI Standard C compiler and library, and it should do
 * the "right" thing when possible.  So, for example, even though system()
 * does not work in any real sense on a number of operating systems,
 * it is in Standard C and is used below.
 *
 * Reference: P.J. Plauger, "The Standard C Library."  Prentice Hall, 1992.
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#ifdef HAVE_STAT
#  include <sys/stat.h>
#endif
#ifndef FILENAME_MAX
#  define FILENAME_MAX 1024
#endif

#include "larceny.h"
#include "assert.h"

#if GENERIC_OS

static stat_time_t real_start;

static void get_rtclock( stat_time_t *real );

void osdep_init( void )
{
  real_start.sec = 0;
  real_start.usec = 0;
  get_rtclock( &real_start );
}

void osdep_poll_events( word *globals )
{
  /* Nothing now. */
}

void osdep_poll_startup_events( void )
{
  /* Nothing now. */
}

/* system() is in Standard C */
void osdep_system( word w_cmd )
{
  char *cmd = string2asciiz( w_cmd );
  globals[ G_RESULT ] = fixnum(system( cmd ));
}

/* Return 0.0 */
void osdep_os_version( int *major, int *minor )
{
  *major = 0;
  *minor = 0;
}

/* Fill in the structures with real, user, system times. */
void 
osdep_time_used( stat_time_t *real, stat_time_t *user, stat_time_t *system )
{
  if (real != 0)
    get_rtclock( real );

  if (user != 0 || system != 0) {
    unsigned t = clock();

    if (user != 0) {
      user->sec = t / CLOCKS_PER_SEC;
      user->usec = ((t - (user->sec * CLOCKS_PER_SEC))*1000000)/CLOCKS_PER_SEC;
    }
    if (system != 0) {
      system->sec = 0;
      system->usec = 0;
    }
  }
}

void osdep_pagefaults( unsigned *major, unsigned *minor )
{
  *major = 0;
  *minor = 0;
}

static void get_rtclock( stat_time_t *real )
{
  real->sec = 0;
  real->usec = 0;
}

#endif /* GENERIC_OS */

#if USE_GENERIC_FILESYSTEM || GENERIC_OS

/* remove() is in Standard C */
void osdep_unlinkfile( word w_fn )
{
  char *fn = string2asciiz( w_fn );
  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  globals[ G_RESULT ] = remove( fn ) ? fixnum(-1) : fixnum(0);
}

/* Standard C does not have a procedure to get the modification time of
   a file, but if stat() exists we can use it.  If not, return a vector
   containing midnight, January 1, 1970 always.  This is consistent with
   the result returned by osdep_access(), below.
   */
void osdep_mtime( word w_fn, word w_buf )
{
  int r = 0;
  struct tm *tm;

#ifdef HAVE_STAT
  struct stat s;
  const char *fn = string2asciiz( w_fn );

  r = stat( fn, &s );
#else
  struct {
    time_t st_mtime;
  } s;
  s.st_mtime = 0;
#endif

  if (r != 0)
  {
    globals[ G_RESULT ] = fixnum(-1);
    return;
  }
  tm = localtime( &s.st_mtime );
  vector_set( w_buf, 0, fixnum( tm->tm_year + 1900 ) );
  vector_set( w_buf, 1, fixnum( tm->tm_mon + 1 ) );
  vector_set( w_buf, 2, fixnum( tm->tm_mday ) );
  vector_set( w_buf, 3, fixnum( tm->tm_hour ) );
  vector_set( w_buf, 4, fixnum( tm->tm_min ) );
  vector_set( w_buf, 5, fixnum( tm->tm_sec ) );
  globals[ G_RESULT ] = fixnum( 0 );
}

/* Standard C does not have a procedure to check whether a file exists. 
   We use stat() if we have it; many systems do.  If not, try to open
   the file in read mode to find out if it exists; this is usually OK
   (not always).  The mode is ignored.
*/
void osdep_access( word w_fn, word w_bits )
{
#ifdef HAVE_STAT
  struct stat s;

  globals[ G_RESULT ]= fixnum(stat(string2asciiz(w_fn), &s ));
#else
  FILE *fp;
  const char *fn = string2asciiz( w_fn );

  if ((fp = fopen( fn, "r" )) != 0)
  {
    fclose( fp );
    globals[ G_RESULT ] = fixnum(0);
  }
  else
    globals[ G_RESULT ] = fixnum(-1);
#endif
}

/* Rename is in Standard C. */
void osdep_rename( word w_from, word w_to )
{
  if (string_length(w_from) > FILENAME_MAX)
    globals[ G_RESULT ] = fixnum(-1);
  else
  {
    char fnbuf[ FILENAME_MAX+1 ];
    strcpy( fnbuf, string2asciiz( w_from ) );
    globals[ G_RESULT ] = fixnum( rename( fnbuf, string2asciiz(w_to) ) );
  }
}
#endif /* USE_GENERIC_FILESYSTEM || GENERIC_OS */

#if USE_GENERIC_IO || GENERIC_OS

struct finfo {
  FILE *fp;
  int  mode;
};

static const int MODE_TEXT = 1;
static const int MODE_BINARY = 2;
static const int MODE_APPEND = 4;
static const int MODE_READ = 8;
static const int MODE_WRITE = 16;
static const int MODE_INTERMITTENT = 32;

static struct finfo *fdarray = 0;
static int num_fds = 0;

#ifdef USE_STDIO
static void check_standard_filedes()
{
  if (fdarray == 0)
  {
    fdarray = (struct finfo*)must_malloc( sizeof(struct finfo)*3 );
    num_fds = 3;
    fdarray[0].fp = stdin;
    fdarray[0].mode = MODE_TEXT | MODE_READ | MODE_INTERMITTENT;
    fdarray[1].fp = stdout;
    fdarray[1].mode = MODE_TEXT | MODE_WRITE;
    fdarray[2].fp = stderr;
    fdarray[2].mode = MODE_TEXT | MODE_WRITE;
  }
}
#endif

void osdep_openfile( word w_fn, word w_flags, word w_mode )
{
  char *fn = string2asciiz( w_fn );
  int i, flags = nativeint( w_flags );
  char newflags[5];
  char *p = newflags;
  int mode = 0;
  FILE *fp;

#ifdef USE_STDIO
  check_standard_filedes();
#endif

  /* This is a real thin pipe for the semantics ... */
  if (flags & 0x01) { *p++ = 'r'; mode |= MODE_READ; }
  if (flags & 0x02) { *p++ = 'w'; mode |= MODE_WRITE; }
  if (flags & 0x04) *p++ = '+';
  if (flags & 0x20) { *p++ = 'b'; mode |= MODE_BINARY; }
  *p = '\0';

  if (!(mode & MODE_BINARY))
    mode |= MODE_TEXT;

  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  fp = fopen( fn, newflags );
  if (fp == NULL) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }

  /* Now register the file and return the table index. */
  for ( i=0 ; i < num_fds && fdarray[i].fp != 0 ; i++ )
    ;
  if (i == num_fds) {
    int n = max(2*num_fds,5);
    struct finfo *narray = (struct finfo*)must_malloc( sizeof(struct finfo)*n );
    if (fdarray != 0)
      memcpy( narray, fdarray, sizeof(struct finfo)*num_fds );
    for ( i=num_fds ; i < n ; i++ )
    {
      narray[i].fp = 0;
      narray[i].mode = 0;
    }
    i = num_fds;
    num_fds = n;
    if (fdarray != 0)
      free( fdarray );
    fdarray = narray;
  }
  fdarray[i].fp = fp;
  fdarray[i].mode = mode;
  globals[ G_RESULT ] = fixnum(i);
}

void osdep_closefile( word w_fd )
{
  int fd = nativeint( w_fd );

#ifdef USE_STDIO
  check_standard_filedes();
#endif

  assert( fd >= 0 && fd < num_fds );

  if (fdarray[fd].fp == 0)
    globals[ G_RESULT ] = fixnum(-1);
  else if (fclose( fdarray[fd].fp ) == EOF)
    globals[ G_RESULT ] = fixnum(-1);
  else 
    globals[ G_RESULT ] = fixnum(0);
  fdarray[fd].fp = 0;
  fdarray[fd].mode = 0;
}

void osdep_readfile( word w_fd, word w_buf, word w_cnt )
{
  int fd = nativeint( w_fd );
  FILE *fp;
  char *buf, *resp;
  size_t nbytes, res;

#ifdef USE_STDIO
  check_standard_filedes();
#endif

  assert( fd >= 0 && fd < num_fds );

  if (fdarray[fd].fp == 0) {
    globals[ G_RESULT ] = fixnum(-1);
    return;
  }
  fp = fdarray[fd].fp;
  buf = string_data(w_buf);
  nbytes = nativeint(w_cnt);
  if ((fdarray[fd].mode & (MODE_TEXT|MODE_INTERMITTENT)) == (MODE_TEXT|MODE_INTERMITTENT))
  {
    // On some platforms, certainly Win32, fread() is not line buffered on stdin.
    resp = fgets( buf, nbytes, fp );
    res = (resp == 0 ? 0 : strlen(buf));
  }
  else
    res = fread( buf, 1, nbytes, fp );
  if (res == 0 && ferror(fp))
    globals[G_RESULT] = fixnum(-1);
  else
    globals[G_RESULT]= fixnum(res);
}

void osdep_writefile( word w_fd, word w_buf, word w_cnt, word w_offset )
{
  int fd = nativeint( w_fd );
  FILE *fp;
  char *buf;
  size_t nbytes, res, offset;

#ifdef USE_STDIO
  check_standard_filedes();
#endif

  assert( fd >= 0 && fd < num_fds );

  if (fdarray[fd].fp == 0) {
    globals[ G_RESULT ] = fixnum(-1);
    return;
  }
  fp = fdarray[fd].fp;
  buf = string_data(w_buf);
  nbytes = nativeint(w_cnt);
  offset = nativeint(w_offset);
  res = fwrite( buf+offset, 1, nbytes, fp );
  if (res < nbytes && ferror(fp))
    globals[G_RESULT] = fixnum(-1);
  else
    globals[G_RESULT] = fixnum(res);
}

/* Standard C does not have a procedure to check for input-ready.
   Return 1 always to indicate input ready.  This is correct for disk 
   files, but not for intermittent input sources (console, etc).
   */
void osdep_pollinput( word w_fd )
{
#ifdef USE_STDIO
  check_standard_filedes();
#endif

  globals[ G_RESULT ] = fixnum(1);
}
#endif /* if USE_GENERIC_IO || GENERIC_OS */


#if USE_GENERIC_ALLOCATOR || GENERIC_OS
/* Memory management.
   
   In a portable system the most we can rely on is malloc, and malloc
   does not come with any alignment guarantees.  Therefore we always
   allocate blocks that are 4096 bytes larger than we need and return
   a pointer to an aligned boundary inside the allocated block.  We
   need to keep track of the mapping from the derived pointer to the
   original so that the block can be freed again; that is done by
   keeping a linear array of mappings that is searched linearly when
   a block is freed.  In practice the number of blocks is small and
   the performance is not a problem.

   The simpleminded allocation technique incurs substantial internal
   fragmentation overheads -- in the DOF collector, where blocks are
   small and remembered-set pool nodes are even smaller, fragmentation
   easily reaches 10% of the live memory.  The code below should be
   improved to do one of several things:

     - allocate larger blocks then parcel out the blocks to fill 
       requests

     - allocate blocks from malloc with smaller overheads than 4096,
       eg overhead of powers of 2 starting at 0 bytes, returning the
       first block that can accomodate the required alignment
   */

static void register_pointer( byte *derived, byte *original );
static byte *find_and_free_pointer( byte *p );

struct regentry {
  byte *original;
  byte *derived;
};

static struct regentry *registry = 0;
static int reg_next = 0;
static int reg_size = 0;
static int fragmentation = 0;

void *osdep_alloc_aligned( int bytes )
{
  byte *p, *q;

again:
  p = (byte*)malloc( bytes+4096 );
  if (p == 0) {
    memfail( MF_MALLOC, "Failed to allocate %d bytes heap memory.", bytes );
    goto again;
  }
  q = (byte*)roundup( (word)p, 4096 );
  fragmentation += 4096;
  register_pointer( q, p );
  return q;
}

void osdep_free_aligned( void *p, int bytes )
{
  fragmentation -= 4096;
  free( find_and_free_pointer( p ) );
}

int osdep_fragmentation( void )
{
  return fragmentation;
}

static void register_pointer( byte *derived, byte *original )
{
  int i, j;

  if (reg_next == reg_size) {
    /* It's full, so compact it and see what happens */
    j = 0;
    for ( i=0 ; i < reg_size; i++ ) {
      if (registry[i].original != 0) {
	registry[j] = registry[i];
	j++;
      }
    }

    if (j < reg_size) {
      /* Compaction succeeded */
      reg_next = j;
    }
    else {
      /* Compaction failed: registry is full, so double its size. */
      struct regentry *new_reg;
      int k;

      k = max( 256, reg_size * 2 );
      new_reg = (struct regentry *)must_malloc( k*sizeof( struct regentry ) );
      for ( i=0 ; i < reg_size ; i++ )
	new_reg[i] = registry[i];
      if (registry != 0) free( registry );
      registry = new_reg;
      reg_size = k;
    }
  }
	
  registry[reg_next].original = original;
  registry[reg_next].derived = derived;
  reg_next++;
}

static byte *find_and_free_pointer( byte *derived )
{
  int i;
  byte *p;

  for ( i=0 ; i < reg_next && registry[i].derived != derived ; i++ )
    ;

  assert( i < reg_next );

  p = registry[i].original;
  registry[i].original = registry[i].derived = 0;
  return p;
}
#endif  /* USE_GENERIC_ALLOCATOR || GENERIC_OS */

/* eof */
