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

#if GENERIC_OS

#include "larceny.h"
#include "assert.h"

static stat_time_t real_start;

static void get_rtclock( stat_time_t *real );
static void register_pointer( byte *derived, byte *original );
static byte *find_and_free_pointer( byte *p );

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

/* Return the current time in milliseconds since initialization */
unsigned stats_rtclock( void )
{
  stat_time_t now;

  get_rtclock( &now );
  return now.sec * 1000 + now.usec / 1000;
}

/* Fill in the structures with real, user, system times. */
void 
stats_time_used( stat_time_t *real, stat_time_t *user, stat_time_t *system )
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

void stats_pagefaults( unsigned *major, unsigned *minor )
{
  *major = 0;
  *minor = 0;
}

static void get_rtclock( stat_time_t *real )
{
  real->sec = 0;
  real->usec = 0;
}

/* remove() is in Standard C */
void osdep_removefile( w_fn )
{
  char *fn = string2asciiz( w_fn );
  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  globals[ G_RESULT ] = remove( fn ) ? fixnum(-1) : fixnum(0);
}

/* Standard C does not have a procedure to get the modification time of
   a file.  Return a vector containing midnight, January 1, 1970 always.  
   This is consistent with the result returned by osdep_access(), below.
   */
void osdep_mtime( w_fn, w_buf )
{
  vector_set( w_buf, 0, fixnum( 1970 ) );
  vector_set( w_buf, 1, fixnum( 1 ) );
  vector_set( w_buf, 2, fixnum( 1 ) );
  vector_set( w_buf, 3, fixnum( 0 ) );
  vector_set( w_buf, 4, fixnum( 0 ) );
  vector_set( w_buf, 5, fixnum( 0 ) );
  globals[ G_RESULT ] = fixnum( 0 );
}

/* Standard C does not have a procedure to check whether a file exists. 
   Return 1 always to indicate that file exists. 
*/
void osdep_access( w_fn, w_buf )
{
  globals[ G_RESULT ] = fixnum(1);
}

/* Rename is in Standard C. */
void osdep_rename( w_from, w_to )
{
  char fnbuf[ 1024 ];

  strcpy( fnbuf, string2asciiz( w_from ) );
  globals[ G_RESULT ] = fixnum( rename( fnbuf, string2asciiz( w_to ) ) );
}

#endif /* GENERIC_OS */

#if USE_GENERIC_IO || GENERIC_OS

/* I/O system.

   FIXME FIXME FIXME
   - This system is not complete, notice the #error tags.
   - This system needs to be integrated with the new Scheme-side I/O.
   */
static FILE **fdarray = 0;
static int num_fds = 0;

void osdep_openfile( w_fn, w_flags, w_mode ) /* w_mode not used */
{
  char *fn = string2asciiz( w_fn );
  int i, flags = nativeint( w_flags );
  char newflags[5];
  char p = newflags;

  /* This is a real thin pipe for the semantics ... */
  if (flags & 0x01) *p++ = 'r';
  if (flags & 0x02) *p++ = 'w';
  if (flags & 0x04) *p++ = '+';
  if (flags & 0x20) *p++ = 'b';
  *p = '\0';

  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  fp = fopen( fn, p );
  if (fp == NULL) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }

  /* Now register the file and return the table index. */
  for ( i=0 ; i < num_fds && fdarray[i] != 0 ; i++ )
    ;
  if (i == num_fds) {
    int n = 2*num_fds;
    FILE **narray = (FILE**)must_malloc( sizeof(FILE*)*n );
    if (fdarray != 0)
      memcpy( narray, fdarray, sizeof(FILE*)*num_fds );
    for ( i=num_fds ; i < n ; i++ )
      narray[i] = 0;
    i = num_fds;
    num_fds = n;
    if (fdarray != 0)
      free( fdarray );
    fdarray = narray;
  }
  fdarray[i] = fp;
  return i;
}

void osdep_closefile( w_fd )
{
  int fd = nativeint( w_fd );

  assert( fd >= 0 && fd < num_fds );

  if (fdarray[fd] == 0)
    globals[ G_RESULT ] = fixnum(-1);
  else if (fclose( fdarray[fd] ) == EOF)
    globals[ G_RESULT ] = fixnum(-1);
  else {
    fdarray[fd] = 0;
    globals[ G_RESULT ] = fixnum(0);
  }
}

void osdep_readfile( w_fd, w_buf, w_cnt )
{
  int fd = nativeint( w_fd );
  FILE *fp;

  assert( fd >= 0 && fd < num_fds );

  if (fdarray[fd] == 0) {
    globals[ G_RESULT ] = fixnum(-1);
    return;
  }
  fp = fdarray[fd];

#error "The generic OS interface has not been completed."

}

void osdep_writefile( w_fd, w_buf, w_cnt, w_offset )
{
  int fd = nativeint( w_fd );
  FILE *fp;

  assert( fd >= 0 && fd < num_fds );

  if (fdarray[fd] == 0) {
    globals[ G_RESULT ] = fixnum(-1);
    return;
  }
  fp = fdarray[fd];

#error "The generic OS interface has not been completed."

}

/* Standard C does not have a procedure to check for input-ready.
   Return 1 always to indicate input ready.  This is correct for disk 
   files, although not for intermittent input sources (console, etc).
   */
void osdep_pollinput( w_fd )
{
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
  q = (byte*)roundup( p, 4096 );
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
