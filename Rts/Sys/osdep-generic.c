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

#if defined(GENERIC_OS)

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "larceny.h"
#include "assert.h"

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

/* Note: the Unix-dependent OS interface on the Scheme side in 1.0a1 and
   later does not match this interface well.  FIXME.
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

/* Standard C does not have a procedure to check for input-ready.
   Return 1 always to indicate input ready.  This is correct for disk 
   files, although not for intermittent input sources (console, etc).
   */
void osdep_pollinput( w_fd )
{
  globals[ G_RESULT ] = fixnum(1);
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

#endif /* defined(GENERIC_OS) */

/* eof */
