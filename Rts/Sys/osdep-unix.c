/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Operating-system dependent functionality -- Unix-type systems.
 */

#include "config.h"

#if defined(UNIX)		/* This file in effect only on Unix systems */

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <time.h>
#include <poll.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(SUNOS4)		/* Not in any header file. */
extern int gettimeofday( struct timeval *tp, struct timezone *tzp );
extern int getrusage( int who, struct rusage *rusage );
extern int rename( const char *oldname, const char *newname );
extern int poll( struct pollfd *fds, unsigned long nfds, int timeout );
#endif

#include "larceny.h"

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
  /* Nothing now; eventually this is the place to check for
     signals and other asynchronous events.
     */
}

void osdep_openfile( w_fn, w_flags, w_mode )
word w_fn, w_flags, w_mode;
{
  char *fn = string2asciiz( w_fn );
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

void osdep_unlinkfile( w_fn )
word w_fn;
{
  char *fn = string2asciiz( w_fn );
  if (fn == 0) {
    globals[ G_RESULT ] = fixnum( -1 );
    return;
  }
  globals[ G_RESULT ] = fixnum( unlink( fn ) );
}

void osdep_closefile( w_fd )
word w_fd;
{
  globals[ G_RESULT ] = fixnum( close( nativeint( w_fd ) ) );
}

void osdep_readfile( w_fd, w_buf, w_cnt )
word w_fd, w_buf, w_cnt;
{
  globals[ G_RESULT ] = fixnum( read( nativeint( w_fd ),
				    string_data( w_buf ),
				    nativeint( w_cnt ) ) );
}

void osdep_writefile( w_fd, w_buf, w_cnt, w_offset )
word w_fd, w_buf, w_cnt, w_offset;
{
  globals[ G_RESULT ] = fixnum( write( nativeint( w_fd ),
				     string_data(w_buf)+nativeint(w_offset),
				     nativeint( w_cnt ) ) );
}

/* File modification time as six-element vector */
void osdep_mtime( w_fn, w_buf )
word w_fn, w_buf;
{
  struct stat buf;
  struct tm *tm;

  if (stat( string2asciiz( w_fn ), &buf ) == -1) {
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

void osdep_access( w_fn, w_bits )
word w_fn, w_bits;
{
  int bits = nativeint( w_bits );
  int newbits = 0;

  if (bits & 0x01) newbits |= F_OK;
  if (bits & 0x02) newbits |= R_OK;
  if (bits & 0x04) newbits |= W_OK;
  if (bits & 0x08) newbits |= X_OK;
  globals[ G_RESULT ] = fixnum(access(string2asciiz(w_fn), newbits ));
}

void osdep_rename( w_from, w_to )
word w_from, w_to;
{
  char fnbuf[ 1024 ];

  strcpy( fnbuf, string2asciiz( w_from ) );
  globals[ G_RESULT ] = fixnum( rename( fnbuf, string2asciiz( w_to ) ) );
}

/* Poll is an X/OPENism */
void osdep_pollinput( w_fd )
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

/* system() is actually in Standard C. (!) */
void osdep_system( word w_cmd )
{
  char *cmd = string2asciiz( w_cmd );
  globals[ G_RESULT ] = fixnum(system( cmd ));
}

void osdep_os_version( int *major, int *minor )
{
  struct utsname name;

  uname( &name );
  sscanf( name.release, "%d.%d", major, minor );
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
    struct rusage buf;

    getrusage( RUSAGE_SELF, &buf );

    if (user != 0) {
      user->sec = buf.ru_utime.tv_sec;
      user->usec = buf.ru_utime.tv_usec;
    }
    if (system != 0) {
      system->sec = buf.ru_stime.tv_sec;
      system->usec = buf.ru_stime.tv_usec;
    }
  }
}

void stats_pagefaults( unsigned *major, unsigned *minor )
{
  struct rusage buf;

  getrusage( RUSAGE_SELF, &buf );
  *major = fixnum( buf.ru_majflt );
  *minor = fixnum( buf.ru_minflt );
}

static void get_rtclock( stat_time_t *real )
{
  struct timeval t;
  int usec, sec;

  gettimeofday( &t, (struct timezone *)0 );
  usec = t.tv_usec - real_start.usec;
  sec = t.tv_sec - real_start.sec;
  if (usec < 0) {
    usec += 1000000;
    sec -= 1;
  }
  real->sec = sec;
  real->usec = usec;
}
#endif /* defined(UNIX) */

/* eof */
