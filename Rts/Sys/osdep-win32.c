/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Operating-system dependent functionality -- barebones win32 (for now).
 */

#include "config.h"

#if defined( WIN32 )		/* This file is in effect only on Win32 */

#error "The WIN32 OS interface has not been completed."

#include <stdio.h>
#include <time.h>
#include <io.h>

#define F_OK 1
#define R_OK 2
#define W_OK 4
#define X_OK 8

#include "larceny.h"

static stat_time_t real_start;

static void get_rtclock( stat_time_t *real );

void osdep_init( void )
{
  real_start.sec = 0;
  real_start.usec = 0;
  get_rtclock( &real_start );
}

void osdep_openfile( w_fn, w_flags, w_mode )
{
  FIXME
}

void osdep_removefile( w_fn )
{
  FIXME
}

void osdep_closefile( w_fd )
{
  FIXME
}

void osdep_readfile( w_fd, w_buf, w_cnt )
{
  FIXME
}

void osdep_writefile( w_fd, w_buf, w_cnt, w_offset )
{
  FIXME
}

void osdep_mtime( w_fn, w_buf )
{
  FIXME
}

void osdep_access( w_fn, w_buf )
{
  FIXME
}

void osdep_rename( w_from, w_to )
{
  FIXME
}

void osdep_pollinput( w_fd )
{
  globals[ G_RESULT ] = fixnum(1); /* close enough */
}

void osdep_system( word w_cmd )
{
  FIXME
}

void osdep_os_version( int *major, int *minor )
{
  *major = 0;
  *minor = 95;
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

#endif /* defined( WIN32 ) */

/* eof */
