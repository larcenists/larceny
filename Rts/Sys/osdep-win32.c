/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Operating-system dependent functionality -- barebones win32 (for now).
 */

#include "config.h"

#if defined( WIN32 )		/* This file is in effect only on Win32 */

#if !USE_GENERIC_IO
#  error "The WIN32 OS interface has not been completed: IO is missing"
#endif

#if !USE_GENERIC_ALLOCATOR
#  error "The WIN32 OS interface has not been completed: memory management is missing"
#endif

#if !USE_GENERIC_FILESYSTEM
#  error "The WIN32 OS interface has not been completed: File system is missing"
#endif

#include <stdio.h>
#include <time.h>
#include <io.h>

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

void osdep_poll_startup_events( void )
{
  /* Nothing now. */
}

/* system() is in ANSI/ISO C. */
void osdep_system( word w_cmd )
{
  char *cmd = string2asciiz( w_cmd );
  globals[ G_RESULT ] = fixnum(system( cmd ));
}

void osdep_os_version( int *major, int *minor )
{
  // FIXME: wrong
  *major = 0;
  *minor = 95;
}

/* Return the current time in milliseconds since initialization */
unsigned osdep_realclock( void )
{
  stat_time_t now;

  get_rtclock( &now );
  return now.sec * 1000 + now.usec / 1000;
}

void osdep_pagefaults( unsigned *major, unsigned *minor )
{
  // FIXME: Unimplemented
  *major = 0;
  *minor = 0;
}

unsigned osdep_cpuclock( void )
{
  // FIXME: It's wrong to return 0 here, because 0 means something magic
  // to the client, but the client needs to change.
  return max(1,(unsigned)((double)clock()*1000/CLOCKS_PER_SEC));
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
      // FIXME: missing
      system->sec = 0;
      system->usec = 0;
    }
  }
}

static void get_rtclock( stat_time_t *real )
{
  // It's wrong to return 0 here.

  // FIXME: this is CPU time, not real time.
  unsigned x = max(1,(unsigned)((double)clock()*1000/CLOCKS_PER_SEC));

  real->sec = x / 1000 ;
  real->usec = x % 1000 * 1000;
}

#endif /* defined( WIN32 ) */

/* eof */
