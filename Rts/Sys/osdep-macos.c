/* Copyright 1998, 1999 Lars T Hansen.
 *
 * $Id$
 *
 * Operating system specific services: Macintosh Operating System.
 * The interface is specified in osdep.h.
 *
 * This implementation depends on Codewarrior because it uses Codewarrior's
 * Unix emulation libraries for some tasks.  FIXME.
 */

#include "config.h"

#if defined(MACOS)		/* This file in effect only on MacOS systems */

#if !defined(CODEWARRIOR)
#  error "The file :Rts:Sys:osdep-macos.c is Codewarrior-dependent."
#endif

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <limits.h>
#include <stdlib.h>
#include <math.h>
#include <unix.h>		/* CodeWarrior */
#include <SIOUX.h>              /* CodeWarrior */
#include "larceny.h"
# include <console.h>

long os_get_next_event( EventRecord *e );
long os_handle_event( EventRecord *e );

void gettimeofday ( struct timeval *t, struct timezone *huh );

static stat_time_t real_start;

static void get_rtclock( stat_time_t *real );

void osdep_init( void )
{
  /* Set the creator for files to 'Plth': Petit Larceny.
     It would be better to get the creator code from the application file. 
     */
  _fcreator = 'Plth';
  real_start.sec = 0;
  real_start.usec = 0;
  get_rtclock( &real_start );

  /* Codewarrior */
  SIOUXSettings.asktosaveonclose = 0;
  SIOUXSettings.autocloseonquit = 1;
  SIOUXSetTitle( "\pPetit Larceny Transcript" );
}

/* Is there a principled way to do this?
   Loop for a while to process OpenDocument Apple Event to get the 
   heap file, if any.
   */
void osdep_poll_startup_events( void )
{
  EventRecord event; 
  int i;

  for ( i=0 ; i < 10 ; i++ ) {
    os_get_next_event( &event );
    os_handle_event( &event );
  }
}

void osdep_poll_events( word *globals )
{
  SIOUXHandleOneEvent( (EventRecord*)0 );
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
  if (flags & 0x20) newflags |= O_BINARY;
  
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
  FILE *fp;

  /* FIXME: this is at best an approximation */
  if ((fp = fopen( string2asciiz(w_fn), "rb" )) != NULL) {
    fclose( fp );
    globals[ G_RESULT ] = fixnum( 0 );
  }
  else
    globals[ G_RESULT ] = fixnum( -1 ); 
}

void osdep_rename( w_from, w_to )
word w_from, w_to;
{
  char fnbuf[ 1024 ];

  strcpy( fnbuf, string2asciiz( w_from ) );
  globals[ G_RESULT ] = fixnum( rename( fnbuf, string2asciiz( w_to ) ) );
}

void osdep_pollinput( w_fd )
word w_fd;
{
  globals[ G_RESULT ] = fixnum( 1 ); /* FIXME */
}

void osdep_system( word w_cmd )  /* FIXME */
{
  hardconsolemsg( "SYSTEM primitive not implemented on this platform." );
  globals[ G_RESULT ] = fixnum( 1 );
}

void osdep_os_version( int *major, int *minor )
{
  /* FIXME: this is wrong */
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

/* e_gettimeofday.c */
/* 18Oct95  e   -- from runtime.c */
/* From Moscow ML 1.43, copyright status unknown.  Author Doug Currie. */

/* NOTE! This does not have adequate resolution for modern machines;
   is is perfectly conceivable that a garbage collection can complete
   in (much) less than one tick, for example.  We need a better timer.
   FIXME.  --lars
   */
  
#define TICKS_PER_SEC 60

static long tod_offset;

void gettimeofday ( struct timeval *t, struct timezone *tz )
{
  #pragma unused ( tz )
  unsigned long now, ticks, secs;
  long offs;
  now = LMGetTime();    // get Mac Time
  ticks = LMGetTicks(); // get Mac Ticks
  // convert ticks to timeofday
  secs = ticks / TICKS_PER_SEC;  // floor
  ticks -= secs * TICKS_PER_SEC; // mod
  secs += tod_offset;            // offset
  ticks = (ticks * 1000000) / TICKS_PER_SEC; // ticks -> usecs
  // see if the Mac clock was changed
  offs = secs - now;
  if( offs != 0 && offs != 1 && offs != -1)
  { // off by one second is OK
    tod_offset -= offs;
    secs -= offs;        // rts - (rts - now) => now
  }
#if ( __MWERKS__ >= 0x1100 )
  /* seconds between 1/1/1900 and 1/1/1904 */
  t->tv_sec = secs + (365L * 4L) * 24L * 60L * 60L;
#else
  t->tv_sec = secs;
#endif
  t->tv_usec = ticks;
}

#endif /* if defined(MACOS) */

/* eof */
