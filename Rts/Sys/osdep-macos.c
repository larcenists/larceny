/* Copyright 1998, 1999 Lars T Hansen.
 *
 * $Id$
 *
 * Operating system specific services: Macintosh Operating System.
 * The interface is specified in osdep.h.
 *
 * FIXME/BUGS
 *  - This implementation depends on Codewarrior because it uses 
 *    Codewarrior's Unix emulation libraries for some tasks.
 *  - File creator code is hardwired.
 *  - osdep_poll_startup_events implementation seems like a hack.
 *  - osdep_access implementation is a hack.
 *  - osdep_poll_input is not right.
 *  - osdep_system is not right, but we need a strategy for that one.
 *  - osdep_open_shared_object is not right, but we need a strategy for that one.
 */

#include "config.h"

#if MACOS		/* This file in effect only on MacOS systems */

#if !CODEWARRIOR
#  error "The file :Rts:Sys:osdep-macos.c is Codewarrior-dependent."
#endif

#include "gc_t.h"

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
     FIXME: It would be better to get the creator code from the application file. 
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

/* Must handle an event if there is one, and periodically must call out to the
   event handling mechanism in any case, to handle such tasks as blinking the
   cursor.
   */
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

/* FSpFGetInfo() may or may not be the right function here. */
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

word osdep_dlopen( const char *path )
{
  OSErr r;
  CFragConnectionID connID;
  Str255 errName;
  Ptr mainAddr;

  r = GetSharedLibrary( (unsigned char *)path, 
  			kPowerPCCFragArch, kLoadCFrag, &connID, &mainAddr, errName );
  if (r < 0) {
    hardconsolemsg( "GetSharedLibrary error: %#s: %#s (%d)\n", path, errName, r );
    return (word)0;
  }
  else
    return (word)connID;
}

word osdep_dlsym( word handle, const char *symbol )
{
  OSErr r;
  CFragConnectionID connID = (CFragConnectionID)handle;
  CFragSymbolClass symClass;
  Ptr symAddr;

  r = FindSymbol( connID, (const unsigned char*)symbol, &symAddr, &symClass );
  if (r < 0)
    return 0;
  else
    return (word)symAddr;
}

/* Note, the parameter must be a `pascal' string! */
void osdep_open_shared_object( word w_param, word results )
{
  char *path = (char *)(ptrof( vector_ref( w_param, 0 ) )+1);
  word desc;
  void *tbl, *ntbl;
  void *data, *ndata;
  gc_t *gc = the_gc( globals );
  word *rh = gc_make_handle( gc, results );
  word *hdata;
  int tbl_bytes, data_bytes;
  
  desc = osdep_dlopen( path );
  if (desc == 0) 
    return;
  tbl = (void*)osdep_dlsym( desc, (char*)"\ptwobit_entry_table" );
  if (tbl == 0)
    return;
  data = (void*)osdep_dlsym( desc, (char*)"\ptwobit_data" );
  if (data == 0)
    return;
  
  /* It's illegal to have tagged pointers to outside the heap,
     so copy the data into heap-allocated data structures.
     */
  tbl_bytes = sizefield(*(word*)tbl)+sizeof(word)*VEC_HEADER_WORDS;
  data_bytes = sizefield(*(word*)data)+sizeof(word);

  ndata = gc_allocate( gc, data_bytes, FALSE, TRUE );
  memcpy( ndata, data, data_bytes );
  hdata = gc_make_handle( gc, tagptr( ndata, BVEC_TAG ) );  /* Another alloc coming... */
  
  ntbl = gc_allocate( gc, tbl_bytes, FALSE, FALSE );
  memcpy( ntbl, tbl, tbl_bytes );
 
  vector_set( *rh, 0, tagptr( ntbl, VEC_TAG ) );
  vector_set( *rh, 1, *hdata );

  gc_free_handle( gc, rh );
}

void osdep_os_version( int *major, int *minor )
{
  long response;
  
  *major = *minor = 0;
  if (Gestalt( gestaltSystemVersion, &response ) == 0) {
    *major = (response & 0x0F00) >> 8;
    *minor = (response & 0xF0) >> 4;
  }
}

/* Return the current time in milliseconds since initialization */
unsigned osdep_realclock( void )
{
  stat_time_t now;

  get_rtclock( &now );
  return now.sec * 1000 + now.usec / 1000;
}

unsigned osdep_cpuclock( void )
{
  return clock() * 1000 / CLOCKS_PER_SEC;
}

/* Fill in the structures with real, user, system times. */
void osdep_time_used( stat_time_t *real, stat_time_t *user, stat_time_t *system )
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
  UnsignedWide now;
  long long t;
  int usec, sec;

  Microseconds( &now );
  t = ((long long)now.hi << 32) + now.lo;
  sec = (t / 1000000) - real_start.sec;
  usec = (t % 1000000) - real_start.usec;
  if (usec < 0) {
    usec += 1000000;
    sec -= 1;
  }
  real->sec = sec;
  real->usec = usec;
}


#endif /* if MACOS */

/* eof */
