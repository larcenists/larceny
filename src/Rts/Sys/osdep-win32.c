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

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define _X86_
#include <windef.h>
#include <winbase.h>

#include <stdio.h>
#include <time.h>
#include <io.h>
#include <ctype.h>
#include <stdarg.h>
#if 0
#include <dirent.h>             /* for listing directories */
#endif

#include "larceny.h"

static stat_time_t real_start;

static void get_rtclock( stat_time_t *real );

void osdep_init( void )
{
  char buf[MAX_PATH + 1];
  char *end;
  size_t l;

  real_start.sec = 0;
  real_start.usec = 0;
  get_rtclock( &real_start );

  if ( getenv( LARCENY_ROOT ) == NULL ) {
    if ( GetModuleFileName(NULL, buf, MAX_PATH + 1) == 0 )
      goto giveup;

    if ( (end = strrchr(buf, '\\')) == NULL )
      goto giveup;

    *end = '\0';

    if ( osdep_setenv(LARCENY_ROOT, buf, 1) )
      panic_exit( "Couldn't set LARCENY_ROOT" );
  }
  return;

giveup:
  return;
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
#ifdef __MWERKS__
  /* system() is broken in CodeWarrior 6, at least: once called
     with a command, it sticks with that command though it allows
     the arguments to be changed. 

     Examining the code for the function (included in the mwerks libs),
     the cause is obvious: it uses strcat on the string returned from
     getenv("COMSPEC").  Gag!  We might be able to hack around by
     preserving COMSPEC around calls to system, but who knows what
     else it clobbers.  So reimplement system() here.
  */
  char *cmd = string2asciiz( w_cmd );
  char *comspec = getenv( "COMSPEC" );
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  char command[1024];
  char *p;
  int n;
  int size;

  if (comspec == NULL || strlen(comspec) + strlen(cmd) + sizeof(" /C ") + 1 > sizeof(command))
  {
    globals[ G_RESULT ] = fixnum(1);
    return;
  }

  strcpy( command, comspec );
  strcat( command, " /C " );
  strcat( command, cmd );
  memset( &si, 0, sizeof( si ) );
  si.cb = sizeof(si);

  if (CreateProcess( NULL, command, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi) == 0)
  {
    globals[ G_RESULT ] = fixnum(1);
    return;
  }
  WaitForSingleObject(pi.hProcess, ~0L);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
  globals[ G_RESULT ] = fixnum(0);
#else
  char *cmd = string2asciiz( w_cmd );
  globals[ G_RESULT ] = fixnum(system( cmd ));
#endif /* __MWERKS__ */
}

void osdep_chdir( word w_cmd )
{
  char *path = string2asciiz( w_cmd );
  globals[ G_RESULT ] = fixnum(chdir(path));
}

/* FIXME: should return UTF-8 or something */

void osdep_cwd( void )
{
  char buf[FILENAME_MAX+1];
  int k;

  k = GetCurrentDirectory( sizeof(buf), buf );
  if (k == 0 || k >= sizeof(buf))
    globals[G_RESULT] = FALSE_CONST;
  else
  {
    int nwords = roundup4(k)/4;
    word *p = alloc_from_heap( (nwords+1)*sizeof(word) );
    *p = mkheader( k, BV_HDR );
    memcpy( p+1, buf, k );
    globals[G_RESULT] = tagptr(p,BVEC_TAG);
  }
}

#if 0
/* FIXME: this should work with gcc and many other compilers, */
/* but probably won't work with Microsoft compilers.          */

/* returns a freshly allocated bytevector containing dp */

void osdep_listdir_open( word w_path )
{
  DIR *dp;
  char *path = string2asciiz( w_path );
  word *q;

  dp = opendir( path );

  if (dp == NULL) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }

  q = alloc_from_heap( sizeof(word) + sizeof(DIR *) );
  *q = mkheader( sizeof(DIR *), BV_HDR );
  *((word **) string_data( q )) = (word *) dp;
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

/* given a bytevector created by osdep_listenv_init, returns */
/* the next file inthe directory and updates the bytevector  */
/* FIXME: hard-codes 4 as the header size (in bytes)         */

void osdep_listdir( generator )
word generator;
{
  DIR *dp = (DIR *) *((word *) (string_data ( generator )));
  struct dirent *entry;
  char *p;
  word *q;
  int l;

  if (dp == NULL) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }

  entry = readdir( dp );
  if (entry == NULL) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }
  
  p = entry->d_name;
  if (p == NULL) {
    globals[ G_RESULT ] = FALSE_CONST;
    return;
  }

  l = strlen( p );
  q = alloc_from_heap( sizeof(word) + l );
  *q = mkheader( l, BV_HDR );
  memcpy( string_data( q ), p, l );
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

void osdep_listdir_close( generator )
word generator;
{
  DIR *dp = (DIR *) *((word *) (string_data ( generator )));

  if ((dp == NULL) || closedir( dp )) {
    globals[G_RESULT] = FALSE_CONST;
  }
  else {
    globals[G_RESULT] = TRUE_CONST;
  }
}

#else

/* Loosely based upon a Microsoft example at
 * https://msdn.microsoft.com/en-us/library/windows/desktop/aa365200(v=vs.85).aspx
 */

void osdep_listdir_open( word w_path )
{
  WIN32_FIND_DATA ffd;
  HANDLE hFind = INVALID_HANDLE_VALUE;
  char *path = string2asciiz( w_path );
  char *p = path;
  word *q;

  /* We're going to add "\\*" at the end (that's two characters), */
  /* plus one extra in case the NUL character needs it or we      */
  /* miscounted. */

  if (strlen(path) > (MAX_PATH - 3)) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }

  /* Convert forward slashes to backslashes. */

  p = path;
  while (*p != 0) {
    if (*p == '/')
      *p = '\\';
    p++;
  }

  strcat( path, "\\*" );    /* FIXME: unsafe */

  hFind = FindFirstFile( path, &ffd );

  if (INVALID_HANDLE_VALUE == hFind) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }

  assert( sizeof(HANDLE) == 4 ); /* FIXME */

  q = alloc_from_heap( sizeof(word) + sizeof(HANDLE) );
  *q = mkheader( sizeof(HANDLE), BV_HDR );
  *((word **) string_data( q )) = (word *) hFind;
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

/* given a bytevector created by osdep_listenv_init, returns */
/* the next file inthe directory and updates the bytevector  */
/* FIXME: hard-codes 4 as the header size (in bytes)         */

void osdep_listdir( generator )
word generator;
{
  WIN32_FIND_DATA ffd;
  HANDLE hFind = (HANDLE) *((word *) (string_data ( generator )));
  char *p;
  word *q;
  int l;

  if (INVALID_HANDLE_VALUE == hFind) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }

  if ( ! FindNextFile( hFind, &ffd ) ) {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }
  
  p = (char *) & ffd.cFileName;

  l = strlen( p );
  q = alloc_from_heap( sizeof(word) + l );
  *q = mkheader( l, BV_HDR );
  memcpy( string_data( q ), p, l );
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

void osdep_listdir_close( generator )
word generator;
{
  HANDLE hFind = (HANDLE) *((word *) (string_data ( generator )));

  if ((INVALID_HANDLE_VALUE == hFind) || (! FindClose( hFind ))) {
    globals[G_RESULT] = FALSE_CONST;
  }
  else {
    globals[G_RESULT] = TRUE_CONST;
  }
}

/* end of FIXME above */

#endif


void osdep_os_version( int *major, int *minor )
{
  OSVERSIONINFO osvi;

  memset( &osvi, 0, sizeof(osvi) );
  osvi.dwOSVersionInfoSize = sizeof( osvi );
  if (!GetVersionEx(&osvi)) 
  {
    globals[G_RESULT] = FALSE_CONST;
    return;
  }

  /* One wonders if this is useful without the platform ID.
     So I encode the platform ID in the second byte.
  */
  switch (osvi.dwPlatformId)
  {
  case VER_PLATFORM_WIN32s:
    osvi.dwMajorVersion += 256;
    break;
  case VER_PLATFORM_WIN32_WINDOWS:
    osvi.dwMajorVersion += 256*2;
    break;
  case VER_PLATFORM_WIN32_NT:
    osvi.dwMajorVersion += 256*3;
    break;
  }
  *major = osvi.dwMajorVersion;
  *minor = osvi.dwMinorVersion;
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
  // Looks like the API function to use is GetProcessTimes()

  return (unsigned)((double)clock()*1000/CLOCKS_PER_SEC);
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

      // Looks like the API function to use is GetProcessTimes(), it might also
      // provide a better value for the user time.

      system->sec = 0;
      system->usec = 0;
    }
  }
}

static void get_rtclock( stat_time_t *real )
{
  // It's wrong to return 0 here.

  // FIXME: this is CPU time, not real time.

  // Looks like the API function to use is GetProcessTimes(),
  // combined with reading the current time.

  unsigned x = max(1,(unsigned)((double)clock()*1000/CLOCKS_PER_SEC));

  real->sec = x / 1000 ;
  real->usec = x % 1000 * 1000;
}

word
osdep_dlopen( char *path )
{
#ifdef DYNAMIC_LOADING
  HINSTANCE dll;

  dll = LoadLibrary(path);
  if (dll == 0) 
    hardconsolemsg( "dlopen error" );
  return (word)dll;
#else
  hardconsolemsg( "Larceny configured without DYNAMIC_LOADING" );
  return 0;
#endif
}

word
osdep_dlsym( word handle, char *sym )
{
#ifdef DYNAMIC_LOADING
  return (word)GetProcAddress( (HINSTANCE)handle, sym );
#else
  return 0;
#endif
}

int
osdep_setenv(const char *name, const char *value, int overwrite)
{
  if (overwrite || getenv(name) == NULL) {
    _putenv_s(name, value);
  }

  return 0;
}

#endif /* defined( WIN32 ) */

/* eof */
