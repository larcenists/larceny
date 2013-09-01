/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Operating-system dependent functionality -- Unix-type systems.
 *
 * Portability issues:
 *  mmap/munmap    not available everywhere; see comments
 *  getrusage      not available everywhere?
 */

#include "config.h"

#if defined(UNIX)		/* This file in effect only on Unix systems */

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <sys/mman.h>		/* For mmap() and munmap() */
#include <limits.h>
#include <unistd.h>
#include <time.h>
#ifdef HAVE_POLL
# include <poll.h>
#endif
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_DLFCN
# include <dlfcn.h>
#endif

#if defined(SUNOS4)		/* Not in any header file. */
extern int gettimeofday( struct timeval *tp, struct timezone *tzp );
extern int getrusage( int who, struct rusage *rusage );
extern int rename( const char *oldname, const char *newname );
extern int poll( struct pollfd *fds, unsigned long nfds, int timeout );
#endif

#include "larceny.h"
#include "memmgr.h"		/* for GC_CHUNK_SIZE */

static stat_time_t real_start;

static void get_rtclock( stat_time_t *real );

void osdep_init( void )
{
  real_start.sec = 0;
  real_start.usec = 0;
  get_rtclock( &real_start );

  if ( getenv( LARCENY_ROOT ) == NULL ) {
    size_t cwdsize = pathconf( ".", _PC_PATH_MAX );
    char *cwd;

    if ( ( cwd = malloc(cwdsize) ) == NULL )
      panic_exit( "Couldn't allocate buffer for getcwd: %s",
                  strerror( errno ) );

    if ( getcwd( cwd, cwdsize ) == NULL )
      panic_exit( "Couldn't get cwd for LARCENY_ROOT: %s",
                  strerror( errno ) );

    if ( osdep_setenv( LARCENY_ROOT, cwd, 1) )
      panic_exit( "Couldn't set LARCENY_ROOT" );

    consolemsg( "LARCENY_ROOT not set; using current directory" );

    free( cwd );
  }
}

void osdep_poll_events( word *globals )
{
  /* Nothing now; eventually this is the place to check for
     signals and other asynchronous events.
     */
}

void osdep_poll_startup_events( void )
{
  /* Nothing now, and probably nothing ever. */
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
#if defined O_BINARY
  if (flags & 0x20) newflags |= O_BINARY;
#endif
#if defined O_RAW
  if (flags & 0x20) newflags |= O_RAW;
#endif

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

/* FIXME: limits offset to the size of a fixnum. */

void osdep_lseekfile( w_fd, w_offset, w_whence )
word w_fd, w_offset, w_whence;
{
  int whence_code = nativeint( w_whence );
  off_t whence;
  if ( whence_code == 0 )
    whence = SEEK_SET;
  else if ( whence_code == 1 )
    whence = SEEK_CUR;
  else if ( whence_code == 2 )
    whence = SEEK_END;
  else assert( 0 );
  globals[ G_RESULT ] = fixnum( lseek( nativeint( w_fd ),
                                       nativeint( w_offset ),
                                       whence ));
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
  tm = localtime( (time_t*)&buf.st_mtime );
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

void osdep_pollinput( w_fd )
word w_fd;
{
#if defined HAVE_POLL
  /* poll() is an X/OPENism, it is probably more efficient than select() */
  int r;
  struct pollfd fd[1];

  fd[0].fd = nativeint( w_fd );
  fd[0].events = POLLIN;
  r = poll( fd, 1, 0 );
  if (r == 1)
    globals[ G_RESULT ] = fixnum( fd[0].revents & (POLLIN|POLLHUP|POLLERR) );
  else
    globals[ G_RESULT ] = fixnum( r );
#elif defined HAVE_SELECT
  fd_set f;
  struct timeval zero;
  int r;

  FD_ZERO( &f );
  FD_SET( nativeint( w_fd ), &f );
  zero.tv_sec = 0;
  zero.tv_usec = 0;
  r = select( nativeint( w_fd )+1, &f, NULL, &f, &zero );
  globals[ G_RESULT ] = fixnum( r );
#else
  globals[ G_RESULT ] = fixnum(1);
#endif
}

/* system() is in ANSI/ISO C. */
void osdep_system( word w_cmd )
{
  char *cmd = string2asciiz( w_cmd );
  globals[ G_RESULT ] = fixnum(system( cmd ));
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

  if (getcwd( buf, sizeof(buf) ) == NULL)
    globals[G_RESULT] = FALSE_CONST;
  else
  {
    int k = strlen( buf );
    int nwords = roundup4(k)/4;
    word *p = alloc_from_heap( (nwords+1)*sizeof(word) );
    *p = mkheader( k, BV_HDR );
    memcpy( p+1, buf, k );
    globals[G_RESULT] = tagptr(p,BVEC_TAG);
  }
}

void osdep_os_version( int *major, int *minor )
{
  struct utsname name;

  uname( &name );
  sscanf( name.release, "%d.%d", major, minor );
}

/* Low level memory management */

#if !USE_GENERIC_ALLOCATOR	/* in osdep-generic.c */
/* mmap() and munmap() are not entirely portable, but work well where
   available, in particular because munmap() allows memory to be
   returned to the operating system.

   The following code has been tested on Solaris 2.6.

   4.4BSD, Linux, and DEC OSF/1 (X/OPEN) all have mmap()/munmap(), 
   though there may be slight variations -- I haven't studied them 
   in detail.
   
   4.3BSD and earlier do not have mmap()/munmap().
   */

#if RETURN_MEMORY_TO_OS
/* Simple strategy to avoid calling mmap()/munmap() all the time:
   keep a buffer of recently freed blocks and try to satisfy the
   request from it, and if there's space, put a block there rather
   than returning it right away.
   */
#define NUM_AVAILABLE 16	/* high for some, low for others? */
static struct {
  void *datum;			/* the block */
  int  bytes;			/* 0 if slot is empty */
} available[ NUM_AVAILABLE ];	/* free blocks not yet returned to OS */
static int  nextvictim;		/* index of next to eject */
#else
#define KILOBYTE      1024
#define NUM_AVAILABLE 4
#define NUM_HIST      5
/* Maintain quick lists of blocks of an integral number of 4K pages
   up to 256KB, and an LRU cache of blocks larger than that.  A
   request for a block from the size range of the quick list
   will be satisfied from the quick list or a new block of the
   right size will be allocated.  A request for a block from
   the size range outside the quick list will be satisfied by
   an exact match, if possible, or by a new block.

   Two mechanisms are used to combat retaining too much memory.

   Each quick list has a short history buffer associated with it: this
   is the history of nonzero numbers of entries of that size allocated
   between successive deallocations.  When presented with a
   deallocation, the size of the quick list is bounded by the average of
   the values in the history buffer; if more blocks than the bound are
   freed, then they are released to the OS.
   
   The LRU cache keeps an age field for each entry, and the age field
   is incremented on every allocation outside the range of the quick
   lists.  If an entry ages to more than twice the size of the cache, 
   it is evicted as being insufficiently often used.  
   */
static struct {
  void *datum;			/* the block */
  int  bytes;			/* 0 if slot is empty */
  int  age;
} available[ NUM_AVAILABLE ];	/* free blocks not yet returned to OS */

static struct {
  word *blocks;
  int  size;
  int  history[ NUM_HIST ];
  int  numallocs;
  int  bound;
} quick[ 256/4 ];

#endif
static int  pagesize = -1;	/* operating system's page size */
static void *addr_hint = 0;	/* address of the first returned block */
static int  fragmentation;	/* current fragmentation */
static int  initialized;

static hrtime_t mmap_time;
static hrtime_t munmap_time;

static void* alloc_block( int bytes )
{
  void *addr;

  if (pagesize == -1) {
    pagesize = getpagesize();
  }
  fragmentation += roundup( bytes, pagesize ) - bytes;
  assert( fragmentation >= 0 );

again:

  /* mmap /dev/zero is unsupported on MacOS X, according to Stevens
     and Rago, "Advanced Programming in the Unix Environment" (APUE).
     APUE provides an example that uses MAP_ANON instead of mmapping
     /dev/zero, which is what Felix changed the code below to do. */
  /* Felix also found the passing the addr_hint argument causes
     failures on Mac OS X; APUE advises passing in 0 as the addr_hint
     for maximal portability.  Felix is not sure why Lars chose to
     pass the most recent address as the addr_hint; perhaps it was an
     attempt to keep the size of the address space small? */

  addr = mmap( 0,
	       bytes,
	       (PROT_READ | PROT_WRITE | PROT_EXEC), 
	       (MAP_PRIVATE | MAP_ANON), 
	       -1, 
	       0 );
  if (addr == MAP_FAILED) {
    memfail( MF_HEAP, "mmap: %s: failed to map %d bytes.", 
	     strerror( errno ), bytes );
    goto again;
  }
  addr_hint = addr;
  return addr;
}

static void free_block( void *block, int bytes )
{
  fragmentation -= roundup( bytes, pagesize ) - bytes;
  assert( fragmentation >= 0 );

  if (munmap( block, bytes ) == -1)
    panic_abort( "munmap: %s: failed to unmap %d bytes.", 
		 strerror(errno), bytes );
}

void *osdep_alloc_aligned( int bytes )
{
  void *addr;
  int i;
  hrtime_t now = gethrtime();
  
  assert( bytes % 4096 == 0 );

  if (!initialized) {
#if !RETURN_MEMORY_TO_OS
    for ( i=0 ; i < sizeof(quick)/sizeof(quick[0]) ; i++ )
      quick[i].blocks = 0;
#endif
    initialized = 1;
  }

#if RETURN_MEMORY_TO_OS
  addr = 0;
  for ( i=0 ; i < NUM_AVAILABLE && addr == 0 ; i++ )
    if (available[i].bytes == bytes) {
      available[i].bytes = 0;
      addr = available[i].datum;
    }
  if (addr == 0)
    addr = alloc_block( bytes );
#else
  if (bytes <= 256*KILOBYTE) {
    i = (bytes / 4096)-1;
    if (quick[i].blocks != 0) {
      addr = (word*)quick[i].blocks;
      quick[i].blocks = (void*)*(word*)quick[i].blocks;
      quick[i].size--;
    }
    else
      addr = alloc_block( bytes );
    quick[i].numallocs++;
  }
  else {
    int j;
    
    addr = 0;
    for ( i=0 ; i < NUM_AVAILABLE && addr == 0 ; i++ )
      if (available[i].bytes == bytes) {
	addr = available[i].datum;
	for ( j=i ; j < NUM_AVAILABLE-1 ; j++ )
	  available[j] = available[j+1];
	available[NUM_AVAILABLE-1].bytes = 0;
      }
    if (addr == 0)
      addr = alloc_block( bytes );

    /* Manipulate age counts, evict the old. */
    i=0;
    while (i < NUM_AVAILABLE) {
      if (available[i].bytes > 0 && ++available[i].age >= NUM_AVAILABLE*2) {
	free_block( available[i].datum, available[i].bytes );
	for ( j=i ; j < NUM_AVAILABLE-1 ; j++ )
	  available[j] = available[j+1];
	available[NUM_AVAILABLE-1].bytes = 0;
      }
      else
	i++;
    }
  }
#endif
  mmap_time += gethrtime() - now;
  return addr;
}

void osdep_free_aligned( void *block, int bytes )
{
  int i;
  hrtime_t now = gethrtime();
 
  assert( bytes % 4096 == 0 );

#if RETURN_MEMORY_TO_OS
  if (bytes > GC_CHUNK_SIZE) 
    free_block( block, bytes );
  else {
    for ( i=0 ; i < NUM_AVAILABLE && available[i].bytes > 0 ; i++ )
      ;
    if (i == NUM_AVAILABLE) {      /* Eject a block */
      i = nextvictim;
      nextvictim = (nextvictim+1) % NUM_AVAILABLE;
      free_block( available[i].datum, available[i].bytes );
    }
    available[i].bytes = bytes;
    available[i].datum = block;
  }
#else
  if (bytes <= (256*KILOBYTE)) {
    i=(bytes / 4096)-1;
    if (quick[i].numallocs > 0) {
      int sum = 0, j;
      void *p;
      
      for ( j=0 ; j < NUM_HIST-1 ; j++ ) {
	sum += quick[i].history[j];
	quick[i].history[j] = quick[i].history[j+1];
      }
      quick[i].history[j] = quick[i].numallocs;
      sum += quick[i].history[j];
      quick[i].bound = sum/NUM_HIST;
      quick[i].numallocs = 0;
      while (quick[i].size > quick[i].bound) {
	p = quick[i].blocks;
	quick[i].blocks = (word*)*(word*)p;
	free_block( p, bytes );
	quick[i].size--;
      }
    }
    if (quick[i].size < quick[i].bound) {
      *(word*)block = (word)quick[i].blocks;
      quick[i].blocks = block;
      quick[i].size++;
    }
    else
      free_block( block, bytes );
  }
  else {
    if (available[NUM_AVAILABLE-1].bytes > 0)
      free_block( available[NUM_AVAILABLE-1].datum,
		  available[NUM_AVAILABLE-1].bytes );
    for (i=NUM_AVAILABLE-1 ; i > 0 ; i-- )
      available[i] = available[i-1];
    available[0].bytes = bytes;
    available[0].datum = block;
    available[0].age = 0;
  }
#endif
  munmap_time += gethrtime() - now;
}

int osdep_fragmentation( void )
{
  return fragmentation;
}

#endif /* !USE_GENERIC_ALLOCATOR */

unsigned osdep_realclock( void )
{
  stat_time_t now;

  get_rtclock( &now );
  return now.sec * 1000 + now.usec / 1000;
}


/* How portable is getrusage()? */
unsigned osdep_cpuclock( void )
{
  struct rusage buf;
  int sec, usec;
  
  getrusage( RUSAGE_SELF, &buf );

  sec = buf.ru_utime.tv_sec + buf.ru_stime.tv_sec;
  usec = buf.ru_utime.tv_usec + buf.ru_stime.tv_usec;

  if (usec > 1000000) {
    usec -= 1000000;
    sec += 1;
  }

  return sec*1000 + usec / 1000;
}


/* How portable is getrusage()? */
void 
osdep_time_used( stat_time_t *real, stat_time_t *user, stat_time_t *system )
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

void osdep_pagefaults( unsigned *major, unsigned *minor )
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

/* One can debate whether the mode choices are right.
   Perhaps the mode should be a parameter to this function.

   Note: libjava.so requires RTLD_GLOBAL; currently that is
   hacked around in Scheme code.  RTLD_GLOBAL does not
   strike me as a reasonable default mode.  --lars
   */
word
osdep_dlopen( char *path )
{
#if defined DYNAMIC_LOADING && defined HAVE_DLFCN
# if defined(SUNOS4)
  int mode = 1;
# elif defined(CYGWIN)
  int mode = RTLD_LAZY;
# else
  int mode = RTLD_LAZY | RTLD_LOCAL;
# endif
  void *desc = dlopen( path, mode );
  if (desc == 0)
    hardconsolemsg( "dlopen error: %s", dlerror() );
  return (word)desc;
#else
# ifndef DYNAMIC_LOADING
  hardconsolemsg( "Larceny configured without DYNAMIC_LOADING" );
# endif
  return 0;
#endif
}

word
osdep_dlsym( word handle, char *sym )
{
#if defined DYNAMIC_LOADING && defined HAVE_DLFCN
  return (word)dlsym( (void*)handle, sym );
#else
  return 0;
#endif
}

/*
 * osdep_setenv needs to keep track of what memory it allocates, so it can free
 * it later.  To do this, we keep a linked list of a pointer to the buffer we
 * allocate and a pointer to the matching value that will be returned by
 * getenv.  (On conforming putenv()s, value will point into buffer, but on some
 * older systems, which copy the argue to putenv, it may not.  This should be
 * correct in either case, provided getenv(FOO) == getenv(FOO).)
 */
struct setenv_struct {
    char *buffer;
    char *value;
    struct setenv_struct *next;
};

int
osdep_setenv(const char *name, const char *value, int overwrite)
{
  static struct setenv_struct *setenv_list = NULL;
  char *oldvalue;

  /*
   * The name had better not have an = in it, or funny things will
   * happen.
   */
  if (strchr(name, '=')) {
      return -1;
  }
  
  /*
   * The value returned by getenv provides an index into setenv_list for
   * buffers we have allocated.
   */
  oldvalue = getenv(name);

  if (overwrite || oldvalue == NULL) {
    char *buf;
    struct setenv_struct *node;

    for (node = setenv_list; node != NULL; node = node->next) {
      if (node->value == oldvalue) {
        /*
         * We're replacing an environment variable that _we_ set earlier.
         * First we have to tell putenv to release its pointer to the old
         * buffer, and then we can free it.  We have to discard the
         * constness from name to avoid a compiler warning -- we know if
         * there's no '=' in name then putenv won't hold onto it.
         */
        putenv((char *)name);
        free(node->buffer);
        break;
      }
    }

    /*
     * We haven't allocated for this env var before, so we need a new node
     * in our linked list.  Stick it on the head of the list.
     */
    if (node == NULL) {
      node = malloc(sizeof(*node));
      if (node == NULL) {
        return -1;
      }

      node->next = setenv_list;
      setenv_list = node;
    }

    buf = malloc(strlen(name) + strlen(value) + 2);
    if ( buf == NULL ) return -1;

    sprintf( buf, "%s=%s", name, value );
    putenv( buf );

    /*
     * Remember the address of the buffer we allocated and the value that
     * getenv will return next time corresponding to that buffer.
     */
    node->buffer = buf;
    node->value  = getenv(name);
  }

  return 0;
}

#endif /* defined(UNIX) */

/* eof */
