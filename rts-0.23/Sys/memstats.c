/*
 * This is the file Sys/memstats.c.
 *
 * Larceny run-time system (Unix) -- run-time statistics module.
 *
 * History
 *   June 28 - July 1, 1994 / lth (v0.20)
 *     Copied to this file from memsupport.c, massively rewritten.
 *
 */

#include <sys/time.h>

#ifdef SUNOS
/* For rusage() et al. */
#include <sys/resource.h>
#endif

#ifdef SOLARIS
/* For rusage() et al, which are obsolete. */
#include <sys/resource.h>
#include <sys/rusage.h>
#endif

#include <stdio.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

static void add();

#define MS_WCOLLECTED_HI   0       /* words reclaimed */
#define MS_WCOLLECTED_LO   1
#define MS_WALLOCATED_HI   2       /* words allocated */
#define MS_WALLOCATED_LO   3
#define MS_TSCANNED_HI     4       /* transactions scanned */
#define MS_TSCANNED_LO     5
#define MS_ECOLLECTIONS    6       /* number of ephemeral collections */
#define MS_TCOLLECTIONS    7       /* number of tenuring collections */
#define MS_FCOLLECTIONS    8       /* number of full collections */
#define MS_FRAMESFLUSHED   9       /* number of frames flushed */
#define MS_GCTIME          10      /* ms spent doing GC */
#define MS_TALLOCATED_HI   11      /* transactions allocated in SSB */
#define MS_TALLOCATED_LO   12

#define MS_SIZE 13

static unsigned memstats[ MS_SIZE ];

static unsigned time_before,       /* for gc stats */
                heap_before,       /* ditto */
                heap_after,        /* ditto */
                rtstart;           /* for memstat_rtclock() */

void memstat_init( show_heapstats )
int show_heapstats;
{
  heap_after = used_espace() + used_tspace();
  rtstart = memstat_rtclock();

#if 0
  if (show_heapstats) {
    consolemsg( "Heap statistics:\n");
    consolemsg( "  Size of ephemeral area: %lu bytes\n", size_espace() );
    consolemsg( "  Size of tenured area: %lu bytes\n", size_tspace() );
    consolemsg( "  Live ephemeral data: %lu bytes\n", used_espace() );
    consolemsg( "  Live tenured data: %lu bytes\n", used_tspace() );
  }
#endif
}

void memstat_before_gc( type )
int type;
{
  time_before = memstat_rtclock();
  heap_before = used_espace() + used_tspace();

  switch (type) {
    case EPHEMERAL_COLLECTION : memstats[ MS_ECOLLECTIONS ]++; break;
    case TENURING_COLLECTION  : memstats[ MS_TCOLLECTIONS ]++; break;
    case FULL_COLLECTION      : memstats[ MS_FCOLLECTIONS ]++; break;
  }

  add( &memstats[ MS_WALLOCATED_HI ],
       &memstats[ MS_WALLOCATED_LO ],
       (heap_before - heap_after) / sizeof( word ) );
}

void memstat_after_gc()
{
  unsigned time_after;

  time_after = memstat_rtclock();
  heap_after = used_espace() + used_tspace();
  
  memstats[ MS_GCTIME ] += time_after - time_before;
  add( &memstats[ MS_WCOLLECTED_HI ],
       &memstats[ MS_WCOLLECTED_LO ],
       (heap_before - heap_after) / sizeof( word ) );
}

void memstat_framesflushed( n )
unsigned n;
{
#ifdef DEBUG
  consolemsg( "[debug] Frames flushed: %u.", n );
#endif
  memstats[ MS_FRAMESFLUSHED ] += n;
}

void memstat_transactions_allocated( n )
unsigned n;
{
#ifdef DEBUG
  consolemsg( "[debug] Transactions allocated: %u.", n );
#endif
  add( &memstats[ MS_TALLOCATED_HI ], &memstats[ MS_TALLOCATED_LO ], n );
}

void memstat_transactions_scanned( n )
unsigned n;
{
#ifdef DEBUG
  consolemsg( "[debug] Transactions scanned: %u.", n );
#endif
  add( &memstats[ MS_TSCANNED_HI ], &memstats[ MS_TSCANNED_LO ], n );
}

unsigned memstat_rtclock()
{
  struct timeval t;
  struct timezone tz;

  if (gettimeofday( &t, &tz ) == -1)
    return -1;
  return (t.tv_sec * 1000 + t.tv_usec / 1000) - rtstart;
}

void memstat_fillvector( vp )
word *vp;
{
  struct rusage buf;
  unsigned usertime, systime, minflt, majflt;

  getrusage( RUSAGE_SELF, &buf );

#ifdef SUNOS
  systime = fixnum( buf.ru_stime.tv_sec * 1000 + buf.ru_stime.tv_usec / 1000);
  usertime = fixnum( buf.ru_utime.tv_sec * 1000 + buf.ru_utime.tv_usec / 1000);
#endif

#ifdef SOLARIS
  systime = fixnum( buf.ru_stime.tv_sec * 1000 + buf.ru_stime.tv_nsec / 1000000);
  usertime = fixnum( buf.ru_utime.tv_sec * 1000 + buf.ru_utime.tv_nsec / 1000000);
#endif
  majflt = fixnum( buf.ru_majflt );
  minflt = fixnum( buf.ru_minflt );

  vp[ STAT_RTIME ] = fixnum( memstat_rtclock() );
  vp[ STAT_STIME ] = systime;
  vp[ STAT_UTIME ] = usertime;
  vp[ STAT_MINFAULTS ] = minflt;
  vp[ STAT_MAJFAULTS ] = majflt;
  vp[ STAT_WCOLLECTED_HI ] = fixnum( memstats[ MS_WCOLLECTED_HI ] );
  vp[ STAT_WCOLLECTED_LO ] = fixnum( memstats[ MS_WCOLLECTED_LO ] );
  vp[ STAT_WALLOCATED_HI ] = fixnum( memstats[ MS_WALLOCATED_HI ] );
  vp[ STAT_WALLOCATED_LO ] = fixnum( memstats[ MS_WALLOCATED_LO ] );
  vp[ STAT_TSCANNED_HI ] = fixnum( memstats[ MS_TSCANNED_HI ] );
  vp[ STAT_TSCANNED_LO ] = fixnum( memstats[ MS_TSCANNED_LO ] );
  vp[ STAT_ECOLLECTIONS ] = fixnum( memstats[ MS_ECOLLECTIONS ] );
  vp[ STAT_TCOLLECTIONS ] = fixnum( memstats[ MS_TCOLLECTIONS ] );
  vp[ STAT_FCOLLECTIONS ] = fixnum( memstats[ MS_FCOLLECTIONS ] );
  vp[ STAT_FRAMESFLUSHED ] = fixnum( memstats[ MS_FRAMESFLUSHED ] );
  vp[ STAT_GCTIME ] = fixnum( memstats[ MS_GCTIME ] );
  vp[ STAT_TALLOCATED_HI ] = fixnum( memstats[ MS_TALLOCATED_HI ] );
  vp[ STAT_TALLOCATED_LO ] = fixnum( memstats[ MS_TALLOCATED_LO ] );
}


/*
 * Adds a word to a doubleword with carry propagation and stuff, both
 * parts of the doubleword are independently represented as fixnums, as is
 * 'x'. [ i.e. all operands better be congruent to 0 mod 4. ]
 */

#define LARGEST_FIXNUM (2147483644L)  /* ((2^29)-1)*4 */

static void add( hi, lo, x )
unsigned *hi, *lo, x;
{
  *lo += x;
  if (*lo > LARGEST_FIXNUM) {
    *lo -= LARGEST_FIXNUM;
    *hi += 4;
  }
}


/* eof */
