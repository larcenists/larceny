/*
 * This is the file Sys/policy.c.
 *
 * Larceny run-time system (Unix) -- memory manager.
 *
 * History
 *   June 28 - July 1, 1994 / lth (v0.20)
 *     Pruned from memsupport.c.
 *
 * This file contains procedures for memory allocatation and garbage 
 * collection policy. It knows a great deal about the layout of the
 * heap.
 *
 * Heap layout:
 *   All other memory must have been allocated; the heap must be at the
 *   top of allocated memory. The heap has the following layout, from
 *   low addresses toward higher addresses.
 *
 *   Static area.
 *   Ephemeral areas (two).
 *   Tenured space A.
 *   Overflow space the size of an ephemeral area.
 *   Tenured space B.
 *   (program break)
 *
 * The stack lives at the high end of the ephemeral area, growing down.
 * The only procedure in here which "knows" this is the one which uses
 * the stack pointer to calculate free ephemeral space. However, if the
 * stack lived outside of the heap and would have to be copied into it
 * on a gc, some changes would have to be made in this file.
 *
 * Some things to do:
 *   - use of madvise() is indicated but not implemented, below.
 *   - may be good to lock the ephemeral area, the ssb, and the remset
 *     in memory to avoid paging them, using mlock().
 */


#include <sys/types.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

/* The value GC_SURV is the number n s.t. 1/n is a 'typical' survival
 * ratio after a minor garbage collection. It is a fudge factor used
 * by the garbage collector to determine whether to do a tenuring or
 * a full collection. We usually want to do as many tenuring collections
 * as possible (since they are quick); however, the heap must be expanded
 * to accomodate them. When the heap is expanded, VM usage goes up, and
 * after a while the system starts thrashing. The problem is to find some
 * sort of 'happy medium'; to put a lid on the heap size when it is
 * large enough to accomodate all live data. GC_SURV is used for this 
 * (see the comments in the implementation, below).
 *
 * If GC_SURV = 1, there will be essentially no tenuring collections; all
 * collections will be ephemeral or full.
 *
 * The ratio of tenuring collections to full collections raises with
 * the value of GC_SURV; however, so does the number of heap expansions.
 * The number of ephemeral collections is not directly affected by this
 * parameter.
 *
 * The value of GC_SURV has been determined empirically using the 10perm8
 * benchmark. A value between 3 and 5 seems to work fine for this benchmark
 * in 16 MB of real memory. The problem with this benchmark is that it is
 * somewhat atypical: it holds on to a lot of memory at a time. Some more
 * benchmarking should be done, with other programs.
 */
#define GC_SURV  3


/***************************************************************************
 *
 * Heap initialization and expansion.
 */

/* 
 * Procedure to intialize garbage collector.  All sizes are in bytes. 
 * Returns 1 if ok, 0 if not.
 */
int allocate_heap( ephemeral_size, tenured_size, static_size, ewatermark, twatermark )
unsigned ephemeral_size;    /* size of espace; 0 = default */
unsigned tenured_size;      /* initial size of tspace; 0 = default */
unsigned static_size;       /* size of sspace; 0 = default */
unsigned ewatermark;        /* tenuring threshold in %, 0 = default */
unsigned twatermark;        /* heap expansion threshold in % */
{
  word *heapptr;

  if (ephemeral_size == 0) ephemeral_size = DEFAULT_ESIZE;
  if (tenured_size == 0) tenured_size = DEFAULT_TSIZE;
  if (static_size == 0) static_size = DEFAULT_SSIZE;
  if (ewatermark <= 0 || ewatermark > 100)
    ewatermark = DEFAULT_EWATERMARK;
  if (twatermark <= 0 || twatermark > 100)
    twatermark = DEFAULT_TWATERMARK;

  ephemeral_size = roundup8( ephemeral_size );
  tenured_size = roundup8( tenured_size );
  static_size = roundup8( static_size );

  heapptr = (word*)sbrk(0);

  /* round up heap ptr to 8-byte boundary */
  heapptr = (word*)sbrk( roundup8( (unsigned)heapptr ) - (word)heapptr );
  if ((caddr_t)heapptr == (caddr_t)-1) return 0;

  if ((caddr_t)sbrk( static_size + 2*tenured_size + 3*ephemeral_size ) == (caddr_t)-1)
    return 0;

  globals[ G_STATIC_BOT ] = (word)heapptr;
  globals[ G_STATIC_TOP ] = (word)heapptr;
  heapptr += static_size / sizeof( word );
  globals[ G_STATIC_LIM ] = (word)heapptr;
  globals[ G_ESPACE1_BOT ] = (word)heapptr;
  heapptr += ephemeral_size / sizeof( word );
  globals[ G_ESPACE1_LIM ] = (word)heapptr;
  globals[ G_ESPACE2_BOT ] = (word)heapptr;
  heapptr += ephemeral_size / sizeof( word );
  globals[ G_ESPACE2_LIM ] = (word)heapptr;
  globals[ G_TSPACE1_BOT ] = (word)heapptr;
  heapptr += tenured_size / sizeof( word );
  globals[ G_TSPACE1_LIM ] = (word)heapptr;
  heapptr += ephemeral_size / sizeof( word ); /* overflow area */
  globals[ G_TSPACE2_BOT ] = (word)heapptr;
  heapptr += tenured_size / sizeof( word );
  globals[ G_TSPACE2_LIM ] = (word)heapptr;

  globals[ G_CONT ] = FALSE_CONST;
  globals[ G_EWATERMARK ] = ewatermark;
  globals[ G_TWATERMARK ] = twatermark;
  globals[ G_EBOT ] = globals[ G_ESPACE1_BOT ];
  globals[ G_ETOP ] = globals[ G_ESPACE1_BOT ];
  globals[ G_ELIM ] = globals[ G_ESPACE1_LIM ];
  globals[ G_TBOT ] = globals[ G_TSPACE1_BOT ];
  globals[ G_TTOP ] = globals[ G_TSPACE1_BOT ];
  globals[ G_TLIM ] = globals[ G_TSPACE1_LIM ];
  globals[ G_GC_MUST_TENURE ] = 0;
  globals[ G_STKP ] = globals[ G_ELIM ];

  if (!create_stack())
    panic( "Unable to create initial stack (you miser)." );

  return 1;
}

int free_espace()
{ return globals[ G_STKP ] - globals[ G_ETOP ]; }

int size_espace()
{ return globals[ G_ELIM ] - globals[ G_EBOT ]; }

int used_espace()
{ return size_espace() - free_espace(); }

int free_tspace()
{ return globals[ G_TLIM ] - globals[ G_TTOP ]; }  /* may be negative */

int size_tspace()
{ return globals[ G_TLIM ] - globals[ G_TBOT ]; }

int used_tspace()
{ return size_tspace() - free_tspace(); }

/*
 * Expand the tenured area. We always expand each tenured space with
 * the size of one ephemeral area; don't know how well this works in
 * practice, probably depends on the size of espace.
 */
static void expand_tspace()
{
  word *heapptr;
  word esize;

  if (globals[ G_TBOT ] != globals[ G_TSPACE1_BOT ])
    panic( "internal error in expand_tspace()." );

  esize = size_espace();
  heapptr = (word*)sbrk( 2*esize );
  if ((caddr_t)heapptr == (caddr_t)-1)
    panic( "Unable to expand the heap." );

  globals[ G_TSPACE1_LIM ] += esize;
  globals[ G_TSPACE2_BOT ] += esize;
  globals[ G_TSPACE2_LIM ] += 2*esize;
  globals[ G_TLIM ] += esize;

  consolemsg( "Expanding tenured area; new size = 0x%08lx bytes.",
	      globals[ G_TSPACE1_LIM ] - globals[ G_TSPACE1_BOT ] );
}

/***************************************************************************
 *
 * Allocation from C (sometimes necessary).
 */

/*
 * Allocate a chunk of ephemeral memory. `n' is a number of bytes.
 */
word *alloc_from_heap( n )
int n;
{
  word p;

  n = roundup8( n );
  if (globals[ G_ETOP ] + n >= globals[ G_ELIM ])
    garbage_collect( EPHEMERAL_COLLECTION, n );

  p = globals[ G_ETOP ];
  globals[ G_ETOP ] += n;
  return (word *)p;
}


/****************************************************************************
 *
 * Garbage collector -- the upper level.
 *
 * This code deals with policy, heap expansion, and all those nasty
 * things. When everything is set up, it calls the low-level collectors
 * in gc.c to do the dirty work.
 */

static void ephemeral_collection();
static void tenuring_collection();
static void full_collection();
static void eflip();
static void tflip();

void garbage_collect( type, request )
int type;       /* collection type requested */
int request;    /* words requested */
{
  request *= 4;  /* to get bytes */

  flush_stack();
  if (!compact_ssb())
    if (type == EPHEMERAL_COLLECTION)
      type = TENURING_COLLECTION;

  if (globals[ G_GC_MUST_TENURE ] && type == EPHEMERAL_COLLECTION)
    type = TENURING_COLLECTION;

 again:
  if (type == TENURING_COLLECTION) {
    if (globals[ G_TBOT ] == globals[ G_TSPACE1_BOT ]) {
      /* Tspace A has data */
      if (free_tspace() <= 0) {
	/* Tspace A has overflowed, so expand and do a full collection */
	expand_tspace();
	type = FULL_COLLECTION;
      }
    }
    else {
      /* tspace B has data */
      if (used_tspace() + used_espace() > size_tspace()) {
	/* B might overflow during this tenuring collection, so
	 * we do a full one; A can accomodate it all because of
	 * the overflow area.
	 */
	type = FULL_COLLECTION;
      }
    }

    if (type == TENURING_COLLECTION
     && used_tspace()+used_espace() < size_tspace()) {
      /* Maybe we should do a major collection w/o expansion? 
       * If the next tenuring collection would likely cause a full collection
       * (most tenuring collections will report that the espace is 'full'
       * before the collection) we do the full collection now, as a full
       * collection later would cause heap expansion.
       *
       * The point of this adjustment is to prevent premature expansion,
       * since too large a heap may cause VM thrashing. This calculation
       * appears to be quite effective at putting a 'lid' on the heap size.
       */
      if (used_tspace()+used_espace() > size_tspace()-size_espace()/GC_SURV)
	type = FULL_COLLECTION;
    }
  }

  if (type == FULL_COLLECTION) {
    if (globals[ G_TBOT ] == globals[ G_TSPACE1_BOT ]) {
      /* collecting from A to B */
      if (free_tspace() <= 0) {
	/* A has overflowed: expand. */
	expand_tspace();
      }
    }
  }

  memstat_before_gc( type );

  switch (type) {
    case EPHEMERAL_COLLECTION : ephemeral_collection(); break;
    case TENURING_COLLECTION  : tenuring_collection(); break;
    case FULL_COLLECTION      : full_collection(); break;
  }
  globals[ G_STKP ] = globals[ G_ELIM ];  /* for free_espace/create_stack */

  memstat_after_gc();

  if (!create_stack()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed create-stack." );
#endif
    if (type != EPHEMERAL_COLLECTION)
      panic( "Internal inconsistency in garbage collector." );
    type = TENURING_COLLECTION;
    goto again;
  }

  if (request > free_espace()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed free-espace %d.", request );
#endif
    if (type == EPHEMERAL_COLLECTION) {
      type = TENURING_COLLECTION;
      goto again;
    }
    else
      panic( "Object of size %d too large to allocate!\n", request );
  }

  if (used_espace() > size_espace()*globals[ G_EWATERMARK ]/100)
    globals[ G_GC_MUST_TENURE ] = 1;
  else
    globals[ G_GC_MUST_TENURE ] = 0;

  if (!restore_frame()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed restore-frame." );
#endif
    if (type != EPHEMERAL_COLLECTION)
      panic( "Unable to restore a stack frame after gc." );
    type = TENURING_COLLECTION;
    goto again;
  }

  /* 
   * If we performed a full collection into tspace A, and more data is
   * live than we consider reasonable, expand the heap to avoid thrashing.
   */
  if (type == FULL_COLLECTION
   && used_tspace() > size_tspace()/globals[ G_TWATERMARK ]*100
   && globals[ G_TBOT ] == globals[ G_TSPACE1_BOT ])
    expand_tspace();

  if (type != EPHEMERAL_COLLECTION) clear_remset();
}

/* This procedure is used by the low-level garbage collector */
void enumerate_roots( enumerator, p1, p2, p3 )
void (*enumerator)();
word *p1, *p2, **p3;
{
  int i;

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    enumerator( &globals[ i ], p1, p2, p3 );
}

static void ephemeral_collection()
{
  word *oldlo, *oldhi;

#ifdef DEBUG
  consolemsg( "[debug] GC: ephemeral collection." );
#endif
  oldlo = (word*)globals[ G_EBOT ];
  oldhi = (word*)globals[ G_ELIM ];
  eflip();
  globals[ G_ETOP ] =
    (word)minor_collection( oldlo, oldhi, (word*)globals[ G_ETOP ] );
}

static void tenuring_collection()
{
  word *oldlo, *oldhi;

#ifdef DEBUG
  consolemsg( "[debug] GC: tenuring collection." );
#endif
  oldlo = (word*)globals[ G_EBOT ];
  oldhi = (word*)globals[ G_ELIM ];
  eflip();
  globals[ G_TTOP ] =
    (word)minor_collection( oldlo, oldhi, (word*)globals[ G_TTOP ] );
}

static void full_collection()
{
  word *oldlo, *oldhi;

#ifdef DEBUG
  consolemsg( "[debug] GC: full collection." );
#endif
  oldlo = (word*)globals[ G_ESPACE1_BOT ];
  oldhi = (word*)globals[ G_TSPACE2_LIM ];

  tflip();
  globals[ G_ETOP ] = globals[ G_EBOT ];  /* reuse current espace */

#if 0
  madvise( MADV_WILLNEED, <tenured tospace> );
  madvise( MADV_SEQUENTIAL, <tenured tospace> ); 
  madvise( MADV_RANDOM, <tenured fromspace> );
#endif

  globals[ G_TTOP ] =
    (word)major_collection( oldlo, oldhi, (word*)globals[ G_TTOP ] );

#if 0
  madvise( MADV_DONTNEED, <tenured fromspace> );
#endif
}

static void eflip()
{
  if (globals[ G_EBOT ] == globals[ G_ESPACE1_BOT ]) {
    globals[ G_EBOT ] = globals[ G_ETOP ] = globals[ G_ESPACE2_BOT ];
    globals[ G_ELIM ] = globals[ G_ESPACE2_LIM ];
  }
  else {
    globals[ G_EBOT ] = globals[ G_ETOP ] = globals[ G_ESPACE1_BOT ];
    globals[ G_ELIM ] = globals[ G_ESPACE1_LIM ];
  }
}

static void tflip()
{
  if (globals[ G_TBOT ] == globals[ G_TSPACE1_BOT ]) {
    globals[ G_TBOT ] = globals[ G_TTOP ] = globals[ G_TSPACE2_BOT ];
    globals[ G_TLIM ] = globals[ G_TSPACE2_LIM ];
  }
  else {
    globals[ G_TBOT ] = globals[ G_TTOP ] = globals[ G_TSPACE1_BOT ];
    globals[ G_TLIM ] = globals[ G_TSPACE1_LIM ];
  }
}

/* eof */
