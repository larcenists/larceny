/*
 * This is the file Sys/policy.c.
 *
 * $Id: policy.c,v 1.2 1995/08/01 04:37:33 lth Exp lth $
 *
 * Larceny run-time system (Unix) -- memory manager.
 *
 * History
 *   Fabruary 1996 / lth (v0.25)
 *     Fixed bug: heap overflow limit was incorrectly calculated during
 *     full collection, causing segmentation fault.
 *
 *   June 17, 1995 / lth (v0.24)
 *     Changed expand_tspace() to account for overflow data.
 *
 *   June 28 - July 15, 1994 / lth (v0.20)
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
 * The only procedures in here which "knows" this are the ones which use
 * the stack pointer to calculate free and used ephemeral space. However, if 
 * the stack lived outside of the heap and would have to be copied into it
 * on a gc, some changes would have to be made in this file.
 *
 * Some things to do:
 *   - use of madvise() is indicated but not implemented, below.
 *   - may be good to lock the ephemeral area, the ssb, and the remset
 *     in memory to avoid paging them, using mlock().
 *   - may be good to set the protection for the static area to read-only.
 *   - may be good to have a read-only static area mmap()'d in from the
 *     heap file so as to allow the static area to be shared among processes
 *     using the same heap file.
 *
 * Points to ponder:
 *   It is not clear that having watermarks in percent of the heap size is
 *   such a good idea, esp. since the expansion and contraction routines
 *   change the heap size by an absolute amount (the size of espace). Perhaps
 *   better would be to use an absolute amount for calculating expansion
 *   and contraction, too, e.g., if less than one espace free after major GC
 *   then expand; if more than two espaces free after major GC then contract.
 */

#include <sys/types.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

char *gctype = "gsgc";

caddr_t sbrk();
int brk();

static unsigned estksize = 0;
static int estack_get_real = 1;


/***************************************************************************
 *
 * Heap initialization and expansion.
 */

/* 
 * Procedure to intialize garbage collector.  All sizes are in bytes. 
 * Returns 1 if ok, 0 if not.
 */
int allocate_heap( ephemeral_size, tenured_size, static_size, ewatermark, thiwatermark, tlowatermark )
unsigned ephemeral_size;    /* size of espace; 0 = default */
unsigned tenured_size;      /* initial size of tspace; 0 = default */
unsigned static_size;       /* size of sspace; 0 = default */
unsigned ewatermark;        /* tenuring threshold in %, 0 = default */
unsigned thiwatermark;      /* heap expansion threshold in % */
unsigned tlowatermark;      /* heap contraction threshold in % */
{
  word *heapptr;

  if (ephemeral_size == 0) ephemeral_size = DEFAULT_ESIZE;
  if (tenured_size == 0) tenured_size = DEFAULT_TSIZE;
  if (static_size == 0) static_size = DEFAULT_SSIZE;
  if (ewatermark <= 0 || ewatermark > 100)
    ewatermark = DEFAULT_EWATERMARK;
  if (thiwatermark <= 0 || thiwatermark > 100)
    thiwatermark = DEFAULT_THIWATERMARK;
  if (tlowatermark <= 0 || tlowatermark > 100)
    tlowatermark = DEFAULT_TLOWATERMARK;

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
  globals[ G_THIWATERMARK ] = thiwatermark;
  globals[ G_TLOWATERMARK ] = tlowatermark;
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

  estack_get_real = 1;
  return 1;
}

/* This is ugly.  Somehow the stack size must be accounted for when
 * GC stats are recorded.  But when memstats_before_gc() is called,
 * the stack has been flushed so just calculating the stack size then
 * is meaningless.  Therefore, before a flush_stack call in the collector,
 * the stack size is recorded in estksize and estack_get_real is set
 * to 0.  When the gc is finished, estack_get_real is set to 1.
 * A client module can call used_estack() to get the stack size regardless.
 *
 * Returned stack size is in bytes.
 */
int used_estack()
{ 
  if (estack_get_real)
    return globals[ G_STKBOT ] - globals[ G_STKP ]; 
  else
    return estksize;
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

int size_sspace()
{ return globals[ G_STATIC_TOP ] - globals[ G_STATIC_BOT ]; }

word *getheaplimit( which )
int which;
{ 
  switch (which) {
    case HL_TBOT : return (word*)globals[ G_TBOT ];
    case HL_TTOP : return (word*)globals[ G_TTOP ];
    case HL_TLIM : return (word*)globals[ G_TLIM ];
    case HL_SBOT : return (word*)globals[ G_STATIC_BOT ];
    case HL_STOP : return (word*)globals[ G_STATIC_TOP ];
    default      : panic( "Bad argument to getheaplimit()." );
  }
  /*NOTREACHED*/
  return 0;
}

void setheaplimit( which, p )
int which;
word *p;
{
  switch (which) {
    case HL_TBOT : globals[ G_TBOT ] = (word)p; break;
    case HL_TTOP : globals[ G_TTOP ] = (word)p; break;
    case HL_TLIM : globals[ G_TLIM ] = (word)p; break;
    case HL_STOP : globals[ G_STATIC_TOP ] = (word)p; break;
    default      : panic( "Bad argument to setheaplimit()." );
  }
}

/*
 * Expand the tenured area. We always expand each tenured space with
 * the size of one ephemeral area plus the amount of overflow data; don't
 * know how well this works in practice, probably depends on the size 
 * of espace.
 */
static void expand_tspace()
{
  int esize, ftspace;

#ifdef DEBUG
  if (globals[ G_TBOT ] != globals[ G_TSPACE1_BOT ])
    panic( "internal error in expand_tspace()." );
#endif

  esize = size_espace();
  ftspace = free_tspace(); /* if negative, the amount of overflow */
  if (ftspace < 0) esize += -ftspace;

  if (sbrk( 2*esize ) == (caddr_t)-1)
    panic( "Unable to expand the heap." );

  globals[ G_TSPACE1_LIM ] += esize;
  globals[ G_TSPACE2_BOT ] += esize;
  globals[ G_TSPACE2_LIM ] += 2*esize;
  globals[ G_TLIM ] += esize;

  consolemsg( "; Expanding tspace; new size = %dKB.", size_tspace()/1024 );
}

/*
 * Contract the tenured area. The tenured area is shrunk by the size of
 * one ephemeral area.
 */
static void contract_tspace()
{
  int esize;

#ifdef DEBUG
  if (globals[ G_TBOT ] != globals[ G_TSPACE1_BOT ])
    panic( "internal error in contract_tspace()." );
#endif

  esize = size_espace();
  if (size_tspace() - esize <= used_tspace()) return;

  if (brk( (caddr_t)((char*)sbrk(0)-2*esize) ) == -1)
    panic( "Unable to contract the heap." );
    
  globals[ G_TSPACE1_LIM ] -= esize;
  globals[ G_TSPACE2_BOT ] -= esize;
  globals[ G_TSPACE2_LIM ] -= 2*esize;
  globals[ G_TLIM ] -= esize;
    
  consolemsg( "; Contracting tspace; new size = %dKB.", size_tspace()/1024 );
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
  if (globals[ G_ETOP ] + n >= globals[ G_STKP ]) /* was: elim */
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
static void gsgc();
static void mlgc();

void garbage_collect( type, request )
int type;       /* collection type requested */
int request;    /* words requested */
{
  gsgc( type, request );
}

/*
 * A simple experiment with an SML/NJ-style collector; every minor collection
 * is a tenuring collection.
 *
 * Not very good; stay away.
 */
static void mlgc( type, request )
int type;
int request;
{
  if (type == EPHEMERAL_COLLECTION)
    type = TENURING_COLLECTION;
  gsgc( type, request );
}

/* The default generation-scavenging collector */
static void gsgc( type, request )
int type;
int request;
{
  request *= 4;  /* to get bytes */

  if (!compact_ssb())
    if (type == EPHEMERAL_COLLECTION)
      type = TENURING_COLLECTION;

  if (globals[ G_GC_MUST_TENURE ] && type == EPHEMERAL_COLLECTION)
    type = TENURING_COLLECTION;

 again:
  /* We flush the stack on each iteration in this procedure so that
   * any restored frames get put back on the chain properly.
   */
  estksize = used_estack();
  estack_get_real = 0;
  flush_stack();

  if (type == TENURING_COLLECTION) {
    if (globals[ G_TBOT ] == globals[ G_TSPACE1_BOT ]) {
      /* Tspace A has data */
      if (free_tspace() - used_espace() <= 0) {
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
  }

  if (type == FULL_COLLECTION) {
    if (globals[ G_TBOT ] == globals[ G_TSPACE1_BOT ]) {
      /* collecting from A to B */
      if (free_tspace() - used_espace() <= 0) {
	/* A has overflowed: expand. */
	expand_tspace();
      }
    }
    else {
      /* Collecting from B to A -- do nothing, everything should be OK */
    }
  }

  memstat_before_gc( type );

  switch (type) {
    case EPHEMERAL_COLLECTION : ephemeral_collection(); break;
    case TENURING_COLLECTION  : tenuring_collection(); break;
    case FULL_COLLECTION      : full_collection(); break;
  }
  globals[ G_STKP ] = globals[ G_ELIM ];    /* for free_espace/create_stack */
  globals[ G_STKBOT ] = globals[ G_STKP ];  /* for flush_stack() */

  memstat_after_gc();

  /* Maybe do a tenuring collection next time? */
  if (used_espace() > size_espace()*globals[ G_EWATERMARK ]/100)
    globals[ G_GC_MUST_TENURE ] = 1;
  else
    globals[ G_GC_MUST_TENURE ] = 0;

  if (!create_stack()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed create-stack." );
#endif
    if (type != EPHEMERAL_COLLECTION)
      panic( "Internal inconsistency in garbage collector." );
    type = TENURING_COLLECTION;
    goto again;
  }

  if (!restore_frame()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed restore-frame." );
#endif
    if (type != EPHEMERAL_COLLECTION)
      panic( "Unable to restore a stack frame after gc." );
    type = TENURING_COLLECTION;
    goto again;
  }

  if (request > free_espace()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed free-espace %d.", request );
#endif
    if (type != EPHEMERAL_COLLECTION)
      panic( "Object of size %d too large to allocate!\n", request );
    type = TENURING_COLLECTION;
    goto again;
  }

  /* 
   * If we performed a full collection into tspace A:
   * - If more data is live than we consider reasonable, expand the heap 
   *   to avoid GC thrashing.
   * - If there is more free space than reasonable, contract the heap to
   *   avoid VM thrashing.
   */
  if (type == FULL_COLLECTION && globals[G_TBOT] == globals[G_TSPACE1_BOT]) {
    if (used_tspace() > size_tspace()/100*globals[ G_THIWATERMARK ]) 
      expand_tspace();
    else if (used_tspace() < size_tspace()/100*globals[ G_TLOWATERMARK ])
      contract_tspace();
  }

  if (type != EPHEMERAL_COLLECTION) clear_remset();
  estack_get_real = 1;
}

/* This procedure is used by the low-level garbage collector */
void enumerate_roots( enumerator )
void (*enumerator)();
{
  int i;

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    enumerator( &globals[ i ] );
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
