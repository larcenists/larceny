/*
 * This is the file Sys/scpolicy.c.
 *
 * Larceny run-time system (Unix) -- stop-and-copy memory manager.
 *
 * For benchmarking purposes only.
 *
 * Heap layout:
 *   All other memory must have been allocated; the heap must be at the
 *   top of allocated memory. The heap has the following layout, from
 *   low addresses toward higher addresses.
 *
 *   Static area.
 *   Ephemeral area 1.
 *   Ephemeral area 2.
 *   (program break)
 */

#include <sys/types.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

char *gctype = "scgc";

caddr_t sbrk();
int brk();


/***************************************************************************
 *
 * Heap initialization and expansion.
 */

/* 
 * Procedure to intialize garbage collector.  All sizes are in bytes. 
 * Returns 1 if ok, 0 if not.
 *
 * Ignores ephemeral_size and ewatermark; uses the tenured size and watermarks.
 * However, it allocates only an ephemeral area and initializes none of
 * the tenured heap pointers.
 */
int allocate_heap( ephemeral_size, tenured_size, static_size, ewatermark, thiwatermark, tlowatermark )
unsigned ephemeral_size;    /* size of espace; ignored */
unsigned tenured_size;      /* initial size of tspace; 0 = default */
unsigned static_size;       /* size of sspace; 0 = default */
unsigned ewatermark;        /* tenuring threshold in %; igonred */
unsigned thiwatermark;      /* heap expansion threshold in % */
unsigned tlowatermark;      /* heap contraction threshold in % */
{
  word *heapptr;

  if (tenured_size == 0) tenured_size = DEFAULT_TSIZE;
  if (static_size == 0) static_size = DEFAULT_SSIZE;
  if (thiwatermark <= 0 || thiwatermark > 100)
    thiwatermark = DEFAULT_THIWATERMARK;
  if (tlowatermark <= 0 || tlowatermark > 100)
    tlowatermark = DEFAULT_TLOWATERMARK;

  tenured_size = roundup8( tenured_size );
  static_size = roundup8( static_size );

  heapptr = (word*)sbrk(0);

  /* round up heap ptr to 8-byte boundary */
  heapptr = (word*)sbrk( roundup8( (unsigned)heapptr ) - (word)heapptr );
  if ((caddr_t)heapptr == (caddr_t)-1) return 0;

  if ((caddr_t)sbrk( static_size + 2*tenured_size ) == (caddr_t)-1)
    return 0;

  globals[ G_STATIC_BOT ] = (word)heapptr;
  globals[ G_STATIC_TOP ] = (word)heapptr;
  heapptr += static_size / sizeof( word );
  globals[ G_STATIC_LIM ] = (word)heapptr;
  globals[ G_ESPACE1_BOT ] = (word)heapptr;
  heapptr += tenured_size / sizeof( word );
  globals[ G_ESPACE1_LIM ] = (word)heapptr;
  globals[ G_ESPACE2_BOT ] = (word)heapptr;
  heapptr += tenured_size / sizeof( word );
  globals[ G_ESPACE2_LIM ] = (word)heapptr;

  globals[ G_CONT ] = FALSE_CONST;
  globals[ G_THIWATERMARK ] = thiwatermark;
  globals[ G_TLOWATERMARK ] = tlowatermark;
  globals[ G_EBOT ] = globals[ G_ESPACE1_BOT ];
  globals[ G_ETOP ] = globals[ G_ESPACE1_BOT ];
  globals[ G_ELIM ] = globals[ G_ESPACE1_LIM ];
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
{ return 0; }

int size_tspace()
{ return 0; }

int used_tspace()
{ return size_tspace() - free_tspace(); }

word *getheaplimit( which )
int which;
{ 
  switch (which) {
    case HL_TBOT : return (word*)globals[ G_EBOT ];
    case HL_TTOP : return (word*)globals[ G_ETOP ];
    case HL_TLIM : return (word*)globals[ G_ELIM ];
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
    case HL_TBOT : globals[ G_EBOT ] = (word)p; break;
    case HL_TTOP : globals[ G_ETOP ] = (word)p; break;
    case HL_TLIM : globals[ G_ELIM ] = (word)p; break;
    default      : panic( "Bad argument to setheaplimit()." );
  }
}

/*
 * Expand the heap. We always expand each semispace with the default size
 * of the ephemeral area (1MB).
 */
static void expand_heap()
{
  int esize;

#ifdef DEBUG
  if (globals[ G_EBOT ] != globals[ G_ESPACE1_BOT ])
    panic( "internal error in expand_heap()." );
#endif

  esize = DEFAULT_ESIZE;
  if (sbrk( 2*esize ) == (caddr_t)-1)
    panic( "Unable to expand the heap." );

  globals[ G_ESPACE1_LIM ] += esize;
  globals[ G_ESPACE2_BOT ] += esize;
  globals[ G_ESPACE2_LIM ] += 2*esize;
  globals[ G_ELIM ] += esize;

  consolemsg( "Expanding heap; new size = %dKB.", size_espace()/1024 );
}

/*
 * Contract the heap. Each semispace is shrunk by one default ephemeral
 * area (1MB by default).
 */
static void contract_heap()
{
  int esize;

#ifdef DEBUG
  if (globals[ G_EBOT ] != globals[ G_ESPACE1_BOT ])
    panic( "internal error in contract_heap()." );
#endif

  esize = DEFAULT_ESIZE;
  if (size_espace() - esize <= used_espace()) return;

  if (brk( (caddr_t)((char*)sbrk(0)-2*esize) ) == -1)
    panic( "Unable to contract the heap." );
    
  globals[ G_ESPACE1_LIM ] -= esize;
  globals[ G_ESPACE2_BOT ] -= esize;
  globals[ G_ESPACE2_LIM ] -= 2*esize;
  globals[ G_ELIM ] -= esize;
    
  consolemsg( "Contracting heap; new size = %dKB.", size_espace()/1024 );
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
  if (globals[ G_ETOP ] + n >= globals[ G_STKP ])  /* was: elim */
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

static void flip();
static void collection();

void garbage_collect( type, request )
int type;       /* collection type requested */
int request;    /* words requested */
{
  request *= 4;  /* to get bytes */

  if (globals[ G_REMSET_POOLTOP ] != globals[ G_REMSET_POOLBOT ]) {
    panic( "Remset is not empty!" );
  }

 again:
  flush_stack();
  memstat_before_gc( EPHEMERAL_COLLECTION );
  collection();
  globals[ G_STKP ] = globals[ G_ELIM ];  /* for free_espace/create_stack */
  globals[ G_STKBOT ] = globals[ G_ELIM ];
  memstat_after_gc();

  if (globals[G_EBOT] == globals[G_ESPACE1_BOT]) {
    if (used_espace() > size_espace()/100*globals[ G_THIWATERMARK ]) 
      expand_heap();
    else if (used_espace() < size_espace()/100*globals[ G_TLOWATERMARK ]) {
#if 0
      contract_heap();
#endif
    }
  }

  /* We go back to again2 after expanding the heap. */

 again2:
  flush_stack();
  globals[ G_STKP ] = globals[ G_ELIM ];  /* for free_espace/create_stack */
  globals[ G_STKBOT ] = globals[ G_ELIM ];

  if (!create_stack()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed create-stack." );
#endif
    if (globals[ G_EBOT ] == globals[ G_ESPACE1_BOT ]) {
      expand_heap();
      goto again2;
    }
    else
      goto again;
  }

  if (!restore_frame()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed restore-frame." );
#endif
    if (globals[ G_EBOT ] == globals[ G_ESPACE1_BOT ]) {
      expand_heap();
      goto again2;
    }
    else
      goto again;
  }

  if (request > free_espace()) {
#ifdef DEBUG
    consolemsg( "[debug] GC: Failed free-espace %d.", request );
#endif
    if (globals[ G_EBOT ] == globals[ G_ESPACE1_BOT ]) {
      expand_heap();
      goto again2;
    }
    else
      goto again;
  }

#ifdef DEBUG
  consolemsg( "Leaving collector\n(BOT=%08lx TOP=%08lx LIM=%08lx STKP=%08lx STKBOT=%08lx)", 
	      globals[ G_EBOT ], globals[ G_ETOP ], globals[ G_ELIM ],
	      globals[ G_STKP ], globals[ G_STKBOT ]);
#endif
}

/* This procedure is used by the low-level garbage collector */
void enumerate_roots( enumerator )
void (*enumerator)();
{
  int i;

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    enumerator( &globals[ i ] );
}

static void collection()
{
  word *oldlo, *oldhi;

#ifdef DEBUG
  consolemsg( "[debug] SCGC: garbage collection." );
#endif
  oldlo = (word*)globals[ G_EBOT ];
  oldhi = (word*)globals[ G_ELIM ];
  flip();
  globals[ G_ETOP ] =
    (word)major_collection( oldlo, oldhi, (word*)globals[ G_ETOP ] );
}

static void flip()
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

/* eof */
