/*
 * Scheme 313 Run-Time System.
 * Memory management system support code.
 *
 * $Id: memsupport.c,v 1.10 91/12/06 15:08:25 lth Exp Locker: lth $
 *
 * The procedures in here initialize the memory system, perform tasks 
 * associated with garbage collection, and manipulate the stack cache.
 * They are likely a bit dependent upon calling conventions etc, although
 * I've tried to keep most such nasties confined to the file "layouts.h".
 */

#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include "machine.h"
#include "gcinterface.h"
#include "layouts.h"
#include "offsets.h"
#include "macros.h"
#include "main.h"
#include "millicode.h"

/* Calculate free bytes in ephemeral space. Can be negative! */
#define free_e_space() ((long)globals[E_LIMIT_OFFSET] - (long)globals[E_TOP_OFFSET])

static local_collect(),
       setup_memory_limits(),
       setup_stack();


/*
 * Initialize memory management system. Allocate spaces, setup limits,
 * and initialize fake continuation in stack cache.
 */
init_mem( e_size, t_size, s_size, stk_size, e_lim )
unsigned e_size, t_size, s_size, stk_size, e_lim;
{
  if (init_collector( s_size, t_size, e_size, stk_size ) == 0)
    return 0;

  globals[ CONTINUATION_OFFSET ] = FALSE_CONST;
  globals[ T_TRANS_OFFSET ] = globals[ T_MAX_OFFSET ];
  globals[ E_MARK_OFFSET ] = roundup8( e_lim );
  setup_memory_limits();
  setup_stack();
}


/*
 * This is the procedure that is called by the assembly-language memory
 * management routines when there is a need for garbage collection.
 *
 * If the argument 'n' is fixnum( -1 ) then we perform a tenuring collection
 * and return. If it is any other number (and it had better be nonnegative,
 * then!), then we perform an ephemeral collection. If, after the collection,
 * there is room to allocate nativeint( n ) words on the heap, we return.
 * Otherwise, we perform a tenuring collection. If, after the collection,
 * we cannot allocate the memory, then the user is requesting a chunk of
 * memory which is bigger than the ephemeral space, and we call gc_trap().
 * in the hope that it will figure out what to do. When gc_trap() returns,
 * we repeat the process. Otherwise, we return.
 */
gcstart2( n )
word n;
{
  long n_bytes = nativeint( n ) * 4;

  flush_stack_cache();

  if (n == fixnum( -1 ))
    local_collect( TENURING_COLLECTION );
  else {
    local_collect( EPHEMERAL_COLLECTION );

    if (n_bytes > free_e_space()) {
      local_collect( TENURING_COLLECTION );

      while (n_bytes > free_e_space()) {
	printf( "attempting to allocate %lu bytes\n", n_bytes );
	gc_trap( EPHEMERAL_TRAP );
	setup_memory_limits();
      }
    }
  }

  flush_icache();
}


/*
 * Load a heap into the tenured area. The heap has the following format:
 *  - the first word is a version number.
 *  - then follow all the rootables that go into the globals table.
 *  - then follows a word count (for the heap area).
 *  - then follows the heap area.
 * All pointers in the roots or in the heap are from some base 0. They
 * are adjusted as the heap is read in.
 *
 * All words are stored in big-endian format.
 *
 * Returns 0 if everything went fine; -1 otherwise.
 */
load_heap( fp )
FILE *fp;
{
  word base, magic, count, *p, w;
  int i;

  base = globals[ T_BASE_OFFSET ];
  magic = getword( fp );
  if (magic != HEAP_VERSION)
    panic( "Wrong version heap image." );

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ ) {
    w = getword( fp );
    globals[ i ] = (isptr( w ) ? w + base : w);
  }

  count = getword( fp );
  if (count*4 > globals[ T_MAX_OFFSET ] - globals[ T_BASE_OFFSET ])
    panic( "Heap image will not fit in tenured heap.");

  p = (word *) base;
  while (count--) {
    w = getword( fp );
    *p++ = (isptr( w ) ? w + base : w);
    if (header( w ) == BV_HDR) {
      i = roundup4( sizefield( w ) ) / 4;
      while (i--)
	*p++ = getword( fp );
    }
  }

  globals[ T_TOP_OFFSET ] = (word) p;
  return 0;
}


static getword( fp )
FILE *fp;
{
  word a = getc( fp );
  word b = getc( fp );
  word c = getc( fp );
  word d = getc( fp );

  return (a << 24) | (b << 16) | (c << 8) | d;
}


/*
 * Dumps the tenured heap. The heap layout is described above.
 * Returns 0 if everything went fine; EOF otherwise.
 *
 * The tenured heap is dumped "as is"; pointers into other areas are
 * dumped literally and should not exist (how do we deal with pointers into
 * the static area? At least pointers into the tenured area can be gc'd away.)
 */
dump_heap( fp )
FILE *fp;
{
  word base, top, count, w, *p;
  int i, align;

  base = globals[ T_BASE_OFFSET ];
  top  = globals[ T_TOP_OFFSET ];
  if (putword( HEAP_VERSION, fp ) == EOF)
    return EOF;

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    if (putword( globals[ i ], fp ) == EOF)
      return EOF;

  count = (top - base) / 4;
  if (putword( count, fp ) == EOF)
    return EOF;

  p = (word *) base;
  while (count--) {
    w = *p++;
    if (putword( isptr( w ) ? w-base : w, fp ) == EOF)
      return EOF;
    
    if (header( w ) == BV_HDR) {
      i = roundup4( sizefield( w ) ) / 4;
      align = (i % 2 == 0);
      while (i--) {
	if (putword( *p++, fp ) == EOF)
	  return EOF;
      }
      if (align)
	if (putword( 0, fp ) == EOF)
	  return EOF;
    }
  }

  return 0;
}


static putword( w, fp )
word w;
FILE *fp;
{
  if (putc( (w >> 24) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 16) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 8) & 0xFF, fp ) == EOF) return EOF;
  if (putc( w & 0xFF, fp ) == EOF) return EOF;
  return 0;
}


/*
 * Do a collection and gather some statistics.
 *
 * HOW TO LIE WITH STATISTICS: The times here do not include the time it
 * takes to do a stack or cache flush, nore any overhead incurred by 
 * gc_trap() in the case of an ephemeral space overflow.
 */
static local_collect( type )
int type;
{
  int realtype;
  unsigned int milliseconds, oldwords;
  struct rusage r1, r2;

#ifdef DEBUG
  printf( "garbage collection commencing, type %d\n", type );
  oldwords = globals[ WCOPIED_OFFSET ];
#endif

  getrusage( RUSAGE_SELF, &r1 );

  realtype = collect( type );
  setup_memory_limits();

  getrusage( RUSAGE_SELF, &r2 );
  milliseconds = ((r2.ru_utime.tv_sec - r1.ru_utime.tv_sec)*1000
		  + (r2.ru_utime.tv_usec - r1.ru_utime.tv_usec)/1000);

#ifdef DEBUG
  printf( "garbage collection done, type %d\n", realtype );
  printf( "words copied: %ld\n", globals[ WCOPIED_OFFSET ]-oldwords );
  printf( "Time spent collecting: %u milliseconds.\n", milliseconds );
#endif
}


/*
 * Given that the spaces have been set up and that pointers are in the
 * "globals" array, we calculate the ephemeral heap limit.
 */
static setup_memory_limits()
{
  /*
   * Currently we reserve twice the stack space for the overflow area.
   * This is almost certainly too much (but that is not harmful).
   */

  globals[ E_LIMIT_OFFSET ] = globals[ E_MAX_OFFSET ]+4 - 
    roundup8( (globals[ STK_MAX_OFFSET ] - globals[ STK_BASE_OFFSET ] + 4)*2 );
}


/*
 * Calculate stack limit in some implementation-dependent fashion, and set
 * up a fake continuation at the bottom (highest addresses) of the stack.
 */
static setup_stack()
{
  extern void stkuflow();
  word *stktop;

#if SPARC
  stktop = (word *) globals[ STK_MAX_OFFSET ];
  *stktop = 0x81726354;                                /* dummy magic # */
  *(stktop-1) = 0;                                     /* saved proc */
  *(stktop-2) = 16;                                    /* continuation size */
  *(stktop-3) = (word) millicode[ M_STKUFLOW+1 ];      /* underflow handler */
  globals[ STK_START_OFFSET ] = (word) (stktop-4);
  globals[ SP_OFFSET ] = (word) (stktop-3);
#ifdef DEBUG
  printf( "Initial stack pointer is %08x\n", globals[ SP_OFFSET ] );
#endif
  /* 
   * Need to calculate max continuation size in order to find the
   * lower stack limit. We'll just gratuitiously assume that 100 words is
   * plenty, and deal with elegance later.
   */

  globals[ STK_LIMIT_OFFSET ] = (word) ((word *)globals[STK_BASE_OFFSET]+100);
#endif
}


/*
 * Flush the instruction cache, if there is one.
 * On some systems this probably cannot be done in a high-level language.
 */
static flush_icache()
{
#if SPARC1 || SPARC2
  /* 
   * Cache flush is a no-op on the Sparcstation 1 and the Sparcstation 2 
   * because these machines have shared I/D caches, ensuring consistency 
   * even if code is moved around.
   */
#else
#endif
}


/*
 * Restore one stack frame from the heap. We also have to maintain the
 * fake continuation at the bottom of the heap; however, that continuation
 * is still intact, and so we simply put the new one on top of it on
 * the stack.
 *
 * We do not check for stack overflow, as this would be a silly thing to do.
 * 
 * Refer to "conventions.txt" for an explanation of the continuation layouts.
 */
void restore_frame()
{
  word *sframe, *hframe;
  unsigned sframesize, hframesize;

#ifdef DEBUG
  printf( "Restoring stack frame; stkptr is %08x.\n", globals[ SP_OFFSET ] );
  if (globals[ CONTINUATION_OFFSET ] == FALSE_CONST)
    panic( "restore_frame: no more continuation frames!" );

  if (globals[ SP_OFFSET ] % 8 != 0) {
    printf( "restore_frame: botched stack pointer %08x!\n", 
	   globals[ SP_OFFSET ] );
    dumpchain();
    localdebugger();
    panic( "Not possible to continue. Goodbye." );
  }
#endif

  /* Get heap continuation */

  hframe = ptrof( globals[ CONTINUATION_OFFSET ] );
  hframesize = sizefield( *hframe ) + 4;

#ifdef DEBUG
  printf( "Frame @ %08x, header %08x, size %d\n", hframe, *hframe, hframesize );
#endif

  /* Allocate stack frame and bump saved stack pointer */

  sframesize = hframesize - HC_OVERHEAD*4 + STK_OVERHEAD*4;
  if (sframesize % 8 != 0) sframesize += 4;
  sframe = (word *) globals[ SP_OFFSET ] - sframesize / 4;
  globals[ SP_OFFSET ] = (word) sframe;

  /* Follow continuation chain. */

  globals[ CONTINUATION_OFFSET ] = *(hframe + HC_DYNLINK);

  /* Setup stack frame header. Special case for procedures with value 0. 
   * Also note that since the offset in the heap frame is a bytevector
   * index (rather than an offset from the bytevector header), then
   * 4 have to be added.
   * Observe the inverse code in flush_stack_cache(), below.
   */

  { word *procptr, codeptr;

    procptr = ptrof( *(hframe + HC_PROC) );
    if ((word) procptr != 0) {
      codeptr = (word) ptrof( *(procptr + PROC_CODEPTR) );
      *(sframe + STK_RETADDR) = (codeptr + *(hframe+HC_RETOFFSET) + 4);
    }
    else
      *(sframe + STK_RETADDR) = *(hframe + HC_RETOFFSET);
    *(sframe + STK_CONTSIZE) = sframesize;
  }
#ifdef DEBUG
  printf( "return address %08x\n", *(sframe + STK_RETADDR ) );
#endif

  /* Copy the generic "saved slots" */

  { word *src, *dest;
    unsigned count;

    count = hframesize / 4 - HC_OVERHEAD;
    src = hframe + HC_SAVED;
    dest = sframe + STK_SAVED;
    while (count-- > 0)
      *dest++ = *src++;
  }

  if (globals[ SP_OFFSET ] % 8 != 0)
    panic( "restore_frame(2): botched stack pointer!" );
}

  
/* 
 * Flush the stack cache to the heap.
 *
 * Due to the setup of and rules for allocating space in the ephemeral
 * area, we are guaranteed that there will be room for the flush.
 */
flush_stack_cache()
{
  word *sp, *first_cont, *e_top, *prev_cont, *stk_start;
#ifdef DEBUG
  int framecount = 0;
#endif

#ifdef DEBUG
  printf( "Flushing stack cache; base = %x, limit = %x, ptr = %x.\n",
	  globals[ STK_START_OFFSET ], globals[ STK_LIMIT_OFFSET ],
	  globals[ SP_OFFSET ]);
#endif

  sp = (word *) globals[ SP_OFFSET ];
  stk_start = (word *) globals[ STK_START_OFFSET ];
  e_top = (word *) globals[ E_TOP_OFFSET ];
  first_cont = prev_cont = NULL;

  while (sp < stk_start) {
    word retaddr, *procptr, *sframe, *hframe, codeptr;
    unsigned sframesize, hframesize;

    /* Get stack frame stuff */

    sframe = sp;
    sframesize = *(sframe + STK_CONTSIZE);   /* unadjusted size of frame */
    retaddr = *(sframe + STK_RETADDR);       /* unadjusted return address */
    procptr = ptrof( *(sframe + STK_PROC) ); /* pointer to the procedure */

#ifdef DEBUG
    printf( "return address %08x\n", retaddr );
#endif

    /* Move stack pointer */

    sp += sframesize / 4;
    if (sframesize % 8 != 0) sp++;

    /* Calculate heap frame size */

    hframesize = sframesize - STK_OVERHEAD*4 + HC_OVERHEAD*4;

    /* Allocate memory on heap */

    hframe = e_top;
    e_top += hframesize / 4;
    if (hframesize % 8 != 0) e_top++;

    /* Setup heap continuation header */

    *hframe = mkheader( hframesize-4, VEC_HDR | CONT_SUBTAG );
    *(hframe + HC_DYNLINK) = FALSE_CONST;
    if ((word) procptr != 0) {
      codeptr = (word) ptrof( *(procptr + PROC_CODEPTR));
      *(hframe + HC_RETOFFSET) = retaddr - codeptr - 4;  /* offset is bv idx */
    }
    else
      *(hframe + HC_RETOFFSET) = retaddr;

    /* Copy saved values */

    { unsigned count;
      word *src, *dest;

      count = sframesize / 4 - STK_OVERHEAD;
      src = sframe + STK_SAVED;
      dest = hframe + HC_SAVED;
      while (count-- > 0)
	*dest++ = *src++;
    }

    /* Link continuation frame into chain */

    if (prev_cont != NULL)
      *(prev_cont + HC_DYNLINK) = (word) tagptr( hframe, VEC_TAG );
    else
      first_cont = hframe;
    prev_cont = hframe;
#ifdef DEBUG
    framecount++;
#endif
  }

  /* Get the last link, too */

  if (prev_cont != NULL)
    *(prev_cont + HC_DYNLINK) = globals[ CONTINUATION_OFFSET ];

  /* Now restore the virtual machine state */

  globals[ SP_OFFSET ] = (word) sp;
  globals[ E_TOP_OFFSET ] = (word) e_top;
  if (first_cont != NULL)
    globals[ CONTINUATION_OFFSET ] = (word) tagptr( first_cont, VEC_TAG );

#ifdef DEBUG
  printf( "%d frames flushed.\n", framecount );
#endif
}



#ifdef DEBUG
dumpchain()
{
  word w, *p;
  int i;

  p = ptrof( globals[ CONTINUATION_OFFSET ] );
  for (i = 0 ; i < 5 ; i++ ) 
    printf( "%08x\n", *p++ );
}
#endif

