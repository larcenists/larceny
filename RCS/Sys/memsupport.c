/*
 * Scheme Run-Time System.
 * Memory management system workhorses.
 *
 * $Id: memsupport.c,v 1.6 91/07/10 10:19:08 lth Exp Locker: lth $
 *
 * The procedures in here initialize the memory system, perform tasks 
 * associated with garbage collection, and manipulate the stack cache.
 * They are likely a bit dependent upon calling conventions etc, although
 * I've tried to keep most such nasties confined to the file "layouts.h".
 *
 * The data in the heap continuation is not adjusted to an even number
 * of words, so on restoring a frame, we must be sure to pad the stack
 * if necessary.
 */

#include <sys/time.h>
#include <sys/resource.h>
#include "machine.h"
#include "gcinterface.h"
#include "layouts.h"
#include "offsets.h"
#include "macros.h"
#include "main.h"
#include "millicode.h"
#ifdef DEBUG
#include <stdio.h>
extern FILE *ofp;
#else
#define NULL       0
#endif

/* Calculate free bytes in ephemeral space. Can be negative! */
#define free_ephem_space()   ((long) globals[E_LIMIT_OFFSET] - (long) globals[E_TOP_OFFSET])

/*
 * Initialize memory management system. Allocate spaces, setup limits,
 * and initialize fake continuation in stack cache.
 */
init_mem( e_size, t_size, s_size, stk_size, e_lim )
unsigned e_size, t_size, s_size, stk_size, e_lim;
{
  if (init_collector( s_size, t_size, e_size, e_lim, stk_size ) == 0)
    return 0;

  globals[ CONTINUATION_OFFSET ] = FALSE_CONST;
  globals[ T_TRANS_OFFSET ] = globals[ T_MAX_OFFSET ];
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
  unsigned int milliseconds;
  struct rusage r1, r2;

#ifdef DEBUG
  getrusage( RUSAGE_SELF, &r1 );
#endif

  flush_stack_cache();

  if (n == fixnum( -1 )) {
    collect( TENURING_COLLECTION );
    setup_memory_limits();
  }
  else {
    collect( EPHEMERAL_COLLECTION );
    setup_memory_limits();

    if (n_bytes > free_ephem_space()) {
      collect( TENURING_COLLECTION );
      setup_memory_limits();

      while (n_bytes > free_ephem_space()) {
	gc_trap( EPHEMERAL_TRAP );
	setup_memory_limits();
      }
    }
  }

  flush_icache();

#ifdef DEBUG
  getrusage( RUSAGE_SELF, &r2 );
  milliseconds = ((r2.ru_utime.tv_sec - r1.ru_utime.tv_sec)*1000
		  + (r2.ru_utime.tv_usec - r1.ru_utime.tv_usec)/1000);
  printf( "Time spent collecting: %u milliseconds.\n", milliseconds );
#endif
}


/*
 * Given that the spaces have been set up and that pointers are in the
 * "globals" array, we calculate the ephemeral heap limit.
 */
setup_memory_limits()
{
  /*
   * currently we reserve twice the stack space for the overflow area.
   * This is allmost certainly too much (but that is not harmful).
   */

  globals[ E_LIMIT_OFFSET ] = globals[ E_MAX_OFFSET ]+4 - 
    roundup8( (globals[ STK_MAX_OFFSET ] - globals[ STK_BASE_OFFSET ] + 4)*2 );
}


/*
 * Calculate stack limit in some implementation-dependent fashion, and set
 * up a fake continuation at the bottom (highest addresses) of the stack.
 */
setup_stack()
{
  extern void stkuflow();
  word *stktop;

#if SPARC1 || SPARC2
  stktop = (word *) globals[ STK_MAX_OFFSET ];
  *stktop = 0x81726354;                                /* dummy magic # */
  *(stktop-1) = 0;                                     /* saved proc */
  *(stktop-2) = 16;                                    /* continuation size */
  *(stktop-3) = (word) millicode[ M_STKUFLOW ];        /* underflow handler */
  globals[ STK_START_OFFSET ] = (word) (stktop-4);
  globals[ SP_OFFSET ] = (word) (stktop-3);

  /* Need to calculate max continuation size in order to find the
   * lower stack limit. We'll just gratuitiously assume that 100 words is
   * plenty, and deal with elegance later.
   */

  globals[ STK_LIMIT_OFFSET ] = (word) ((word *)globals[STK_BASE_OFFSET]+100);
#else
#endif
}


/*
 * Flush the instruction cache, if there is one.
 * On some systems this probably cannot be done in a high-level language.
 */
flush_icache()
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

  if (globals[ SP_OFFSET ] % 8 != 0)
    panic( "restore_frame: botched stack pointer!" );

  if (globals[ CONTINUATION_OFFSET ] == FALSE_CONST)
    panic( "restore_frame: no more continuation frames!" );
  else {
    /* Get heap continuation */

    hframe = ptrof( globals[ CONTINUATION_OFFSET ] );
    hframesize = sizefield( *hframe ) + 4;

    /* Allocate stack frame and bump saved stack pointer */

    sframesize = hframesize - HC_OVERHEAD*4 + STK_OVERHEAD*4;
    if (sframesize % 8 != 0) sframesize += 4;
    sframe = (word *) globals[ SP_OFFSET ] - sframesize / 4;
    globals[ SP_OFFSET ] = (word) sframe;

#if 0
#ifdef DEBUG
  printf( "restoring frame. hsz=%u, ssz=%u, hfr=0x%lx, sfr=0x%lx\n", hframesize, sframesize, (word) hframe, (word) sframe );
#endif
#endif

    /* Follow continuation chain. */

    globals[ CONTINUATION_OFFSET ] = *(hframe + HC_DYNLINK);

    /* Setup stack frame header. Special case for procedures with value 0. */

    { word *procptr, *codeptr;

      procptr = ptrof( *(hframe + HC_PROC) );
      if ((word) procptr != 0) {
	codeptr = ptrof( *(procptr + PROC_CODEPTR) );
	*(sframe + STK_RETADDR) = (word) (codeptr + *(hframe + HC_RETOFFSET));
      }
      else
	*(sframe + STK_RETADDR) = *(hframe + HC_RETOFFSET);
      *(sframe + STK_CONTSIZE) = sframesize;
    }

    /* Copy the generic "saved slots" */

    { word *src, *dest;
      unsigned count;

      count = hframesize / 4 - HC_OVERHEAD;
      src = hframe + HC_SAVED;
      dest = sframe + STK_SAVED;
      while (count-- > 0)
	*dest++ = *src++;
    }
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
/*  printf( "flushing\n" ); */
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
      codeptr = *(procptr + PROC_CODEPTR);            /* raw word value */
      *(hframe + HC_RETOFFSET) = retaddr - codeptr;   /* offset */
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
  }

  /* Get the last link, too */

  if (prev_cont != NULL)
    *(prev_cont + HC_DYNLINK) = globals[ CONTINUATION_OFFSET ];

  /* Now restore the virtual machine state */

  globals[ SP_OFFSET ] = (word) sp;
  globals[ E_TOP_OFFSET ] = (word) e_top;
  if (first_cont != NULL)
    globals[ CONTINUATION_OFFSET ] = 
          (word) tagptr( first_cont, VEC_TAG );
#ifdef DEBUG
/*  printf( "done flushing.\n" ); */

  if (globals[ E_TOP_OFFSET ] > globals[ E_MAX_OFFSET ]) {
    printf( "%lx %lx\n", globals[ E_TOP_OFFSET ], globals[ E_MAX_OFFSET ] );
    panic( "Ephemeral heap overflow after stack flush." );
  }
#endif
}



