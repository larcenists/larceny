/*
 * Scheme Run-Time System.
 * Memory management system workhorses.
 *
 * $Id$
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

#include "layouts.h"
#include "offsets.h"
#include "macros.h"

#define NULL       0

/*
 * Initialize memory management system
 */
init_mem()
{
  init_collector( ... );

  /* must setup fake continuation at bottom of stack, setup 
     STACK_START_OFFSET */

  setup_memory_limits();
}


/*
 */
void gcstart2( n )
word n;
{
  flush_stack_cache();

  if (n == fixnum( -1 ))
    collect( 2 );
  else
    collect( 1 );

  flush_icache();
  setup_memory_limits();
}


/*
 * Given that the spaces have been set up and that pointers are in the
 * "globals" array, we calculate the ephemeral heap limit and the stack
 * cache limit and store these into the array.
 */
setup_memory_limits()
{
}


/*
 * Flush the instruction cache, if there is one.
 * On some systems this probably cannot be done in a high-level language.
 */
flush_icache();
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
void restore_frame( void )
{
  word *sframe, *hframe;
  unsigned sframesize, hframesize;

  if (globals[ CONTINUATION_OFFSET ] == FALSE_CONST)
    panic( "restore_frame: no more continuation frames!" );
  else {
    /* Get heap continuation */

    hframe = ptrof( globals[ CONTINUATION_OFFSET ] );
    hframesize = sizefield( *hframe );

    /* Allocate stack frame and bump saved stack pointer */

    sframesize = hframesize - HC_OVERHEAD*4 + STK_OVERHEAD*4;
    if (sframesize % 4 != 0) sframesize += 4;
    sframe = (word *) globals[ SP_OFFSET ] - sframesize / 4;
    globals[ SP_OFFSET ] = sframe;

    /* Follow continuation chain. */

    globals[ CONTINUATION_OFFSET ] = *(hframe + HC_DYNLINK);

    /* Setup stack frame header */

    { word *procptr, *codeptr;

      sframe = sp;
      procptr = ptrof( *(hframe + HC_PROC) );
      codeptr = ptrof( *(procptr + PROC_CODEPTR) );
      *(sframe + STK_RETADDR) = codeptr + *(hframe + HC_RETOFFSET);
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
}

  
/* 
 * Flush the stack cache to the heap.
 *
 * Due to the setup of and rules for allocating space in the ephemeral
 * area, we are guaranteed that there will be room for the flush.
 */
flush_stack_cache()
{
  word *sp, *first_cont, *e_top, *prev_cont, stk_start;

  sp = globals[ SP_OFFSET ];
  stk_start = globals[ STK_START_OFFSET ];
  e_top = globals[ E_TOP_OFFSET ];
  prev_cont = NULL;

  while (sp < stk_start) {
    word retaddr, proc;

    sframe = sp;
    sframesize = *(sframe + STK_CONTSIZE);   /* unadjusted size of frame */
    retaddr = *(sframe + STK_RETADDR);       /* unadjusted return address */
    procptr = ptrof( *(sframe + STK_PROC) ); /* pointer to the procedure */

    sp += sframesize;
    if (sframesize %4 != 0) sp++;

    hframesize = sframesize - STK_OVERHEAD*4 + HC_OVERHEAD*4;

    /* Allocate memory */
    hframe = e_top;
    e_top += hframesize;
    if (hframesize % 4 != 0) e_top++;

    *hframe = mkheader( framesize, CONT_TAG );
    *(hframe + HC_DYNLINK) = FALSE_CONST;
    codeptr = *(procptr + PROC_CODEPTR);
    *(hframe + HC_RETOFFSET) = retaddr - codeptr;

    { unsigned count;
      word *src, *dest;

      count = sframesize / 4 - STK_OVERHEAD;
      src = sframe + STK_SAVED;
      dest = hframe + HC_SAVED;
      while (count-- > 0)
	*dest++ = *src++;
    }

    if (prev_cont != NULL)
      *(prev_cont + HC_DYNLINK) = hframe;
    else
      first_cont = hframe;
    prev_cont = hframe;
  }

  if (prev_count != NULL)   /* strictly for sanity */
    *(prev_cont + HC_DYNLINK) = globals[ CONTINUATION_OFFSET ];

  /* Now restore the virtual machine state */

  globals[ SP_OFFSET ] = globals[ STACK_START_OFFSET ];
  globals[ CONTINUATION_OFFSET ] = tagptr( first_cont, VEC_TAG | CONT_SUBTAG);
  globals[ E_TOP_OFFSET ] = e_top;
}
