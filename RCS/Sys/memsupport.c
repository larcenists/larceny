/*
 * Scheme Run-Time System.
 * Memory management system workhorses.
 *
 * $Id: memsupport.c,v 1.2 91/06/25 15:34:56 lth Exp Locker: lth $
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

#include "machine.h"
#include "gcinterface.h"
#include "layouts.h"
#include "offsets.h"
#include "macros.h"
#include "main.h"

#define NULL       0

/* Calculate free bytes in ephemeral space */
#define free_ephem_space()   (globals[E_LIMIT_OFFSET] - globals[E_TOP_OFFSET])

/*
 * Initialize memory management system. Allocate spaces, setup limits,
 * and initialize fake continuation in stack cache.
 */
init_mem( e_size, t_size, s_size, stk_size, e_lim )
unsigned e_size, t_size, s_size, stk_size, e_lim;
{
  if (init_collector( s_size, t_size, e_size, e_lim, stk_size ) == 0)
    return 0;

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
 * memory which is bigger than the ephemeral space, and we call gctrap().
 * in the hope that it will figure out what to do. When gctrap() returns,
 * we repeat the process. Otherwise, we return.
 */
gcstart2( n )
word n;
{
  int n_bytes = nativeint( n ) * 4;

  flush_stack_cache();

  if (n == fixnum( -1 )) {
    collect( TENURING_COLLECTION );
    setup_memory_limits();
  }
  else {
    collect( EPHEMERAL_COLLECTION );
    setup_memory_limits();

    if (n_bytes * 4 > free_ephem_space()) {
      collect( TENURING_COLLECTION );
      setup_memory_limits();

      while (n_bytes * 4 > free_ephem_space()) {
	gctrap( EPHEMERAL_TRAP );
	setup_memory_limits();
      }
    }
  }

  flush_icache();
}


/*
 * Given that the spaces have been set up and that pointers are in the
 * "globals" array, we calculate the ephemeral heap limit and the stack
 * cache limit and store these into the array.
 */
setup_memory_limits()
{
  /* Since all stack frames are doubleword aligned and a stack frame
   * takes as much space as the heap frame, we need to reserve no more
   * heap space than there is stack space.
   */

  globals[ E_LIMIT_OFFSET ] = globals[ E_MAX_OFFSET ] - 
             (globals[ STK_MAX_OFFSET ] - globals[ STK_BASE_OFFSET ] + 1)*4;
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
  *stktop = 0;                                         /* dummy */
  *(stktop-1) = 0;                                     /* saved proc */
  *(stktop-2) = 16;                                    /* continuation size */
  *(stktop-3) = (word) stkuflow;                       /* underflow handler */
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
    globals[ SP_OFFSET ] = (word) sframe;

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

    sp += sframesize;
    if (sframesize %4 != 0) sp++;

    /* Calculate heap frame size */

    hframesize = sframesize - STK_OVERHEAD*4 + HC_OVERHEAD*4;

    /* Allocate memory on heap */

    hframe = e_top;
    e_top += hframesize;
    if (hframesize % 4 != 0) e_top++;

    /* Setup heap continuation header */

    *hframe = mkheader( hframesize, CONT_SUBTAG );
    *(hframe + HC_DYNLINK) = FALSE_CONST;
    if ((word) procptr != 0) {
      codeptr = *(procptr + PROC_CODEPTR);            /* raw word value */
      *(hframe + HC_RETOFFSET) = retaddr - codeptr;   /* offset */
    }
    else
      *(hframe + HC_RETOFFSET) = 0;

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
      *(prev_cont + HC_DYNLINK) = (word) hframe;
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
          (word) tagptr( first_cont, VEC_TAG | CONT_SUBTAG );
}
