/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Petit Larceny control-flow discipline configuration.
 */

#ifndef PETIT_CONFIG_H
#define PETIT_CONFIG_H

#include "larceny-types.h"

/* Jump discipline.  Exactly one of these three should be true */

#define USE_LONGJUMP               0
   /* Jump, invoke and return are implemented as calls; when the timer
      expires, a longjump is performed to prune the stack.  The extern 
      variable 'twobit_cont_label' holds the address to jump to following 
      the longjump.
      */

#define USE_RETURN_WITHOUT_VALUE   0
  /* Jump, invoke, and return are implemented as returns to a dispatch 
     loop.  The extern variable 'twobit_cont_label' holds the address 
     to jump to following the longjmp.
     */

#define USE_RETURN_WITH_VALUE      1
  /* Jump, invoke, and return are implemented as returns to a dispatch
     loop.  The address to jump to is returned to the dispatch loop 
     rather than being stored in a global.
     */


/* Additional control of control flow discipline.  Combine this with one
   of the preceding three.
   */

#define USE_GOTOS_LOCALLY          1
  /* If set to one, distinguish between local control transfers (BRANCH, 
     BRANCHF, and SKIP) and nonlocal control transfers (INVOKE, RETURN, 
     APPLY, and JUMP).  The nonlocal transfers uses the discipline 
     selected above; the local transfers use GOTO.

     This local control transfer discipline improves performance, reduces
     code size, makes register caching worthwhile, and reduces the number 
     of function pointers (which makes life easier on MacOS, at least).

     A code address is a pair: function pointer and address within the 
     function.  The address within the function is of type cont_t; in a
     portable implementation this is an integral type.
	 
     Larceny's representations do not support code addresses larger than
     one word, but it turns out that the two-word structure can be represented
     partially in all cases, because it is never exposed fully to anyone --
     only compiled code and the run-time system manipulates code addresses.
     Code addresses are stored in procedures and in stack frames:
	 
        - In a procedure, the code pointer has the function pointer and the
          internal address is an implicitly stored 0.
        - In a stack frame, the return address is the internal address;
          the code pointer is supplied by the procedure stored in the
          saved REG0.
     
     The external variable twobit_cont_label, if defined, has type cont_t
     and holds only the internal address; it must be interpreted in the 
     context the procedure in R0. 
     */


/* Return type of all compiled procedures, and an expression to
   return the value of an expression of that type. */

#if USE_LONGJUMP
# define RTYPE void
# define RETURN_RTYPE( expr ) (void)(expr)
#elif USE_RETURN_WITHOUT_VALUE
# define RTYPE void
# define RETURN_RTYPE( expr ) (void)(expr)
#elif USE_RETURN_WITH_VALUE
# define RTYPE cont_t
# define RETURN_RTYPE( expr ) return (expr)
#else
# error "No suitable call/return dicipline has been selected."
#endif

/* CONT_PARAMS is the parameter list of all compiled procedures.
   cont_t is the approximate type of a Twobit label.
   */

#if USE_GOTOS_LOCALLY

# define ENTRY_LABEL  ___k
# define CONT_PARAMS   word *globals, unsigned ENTRY_LABEL
# define CONT_ACTUALS  globals, ENTRY_LABEL
  typedef unsigned cont_t;
  typedef RTYPE (*codeptr_t)( word *globals, unsigned k );
 
#else

# define CONT_PARAMS     word *globals
# define CONT_ACTUALS    globals

typedef void (*cont_t)( CONT_PARAMS );
   /* cont_t is really typedef RTYPE (*cont_t)( CONT_PARAMS ), but 
      try telling a C compiler that.
      */
  typedef RTYPE (*codeptr_t)( word *globals );
#endif


/* Timer ticks */

#if USE_LONGJUMP
# define STACK_SIZE 262144	            /* 256 KB */
# define FRAME_SIZE 96		            /* For SPARC */
# define TIMER_STEP (STACK_SIZE/FRAME_SIZE)
#else
# define TIMER_STEP 12500
#endif
  /* TIMER_STEP is the number of ticks on the short-term timer -- the
     number of ticks between each interrupt check.

     When the jump discipline is USE_LONGJUMP then this should not be
     set too high -- if it is, then the stack will grow unreasonably,
     resulting in cache and virtual memory thrashing or stack overflow.
     A reasonable value is stack-size / frame-size, where stack-size is 
     the amount of memory you're willing to dedicate to the C stack,
     and frame-size is the expected stack frame size.  Be conservative
     in your estimate of available stack size -- frames may be larger
     than you expect, and the run-time system uses some stack also.
     */

#define TEMPORARY_FUEL 100
   /* TEMPORARY_FUEL is the number of ticks that the timer receives if
      the timer expires while interrupts are disabled.  It allows the
      program to continue at full speed for a little while, so that
      it can finish the critical work and be preempted again soon.
      */


#endif /* ifndef PETIT_CONFIG_H */
