/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Petit Larceny -- common configuration header file
 */

#ifndef PETIT_CONFIG_H
#define PETIT_CONFIG_H

/* Jump discipline.  Exactly one of these should be true */

#define USE_LONGJUMP               0
   /* Calls and returns are implemented as calls; when the timer expires, a
      longjump is performed to prune the stack.  The extern variable 
      'c_label' holds the address to jump to following the longjump.
      */

#define USE_RETURN_WITHOUT_VALUE   0 
  /* Calls are implemented as returns to a dispatch loop, as are
     returns.  The extern variable 'c_label' holds the address to jump
     to following the longjmp.
     */

#define USE_RETURN_WITH_VALUE      1
  /* Calls are implemented as returns to a dispatch loop, as are returns.
     The address to jump to is returned to the dispatch loop rather than
     being stored in a global.
     */


/* Return type of all compiled procedures. */

#if USE_LONGJUMP
# define RTYPE void
#elif USE_RETURN_WITHOUT_VALUE
# define RTYPE void
#elif USE_RETURN_WITH_VALUE
# define RTYPE cont_t
#else
# error "No suitable call/return dicipline has been selected."
#endif

// #define CONT_PARAMS     void
#define CONT_PARAMS     word *globals /* For argument lists */
#define CONT_ACTUALS    globals	      /* Call in trampoline */
  /* Parameter list of all compiled procedures. 
     */

typedef void (*cont_t)( CONT_PARAMS );
   /* cont_t is the approximate type of a Twobit label. 
      (It's really typedef cont_t (*cont_t)( CONT_PARAMS ), but try telling
      a C compiler that.)
      */


/* Timer ticks */

#define TEMPORARY_FUEL  1000
   /* TEMPORARY_FUEL is the number of ticks that the timer receives if
      the timer expires while interrupts are disabled.  It allows the
      program to continue at full speed for a little while, so that
      it can finish the critical work and be preempted again soon.
      */

#define TIMER_STEP      50000
  /* TIMER_STEP is the number of ticks on the short-term timer -- the
     number of ticks between each interrupt check.
     */

#endif /* ifndef PETIT_CONFIG_H */
