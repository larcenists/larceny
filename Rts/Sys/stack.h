/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- run-time stack interface
 *
 * The stack is maintained by the young heap in a GC, but the details of
 * its layout are somewhat hidden by the stack interface functions.
 */

#ifndef INCLUDED_STACK_H
#define INCLUDED_STACK_H

#include "larceny-types.h"

#define MAX_STACK_FRAME   4096      /* Maximum size of a stack frame (bytes) */

int stk_create( word *globals );
  /* Create a new stack below the heap location indicated by globals[G_STKP]
     and adjust globals[G_STKP].  Returns 1 if the creasion succeeded, 0 
     otherwise.
     */

void stk_clear( word *globals );
  /* Clear the current stack.
     */

void stk_flush( word *globals );
  /* Flush the stack cache to the heap, converting stack frames to heap
     frames, and setting globals[G_CONT] to point to the topmost frame.
     */

int stk_restore_frame( word *globals );
  /* Restore one stack frame from the heap into the stack cache.
     The cache must be clean.
     Returns 1 if it succeeded, 0 if it didn't (frame didn't fit).
     */

int stk_size_for_top_stack_frame( word *globals );
  /* Compute the number of bytes required to accomodate a new stack as well
     as the top stack frame in the current stack.
     */

void stk_stats( word *globals, stack_stats_t *stats );
  /* Fill in the stats structure with statistics about the stack module.
     */


/* IMPORTED ROUTINES */

void stk_initialize_underflow_frame( word *stktop );
  /* Given a pointer to a four-word initialized frame, store data in it
     that allow it to function as a stack underflow frame.
     */ 

#endif /* INCLUDED_STACK_H */

/* eof */
