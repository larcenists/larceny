/*
 * This file to be included by the mutator.
 *
 * $Id$
 */

#ifndef GCINTERFACE
#define GCINTERFACE

/* Fundamental data type */
typedef unsigned long word;

/* Initializes collector */
extern init_collector(/* s_size, t_size, e_size, e_lim, stack_size */);
  /* unsigned s_size, t_size, e_size, e_lim, stack_size */

/* procedure to call when something runs out of space and we can't collect */
extern gc_trap(/* type */);
  /* int type; */

/* Procedure to call to collect. */
extern collect(/* type */);
  /* unsigned type; */

#endif /* ifndef GCINTERFACE */
