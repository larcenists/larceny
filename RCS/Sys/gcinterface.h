/* -*- Fundamental -*-
 *
 * Scheme Run-time System
 *
 * Definitions and types for the garbage collection system;
 * procedures exported by the collector.
 *
 * $Id: gcinterface.h,v 1.4 1991/07/24 11:50:54 lth Exp lth $
 */

#ifndef GCINTERFACE
#define GCINTERFACE

/* Collection types */

#define EPHEMERAL_COLLECTION	1
#define TENURING_COLLECTION	2
#define FULL_COLLECTION		3

/* Memory trap types */

#define EPHEMERAL_TRAP		1
#define TENURED_TRAP		2
#define STATIC_TRAP		3
#define STACK_TRAP		4

/* Fundamental data type */
#ifndef _D_WORD
#define _D_WORD
typedef unsigned long word;
#endif

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
