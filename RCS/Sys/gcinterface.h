/*
 * This file to be included by the mutator.
 * Assumes "nice" 32-bit architecture.
 */

/* Fundamental data type */
typedef unsigned long word;

/* Space variables */
extern word *e_base, *e_top, *e_limit, *e_mark, *e_max;
extern word *t_base, *t_top, *t_limit, *t_entries;
extern word *s_base, *s_top, *s_limit;
extern word *stack_base, *stack_limit, *stack_mark;

/* Statistics variables */
extern unsigned collections, words_collected, words_allocated;

/* Initializes collector */
extern init_collector(/* s_size, t_size, e_size, e_lim, stk_size, stk_lim */);
  /* unsigned s_size, t_size, e_size, e_lim, stk_size, stk_lim; */

/* procedure to call when something runs out of space and we can't collect */
extern gc_trap(/* type */);
  /* int type; */

/* Procedure to call to collect. */
extern collect(/* type */);
  /* unsigned type; */

