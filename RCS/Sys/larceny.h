/*
 * This file defines the "word" data type and all global names in Larceny.
 *
 * $Id$
 *
 * Canonical list of source files
 *   Sys/main.c
 *   Sys/memsupport.c
 *   Sys/cglue.c
 *   Sys/localdebugger.c
 *   Sys/gc.c
 *   Sys/version.c
 *   Sparc/generic.s
 *   Sparc/glue.s
 *   Sparc/memsupport.s
 *   Sparc/tables.s
 */

#ifndef _D_WORD
typedef unsigned long word;
#define _D_WORD
#endif

/* "Sys/main.c"
 *
 * This file has main() and support stuff for it. Arguably, the correct name
 * of this file is "larceny.c".
 */

extern word globals[];
  /* Table of all global variables except the millicode table. */

extern void C_panic(/* char *msg */);
  /* Given a string, print the string on the standard error and exit() to
   * the operating system.
   * Should be converted to taking a variable number of arguments a la printf.
   */


/* "Sys/memsupport.c"
 *
 * This file has memory support procedures which are callable both from C
 * and from millicode. They all assume that the virtual machine registers have
 * been stored in the "globals" table.
 */

extern int C_init_mem(/* int e_size, 
			 int t_size,
			 int e_size,
			 int stk_size,
			 int e_lim */);
  /* Init the memory management subsystem with the given area sizes and the
   * given limit. Returns 0 on failure, 1 on success.
   */

extern word *C_alloc(/* unsigned n */);
  /* Allocate 'n' bytes from the Scheme heap and return a pointer to it.
   * May invoke the garbage collector.
   */

extern word C_gcstart2(/* int n */);
  /* Start a garbage collection, and return an untagged pointer (as a word) to
   * the requested number of words of memory. "N" may legally be -1 (to trigger
   * a tenuring collection) or -2 (to trigger a full collection); in these
   * cases, the return value is void.
   */

extern int C_load_heap(/* FILE *fp, int which_heap */);
  /* Given a pointer to a heap file, load the heap. The "which_heap" parameter
   * controls which heap to load the non-static part of the image into. If
   * this parameter is 0, then the ephemeral area is used; otherwise, if it
   * is 1, then the tenured area is used. Otherwise, it is an error.
   */

extern int C_dump_heap(/* FILE *fp */);
  /* Given a pointer to a file, dump the heap into that file. The heap image
   * produced is split if anything was intially loaded into the static heap,
   * otherwise, the new heap image is unified. No ephemeral data is dumped into
   * the heap image, so the mutator *must* perform a tenuring of (usually) full
   * collection just prior to dumping the heap.
   */

extern void C_restore_frame(/* void */);
  /* Restore one stack frame from the current continuation chain into the
   * bottom of the stack cache. Bombs if there is no such frame.
   */

extern void C_flush_stack_cache(/* void */);
  /* Flush the entire stack cache to the heap, creating an extension of the
   * current continuation chain. Leaves the stack cache empty.
   */


/* "Sys/cglue.c"
 *
 * This file has many C support procedures for millicode; procedures which do
 * not belong in an obvious place end up here.
 */

extern void C_varargs(/* void */);
  /* Globals[ RESULT ] has the number of supplied arguments and 
   * globals[ ARGREG2 ] has the number of fixed arguments to a procedure.
   * The former must be greater than the latter.
   * The excess arguments are put in a fresh list on the heap and a tagged
   * pointer to the list is put into globals[ RESULT ].
   */

extern void C_exception(/* int i */);
  /* Low-level exception handler. "i" is the code for the general class of
   * the exception, defined in ./exceptions.cfg. This exception handling
   * interface is pretty much obsolete, as the new (Scheme-based) exception
   * handler is now used by everything.
   */

extern void C_break(/* void */);
  /* This procedure does some magic and invokes the breakpoint handler under
   * certain obscure conditions. This interface to breakpoints is obsolete.
   */

extern void C_singlestep(/* word s */);
  /* The argument is a tagged pointer to a string. Ths string is printed on the
   * standard output and the localdebugger is invoked. This facility is useful
   * in implementing simple single stepping on the level of MacScheme assembly
   * language.
   */

extern word C_getrusage(/* void */);
  /* Returns a millisecond count of user time from the getrusage system call,
   * as an integer (*not* as a fixnum).
   */


/* "Sys/localdebugger.c"
 *
 * This has the really-low-level debugger which is used (in principle) only
 * when the system is brought up on a new architecture, or when something
 * is terribly wrong.
 */

extern void C_localdebugger(/* void */);
  /* Throws the user into a simple interaction loop where memory and registers
   * can be inspected and changed.
   */


/* "Sys/gc.c"
 *
 */

/* Initializes collector */
extern init_collector(/* s_size, t_size, e_size, e_lim, stack_size */);
  /* unsigned s_size, t_size, e_size, e_lim, stack_size */

/* procedure to call when something runs out of space and we can't collect */
extern gc_trap(/* type */);
  /* int type; */

/* Procedure to call to collect. */
extern collect(/* type */);
  /* unsigned type; */


/* In "Sparc/tables.s" */

extern word (*millicode[])();
  /* Jump table to millicode procs. */

/* eof */


