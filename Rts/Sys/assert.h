/* Rts/Sys/assert.h
 * Larceny run-time system -- assert() macro
 * 
 * January 10, 1997
 */

#ifndef NDEBUG
#define assert( expr ) \
  ((expr) \
   ? (void)0 \
   : (void)panic_abort( "%s;%d: Assertion failed.", __FILE__, __LINE__ ))
#else
#define assert( expr ) ((void)0)
#endif

/* eof */
