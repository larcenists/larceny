/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- assert() macro
 */

#undef assert
#ifndef NDEBUG
extern int panic_abort( const char *fmt, ... );
#define assert( expr ) \
  ((expr) \
   ? (void)0 \
   : (void)panic_abort( "%s;%d: Assertion failed.", __FILE__, __LINE__ ))
#else
#define assert( expr ) ((void)0)
#endif

/* eof */
