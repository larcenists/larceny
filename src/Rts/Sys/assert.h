/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- assert() and assert2() macros
 */

#undef assert
#undef assert2

#ifndef NDEBUG
extern int panic_abort( const char *fmt, ... );
#define assert( expr ) \
  ((expr) \
   ? (void)0 \
   : (void)panic_abort( "%s;%d: Assertion failed.", __FILE__, __LINE__ ))
#else
#define assert( expr ) ((void)0)
#endif

#ifndef NDEBUG2
extern int panic_abort( const char *fmt, ... );
#define assert2( expr ) \
  ((expr) \
   ? (void)0 \
   : (void)panic_abort( "%s;%d: Assertion failed.", __FILE__, __LINE__ ))
#else
#define assert2( expr ) ((void)0)
#endif

/* eof */
