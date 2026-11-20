/* Copyright 1998 Lars T Hansen
 *
 * $Id: config.c 2543 2005-07-20 21:54:03Z pnkfelix $
 *
 * config.c -- configuration-dependent values.
 */

#include "larceny.h"

char *larceny_system_name = "Larceny";
#if defined(ARM) && defined(BITS_64)
char *larceny_heap_name = "arm64.heap";
char *larceny_architecture = "ARM";
#elif defined(ARM)
char *larceny_heap_name = "arm.heap";
char *larceny_architecture = "ARM";
#elif defined(X86_64)
char *larceny_heap_name = "x86-64.heap";
char *larceny_architecture = "X86-64";
#else
#error "Unknown Fence/Cant architecture"
#endif

/* eof */
