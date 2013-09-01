/* Copyright 1998 Lars T Hansen
 *
 * $Id: config.c 2543 2005-07-20 21:54:03Z pnkfelix $
 *
 * config.c -- configuration-dependent values.
 */

#include "larceny.h"

char *larceny_system_name = "Larceny";
#ifdef ARM
char *larceny_heap_name = "arm.heap";
char *larceny_architecture = "ARM";
#else
#error "Unknown Fence/Cant architecture"
#endif

/* eof */
