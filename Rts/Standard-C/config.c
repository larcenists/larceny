/* Copyright 1998 Lars T Hansen
 *
 * $Id$
 *
 * config.c -- configuration-dependent values.
 */

#include "larceny.h"

#ifdef X86_NASM
char *larceny_system_name = "Larceny";
char *larceny_heap_name = "petit.heap";  /* gag */
char *larceny_architecture = "X86-NASM";
#else
char *larceny_system_name = "Petit Larceny";
char *larceny_heap_name = "petit.heap";
char *larceny_architecture = "Standard-C";
#endif

/* eof */
