/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 */

#include "config.h"

#ifdef SUNOS4
/* # include "unix-alloc.c" */  /* Broken. */
# include "posix-alloc.c"
#endif

#ifdef SUNOS5
# include "posix-alloc.c"
#endif
