/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 */

#include "config.h"

#if defined(SUNOS4)
/* # include "unix-alloc.c" */  /* Broken. */
# include "posix-alloc.c"
#elif defined(SUNOS5)
# include "posix-alloc.c"
#else
# include "posix-alloc.c"
#endif
