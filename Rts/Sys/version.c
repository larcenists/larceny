/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- the version number :-)
 */

#include "config.h"

int  larceny_major_version = 0;
int  larceny_minor_version = 42;
char *larceny_version_qualifier = "";

char *date = DATE;
char *user = USER;

#if defined(SUNOS5)
  char *osname = "SunOS5";
#elif defined(SUNOS4)
  char *osname = "SunOS4";
#elif defined(STDC_SOURCE)
  char *osname = "Generic";
#else
# error "Can't defined OSNAME."
#endif

/* eof */
