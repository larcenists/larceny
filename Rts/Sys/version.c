/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.1.1.1 1998/11/19 21:51:44 lth Exp $
 */

#include "config.h"

int larceny_major_version = 0;
int larceny_minor_version = 36;

char *date = DATE;
char *user = USER;

#ifdef SUNOS5
  char *osname = "SunOS5";
#endif

#ifdef SUNOS4
  char *osname = "SunOS4";
#endif

/* eof */
