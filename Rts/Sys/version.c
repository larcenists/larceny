/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.10 1997/09/23 19:57:44 lth Exp lth $
 */

#include "config.h"

char *version = "0.33";
char *date = DATE;
char *user = USER;

#ifdef SUNOS5
  char *osname = "SunOS5";
#endif

#ifdef SUNOS4
  char *osname = "SunOS4";
#endif

/* eof */
