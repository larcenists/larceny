/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.9 1997/09/17 15:17:26 lth Exp lth $
 */

char *version = "0.29d";
char *date = DATE;
char *user = USER;

#ifdef SOLARIS
  char *osname = "Solaris";
#endif

#ifdef SUNOS
  char *osname = "SunOS";
#endif

/* eof */
