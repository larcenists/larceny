/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.7 1997/07/07 20:13:53 lth Exp lth $
 */

char *version = "0.28d";
char *date = DATE;
char *user = USER;

#ifdef SOLARIS
  char *osname = "Solaris";
#endif

#ifdef SUNOS
  char *osname = "SunOS";
#endif

/* eof */
