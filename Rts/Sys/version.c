/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.6 1997/05/23 13:50:06 lth Exp $
 */

char *version = "0.28b";
char *date = DATE;
char *user = USER;

#ifdef SOLARIS
  char *osname = "Solaris";
#endif

#ifdef SUNOS
  char *osname = "SunOS";
#endif

/* eof */
