/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.10 1997/09/23 19:57:44 lth Exp lth $
 */

char *version = "0.31";
char *date = DATE;
char *user = USER;

#ifdef SOLARIS
  char *osname = "Solaris";
#endif

#ifdef SUNOS
  char *osname = "SunOS";
#endif

/* eof */
