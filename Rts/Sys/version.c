/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.5 1997/04/30 16:01:41 lth Exp $
 */

char *version = "0.28a";
char *date = DATE;
char *user = USER;
#ifdef SOLARIS
  char *osname = "Solaris";
#else
#ifdef SUNOS
   char *osname = "SunOS";
#else
    this is an error
#endif
#endif

/* eof */
