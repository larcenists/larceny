/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.4 1997/02/27 16:40:26 lth Exp $
 */

char *version = "0.27e";
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
