/* Rts/Sys/version.c.
 * Larceny run-time system -- the version number :-)
 *
 * $Id: version.c,v 1.2 1997/02/11 14:30:55 lth Exp $
 */

char *version = "0.26";
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
