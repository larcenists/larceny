/*
 * This is the file Sys/version.c.
 *
 * Larceny run-time system -- the version number :-)
 */

char *version = "0.24";
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
