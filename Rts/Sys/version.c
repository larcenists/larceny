/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- the version number :-)
 *
 * Some nicknames to choose from:
 *  Burpelson                  1.0
 *  Doomsday Device
 *  Fluoridation
 *  Guano
 *  Kissoff
 *  Kong
 *  Mandrake
 *  Muffley
 *  Operation Drop-Kick
 *  Plan R
 *  Precious Bodily Fluids
 *  Ripper
 *  de Sadesky
 *  Strangelove
 *  Turgidson
 */

#include "config.h"

int  larceny_major_version = 0;
int  larceny_minor_version = 46;
char *larceny_version_qualifier = " \"Burpelson\"";

char *date = DATE;
char *user = USER;

/* Corresponds to the OS list in Rts/Sys/config.h */
#if defined(SUNOS4)
  char *osname = "SunOS4";
#elif defined(SUNOS5)
  char *osname = "SunOS5";
#elif defined(LINUX)
  char *osname = "Linux";
#elif defined(BSD_UNIX)
  char *osname = "BSD Unix";
#elif defined(POSIX_UNIX)
  char *osname = "Posix Unix";
#elif defined(XOPEN_UNIX)
  char *osname = "X/Open Unix";
#elif defined(WIN32)
  char *osname = "Win32";
#elif defined(MACOS)
  char *osname = "MacOS";
#elif defined(GENERIC_OS)
  char *osname = "Generic OS";
#else
# error "Can't define OSNAME."
#endif

/* eof */
