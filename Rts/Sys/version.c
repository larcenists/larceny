/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- the version number :-)
 *
 * Some nicknames to choose from:
 *  Burpelson AFB               0.46
 *  'Bat' Guano                 0.47
 *  Big Board                   0.48
 *  'Buck' Turgidson            0.49
 *  Ambassador de Sadesky       0.50
 *  The Coca-Cola Company       0.51
 *  CRM-114                     0.52
 *  Dear John                   0.53
 *  Operation Drop-Kick         0.90
 *  Doomsday Device
 *  Fluoridation
 *  General Ripper 
 *  Group Commander Mandrake
 *  Hi There!
 *  'King' Kong
 *  Missile Complex at Laputa
 *  Peace is our Profession
 *  Plan R
 *  Precious Bodily Fluids
 *  Premier Kissoff
 *  President Muffley
 *  Dr Strangelove
 *  War Room
 */

#include "config.h"

int  larceny_major_version = 0;
int  larceny_minor_version = 90;
char *larceny_version_qualifier = " \"Operation Drop-Kick\"";

char *date = DATE;
char *user = USER;

/* Corresponds to the OS list in Rts/Sys/config.h */
#if defined(SUNOS4)
  char *osname = "SunOS4";
#elif defined(SUNOS5)
  char *osname = "SunOS5";
#elif defined(LINUX)
  char *osname = "Linux";
#elif defined(CYGWIN)
  char *osname = "Cygwin";
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
