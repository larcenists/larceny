/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- the version number :-)
 *
 * Some nicknames to choose from:
 *  'Bat' Guano
 *  Big Board
 *  Burpelson AFB               0.46
 *  Captain Mandrake
 *  CRM-114
 *  de Sadesky
 *  Dear John
 *  Doomsday Device
 *  Dr Strangelove
 *  Fluoridation
 *  General Ripper 
 *  Hi There!
 *  'King' Kong
 *  Missile Complex at Laputa
 *  Operation Drop-Kick
 *  Peace is our Profession
 *  Plan R
 *  Precious Bodily Fluids
 *  Premier Kissoff
 *  President Muffley
 *  Turgidson
 *  War Room
 */

#include "config.h"

int  larceny_major_version = 0;
int  larceny_minor_version = 46;
char *larceny_version_qualifier = " \"Burpelson AFB\"";

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
