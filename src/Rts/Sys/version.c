/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- the version number :-)
 *
 * Version history, with more nicknames for future releases:
 *                                          release date    revision
 *                              0.36        20 Nov 1998     28, 12, 31
 *                              0.37        11 Dec 1998     250, 249
 *                              0.38        14 Dec 1998     290, 289
 *                              0.40        16 Dec 1998     352, 351
 *  Burpelson AFB               0.46        20 Oct 1999     1081, 1080
 *  'Bat' Guano                 0.47        19 Nov 1999     1226, 1225
 *  Big Board                   0.48        25 Mar 2000     1422, 1421
 *  'Buck' Turgidson            0.49        11 Sep 2000     1461, 1459
 *  Ambassador de Sadesky       0.50        28 Sep 2000
 *  The Coca-Cola Company       0.51
 *  CRM-114                     0.52        06 Nov 2003     1742, 1719
 *  Dear John                   0.53        14 Jun 2005     2499, 1632
 *  Operation Drop-Kick         0.90        15 Feb 2006
 *  Children's Ice Cream        0.91        23 May 2006     3039
 *  Definite Missile Track      0.92        21 Jul 2006     3221
 *  Deviated Prevert            0.93         9 Nov 2006     3782, 3944
 *  Doomsday Device             0.94         3 Jul 2007     4663
 *  First Safety                0.95         8 Nov 2007     5108
 *  Fluoridation                0.96
 *  Funny in the Head
 *  General Ripper 
 *  Goldie
 *  Grain Alcohol and Rainwater
 *  Group Commander Mandrake
 *  Hi There!
 *  Horsing Around
 *  Ignorant Peons
 *  Important Promotions
 *  'King' Kong
 *  Loss of Essence
 *  Mineshaft Gap
 *  Missile Complex at Laputa
 *  Nuclear Combat
 *  Ominous Rumors
 *  Peace is our Profession
 *  People's Central Air Defense
 *  Personal Feelings
 *  Picnic and a Rodeo
 *  Plan R
 *  Precious Bodily Fluids
 *  Premier Kissoff
 *  President Muffley
 *  Primary Target
 *  Purity of Essence
 *  Recall Code
 *  Ridiculous Camera
 *  A Silly Thing
 *  A Single Slip-up
 *  Sleigh Bells
 *  Some Sunny Day
 *  Dr Strangelove
 *  Strategic Thought
 *  Vegas
 *  War Room
 *  World Targets in MegaDeaths
 *  Zhokhov Islands
 *  Mein Fuhrer...I Can Walk!
 */

#include "config.h"

int  larceny_major_version = 0;
int  larceny_minor_version = 96;
char *larceny_version_qualifier = "a1 \"Fluoridation\"";

char *date = DATE " " TIME;
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
