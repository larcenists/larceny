/* Copyright 1998 Lars T Hansen
 *
 * $Id$
 *
 * Print out machine-type definitions to use on a command line.
 * Used by the make system for the assembly-language files.
 */

#include <stdio.h>
#include "../Sys/config.h"

main()
{
#if defined(SUNOS4)
  printf( "-DSUNOS4\n" );
#endif
#if defined(SUNOS5)
  printf( "-DSUNOS5\n" );
#endif
}
