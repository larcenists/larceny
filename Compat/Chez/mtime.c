/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Return modification time of a file.
 */

#include <sys/types.h>
#include <sys/stat.h>

unsigned mtime( fn )
char *fn;
{
  struct stat buf;

  if (stat( fn, &buf ) == -1)
    return 0;
  return buf.st_mtime;
}

