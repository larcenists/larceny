/* 
 * Return modification time of a file.
 *
 * $Id: mtime.c,v 1.1 1995/08/01 04:41:38 lth Exp lth $
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

