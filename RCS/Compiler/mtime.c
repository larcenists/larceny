/* 
 * Return modification time of a file.
 *
 * $Id: mtime.c,v 1.1 92/01/16 13:28:01 lth Exp Locker: lth $
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

