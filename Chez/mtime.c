/* 
 * Return modification time of a file.
 *
 * $Id: mtime.c,v 1.1 1997/02/11 21:50:45 lth Exp $
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

