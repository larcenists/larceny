/*
 * This is the file Sys/malloc.c.
 *
 * Larceny run-time system (Unix) -- malloc replacement.
 *
 * _Very_ simple drop-in replacement for malloc et. al. so that the run-time
 * system can use sbrk() w/o interference. It pays off to make it at least a
 * little sophisticated, so that the run-time system does not have to worry 
 * too much about which library procedures call malloc etc.
 *
 * History
 *   June 27 - July 1, 1994 / lth (v0.20)
 *     Created.
 */

#include <stdio.h>
#include <sys/types.h>

#define SIGNATURE 0xDEADBEEF

static char arena[ 65536 ];
static char *aptr = arena;
static char *alimit = arena + sizeof( arena );
static unsigned long *freelist = 0;

char *malloc( bytes )
size_t bytes;
{
  unsigned long *q;

  bytes = ((bytes + 7) & ~7) + 8;
  q = freelist;
  while (q != 0 && *(q+1) < bytes)       /* first-fit */
    q = (unsigned long *)*q;
  if (q == 0) {
    if (aptr + bytes > alimit) return 0;
    q = (unsigned long *)aptr;
    *(q+1) = (unsigned long)bytes;
    aptr += bytes;
  }
  *q = SIGNATURE;
  return (char *)(q + 2);
}

void free( obj )
char *obj;
{
  unsigned long *q;

  q = (unsigned long *)obj;
  q -= 2;
  if (*q != SIGNATURE) return;
  *q = (unsigned long)freelist;
  freelist = q;
}

char *calloc( count, size )
size_t count, size;
{
  char *p;

  p = malloc( count * size );
  if (p != 0)
    memset( (char*)p, 0, count * size );
  return p;
}

char *realloc( obj, newsize )
char *obj;
size_t newsize;
{
  char *p;
  unsigned long *q;

  if (obj != 0) {
    q = (unsigned long*)obj - 2;
    if (*q != SIGNATURE) return 0;
    if (*(q+1) >= newsize) return obj;
  }
  p = malloc( newsize );
  if (p != 0 && obj != 0) {
    memcpy( p, obj, newsize );
    free( obj );
  }
  return p;
}

/* eof */

