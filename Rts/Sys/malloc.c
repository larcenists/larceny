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
 *   January 23, 1996 (!)
 *     Fixed serious bug: object was not unlinked from free list.
 *
 *   June 27 - July 1, 1994 / lth (v0.20)
 *     Created.
 */

#include <stdio.h>
#include <sys/types.h>

#define SIGNATURE 0xDEADBEEF

/*FIXME: No proof that arena[] has correct initial alignment */

static char arena[ 65536 ];
static char *aptr = arena;
static char *alimit = arena + sizeof( arena );
static unsigned long *freelist = 0;

char *malloc( bytes )
size_t bytes;
{
  unsigned long *q, *prev;

  bytes = ((bytes + 7) & ~7) + 8;
  q = freelist;
  prev = 0;
  while (q != 0 && *(q+1) < bytes) {       /* first-fit */
    prev = q;
    q = (unsigned long *)*q;
  }
  if (q == 0) {
    if (aptr + bytes > alimit) { write( 1, "MALLOCFAIL", 10 ); return 0; }
    q = (unsigned long *)aptr;
    *(q+1) = (unsigned long)bytes;
    aptr += bytes;
  }
  else {
    /* Unlink q from free list */
    if (prev != 0)
      *prev = *q;
    else
      freelist = (unsigned long *)*q;
  }
  *q = SIGNATURE;
  return (char *)(q + 2);
}

void free( obj )
char *obj;
{
  unsigned long *q;

  q = (unsigned long *)obj - 2;
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
    memcpy( p, obj, newsize <= *(q+1) ? newsize : (size_t)*(q+1) );
    free( obj );
  }
  return p;
}

/* eof */

