/* Rts/Sys/semispace.c
 * Larceny run-time system -- semispace ADT.
 *
 * $Id: semispace.c,v 1.3 1997/02/20 15:55:03 lth Exp $
 *
 * The semispace_t ADT maintains a growable/shrinkable set of heap chunks
 * for a semispace; all chunks have the same generation number.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gclib.h"

semispace_t *
create_semispace( unsigned bytes, int heap_no, int gen_no )
{
  const int n = 5;
  semispace_t *s;
  int i;

 again:
  if ((s = (semispace_t*)malloc( sizeof( semispace_t ) )) == 0) {
    memfail( MF_MALLOC, "Unable to allocate semispace structure." );
    goto again;
  }

  s->heap_no = heap_no;
  s->gen_no = gen_no;
  s->allocated = 0;
  s->used = 0;
  s->current = -1;  /* magic */
  s->n = n;
  s->chunks = (chunk_t*)malloc( sizeof( chunk_t )*n );

 again2:
  if (s->chunks == 0) {
    memfail( MF_MALLOC, "Unable to allocate semispace chunks." );
    goto again2;
  }
  for ( i = 0; i < n; i++ ) 
    s->chunks[i].bytes = 0;
  ss_expand( s, bytes );

  return s;
}


/* Expand the semispace by allocating a chunk large enough to hold the
 * request.  A chunk larger than the request will be used if it exists;
 * otherwise, one will be allocated that's large enough to hold the
 * request but not bigger (rounded up to pagesize).  It's the responsibility
 * of the caller to pass a reasonable value.
 */
void ss_expand( semispace_t *s, unsigned bytes_request )
{
  int i, empty = -1;
  word *p;
  
  bytes_request = roundup_page( bytes_request );

  for ( i=s->current+1 ; i < s->n && empty < 0 ; i++ ) {
    if (s->chunks[i].bytes >= bytes_request) {
      chunk_t tmp = s->chunks[i];
      s->chunks[i] = s->chunks[s->current+1];
      s->chunks[s->current+1] = tmp;
      s->current++;
      return;
    }
    else if (s->chunks[i].bytes == 0)
      empty = i;
  }

  /* No chunk large enough was found.  If the array is full, we
   * must extend it, otherwise we can use an empty slot.
   */
  if (empty == -1) {
    int n = s->n*2;
    chunk_t *c;
  again:
    c = (chunk_t*)realloc( s->chunks, sizeof( chunk_t )*n );
    if (c == 0) {
      memfail( MF_REALLOC, "Unable to extend semispace chunk array." );
      goto again;
    }
    s->chunks = c;
    for ( i = s->n ; i < n ; i++ )
      c[i].bytes = 0;
    s->n = n;
  }
  else
    s->chunks[empty] = s->chunks[s->current+1];  /* save any too small chunk */

  i = ++s->current;

 again2:
  p = (word*)gclib_alloc_heap( bytes_request, s->heap_no, s->gen_no );
  if (p == 0) {
    memfail( MF_HEAP, "Could not expand semispace, request=%u bytes.\n", 
	     bytes_request );
    goto again2;
  }

  s->allocated += bytes_request;
  s->chunks[i].bytes = bytes_request;
  s->chunks[i].bot = p;
  s->chunks[i].top = p;
  s->chunks[i].lim = p+bytes_request/sizeof(word);
}


/* Reset the semispace: set each chunk's top pointer equal to the bot
 * pointer, set usage to 0 and set the current chunk to the first.
 */
void ss_reset( semispace_t *s )
{
  int i;

  for ( i = 0 ; i < s->n ; i++ )
    if (s->chunks[i].bytes > 0)
      s->chunks[i].top = s->chunks[i].bot;
  s->current = 0;
  s->used = 0;
}


/* Make a pass over all the chunks and compute the number of bytes in
 * use in the semispace.
 */
void ss_sync( semispace_t *s )
{
  int i;

  s->used = 0;
  for ( i = 0 ; i <= s->current ; i++ )
    if (s->chunks[i].bytes > 0)
      s->used += (s->chunks[i].top - s->chunks[i].bot)*sizeof(word);
}


/* Free a semispace and all its data.
 */
void ss_free( semispace_t *s )
{
  int i;

  for ( i=0 ; i < s->n ; i++ ) {
    if (s->chunks[i].bytes > 0)
      gclib_free( s->chunks[i].bot, s->chunks[i].bytes );
    s->chunks[i].bot = s->chunks[i].top = 0;  /* catches bugs */
  }
  free( s->chunks );
  s->chunks = 0;      /* catches bugs */
  free( s );
}

/* eof */
