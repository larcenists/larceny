/* Rts/Sys/semispace.c
 * Larceny run-time system -- semispace ADT.
 *
 * $Id: semispace.c,v 1.5 1997/05/23 13:50:06 lth Exp $
 *
 * The semispace_t ADT maintains a growable/shrinkable set of heap chunks
 * for a semispace; all chunks have the same generation number.
 *
 * For reasons having to do with the implementation of the garbage 
 * collector, chunks are allocated in strict sequential order through 
 * the chunk array, and all chunks from 0 through s->current are 
 * part of the valid semispace.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "assert.h"
#include "gclib.h"

static void extend_chunk_array( semispace_t *s );

semispace_t *
create_semispace( unsigned bytes, int heap_no, int gen_no )
{
  const int n = 5;
  semispace_t *s;
  int i;

  s = (semispace_t*)must_malloc( sizeof( semispace_t ) );
  s->chunks = (chunk_t*)must_malloc( sizeof( chunk_t )*n );

  s->heap_no = heap_no;
  s->gen_no = gen_no;
  s->allocated = 0;
  s->used = 0;
  s->current = -1;  /* Magic for initial call to ss_expand() */
  s->n = n;

  for ( i = 0; i < n; i++ ) 
    s->chunks[i].bytes = 0;
  ss_expand( s, bytes );

  return s;
}


/* Expand the semispace by allocating a chunk large enough to hold the
 * request.  A chunk larger than the request will be used if it exists;
 * otherwise, one will be allocated that's large enough to hold the
 * request but not bigger (rounded up to pagesize).  When the procedure 
 * returns, s->current has been incremented, and 
 * s->current.top = s->current.bot.
 *
 * There are three cases:
 *  - found a chunk
 *  - didn't find a chunk but found an empty slot
 *  - didn't find a chunk, didn't find an empty slot
 */
void ss_expand( semispace_t *s, unsigned bytes_request )
{
  int i, empty = -1;
  word *p;

  assert( bytes_request > 0 );

  bytes_request = roundup_page( bytes_request );

  for ( i=s->current+1 ; i < s->n ; i++ ) {
    if (s->chunks[i].bytes >= bytes_request) {
      /* Case 1: found a chunk.  Swap it into the next slot, and return. */
      chunk_t tmp = s->chunks[i];
      s->chunks[i] = s->chunks[s->current+1];
      s->chunks[s->current+1] = tmp;
      s->current = s->current + 1;
      return;
    }
    else if (s->chunks[i].bytes == 0 && empty < 0) {
      /* Remember the first empty slot if we can't find a chunk */
      empty = i;
    }
  }

  /* Case 2: empty > -1, so we found a hole at location 'empty' */
  /* Case 3: empty == -1, so no chunk, and no hole */

  if (empty == -1) {
    /* Case 3: create a hole */
    empty = s->n;              /* First slot of extension will be empty */
    extend_chunk_array( s );
  }

  /* Cases 2 and 3: move next chunk to location 'empty', then
   * allocate a new chunk in the next location.
   */

  s->chunks[empty] = s->chunks[s->current+1];

 again:
  p = (word*)gclib_alloc_heap( bytes_request, s->heap_no, s->gen_no );
  if (p == 0) {
    memfail( MF_HEAP, "Could not expand semispace, request=%u bytes.\n", 
	     bytes_request );
    goto again;
  }

  s->current = s->current + 1;

  s->allocated += bytes_request;
  s->chunks[s->current].bytes = bytes_request;
  s->chunks[s->current].bot = p;
  s->chunks[s->current].top = p;
  s->chunks[s->current].lim = p+bytes_request/sizeof(word);
}


static void
extend_chunk_array( semispace_t *s )
{
  const int n = s->n*2;
  chunk_t *c;
  int i;

  c = (chunk_t*)must_realloc( s->chunks, sizeof( chunk_t )*n );
  s->chunks = c;
  for ( i = s->n ; i < n ; i++ )
    c[i].bytes = 0;
  s->n = n;
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


/* Prune the semispace: deallocate all chunks except the first, and reset
 * the first chunks's top pointer, and set the current chunk to the first.
 */
void ss_prune( semispace_t *s )
{
  int i;

  for ( i = 1 ; i < s->n ; i++ )
    if (s->chunks[i].bytes > 0) {
      gclib_free( s->chunks[i].bot, s->chunks[i].bytes );
      s->chunks[i].bytes = 0;
      s->chunks[i].top = s->chunks[i].bot = 0;
    }
  ss_reset( s );
}

/* Deallocate all unused chunks (except the first), set the lim pointer
 * of the last used chunk to equal the top pointer (modulo roundup), and 
 * free any excess memory from the last used chunk.
 */
void ss_shrinkwrap( semispace_t *s )
{
  int i;
  word *newlim;
  unsigned newbytes;

  for ( i = s->current+1 ; i < s->n ; i++ )
    if (s->chunks[i].bytes > 0) {
      gclib_free( s->chunks[i].bot, s->chunks[i].bytes );
      s->chunks[i].bytes = 0;
    }
  newlim = (word*)roundup_page( (unsigned)s->chunks[s->current].top );
  newbytes = (unsigned)newlim-(unsigned)s->chunks[s->current].bot;
  gclib_free( newlim, s->chunks[s->current].bytes - newbytes );
  s->chunks[s->current].lim = newlim;
  s->chunks[s->current].bytes = newbytes;
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


/* Free a semispace and all its data. */
void ss_free( semispace_t *s )
{
  int i;

  for ( i=0 ; i < s->n ; i++ ) {
    if (s->chunks[i].bytes > 0)
      gclib_free( s->chunks[i].bot, s->chunks[i].bytes );
    s->chunks[i].bot = s->chunks[i].top = 0;  /* catches bugs */
  }
  free( s->chunks );
  s->chunks = 0;                              /* catches bugs */
  free( s );
}

/* eof */
