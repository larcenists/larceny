/* Rts/Sys/semispace.c
 * Larceny run-time system -- semispace ADT implementation.
 *
 * $Id: semispace.c,v 1.1.1.1 1998/11/19 21:51:43 lth Exp $
 *
 * Interface defined in "Rts/Sys/semispace_t.h".
 */

#define GC_INTERNAL

#include <stdlib.h>
#include "larceny.h"
#include "gclib.h"
#include "semispace_t.h"

static void extend_chunk_array( semispace_t *ss );
static void allocate_chunk_memory( semispace_t *ss, int slot, int bytes );
static void free_chunk_memory( semispace_t *ss, int slot );
static void clear( semispace_t *ss, int i );

semispace_t *create_semispace( int bytes, int heap_no, int gen_no )
{
  const int n = 5;
  semispace_t *ss;
  int i;

  assert( bytes > 0 );
  assert( heap_no >= 0 );
  assert( gen_no >= heap_no );

  bytes = roundup_page( bytes );

  ss = (semispace_t*)must_malloc( sizeof( semispace_t ) );

  ss->heap_no = heap_no;
  ss->gen_no = gen_no;
  ss->n = n;
  ss->current = 0;
  ss->chunks = (ss_chunk_t*)must_malloc( sizeof( ss_chunk_t )*n );
  ss->allocated = 0;
  ss->used = 0;

  for ( i = 0; i < n; i++ ) 
    ss->chunks[i].bytes = 0;

  allocate_chunk_memory( ss, 0, bytes );
  return ss;
}

void ss_expand( semispace_t *s, int bytes_needed )
{
  int i, empty;
  word *p;

  assert( bytes_needed > 0 );

  bytes_needed = roundup_page( bytes_needed );

  /* Search for a large-enough chunk, and remember the first empty slot. */
  empty = -1;
  for ( i=s->current+1 ; i < s->n && s->chunks[i].bytes < bytes_needed ; i++ )
    if (s->chunks[i].bytes == 0 && empty < 0)
      empty = i;

  /* Found a chunk at slot `i'.  Increment current, and swap the found chunk
   * into the now-current slot, then return.
   */
  if (i < s->n) {
    ss_chunk_t tmp;

    s->current = s->current + 1;
    tmp = s->chunks[i];
    s->chunks[i] = s->chunks[s->current];
    s->chunks[s->current] = tmp;
    s->chunks[s->current].top = s->chunks[s->current].bot;
    return;
  }

  /* `Empty' == -1, so no chunk was found, nor an empty slot.  Create an 
   * empty slot and note its location in `empty'.
   */
  if (empty == -1) {
    empty = s->n;              /* First slot of extension will be empty */
    extend_chunk_array( s );
  }

  /* We have an empty slot, but no chunk yet.  Increment current, move
   * the chunk from the current slot to slot 'empty' (to preserve its 
   * contents -- it has memory, just not enough), then allocate a
   * new chunk in the current slot.
   */
  s->current = s->current + 1;
  s->chunks[empty] = s->chunks[s->current];
  allocate_chunk_memory( s, s->current, bytes_needed );
}

void ss_reset( semispace_t *ss )
{
  int i;

  for ( i = 0 ; i < ss->n ; i++ )
    if (ss->chunks[i].bytes > 0)
      ss->chunks[i].top = ss->chunks[i].bot;

  ss->current = 0;
  ss->used = 0;
}

void ss_prune( semispace_t *ss )
{
  int i;

  for ( i = 1 ; i < ss->n ; i++ )
    if (ss->chunks[i].bytes > 0)
      free_chunk_memory( ss, i );
  ss_reset( ss );
}

void ss_shrinkwrap( semispace_t *ss )
{
  int i;
  int newbytes;
  ss_chunk_t *c;

  for ( i = ss->current+1 ; i < ss->n ; i++ )
    if (ss->chunks[i].bytes > 0) 
      free_chunk_memory( ss, i );

  c = &ss->chunks[ss->current];
  c->lim = c->top = (word*)roundup_page( (unsigned)c->top );
  newbytes = (c->top - c->bot)*sizeof( word );
  gclib_shrink_block( c->bot, c->bytes, newbytes );
  ss->allocated -= c->bytes - newbytes;
  c->bytes = newbytes;
}

void ss_sync( semispace_t *ss )
{
  int i;

  ss->used = 0;
  for ( i = 0 ; i <= ss->current ; i++ )
    if (ss->chunks[i].bytes > 0)
      ss->used += (ss->chunks[i].top - ss->chunks[i].bot)*sizeof(word);
}

void ss_free( semispace_t *ss )
{
  int i;

  for ( i=0 ; i < ss->n ; i++ )
    if (ss->chunks[i].bytes > 0)
      free_chunk_memory( ss, i );

  free( ss->chunks );
  ss->chunks = 0;		/* Catches bugs. */
  free( ss );
}

int ss_allocate_and_insert_block( semispace_t *ss, int nbytes )
{
  int curr = ss->current;
  ss_chunk_t tmp;

  ss_expand( ss, nbytes );
  tmp = ss->chunks[ curr ];
  ss->chunks[ curr ] = ss->chunks[ ss->current ];
  ss->chunks[ ss->current ] = tmp;
  return curr;
}

int ss_move_block_to_semispace( semispace_t *from, int i, semispace_t *to )
{
  int j;

  /* Look for an empty slot in `to' */
  for ( j=0 ; j < to->n ; j++ )
    if (to->chunks[j].bytes == 0)
      break;

  /* If no empty slot was found, then extend `to'; `j' remains correct. */
  if (j == to->n)
    extend_chunk_array( to );

  /* `j' is the index of the empty slot: swap the chunk into it. */
  to->chunks[j] = to->chunks[to->current+1];
  to->chunks[to->current+1] = to->chunks[to->current];
  j = to->current;
  to->current = to->current + 1;
  to->chunks[j] = from->chunks[i];

  /* Fill hole in `from' */
  if (from->current > 0) {
    from->chunks[i] = from->chunks[from->current-1];
    from->chunks[from->current-1] = from->chunks[from->current];
  }
  clear( from, from->current );
  from->current = from->current - 1;

  /* Update statistics */
  to->allocated += to->chunks[j].bytes;
  from->allocated -= to->chunks[j].bytes;

  /* Set generation number on moved memory */
  gclib_set_generation( to->chunks[j].bot, to->chunks[j].bytes, to->gen_no );

  return j;
}

void ss_set_gen_no( semispace_t *s, int gen_no )
{
  int i;

  s->gen_no = gen_no;
  for ( i=0 ; i < s->n ; i++ ) {
    if (s->chunks[i].bytes == 0) continue;
    gclib_set_generation( s->chunks[i].bot, s->chunks[i].bytes, gen_no );
  }
}

/* Internal */

static void clear( semispace_t *ss, int i )
{
  ss->chunks[i].bytes = 0;
  ss->chunks[i].bot = (word*)0xDEADBEEF;
  ss->chunks[i].top = (word*)0xDEADBEEF;
  ss->chunks[i].lim = (word*)0xDEADBEEF;
}

static void extend_chunk_array( semispace_t *ss )
{
  const int n = ss->n*2;
  ss_chunk_t *c;
  int i;

  c = (ss_chunk_t*)must_realloc( ss->chunks, sizeof( ss_chunk_t )*n );
  ss->chunks = c;
  for ( i = ss->n ; i < n ; i++ ) 
    clear( ss, i );
  ss->n = n;
}

static void allocate_chunk_memory( semispace_t *ss, int slot, int bytes )
{
  word *p;

  assert( bytes % PAGESIZE == 0 );
  assert( slot >= 0 && slot < ss->n );
  assert( ss->chunks[slot].bytes == 0 );

 again:
  p = (word*)gclib_alloc_heap( bytes, ss->gen_no );
  if (p == 0) {
    memfail( MF_HEAP, "Could not expand semispace, request=%u bytes.\n", 
	     bytes );
    goto again;
  }

  ss->allocated += bytes;
  ss->chunks[slot].bytes = bytes;
  ss->chunks[slot].bot = p;
  ss->chunks[slot].top = p;
  ss->chunks[slot].lim = (word*)((char*)p + bytes);
}

static void free_chunk_memory( semispace_t *ss, int slot )
{
  ss_chunk_t *c;

  assert( slot >= 0 && slot < ss->n );
  assert( ss->chunks[slot].bytes > 0 );

  c = &ss->chunks[slot];
  gclib_free( c->bot, c->bytes );
  ss->allocated -= c->bytes;
  clear( ss, slot );
}

/* eof */
