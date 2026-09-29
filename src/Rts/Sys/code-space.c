/* Copyright 2026.                        -*- indent-tabs-mode: nil -*-
 *
 * Larceny -- the code space; see code-space.h.
 */

#define GC_INTERNAL

#include <string.h>
#include "larceny.h"
#include "gclib.h"
#include "memmgr.h"            /* GC_CHUNK_SIZE */
#include "semispace_t.h"
#include "code-space.h"

#if CODE_SPACE

#define FORWARD_HDR ((word)-2)     /* As in cheney.h */

static void writing( code_space_t *cs, int on )
{
  if (cs->writing != on) {
    osdep_code_writing( on );
    cs->writing = on;
  }
}

/* Allocate nbytes (a multiple of the object alignment) in ss. */

static word *ss_alloc( semispace_t *ss, int nbytes )
{
  ss_chunk_t *c = &ss->chunks[ss->current];
  word *p;

  if ((char*)c->lim - (char*)c->top < nbytes) {
    ss_expand( ss, max( nbytes, GC_CHUNK_SIZE ) );
    c = &ss->chunks[ss->current];
  }
  p = c->top;
  c->top = (word*)((char*)p + nbytes);
  return p;
}

/* Make the code in every chunk of ss executable and visible to
   instruction fetch.  */

static void flush_semispace( semispace_t *ss )
{
  int i;

  for ( i=0 ; i <= ss->current ; i++ )
    if (ss->chunks[i].bytes > 0 && ss->chunks[i].top > ss->chunks[i].bot)
      osdep_code_flush( ss->chunks[i].bot, ss->chunks[i].top );
}

code_space_t *create_code_space( int gen_no, int by_scanning )
{
  code_space_t *cs = (code_space_t*)must_malloc( sizeof( code_space_t ) );

  cs->current = create_code_semispace( GC_CHUNK_SIZE, gen_no );
  cs->tospace = 0;
  cs->gen_no = gen_no;
  cs->writing = 0;
  cs->load_bot = cs->load_top = 0;
  cs->installed = 0;
  cs->live = 0;
  cs->by_scanning = by_scanning;
  cs->scan_requested = 0;
  return cs;
}

void cs_set_gen_no( code_space_t *cs, int gen_no )
{
  cs->gen_no = gen_no;
  ss_set_gen_no( cs->current, gen_no );
  if (cs->tospace)
    ss_set_gen_no( cs->tospace, gen_no );
}

word *cs_load_area( code_space_t *cs, int nbytes )
{
  assert( nbytes % BYTE_ALIGNMENT == 0 );
  assert( cs->load_bot == 0 );

  writing( cs, 1 );
  cs->load_bot = ss_alloc( cs->current, nbytes );
  cs->load_top = (word*)((char*)cs->load_bot + nbytes);
  return cs->load_bot;
}

void cs_load_complete( code_space_t *cs )
{
  if (cs->load_bot != 0) {
    writing( cs, 0 );
    osdep_code_flush( cs->load_bot, cs->load_top );
    cs->load_bot = cs->load_top = 0;
  }
}

word cs_install( code_space_t *cs, word bv )
{
  word *src = ptrof( bv );
  int nbytes = (int)object_size_bytes( *src );
  word *dest;

  assert( tagof( bv ) == BVEC_TAG );

  writing( cs, 1 );
  dest = ss_alloc( cs->current, nbytes );
  memcpy( dest, src, nbytes );
  writing( cs, 0 );
  osdep_code_flush( dest, (char*)dest + nbytes );
  cs->installed += nbytes;
  return tagptr( dest, BVEC_TAG );
}

/* Code is normally collected when its generation fills up with data,
   but a program that creates a lot of code (and little data) could
   exhaust the code arena first.  */

#define CS_MIN_INSTALLED  (8*1024*1024)

int cs_wants_collection( code_space_t *cs )
{
  return cs->installed > max( CS_MIN_INSTALLED, cs->live );
}

void cs_collected( code_space_t *cs )
{
  ss_sync( cs->current );
  cs->live = cs->current->used;
  cs->installed = 0;
}

word cs_forward( code_space_t *cs, word obj )
{
  word *src = ptrof( obj );
  int nbytes = (int)object_size_bytes( *src );
  word *dest;

  assert( tagof( obj ) == BVEC_TAG );
  assert( ishdr( *src ) );

  /* The forwarding pointer is written into the old code, so the old
     code space must be writable as well as the new one.  */
  writing( cs, 1 );
  if (cs->tospace == 0)
    cs->tospace = create_code_semispace( max( nbytes, GC_CHUNK_SIZE ),
                                         cs->gen_no );
  dest = ss_alloc( cs->tospace, nbytes );
  memcpy( dest, src, nbytes );
  src[0] = FORWARD_HDR;
  src[1] = tagptr( dest, BVEC_TAG );
  return src[1];
}

word cs_update_pointer( code_space_t *cs, word w )
{
  word *p;

  if (tagof( w ) != BVEC_TAG || !is_code_page( w ))
    return w;
  p = ptrof( w );
  if (cs->tospace && ss_is_address_mapped( cs->tospace, p, FALSE ))
    return w;                   /* Already copied */
  if (*p == FORWARD_HDR)
    return p[1];
  return cs_forward( cs, w );
}

void cs_after_collection( code_space_t *cs )
{
  if (cs->tospace != 0) {
    ss_free( cs->current );
    cs->current = cs->tospace;
    cs->tospace = 0;
    cs_collected( cs );
    supremely_annoyingmsg( "Code space after collection: %d bytes.",
                           cs->current->used );
  }
  if (cs->writing) {
    writing( cs, 0 );
    flush_semispace( cs->current );
  }
}

semispace_t *cs_semispace( code_space_t *cs )
{
  ss_sync( cs->current );
  return cs->current;
}

#endif /* CODE_SPACE */

/* eof */
