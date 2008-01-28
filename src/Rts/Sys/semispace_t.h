/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- semispace_t ADT
 *
 * A semispace_t maintains a set of memory chunks of the same generation.
 * The set can grow and shrink as necessary; typically, it is grown by the
 * low-level memory allocator (gclib) when the semispace overflows during 
 * GC or promotion, and shrunk by the heap under direction of policy.
 *
 * A semispace_t `s' that has not been passed to ss_free() has invariants:
 *   -1 <= s.current < s.n
 *   s.allocated > 0 || s.current == -1
 *   s.chunks != NULL
 *   s.chunks[-1].bytes == 0
 *   if 0 <= k <= s.current then s.chunks[ k ].bytes > 0
 *   if k > s.current and s.chunks[k].bytes > 0 then
 *     the memory pointed to by s.chunks[k].bot is free
 *   s.allocated is always accurate
 *   s.used is accurate only following a call to ss_sync()
 *
 * An ss_chunk_t `c' = s.chunks[i] where -1 <= i < s.n has invariants:
 *   c.bot <= c.top <= c.lim
 *   c.bot <= p < c.lim are dereferencable addresses
 *   c.bytes = (c.lim - c.top) * sizeof(word)
 */

#ifndef INCLUDED_SEMISPACE_T_H
#define INCLUDED_SEMISPACE_T_H

#include "larceny-types.h"

struct ss_chunk {
  int  bytes;            /* # of bytes allocated; 0 => pointers are garbage */
  word *bot;             /* Pointer to first word */
  word *top;             /* Pointer to next free word */
  word *lim;             /* Pointer past last free word */
};

struct semispace {
  int        gen_no;     /* Generation identifier */
  int        n;          /* Length of chunk array */
  int        current;    /* Index of current chunk (may be -1 briefly) */
  ss_chunk_t *chunks;    /* Array of chunks */
  int        allocated;  /* Total allocated bytes in semispace */
  int        used;       /* Total used bytes in semispace */
};

semispace_t *create_semispace( int bytes, int gen_no );
  /* Create a semispace_t with a single chunk that can hold at least
     `bytes' bytes, with the given generation number.

     bytes > 0
     gen_no >= 0
     */

semispace_t *create_semispace_n( int chunk_bytes, int chunks, int gen_no );
  /* Create a semispace_t with `chunks' chunks of `chunk_bytes' each,
     with the given generation number.  `Chunks' can be 0.

     chunks >= 0
     chunk_bytes > 0 || chunks == 0
     gen_no >= 0
     */

void ss_free( semispace_t *ss );
  /* Destroy a semispace and all its data.  Invalidates ss.
     */

void ss_expand( semispace_t *ss, int bytes_needed );
  /* Expand the semispace by allocating a chunk large enough to hold
     the request, reusing an unused chunk if possible.  Increments 
     ss->current, and puts the new chunk at the new ss->current,
     with ss->chunks[ss->current].top == ss->chunks[ss->current].bot.

     bytes_needed >= 0
     */

int ss_allocate_and_insert_block( semispace_t *ss, int bytes );
  /* Allocate a chunk that will hold `bytes' bytes and insert it into
     the semispace before the current chunk, unless ss.current == -1
     in which case ss.current will point to the new chunk.  May 
     reuse an existing, unused chunk.  Return its chunk index.

     bytes > 0
     */

int ss_allocate_block_unconditionally( semispace_t *ss, int bytes );
  /* Allocate a chunk that will hold `bytes' bytes and insert it into the
     semispace after the current chunk.  Will not reuse any existing but
     unused chunk.  Will not change ss->current.  Returns the index of
     the new chunk.

     bytes > 0
     */

int ss_move_block_to_semispace( semispace_t *from, int i, semispace_t *to );
  /* Move the memory associated with chunk `i' in semispace `from' to 
     semispace `to'.  Change the generation number on the pages in the
     chunk to correspond to its new home.  Return the new chunk index.

     from->current remains unchanged unless that would leave it to point
     to a chunk with nonzero bytes, in which case it is decremented
     until it points to a chunk with nonzero bytes, or to chunk -1.

     The chunk is moved to the first free slot past to->current.  If 
     to->current >= 0, then to->current remains unchanged, otherwise
     to->current is set to 0.

     0 <= i < from.n && from.chunks[i].bytes > 0
     */

int ss_insert_block_in_semispace( semispace_t *from, int i, semispace_t *to );
  /* Like move_block_to_semispace(), but inserts the block before 
     to->current if to->current >= 0 and at 0 if to->current == -1.  
     Updates to->current.
     
     0 <= i < from.n && from.chunks[i].bytes > 0
     */

void ss_free_block( semispace_t *ss, int i );
  /* Free the memory associated with chunk `i' in semispace `ss'.
     
     ss->current remains unchanged unless that would leave it to point
     to a chunk with nonzero bytes, in which case it is decremented
     until it points to a chunk with nonzero bytes, or to chunk -1.
     
     0 <= i < ss.n && ss.chunks[i].bytes > 0
     */

void ss_reset( semispace_t *ss );
  /* Reset the semispace:
       For each chunk c, set c.top = c.bot
       Set ss->used = 0 and ss->current = 0 or -1 as appropriate.
     */

void ss_prune( semispace_t *ss );
  /* Prune the semispace: 
       Free all chunk data except for chunk 0.
       Reset the semispace.
     */

void ss_free_unused_chunks( semispace_t *ss );
  /* Free all unused chunks past ss->current. 
     */

void ss_shrinkwrap( semispace_t *ss );
  /* Shrinkwrap the semispace: 
       Free all unused chunks past ss->current.
       Let c = ss->chunks[ss->current]
         Set c.lim = c.top (modulo roundup)
         Free memory between c.top and the old c.lim
     */

void ss_sync( semispace_t *ss );
  /* Compute the accurate value of ss->used.
     */

void ss_set_gen_no( semispace_t *ss, int gen_no );
  /* Set the generation number for all pages in the semispace `ss' 
     to `gen_no'.  This only changes the ss structure and the descriptor
     tables -- no heap memory is moved or otherwise changed.

     gen_no >= 0
     */

void* ss_enumerate( semispace_t *ss, 
                    void *(*visitor)( word *addr, int tag, void *accum ), 
                    void *accum_init );
  /* Invokes visitor on each object allocated in the semispace.
     This might include objects unreachable from the roots.
     */

bool ss_is_address_mapped( semispace_t *ss, word *addr, bool noisy );
  /* Returns true iff 'addr' is an object in 'ss'.
     */

#endif  /* INCLUDED_SEMISPACE_T_H */

/* eof */
