/* Rts/Sys/semispace.h
 * Larceny run-time system -- semispace ADT
 *
 * $Id: semispace.h,v 1.1 1997/05/23 13:50:06 lth Exp $
 *
 * A semispace is a set of memory chunks of the same generation. The set
 * can grow and shrink as necessary; typically, it is grown by the gclib
 * when the semispace overflows during gc or promotion, and shrunk by
 * the heap under direction of policy.
 */

#ifndef INCLUDED_SEMISPACE_H
#define INCLUDED_SEMISPACE_H

typedef struct semispace semispace_t;
typedef struct chunk chunk_t;

semispace_t *create_semispace( unsigned bytes, int heap_no, int gen_no );

struct chunk {
  unsigned bytes;      /* # of bytes allocated; 0 => pointers are garbage */
  word *bot;           /* Pointer to first word */
  word *top;           /* Pointer to next free word */
  word *lim;           /* Pointer past last free word */
};

struct semispace {
  int      heap_no;    /* Heap identifier */
  int      gen_no;     /* Generation identifier */
  unsigned allocated;  /* Total allocated bytes in semispace */
  unsigned used;       /* Total used bytes in semispace */
  int      current;    /* Index of current chunk (may be -1 briefly) */
  unsigned n;          /* Length of chunk array */
  chunk_t  *chunks;    /* Array of chunks */
};


void ss_expand( semispace_t *ss, unsigned bytes_needed );
void ss_reset( semispace_t *ss );
void ss_prune( semispace_t *ss );
void ss_shrinkwrap( semispace_t *s );
void ss_sync( semispace_t *ss );
void ss_free( semispace_t *ss );

#endif  /* INCLUDED_SEMISPACE_H */

/* eof */
