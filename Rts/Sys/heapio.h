/* Rts/Sys/heapio.h
 * Larceny run-time system -- heap I/O header file.
 *
 * $Id: heapio.h,v 1.1 1997/09/17 15:17:26 lth Exp $
 */

#ifndef INCLUDED_HEAPIO_H
#define INCLUDED_HEAPIO_H

#include "cdefs.h"
#include <stdio.h>

typedef struct heapio_t heapio_t;
typedef struct metadata_block_t metadata_block_t;

struct heapio_t {
  int (*open)( heapio_t *h, const char *filename );
  int (*create)( heapio_t *h, const char *filename, int type );
  int (*close)( heapio_t *h );
  int (*type)( heapio_t *h );

  /* Major interfaces to loading and dumping */
  int (*load_bootstrap)( heapio_t *h, word *text, word *data, word *globals );
  int (*dump_bootstrap)( heapio_t *h, semispace_t *text, semispace_t *data,
			 word *globals );

  /* Utility functions used by the heaps' load/dump functions */
  int (*load_roots)( heapio_t *h, word *globals );
  int (*read_data)( heapio_t *h, word *start, unsigned words );
  int (*write_data)( heapio_t *h, word *start, unsigned words );
  int (*read_ptr)( heapio_t *h, word **wp );
  int (*read_word)( heapio_t *h, word *w );
  int (*write_ptr)( heapio_t *h, word *wp );
  int (*write_word)( heapio_t *h, word w );

  /* Public fields for single and split heaps */
  unsigned text_size;               /* Size (words) of text */
  unsigned data_size;               /* Size (words) of data */

  /* Public fields for dumped heaps */
  unsigned data_pages;                   /* Number of dumped pages (total) */
  metadata_block_t *metadata;            /* Chain of metadata blocks */
  word **pagetable;                      /* Used during pointer adjustment */

  /* Private data */
  FILE *fp;
  word magic;                            /* Header word */
  word roots[ LAST_ROOT-FIRST_ROOT+1 ];  /* Root load area */
  int  split_heap;                       /* 1 if the heap has a static area */
  int  bootstrap_heap;                   /* 1 if single or split heap */
  int  input;                            /* 1 if open for input */
  int  output;                           /* 1 if open for output */
};

struct metadata_block_t {
  word metadata_length;                  /* length of metadata (words) */
  word data_pages;                       /* Number of pages in data block */
  word code;                             /* heap type word */
  word *data;                            /* metadata or 0 */
  metadata_block_t *next;                /* next block on chain */
};


#define HEAPIO_OK             0
#define HEAPIO_WRONGTYPE     -1          /* wrong heap type */
#define HEAPIO_WRONGVERSION  -2          /* wrong heap version */
#define HEAPIO_CANTREAD      -3          /* read error */
#define HEAPIO_CANTOPEN      -4          /* open error */
#define HEAPIO_NOTOPEN       -5          /* heap not open */
#define HEAPIO_CANTWRITE     -6          /* write error */
#define HEAPIO_CODEMATCH     -7          /* unmatched heap code during load */

/* These are not random. */
#define HEAP_SINGLE  0
#define HEAP_SPLIT   1
#define HEAP_DUMPED  2

extern heapio_t *create_heapio( void );
extern void delete_heapio( heapio_t * );

#endif /* ifndef INCLUDED_HEAPIO_H */

/* eof */
