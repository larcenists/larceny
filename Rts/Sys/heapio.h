/* Rts/Sys/heapio.h
 * Larceny run-time system -- heap I/O header file.
 *
 * $Id: heapio.h,v 1.1.1.1 1998/11/19 21:51:46 lth Exp $
 */

#ifndef INCLUDED_HEAPIO_H
#define INCLUDED_HEAPIO_H

#include "larceny-types.h"
#include "cdefs.h"
#include <stdio.h>

typedef struct hio_tbl hio_tbl;

struct heapio_t {
  /* Public fields for single and split heaps */
  int     text_size;            /* Size (words) of text */
  int     data_size;            /* Size (words) of data */
  int     type;                 /* Type code */

  /* Private data */
  FILE    *fp;
  word    magic;                /* Header word */
  word    roots[ LAST_ROOT-FIRST_ROOT+1 ];
  bool    split_heap;           /* 1 if the heap has a static area */
  bool    bootstrap_heap;       /* 1 if single or split heap */
  bool    input;                /* 1 if open for input */
  bool    output;               /* 1 if open for output */
  word    *globals;
  hio_tbl *text_segments; 
  hio_tbl *data_segments;
};

#define HEAPIO_OK             0
#define HEAPIO_WRONGTYPE     -1          /* wrong heap type */
#define HEAPIO_WRONGVERSION  -2          /* wrong heap version */
#define HEAPIO_CANTREAD      -3          /* read error */
#define HEAPIO_CANTOPEN      -4          /* open error */
#define HEAPIO_NOTOPEN       -5          /* heap not open */
#define HEAPIO_CANTWRITE     -6          /* write error */
#define HEAPIO_CODEMATCH     -7          /* unmatched heap code during load */
#define HEAPIO_CANTCLOSE     -8          /* close error */

/* Heap type values.  These are not random. */
#define HEAP_SINGLE          0
#define HEAP_SPLIT           1
#define HEAP_DUMPED          2

/* Codes for hio_dump_segment. */

#define TEXT_SEGMENT         0
#define DATA_SEGMENT         1


extern heapio_t *create_heapio( void );
  /* Create a heapio_t structure.
     */

extern int hio_open( heapio_t *h, const char *filename );
  /* Open the heap image file and read metadata into the heapio structure.

     Returns 0 on success or a negative error code on failure.
     */

extern int hio_create( heapio_t *h, const char *filename, int type );
  /* Create a heap image file for the given type of heap.

     Returns 0 on success or a negative error code on failure.
     */

extern int hio_close( heapio_t *h );
  /* Close the heap image file and deallocate the structure.

     Returns 0 on success or a negative error code on failure.
     */

extern int hio_dump_initiate( heapio_t *h, word *globals );
  /* Start a dump.

     Returns 0 on success or a negative error code on failure.
     */

extern int hio_dump_segment( heapio_t *h, int type, word *bot, word *top );
  /* Set up a dump of the array of words between bot (inclusive) and top
     (exclusive) to the file represented by 'h'.  The memory may not be
     dumped at this time, and should not be altered until after a subsequent
     call to hio_dump_commit().

     Returns 0 on success or a negative error code on failure.
     */

extern int hio_dump_commit( heapio_t *h );
  /* Finish the dump.

     Returns 0 on success or a negative error code on failure.
     */

extern int
hio_load_bootstrap( heapio_t *h, word *text, word *data, word *globals );
  /* If h is an open heap of type HEAP_SPLIT or HEAP_SINGLE, then load the
     heap image into the text and data areas.

     Returns 0 on success or a negative error code on failure.
     */

#if 0
extern int
hio_dump_bootstrap( heapio_t *h, semispace_t *text, semispace_t *data, 
		   word *globals );
  /* For backward compatibility */
#endif


#endif /* ifndef INCLUDED_HEAPIO_H */

/* eof */
