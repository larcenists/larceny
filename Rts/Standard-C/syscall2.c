/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Petit Larceny -- private syscalls.
 */

#include "larceny.h"
#include "twobit.h"

extern cont_t *twobit_load_table[];

void larceny_segment_code_address( word w_id, word w_number )
{
  word x = (word)twobit_load_table[nativeuint(w_id)][nativeuint(w_number)];

#if 0
  if (is_fixnum( x ))
    globals[ G_RESULT ] = x;
  else
    panic_abort( "Table lookup failed in segment_code_address: unaligned code pointer!" );
#else
  /* It actually does not matter if the pointer is not aligned, since
     the GC does not inspect the object pointed to -- it is outside the
     heap. */
  globals[ G_RESULT ] = x;
#endif
}

/* eof */
