/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Petit Larceny -- private syscalls.
 */

#include "larceny.h"
#include "twobit.h"

extern cont_t *twobit_load_table[];

void larceny_segment_code_address( word id, word number )
{
  word x = (word)twobit_load_table[nativeuint(id)][nativeuint(number)];

  if (is_fixnum( x ))
    globals[ G_RESULT ] = x;
  else
    panic_abort( "Table lookup failed in segment_code_address!" );
}

/* eof */
