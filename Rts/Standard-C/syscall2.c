/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Petit Larceny -- private syscalls.
 */

#include "larceny.h"
#include "petit-instr.h"

extern cont_t *twobit_load_table[];

void larceny_segment_code_address( word w_id, word w_number )
{
  globals[G_RESULT] = 
      ENCODE_CODEPTR(twobit_load_table[nativeuint(w_id)][nativeuint(w_number)]);
}

/* eof */
