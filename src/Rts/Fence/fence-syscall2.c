/* Copyright 1998 Lars T Hansen.
 *
 * Larceny -- private syscalls on Fence/Cant
 */

#include "larceny.h"

void larceny_segment_code_address( word id, word number )
{
  panic_exit( "Syscall `segment_code_address' not available in Fence/Cant Larceny." );
}

/* eof */
