/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * SPARC Larceny -- private syscalls.
 */

#include "larceny.h"

void larceny_segment_code_address( word id, word number )
{
  panic( "Syscall `segment_code_address' not available in SPARC Larceny." );
}

/* eof */
