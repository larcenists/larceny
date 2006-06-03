/* Copyright 1998 Lars T Hansen.
 *
 * $Id: syscall2.c 2543 2005-07-20 21:54:03Z pnkfelix $
 *
 * IAssassin Larceny -- private syscalls.
 */

#include "larceny.h"

void larceny_segment_code_address( word id, word number )
{
  panic_exit( "Syscall `segment_code_address' not available in IAssassin Larceny." );
}

/* eof */
