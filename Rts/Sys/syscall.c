/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny RTS -- syscall functionality.
 */

#include "larceny.h"
#include "signals.h"

typedef void (*fptr)();

/* The ordering in this table is important, see Lib/unix.sch. */

static struct {
  fptr proc;
  int  nargs;
  int  interruptible;
} syscall_table[] = { { (fptr)osdep_openfile, 3, 1 },
		      { (fptr)osdep_unlinkfile, 1, 1 },
		      { (fptr)osdep_closefile, 1, 1  },
		      { (fptr)osdep_readfile, 3, 1 },
		      { (fptr)osdep_writefile, 4, 1 },
		      { (fptr)primitive_get_stats, 1, 1 },
		      { (fptr)primitive_dumpheap, 2, 1 },
		      { (fptr)primitive_exit, 1, 0 },
		      { (fptr)osdep_mtime, 2, 1 },
		      { (fptr)osdep_access, 2, 1 },
		      { (fptr)osdep_rename, 2, 1 },
		      { (fptr)osdep_pollinput, 1, 0 },
		      { (fptr)primitive_getenv, 1, 1 },
		      { (fptr)primitive_garbage_collect, 2, 0 },
		      { (fptr)primitive_flonum_log, 2, 0 },
		      { (fptr)primitive_flonum_exp, 2, 0 },
		      { (fptr)primitive_flonum_sin, 2, 0 },
		      { (fptr)primitive_flonum_cos, 2, 0 },
		      { (fptr)primitive_flonum_tan, 2, 0 },
		      { (fptr)primitive_flonum_asin, 2, 0 },
		      { (fptr)primitive_flonum_acos, 2, 0 },
		      { (fptr)primitive_flonum_atan, 2, 0 },
		      { (fptr)primitive_flonum_atan2, 3, 0 },
		      { (fptr)primitive_flonum_sqrt, 2, 0 },
		      { (fptr)primitive_stats_dump_on, 1, 1 },
		      { (fptr)primitive_stats_dump_off, 0, 1 },
		      { (fptr)primitive_iflush, 1, 0 },
		      { (fptr)primitive_gcctl_np, 3, 0 },
		      { (fptr)primitive_block_signals, 1, 0 },
		      { (fptr)primitive_flonum_sinh, 2, 0 },
		      { (fptr)primitive_flonum_cosh, 2, 0 },
		      { (fptr)osdep_system, 1, 1 },
		      { (fptr)larceny_C_ffi_apply, 4, 1 },
		      { (fptr)larceny_C_ffi_dlopen, 1, 0 },
		      { (fptr)larceny_C_ffi_dlsym, 2, 0 },
		      { (fptr)primitive_allocate_nonmoving, 2, 0 },
		      { (fptr)primitive_object_to_address, 1, 0 },
		      { (fptr)larceny_C_ffi_getaddr, 1, 0 },
 		      { (fptr)primitive_sro, 3, 0 },
		      { (fptr)primitive_sysfeature, 1, 0 },
		      { (fptr)larceny_peek_bytes, 3, 0 },
		      { (fptr)larceny_poke_bytes, 3, 0 },
		      { (fptr)larceny_segment_code_address, 2, 0 },
		    };

void larceny_syscall( int nargs, int nproc, word *args )
{
  fptr proc;

  if (nproc < 0 || nproc >= sizeof( syscall_table )/sizeof( fptr ))
    panic( "syscall: index out of range: %d.", nproc );

  if (nargs != syscall_table[ nproc ].nargs)
    panic( "syscall: wrong number of arguments to #%d\n", nproc );

  proc = syscall_table[ nproc ].proc;

  if (syscall_table[ nproc ].interruptible)
    BEGIN_INTERRUPTIBLE_SYSCALL();
  else
    in_noninterruptible_syscall = 1;

  switch (nargs) {
    case 0 : proc(); break;
    case 1 : proc( args[0] ); break;
    case 2 : proc( args[0], args[1] ); break;
    case 3 : proc( args[0], args[1], args[2] ); break;
    case 4 : proc( args[0], args[1], args[2], args[3] ); break;
    default: panic( "syscall: Too many arguments." ); break;
  }

  if (syscall_table[ nproc ].interruptible)
    END_INTERRUPTIBLE_SYSCALL();
  else
    in_noninterruptible_syscall = 0;
}

/* eof */
