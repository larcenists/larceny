/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * x86-nasm Larceny -- run-time support for Petit Larceny framework.
 */

#define NOGLOBALS

#include <assert.h>
#include "larceny.h"
#include "millicode.h"

extern void i386_dispatch_loop_return();

void scheme_init( word *globals )
{
  initialize_generic_arithmetic();
}

void scheme_start( word *globals )
{
  cont_t f = 0;
  word *stkp = (word*)globals[ G_STKP ];
  int x;

  if (already_running)
    panic_abort( "Recursive call to scheme_start (FFI?)" );
  already_running = 1;

  /* Patch in bootstrap code if necessary */
  if (procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE ) == FALSE_CONST)
    procedure_set( globals[ G_REG0 ], IDX_PROC_CODE, (word)twobit_start );

  /* Return address for bottom-most frame */
  stkp[ STK_RETADDR ] = (word)i386_dispatch_loop_return;
  stkp[ STK_REG0 ] = 0;

  /* The dispatch loop is a doubly-nested quasi-loop.  

     The outer loop uses setjmp/longjmp for control and is entered but 
     rarely; most of the time is spent in the inner loop.  The job of
     the outer loop is to provide the inner loop with the address of
     the first block to execute.

     The inner loop is implemented entirely in compiled code: we just
     jump to the entry point, and any return is done through a longjmp
     to the outer loop.
     */

  /* Outer loop */
  switch (x = setjmp( dispatch_jump_buffer )) {
  case 0 :
  case DISPATCH_CALL_R0 :
    f = procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE );
    break;
  case DISPATCH_EXIT:
    already_running = 0;
    return;
  case DISPATCH_RETURN_FROM_S2S_CALL :
    f = restore_context( globals );
    break;
  case DISPATCH_STKUFLOW :
    f = refill_stack_cache( globals );
    break;
  case DISPATCH_SIGFPE :
    handle_sigfpe( globals );
    panic_exit( "handle_sigfpe() returned." );
  default :
    panic_exit( "Unexpected value %d from setjmp in scheme_start()", x );
  }

  /* Inner loop */
  i386_scheme_jump(globals,f);   /* Never returns */  
}

extern void i386_stack_underflow();

void stk_initialize_underflow_frame( word *stkp )
{
  stkp[ STK_RETADDR ] = (word)i386_stack_underflow;
  stkp[ STK_REG0 ] = 0;
}

