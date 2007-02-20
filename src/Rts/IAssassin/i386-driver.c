/* Copyright 1998 Lars T Hansen.
 *
 * $Id: i386-driver.c 2543 2005-07-20 21:54:03Z pnkfelix $
 *
 * x86-nasm Larceny -- run-time support for Petit Larceny framework.
 */

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
  jmp_buf *old_jump_buffer = dispatch_jump_buffer;
  if (already_running)
    consolemsg( "Recursive call to scheme_start (FFI?)" );
  already_running = 1;

  dispatch_jump_buffer = malloc(sizeof(jmp_buf));
  if (dispatch_jump_buffer == NULL)
    panic_abort("Couldn't allocate fresh jmp_buf");

#if 0
  /* Patch in bootstrap code if necessary */
  if (procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE ) == FALSE_CONST)
    procedure_set( globals[ G_REG0 ], IDX_PROC_CODE, (word)twobit_start );
#endif

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
  switch (x = setjmp( *dispatch_jump_buffer )) {
  case 0 :
  case DISPATCH_CALL_R0 :
    assert2( tagof( globals[ G_REG0 ]) == PROC_TAG );
    f = procedure_ref( globals[ G_REG0 ], IDX_PROC_CODE );
    f = (cont_t)(ptrof(f)+1); /* skip over bytevector header */
    break;
  case DISPATCH_EXIT:
    already_running = 0;
    free(dispatch_jump_buffer);
    dispatch_jump_buffer = old_jump_buffer;
    globals[ G_STKP ] -= 4; /* adjust stkp (the .cont action) */
    return;
  case DISPATCH_RETURN_FROM_S2S_CALL :
    f = restore_context( globals );
    assert2( tagof( globals[ G_REG0 ]) == PROC_TAG );
    break;
  case DISPATCH_STKUFLOW :
    f = refill_stack_cache( globals );
    globals[ G_STKP ] += 4+4*STK_RETADDR; /* The '4*' compensates for layouts.cfg oversight */
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
  /* Double check that the function is aligned to 4-byte boundary, so
   * that it will be treated as a tagged-fixnum. */
  assert( (((int)i386_stack_underflow) & 3) == 0);
  stkp[ STK_RETADDR ] = (word)i386_stack_underflow;
  stkp[ STK_REG0 ] = 0;
}

