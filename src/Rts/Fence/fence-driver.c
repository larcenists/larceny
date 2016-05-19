/* Copyright 1998 Lars T Hansen.
 *
 * Larceny -- run-time support for the Fence/Cant framework.
 */

#include "larceny.h"
#include <assert.h>
#include "millicode.h"
#include "gclib.h"

/* Cache flushing.
 *
 * On ARM instruction cache flushing is performed by interacting with
 * CP15 using privileged instructions, so we need to make a kernel
 * call.
 */

#if defined ARM && defined LINUX
/* It seems that libc exposes the necessary system call poorly, so
 * bypass libc for now by making the system call directly.
 *
 * This code may be brittle but we won't know until we try porting
 * it to non-Android platforms.
 */
#if 0
/* This is correct for Android - cacheflush is the libc call - but it
   makes no difference whether we use this or the more general syscall.
   For gcc on Linux there's an intrinsic, __clear_cache, that can be
   used as well. */
static void
arm_linux_icache_flush(unsigned char* begin, unsigned char *end)
{
  cacheflush((int)begin, (int)end, 0);
}
#else
static void __attribute__((noinline))
arm_linux_icache_flush(unsigned char* begin, unsigned char *end)
{       
  register const unsigned char *r0 asm("r0") = begin;
  register const unsigned char *r1 asm("r1") = end;
  register const int r2 asm("r2") = 0;
  register const int r7 asm("r7") = 0xf0002;
  asm volatile ("svc 0x0" :: "r" (r0), "r" (r1), "r" (r2), "r" (r7));
}
#endif
#endif /* ARM && LINUX */

extern word globals[];          /* FIXME: Should not need this */

void cache_setup( void )
{
#if defined ARM
  globals[G_CACHE_FLUSH] = 1;
#endif
}

int flushes = 0;

void mem_icache_flush( void *lo, void *limit )
{
#if defined ARM
#if defined LINUX
  flushes++;
  arm_linux_icache_flush((unsigned char*)lo, (unsigned char*)limit);
#else
  #error "ARM requires an OS-dependent icache flushing mechanism"
#endif
#endif
}

extern void fence_dispatch_loop_return();
extern void fence_scheme_jump(word *globals);
extern void fence_stack_underflow();

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
    annoyingmsg( "Recursive call to scheme_start (FFI?)" );
  already_running = 1;

  dispatch_jump_buffer = gclib_alloc_rts(sizeof(jmp_buf), 0);
  if (dispatch_jump_buffer == NULL)
    panic_abort("Couldn't allocate fresh jmp_buf");

  /* Return address for bottom-most frame */
  stkp[ STK_RETADDR ] = (word)fence_dispatch_loop_return;
  stkp[ STK_REG0 ] = 0;

  /* Here we do not flush the icache across the heap addresses because
   * none of the heap memory will be in the icache.  (That's a
   * questionable assumption if Larceny runs as a subsystem of a
   * larger system, or as part of a process that can start and
   * shutdown Larceny several times, but those are not use cases.)
   */

  /* We use longjump for handling system exit and SIGFPE (integer
   * divide by zero).  See code above to handle recursive invocation
   * of the Scheme engine.
   *
   * In general it would be better not to rely on SIGFPE but to
   * explicitly check for zero during integer division.
   */

  switch (x = setjmp( *dispatch_jump_buffer )) {
  case 0 :
  case DISPATCH_CALL_R0 :
    assert2( tagof( globals[ G_REG0 ]) == PROC_TAG );
    globals[G_RETADDR] = 0;     /* Start at the beginning of the code vector */
    fence_scheme_jump(globals); /* Never returns */  
    break;
  case DISPATCH_EXIT:
    already_running = 0;
    gclib_free(dispatch_jump_buffer, sizeof(jmp_buf));
    dispatch_jump_buffer = old_jump_buffer;
    return;
  case DISPATCH_SIGFPE :
    handle_sigfpe( globals );   /* Invokes the Scheme exception handler, should never return */
    panic_exit( "handle_sigfpe() returned." );
  default :
    panic_exit( "Unexpected value %d from setjmp in scheme_start()", x );
  }
}

void stk_initialize_underflow_frame( word *stkp )
{
  /* Double check that the function is aligned to 4-byte boundary, so
   * that it will be treated as a tagged-fixnum. */
  assert( (((int)fence_stack_underflow) & 3) == 0);
  stkp[ STK_RETADDR ] = (word)fence_stack_underflow;
  stkp[ STK_REG0 ] = 0;
}

