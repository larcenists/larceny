/* Rts/Sys/signals.h
 * Larceny Run-time system -- Unix signal handling.
 *
 * $Id: signals.h,v 1.1 1997/09/17 15:17:26 lth Exp $
 */

#ifndef INCLUDED_SIGNALS_H
#define INCLUDED_SIGNALS_H

#include <signal.h>
#include <setjmp.h>

#define OLD_SIGNAL_HANDLER   0    /* misc. asynch. interrupts */
#define OLD_FPE_HANDLER      0    /* arithmetic exceptions (synchronous) */

#define SYNCHRONOUS_ERROR    1    /* SIGFPE */
#define ASYNCHRONOUS_ERROR   2    /* SIGINT, etc */

#if OLD_SIGNAL_HANDLER
# define BEGIN_INTERRUPTIBLE_SYSCALL()
# define END_INTERRUPTIBLE_SYSCALL()
#else

extern jmp_buf syscall_interrupt_buf;
extern int     in_interruptible_syscall;
extern int     syscall_mask;

# define begin_critical_section()

# define BEGIN_INTERRUPTIBLE_SYSCALL() \
  do { \
    syscall_mask = sigsetmask(-1); \
    in_interruptible_syscall = 1; \
    if (setjmp( syscall_interrupt_buf ) != 0) { \
      sigsetmask( -1 ); \
      in_interruptible_syscall = 0; \
      globals[ G_RESULT ] = UNDEFINED_CONST; \
      sigsetmask( syscall_mask ); \
      return; \
    } \
    sigsetmask( syscall_mask ); \
  } while(0)

# define END_INTERRUPTIBLE_SYSCALL() \
  do { \
    sigsetmask( -1 ); \
    in_interruptible_syscall = 0; \
    sigsetmask( syscall_mask ); \
  } while(0)
#endif

#endif

/* eof */
