/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny Run-time system -- Unix signal handling.
 */

#ifndef INCLUDED_SIGNALS_H
#define INCLUDED_SIGNALS_H

#include <signal.h>
#include <setjmp.h>
#if defined(SUNOS5)
#include <ucontext.h>
#endif

#include "config.h"

#if defined(SUNOS4)
extern int sigsetmask( int mask );
#endif

#if defined(SUNOS5)
void block_all_signals( sigset_t *s );
void unblock_signals( sigset_t *s );
void unblock_all_signals( void );
#endif

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

#if defined(SUNOS4)
extern int syscall_mask;

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

#if defined(SUNOS5)
extern sigset_t syscall_blocked_signals;

# define BEGIN_INTERRUPTIBLE_SYSCALL() \
  do { \
    block_all_signals( &syscall_blocked_signals ); \
    in_interruptible_syscall = 1; \
    if (setjmp( syscall_interrupt_buf ) != 0) { \
      block_all_signals( 0 ); \
      in_interruptible_syscall = 0; \
      globals[ G_RESULT ] = UNDEFINED_CONST; \
      unblock_signals( &syscall_blocked_signals ); \
      return; \
    } \
    unblock_signals( &syscall_blocked_signals ); \
  } while(0)

# define END_INTERRUPTIBLE_SYSCALL() \
  do { \
    block_all_signals( 0 ); \
    in_interruptible_syscall = 0; \
    unblock_signals( &syscall_blocked_signals ); \
  } while(0)
#endif

void execute_sigfpe_magic( void *context );

#endif

/* eof */
