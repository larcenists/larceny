/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny Run-time system -- signal handling.
 *
 * Signals are either synchronous (SIGFPE, SIGBUS, SIGSEGV, SIGILL) or 
 * asynchronous (SIGINT, SIGQUIT, etc).  Synchronous signals must be 
 * handled as they occur, whereas asynchronous signals can be handled
 * when it is convenient to do so.
 *
 * A signal can occur when the virtual machine is in one of three modes:
 *  - In compiled code or millicode.
 *  - In a non-interruptible syscall or in system code (eg, GC).  For
 *    the benefit of signal handling, system code callouts are flagged
 *    as noninterruptible syscalls.
 *  - In an interruptible syscall, including user-written foreign functions.
 *
 * Thus we have six cases to deal with.
 *
 * (1) Synchronous signals in compiled code or millicode: since we are in
 *     control over the code that is executing, it is straightforward to
 *     guarantee that the synchronous signals of interest either do not
 *     occur or occur at well-defined places (eg SIGFPE), or can be treated
 *     as an error, (eg SIGBUS, SIGSEGV).  In any event, these errors are
 *     handled by what is effectively a longjump to a handler for these
 *     types of errors (currently called execute_sigfpe_magic(), but that
 *     name should change).
 *
 * (2) Synchronous signals in non-interruptible syscall or in system code:
 *     these are errors when they occur since the only non-interruptible
 *     syscalls that exist are under our control and can be known not to
 *     create conditions that result in synchronous signals.  Ditto system
 *     code.
 *
 * (3) Synchronous signals in interruptible syscalls or foreign functions: 
 *     SIGBUS and SIGSEGV are errors and can be handled as such by aborting
 *     the syscall and signalling the error, but things like SIGFPE are more
 *     problematic because they may occur for legitimate reasons, yet it is
 *     impossible to continue execution.  It seems that a reasonable solution
 *     would be to allow for a user-installable signal handler procedure
 *     that can be called by Larceny's signal handler, and that may control
 *     execution in the syscall or foreign functions.
 *
 * (4) Asynchronous signals in compiled code or millicode: these are recorded
 *     as delivered in the virtual machine context and are handled in
 *     millicode or compiled code at the next timer expiration (or when the
 *     timer is next manipulated).  After recording the signal, the signal
 *     handler returns.
 *
 * (5) Asynchronous signals in non-interruptible syscalls: these are handled
 *     exactly as for case (4).
 *
 * (6) Asynchronous signals in interruptible syscalls or foreign functions:
 *     these are recorded as delivered in the virtual machine context and
 *     are handled in millicode or compiled code at the next timer expiration
 *     (or when the timer is next manipulated).  After recording the signal,
 *     the signal handler aborts the syscall or foreign function by a
 *     longjump to the callout point.
 *
 * The interface defined in this file should generalize to signal handling
 * systems of capability similar to that of 4.3 BSD or POSIX.  Note that
 * this does not include ANSI/ISO C, which has an extremely weak signal
 * specification.
 *
 * The implementation is scattered across Sys/signals.h, Sys/signals.c, and
 * $MACHINE/signals.c.
 */

#ifndef INCLUDED_SIGNALS_H
#define INCLUDED_SIGNALS_H

#include <signal.h>
#include <setjmp.h>

#include "config.h"

#if defined(BSD_SIGNALS)
# define signal_set_t int
#elif defined(STDC_SIGNALS) || defined(WIN32_SIGNALS)
# define signal_set_t int
#elif defined(POSIX_SIGNALS) || defined(XOPEN_SIGNALS)
# define signal_set_t sigset_t
#else
# error "Unknown signal type in signals.h"
#endif

/* These are values returned to the setjmp() below */
#define SYNCHRONOUS_ERROR    1    /* SIGFPE, SIGBUS, SIGSEGV, SIGILL */
#define ASYNCHRONOUS_ERROR   2    /* SIGINT, SIGQUIT, etc */

/* In signals.c */
extern void         block_all_signals( signal_set_t *s );
extern void         unblock_signals( signal_set_t *s );
extern void         unblock_all_signals( void );
extern jmp_buf      syscall_interrupt_buf;
extern int          in_interruptible_syscall;
extern int          in_noninterruptible_syscall;
extern signal_set_t syscall_blocked_signals;

/* In $MACHINE/signals.c */
extern void execute_sigfpe_magic( void *context ); /* misnamed */

/* This facility is not reentrant */
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

/* eof */
