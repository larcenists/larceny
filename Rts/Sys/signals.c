/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny Run-time system -- Unix signal handling.
 */

#include "config.h"

#include <signal.h>
#include <setjmp.h>
#if defined(SUNOS5)
#include <ucontext.h>
#endif

#include "larceny.h"
#include "signals.h"

#if defined(SUNOS4)
static void inthandler( int, int, struct sigcontext *, char * );
static void fpehandler( int, int, struct sigcontext *, char * );
int syscall_mask = 0;
#endif

#if defined(SUNOS5)
static void inthandler( int, siginfo_t *, void * );
static void fpehandler( int, siginfo_t *, void * );
sigset_t syscall_blocked_signals;
#endif

jmp_buf syscall_interrupt_buf;
int     in_interruptible_syscall = 0;
int     in_noninterruptible_syscall = 0;
int     syscall_synch_error = 0;

void setup_signal_handlers( void )
{
#if defined(SUNOS4)
  signal( SIGINT, inthandler );
  signal( SIGFPE, fpehandler );
#endif

#if defined(SUNOS5)
  struct sigaction act;

  act.sa_handler = 0;
  act.sa_flags = SA_SIGINFO | SA_RESTART;
  sigfillset( &act.sa_mask );

  act.sa_sigaction = inthandler;
  sigaction( SIGINT, &act, (struct sigaction*)0 );

  act.sa_sigaction = fpehandler;
  sigaction( SIGFPE, &act, (struct sigaction*)0 );
#endif
}

#if defined(SUNOS4)
static void inthandler( int sig, int code, struct sigcontext *c, char *a )
#endif
#if defined(SUNOS5)
static void inthandler( int sig, siginfo_t *siginfo, void *context )
#endif
{
  globals[ G_SIGNAL ] = 1;
  globals[ G_SIGINT ] = 1;

  if (!in_interruptible_syscall) {
    /* In Scheme code or non-interruptible syscall -- so return,
     * and wait for the flag to be discovered.
     */
    return;
  }

  /* Otherwise, we are in an interruptible syscall, so interrupt it by
   * longjumping back to the callout point, where cleanup will take
   * place.  Again, this is not the right thing, but it's OK now.
   */
#if defined(SUNOS5)
  unblock_all_signals();	/* Before longjmp */
#endif
  longjmp( syscall_interrupt_buf, ASYNCHRONOUS_ERROR );
}


#if defined(SUNOS4)
static void fpehandler( int sig, int code, struct sigcontext *scp, char *addr )
#endif
#if defined(SUNOS5)
static void fpehandler( int sig, siginfo_t *siginfo, void *context )
#endif
{
#if defined(SUNOS5)
  int code = siginfo->si_code;
#endif

  globals[ G_FPE_CODE ] = fixnum( code );

  if (in_interruptible_syscall) {
#if defined(SUNOS5)
    unblock_all_signals();	/* Before longjmp */
#endif
    longjmp( syscall_interrupt_buf, SYNCHRONOUS_ERROR );
  }
  else if (in_noninterruptible_syscall) {
    /* Install a null signal handler, then retry, and handle the error
     * in the callout.  This is not ideal -- when we return, other code
     * may be executed that depends on the problem.  That is not much
     * of an issue in the current version of Larceny, but it must
     * be corrected eventually.  FIXME.
     */
    syscall_synch_error = 1;
    signal( SIGFPE, SIG_IGN );
    return;
  }
  else {
    /* Assumption: error happened in compiled code or millicode. */
#if defined(SUNOS4)
    execute_sigfpe_magic( (void*)scp );
    /* DO NOT PUT CODE HERE */
    return;
#endif

#if defined(SUNOS5)
    execute_sigfpe_magic( context );
    /* DO NOT PUT CODE HERE */
    return;
#endif
  }
}

#if defined(SUNOS5)
void block_all_signals( sigset_t *s )  /* s may be NULL */
{
  sigset_t t;

  sigfillset( &t );
  sigprocmask( SIG_SETMASK, &t, s );
}

void unblock_signals( sigset_t *s )
{
  sigprocmask( SIG_SETMASK, s, (sigset_t*)0 );
}

void unblock_all_signals( void )
{
  sigset_t s;

  sigemptyset( &s );
  sigprocmask( SIG_SETMASK, &s, (sigset_t*)0 );
}
#endif

/* ---------------------------------------------------------------------- */


#if 0
/* This code also works, but the world is not ready for it */
static void inthandler( int sig, int code, struct sigcontext *scp, char *addr )
{
  word g;

  g = globals[ G_SIGNALS ];     /* not the same as G_SIGNAL */
  if (g != FALSE_CONST)		/* make sure it's installed */
    vector_set(gcell_ref(g), sig, vector_ref(gcell_ref(g), sig)+fixnum( 1 ));
}
#endif


/* eof */
