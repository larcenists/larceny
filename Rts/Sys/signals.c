/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny Run-time system -- signal handling.
 */

#include <signal.h>
#include <setjmp.h>

#include "config.h"
#include "larceny.h"
#include "signals.h"

/* Signal implementation.
   
   See the comment at the head of signals.h for a general explanation
   of signal handling strategy.

   Asynchronous exceptions.

   Currently only SIGINT is handled.  

   The signal delivery is recorded in two variables in the virtual
   machine context, G_SIGNAL and G_SIGINT, both of which are set to 1 by
   the handler.  The handler returns by either a normal return or by a
   longjump, depending on the machine state.

   One generalization of this mechanism renames G_SIGINT as G_SIGNAL;
   when a signal is delivered, the signal number is stored in G_SIGNAL.
   After one signal has been delivered, other signals are blocked until
   the first has been handled by Scheme code.  Blocking can be done by
   operating system facilities or by keeping an internal queue of
   delivered signals.

   Synchronous exceptions.

   Currently only SIGFPE is handled, but SIGSEGV, SIGBUS, and SIGILL
   should be handled also; they can be handled using the same mechanism.

   The native implementation needs to manipulate the signal context to
   recover from a synchronous signal encountered while executing
   compiled code or millicode.  Thus, for the native implementations, we
   use a signal interface that provide access to the signal context.
   That interface is OS-dependent.

   The context is manipulated such that when the signal handler returns,
   the next instruction executed is the first instruction of the
   millicode procedure m_fpe_handler (in Sparc/mcode.s, q.v.).  That
   procedure, which executes in a virtual machine context identical to
   that of the exception except for the value of the program counter,
   can save the entire exception state and then invoke the Scheme
   exception handler.

   It is difficult for the native implementation to use a more portable
   method (for example a longjump), as the part of the virtual machine
   state kept in registers at the time of the exception would be lost,
   notably the Scheme stack pointer, REG0, RESULT, and SECOND.  (The
   original instruction pointer is currently not preserved but this is
   not hard to fix.) If we're willing to accept the cost of storing
   these registers in the virtual machine context before every operation
   that might signal a SIGFPE, then a more portable mechanism can be
   used.  That might not generalize to other synchronous exceptions, but
   their contexts are relatively less important.

   The procedure that manipulates the context is the inappropriately
   named `execute_sigfpe_magic()', in $MACHINE/signals.c (usually).

   The portable implementation does not need the context, and its 
   implementation of execute_sigfpe_magic() simply calls longjmp(),
   which works fine because none of the virtual machine state is in
   registers at the time of an exception 
   */

#if defined(SUNOS4)
int sigsetmask( int );		/* Should be in <signal.h> but isn't */
#endif

#if defined(BSD_SIGNALS)
  static void inthandler( int, int, struct sigcontext *, char * );
  static void fpehandler( int, int, struct sigcontext *, char * );
#elif defined(XOPEN_SIGNALS)
  static void inthandler( int, siginfo_t *, void * );
  static void fpehandler( int, siginfo_t *, void * );
#elif defined(POSIX_SIGNALS)
  static void inthandler( int );
  static void fpehandler( int );
#elif defined(STDC_SIGNALS)
  static void inthandler( int );
  static void fpehandler( int );
#else
# error "No signal handler could be selected for chosen feature set."
#endif

signal_set_t syscall_blocked_signals;
jmp_buf      syscall_interrupt_buf;
int          in_interruptible_syscall = 0;
int          in_noninterruptible_syscall = 0;
int          syscall_synch_error = 0;

void setup_signal_handlers( void )
{
#if defined(BSD_SIGNALS)
  signal( SIGINT, inthandler );	/* FIXME: Should use sigvec */
  signal( SIGFPE, fpehandler );	/* FIXME: Should use sigvec */
# if defined(DEBIAN_SPARC)
  signal( SIGILL, fpehandler ); /* FIXME: Should use sigvec */
# endif
#elif defined(XOPEN_SIGNALS)
  struct sigaction act;

  act.sa_handler = 0;
  act.sa_flags = SA_SIGINFO | SA_RESTART;
  sigfillset( &act.sa_mask );

  act.sa_sigaction = inthandler;
  sigaction( SIGINT, &act, (struct sigaction*)0 );

  act.sa_sigaction = fpehandler;
  sigaction( SIGFPE, &act, (struct sigaction*)0 );
# if defined(DEBIAN_SPARC)
  /* Integer division by zero is sometimes(?) signalled as a SIGILL with 
     si_code=ILL_OPN, so install that here too. */
  sigaction( SIGILL, &act, (struct sigaction*)0 );
# endif
#elif defined(POSIX_SIGNALS)
  struct sigaction act;

  act.sa_handler = 0;
  act.sa_flags = SA_RESTART;
  sigfillset( &act.sa_mask );

  act.sa_handler = inthandler;
  sigaction( SIGINT, &act, (struct sigaction*)0 );

  act.sa_handler = fpehandler;
  sigaction( SIGFPE, &act, (struct sigaction*)0 );
#elif defined(STDC_SIGNALS)
  signal( SIGINT, inthandler );
  signal( SIGFPE, fpehandler );
#else
# error "No signal handler setup."
#endif
}

/* Asynchronous signal -- only SIGINT for now. */
#if defined(BSD_SIGNALS)
static void inthandler( int sig, int code, struct sigcontext *c, char *a )
#elif defined(XOPEN_SIGNALS)
static void inthandler( int sig, siginfo_t *siginfo, void *context )
#elif defined(POSIX_SIGNALS)
static void inthandler( int sig )
#elif defined(STDC_SIGNALS)
static void inthandler( int sig )
#else
# error "No definition of inthandler."
#endif
{
#if defined(STDC_SIGNALS)
  signal( sig, inthandler );
#endif
  /* Record the event */
  globals[ G_SIGNAL ] = 1;
  globals[ G_SIGINT ] = 1;

  if (!in_interruptible_syscall) {
    /* Scheme code, system code, or non-interruptible syscall.  Return and
       wait for the flag to be discovered.
       */
    return;
  }

#if !NO_SYNCHRONOUS_SIGNALS
  /* Interruptible syscall.  Interrupt it by longjumping back to the
     callout point, where cleanup will take place. 
     */
  longjmp( syscall_interrupt_buf, ASYNCHRONOUS_ERROR );
#endif
}


/* Synchronous signal -- only SIGFPE for now. */
#if defined(BSD_SIGNALS)
static void fpehandler( int sig, int code, struct sigcontext *scp, char *addr )
#elif defined(XOPEN_SIGNALS)
static void fpehandler( int sig, siginfo_t *siginfo, void *context )
#elif defined(POSIX_SIGNALS)
static void fpehandler( int sig )
#elif defined(STDC_SIGNALS)
static void fpehandler( int sig )
#else
# error "No definition of fpehandler."
#endif
{
#if defined(BSD_SIGNALS)
  void *ctx = (void*)scp;
#elif defined(XOPEN_SIGNALS)
  void *ctx = context;
  int code = siginfo->si_code;
#elif defined(PETIT_LARCENY)
  void *ctx = (void*)0;
  int code = 0;
#else
# error "No context/code variables."
#endif

#if defined(STDC_SIGNALS)
  signal( sig, fpehandler );
#endif

  globals[ G_FPE_CODE ] = fixnum( code );
    
  if (in_interruptible_syscall) {
    /* Interrupt the call and return to the callout point, where
       cleanup will take place.
       */
    longjmp( syscall_interrupt_buf, SYNCHRONOUS_ERROR );
  }
  else if (in_noninterruptible_syscall) {
    /* This is really an error */
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
    /* Error happened in compiled code or millicode.  The call to
       execute_sigfpe_magic may or may not return, so never place
       code that must be executed following the call.
       */
    execute_sigfpe_magic( ctx );
    return;
  }
}

void block_all_signals( signal_set_t *s )  /* s may be NULL */
{
#if defined(BSD_SIGNALS)
  if (s != 0)
    *s = sigsetmask( -1 );
  else
    sigsetmask( -1 );
#elif defined(POSIX_SIGNALS) || defined(XOPEN_SIGNALS)
  sigset_t t;

  sigfillset( &t );
  sigprocmask( SIG_SETMASK, &t, s );
#elif defined(STDC_SIGNALS)
  /* Can't block anything! */
#else
# error "No implementation for block_all_signals."
#endif
}

void unblock_signals( signal_set_t *s )
{
#if defined(BSD_SIGNALS)
  sigsetmask( *s );
#elif defined(POSIX_SIGNALS) || defined(XOPEN_SIGNALS)
  sigprocmask( SIG_SETMASK, s, (sigset_t*)0 );
#elif defined(STDC_SIGNALS)
  /* Can't unblock anything! */
#else
# error "No implementation for unblock_signals."
#endif
}

/* Used by the portable implementation to restore the signal mask
   before a longjump out of a signal handler.
   */
void unblock_all_signals( void )
{
#if defined(BSD_SIGNALS)
  sigsetmask( 0 );
#elif defined(POSIX_SIGNALS) || defined(XOPEN_SIGNALS)
  sigset_t s;

  sigemptyset( &s );
  sigprocmask( SIG_SETMASK, &s, (sigset_t*)0 );
#elif defined(STDC_SIGNALS)
  /* Can't unblock all! */
#else
# error "No implementation for unblock_all_signals."
#endif
}

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
