/* Rts/Sys/signals.c
 * Larceny Run-time system -- Unix signal handling.
 *
 * $Id: signals.c,v 1.2 1997/09/23 19:57:44 lth Exp lth $
 */

#include <signal.h>
#include <setjmp.h>
#if defined(SOLARIS)
#include <ucontext.h>
#endif

#include "signals.h"
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

#if defined(SUNOS)
static void inthandler( int, int, struct sigcontext *, char * );
static void fpehandler( int, int, struct sigcontext *, char * );
#endif

#if defined(SOLARIS)
static void inthandler( int, siginfo_t *, void * );
static void fpehandler( int, siginfo_t *, void * );
#endif

jmp_buf syscall_interrupt_buf;
int     in_interruptible_syscall = 0;
int     in_noninterruptible_syscall = 0;
int     syscall_mask = 0;
int     syscall_synch_error = 0;

void setup_signal_handlers( void )
{
#if defined(SUNOS)
  signal( SIGINT, inthandler );
  signal( SIGFPE, fpehandler );
#endif

#if defined(SOLARIS)
  struct sigaction act;

  act.sa_handler = 0;
  act.sa_sigaction = inthandler;
  act.sa_flags = SA_SIGINFO | SA_RESTART;
  sigfillset( &act.sa_mask );
  sigaction( SIGINT, &act, (struct sigaction*)0 );
  act.sa_sigaction = fpehandler;
  sigaction( SIGFPE, &act, (struct sigaction*)0 );
#endif
}

#if defined(SUNOS)
static void inthandler( int sig, int code, struct sigcontext *c, char *a )
#endif
#if defined(SOLARIS)
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
#if defined(SOLARIS)
  setup_signal_handlers();  /* Re-enable signal before longjmp on Solaris */
#endif
  longjmp( syscall_interrupt_buf, ASYNCHRONOUS_ERROR );
}


#if defined(SUNOS)
static void fpehandler( int sig, int code, struct sigcontext *scp, char *addr )
#endif
#if defined(SOLARIS)
static void fpehandler( int sig, siginfo_t *siginfo, void *context )
#endif
{
  void m_fpe_handler();
#if defined(SOLARIS)
  int code = siginfo->si_code;
#endif

  globals[ G_FPE_CODE ] = fixnum( code );

  if (in_interruptible_syscall) {
#if defined(SOLARIS)
    setup_signal_handlers();  /* Re-enable signal before longjmp on Solaris */
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
    /* Assumption: error happened in compiled code or millicode.
     *
     * SPARC specific code ahead!
     * Setup a return address to millicode exception code.
     */
#if defined(SUNOS)
    scp->sc_pc = (int)m_fpe_handler;
    scp->sc_npc = (int)m_fpe_handler + 4;

    return;
#endif

#if defined(SOLARIS)
    ucontext_t *ucontext = (ucontext_t*)context;

    ucontext->uc_mcontext.gregs[ REG_PC ] = (greg_t)m_fpe_handler;
    ucontext->uc_mcontext.gregs[ REG_nPC ] = (greg_t)m_fpe_handler + 4;

    return;
#endif
  }
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
