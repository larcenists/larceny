/* Rts/Sys/signals.c
 * Larceny Run-time system -- Unix signal handling.
 *
 * $Id: signals.c,v 1.1 1997/08/25 13:07:31 lth Exp $
 */

#include <signal.h>
#include <setjmp.h>
#include "signals.h"
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

static void inthandler( int, int, struct sigcontext *, char * );
static void fpehandler( int, int, struct sigcontext *, char * );

#if !OLD_SIGNAL_HANDLER
jmp_buf syscall_interrupt_buf;
int     in_interruptible_syscall = 0;
int     in_noninterruptible_syscall = 0;
int     syscall_mask = 0;
int     syscall_synch_error = 0;
#endif

void setup_signal_handlers( void )
{
  signal( SIGINT, inthandler );
  signal( SIGFPE, fpehandler );
}

#if OLD_SIGNAL_HANDLER
static void inthandler( int sig, int code, struct sigcontext *scp, char *addr )
{
  /* Can't go to localdebugger because the registers are not saved. */

  hardconsolemsg( "Caught signal -- exiting.\n" );
  exit( 1 );
}
#else
static void inthandler( int sig, int code, struct sigcontext *scp, char *addr )
{
  globals[ G_SIGNAL ] = 1;
  globals[ G_SIGINT ] = 1;

  if (!in_interruptible_syscall) {
    /* In Scheme code or non-interruptible syscall -- so return,
     * and wait for the flag to be discovered.  If the timer has 
     * some massive value, this can take quite a while; we need to be 
     * smarter, but this is a temporary solution to allow keyboard 
     * interrupts to work at all.
     */
    return;
  }

  /* Otherwise, we are in an interruptible syscall, so interrupt it by
   * longjumping back to the callout point, where cleanup will take
   * place.  Again, this is not the right thing, but it's OK now.
   */
  longjmp( syscall_interrupt_buf, ASYNCHRONOUS_ERROR );
}
#endif

#if OLD_FPE_HANDLER
static void fpehandler( int sig, int code, struct sigcontext *scp, char *addr )
{
  hardconsolemsg( "Arithmetic exception (SIGFPE).\n" );
  exit( 1 );
}

#else

static void fpehandler( int sig, int code, struct sigcontext *scp, char *addr )
{
  void m_fpe_handler();

  globals[ G_FPE_CODE ] = fixnum( code );

  if (in_interruptible_syscall) {
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

    scp->sc_pc = (int)m_fpe_handler;
    scp->sc_npc = (int)m_fpe_handler + 4;

    return;
  }
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
