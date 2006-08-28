/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * SPARC low-level signal-handling code.
 *
 * execute_sigfpe_magic() sets the return address in the exception
 * structure to point to the millicode exception handler, then returns
 * (and counts on the handler to return to the OS signal code for the
 * jump to the exception handler).
 */

#include "config.h"

#include <signal.h>
#include <setjmp.h>
#if defined(SUNOS5)
#include <ucontext.h>
#endif

#include "larceny.h"
#include "signals.h"

extern void m_fpe_handler();

#if defined(SUNOS4)
void execute_sigfpe_magic( void *p )
{
  struct sigcontext *scp = (struct sigcontext *)p;

  scp->sc_pc = (int)m_fpe_handler;
  scp->sc_npc = (int)m_fpe_handler + 4;

  return;
}
#endif

#if defined(DEBIAN_SPARC)
void execute_sigfpe_magic( void *p )
{
  struct sigcontext *scp = (struct sigcontext *)p;

  scp->sigc_pc = (int)m_fpe_handler;
  scp->sigc_npc = (int)m_fpe_handler + 4;

  return;
}
#endif

#if defined(SUNOS5)
void execute_sigfpe_magic( void *p )
{
  ucontext_t *ucontext = (ucontext_t *)p;

  ucontext->uc_mcontext.gregs[ REG_PC ] = (greg_t)m_fpe_handler;
  ucontext->uc_mcontext.gregs[ REG_nPC ] = (greg_t)m_fpe_handler + 4;

  return;
}
#endif
