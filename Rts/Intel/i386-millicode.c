/* Copyright 2001 Lars T Hansen
 *
 * Intel i386 family millicode entry points and millicode jump vector
 * initialization.
 *
 * $Id$
 *
 * The jump vector contains addresses of millicode procedures; a call
 * to a millicode procedure is an indirect call:
 *	CALL [GLOBALS+MC_...]
 * The procedure i386_millicode() should be called once at startup;
 * it patches the millicode entry points into the globals vector.
 *
 * Note that since a CALL is used, there must be room on the Scheme
 * stack for one word of return address.  On entry, the value of CONT 
 * is four bytes below the value of CONT that is in the globals vector.
 */

#include "millicode.h"
#include "globals.ah"

/* Register assignments.  EAX is used as a temp, and to pass SECOND;
 * it is never a root for garbage collection, so SECOND must be flushed
 * to memory by millicode.
 */
#define SECOND   eax
#define RESULT   ebx
#define REG1     ecx
#define REG2     edx
#define REG3     edi
#define REG4     esi
#define GLOBALS  ebp
#define CONT     esp

/* Rational names, to be used in the future */
#define M_BYTEVECTOR_LIKE_COMPARE   M_BVLCMP
#define M_ENABLE_INTERRUPTS         M_EINTR
#define M_DISABLE_INTERRUPTS        M_DINTR
#define M_FULL_BARRIER              M_ADDTRANS
#define G_SECOND                    G_ARGREG2
#define G_THIRD                     G_ARGREG3

/* These are missing right now.  FIXME! */
#define M_ABS                       0
#define M_SCHEME_CALLOUT            0
#define M_BYTEVECTOR_LIKE_FILL      0
#define G_SAVED_EBP                 0
#define G_SAVED_ESP                 0

/* Pop the return address off the Scheme stack and save it; then
 * jump to code that switches to the C context, calls the named 
 * C procedure with the globals vector as an argument, and on return
 * from C switches the context back and returns to Scheme code.
 */

#define CALLC( NAME ) \
    __asm pop   eax \
    __asm mov   [GLOBALS+G_RETADDR], eax \
    __asm mov   eax, NAME \
    __asm jmp   offset callout_to_C

/* As above, but save eax into SECOND first. */

#define CALLC2( NAME ) \
    __asm mov   [GLOBALS+G_SECOND], eax \
    __asm pop   eax \
    __asm mov   [GLOBALS+G_RETADDR], eax \
    __asm mov   eax, offset NAME \
    __asm jmp   callout_to_C

extern word larceny_globals[];

/* FIXME: CodeWarrior dies horribly on this, though according to the
 * CW documentation it's fine.
 *
 * FIXME: go thru and use CALLC and CALLC2 as appropriate.
 */
void i386_millicode( word *globals )
{
  __asm {
    jmp   initialize

  L1000:
  i386_alloc_bv:
    CALLC( mc_alloc_bv )
  i386_alloc:
    CALLC( mc_alloc )
  i386_alloci:
    CALLC2( mc_alloci )
  i386_stack_overflow:
    CALLC( mc_stack_overflow )
  i386_capture_continuation:
    CALLC( mc_capture_continuation )
  i386_restore_continuation:
    CALLC( mc_restore_continuation )
  i386_full_barrier:
    CALLC2( mc_full_barrier )
  i386_break:
    CALLC( mc_break )
  i386_timer_exception:
    CALLC( mc_timer_exception )
  i386_enable_interrupts:
    CALLC( mc_enable_interrupts )
  i386_disable_interrupts:
    CALLC( mc_disable_interrupts )
  i386_exception:
    CALLC2( mc_exception )
  i386_apply:
    CALLC2( mc_apply )
  i386_restargs:
    CALLC2( mc_restargs )
  i386_syscall:
    CALLC2( mc_syscall )
  i386_typetag:
    CALLC( mc_typetag )
  i386_typetag_set:
    CALLC2( mc_typetag_set )
  i386_eqv:
    CALLC2( mc_eqv )
  i386_partial_list2vector:
    CALLC( mc_partial_list2vector )
  i386_bytevector_like_fill:
    CALLC( mc_bytevector_like_fill )
  i386_bytevector_like_compare:
    CALLC( mc_bytevector_like_compare )
  i386_scheme_callout:
    CALLC( mc_scheme_callout )
  i386_add:
    CALLC( mc_add )
  i386_sub:
    CALLC( mc_sub )
  i386_mul:
    CALLC( mc_mul )
  i386_div:
    CALLC( mc_div )
  i386_quo:
    CALLC( mc_quo )
  i386_rem:
    CALLC( mc_rem )
  i386_neg:
    CALLC( mc_neg )
  i386_abs:
    CALLC( mc_abs )
  i386_equalp:
    CALLC( mc_equalp )
  i386_lessp:
    CALLC( mc_lessp )
  i386_less_or_equalp:
    CALLC( mc_less_or_equalp )
  i386_greaterp:
    CALLC( mc_greaterp )
  i386_greater_or_equalp:
    CALLC( mc_greater_or_equalp )
  i386_exact2inexact:
    CALLC( mc_exact2inexact )
  i386_inexact2exact:
    CALLC( mc_inexact2exact )
  i386_real_part:
    CALLC( mc_real_part )
  i386_imag_part:
    CALLC( mc_imag_part )
  i386_round:
    CALLC( mc_round )
  i386_truncate:
    CALLC( mc_truncate )
  i386_zerop:
    CALLC( mc_zerop )
  i386_complexp:
    CALLC( mc_complexp )
  i386_rationalp:
    CALLC( mc_rationalp )
  i386_integerp:
    CALLC( mc_integerp )
  i386_exactp:
    CALLC( mc_exactp )
  i386_inexactp:
    CALLC( mc_inexactp )

  callout_to_C:
    /* On entry, eax contains the address of the C procedure to call,
     * and the scheme return address has been saved in G_RETADDR.
     * The machine is in Scheme mode.  Save state, call the C
     * procedure, then restore the state and return.
     */
    mov    [GLOBALS + G_CONT], CONT
    mov    [GLOBALS + G_RESULT], RESULT
    mov	   [GLOBALS + G_REG1], REG1
    mov	   [GLOBALS + G_REG2], REG2
    mov    [GLOBALS + G_REG3], REG3
    mov	   [GLOBALS + G_REG4], REG4

    mov	   ebx, GLOBALS
    mov	   esp, [GLOBALS + G_SAVED_ESP]
    mov	   ebp, [GLOBALS + G_SAVED_EBP]
    /* FIXME: are these the conventions we want? */
    push   [ebx + G_THIRD]
    push   [ebx + G_SECOND]
    push   [ebx + G_RESULT]
    call   eax

    mov	   GLOBALS, offset larceny_globals
    mov	   CONT, [GLOBALS + G_CONT]
    mov	   RESULT, [GLOBALS + G_RESULT]
    mov	   REG1, [GLOBALS + G_REG1]
    mov	   REG2, [GLOBALS + G_REG2]
    mov	   REG3, [GLOBALS + G_REG3]
    mov	   REG4, [GLOBALS + G_REG4]
    jmp    [GLOBALS + G_RETADDR]

  initialize:
    mov    ebx, globals
    mov    eax, offset i386_alloc_bv
    mov    [ebx+M_ALLOC_BV], eax
    mov    eax, offset i386_alloc
    mov    [ebx+M_ALLOC], eax
    mov    eax, offset i386_alloci
    mov    [ebx+M_ALLOCI], eax
    mov    eax, offset i386_stack_overflow
    mov    [ebx+M_STKOFLOW], eax
    mov    eax, offset i386_capture_continuation
    mov    [ebx+M_CREG], eax
    mov    eax, offset i386_restore_continuation
    mov    [ebx+M_CREG_SET], eax
    mov    eax, offset i386_full_barrier
    mov    [ebx+M_FULL_BARRIER], eax
    mov    eax, offset i386_break
    mov    [ebx+M_BREAK], eax
    mov    eax, offset i386_timer_exception
    mov    [ebx+M_TIMER_EXCEPTION], eax
    mov    eax, offset i386_enable_interrupts
    mov    [ebx+M_ENABLE_INTERRUPTS], eax
    mov    eax, offset i386_disable_interrupts
    mov    [ebx+M_DISABLE_INTERRUPTS], eax
    mov    eax, offset i386_exception
    mov    [ebx+M_EXCEPTION], eax
    mov    eax, offset i386_apply
    mov    [ebx+M_APPLY], eax
    mov    eax, offset i386_restargs
    mov    [ebx+M_VARARGS], eax
    mov    eax, offset i386_syscall
    mov    [ebx+M_SYSCALL], eax
    mov    eax, offset i386_typetag
    mov    [ebx+M_TYPETAG], eax
    mov    eax, offset i386_typetag_set
    mov    [ebx+M_TYPETAGSET], eax
    mov    eax, offset i386_eqv
    mov    [ebx+M_EQV], eax
    mov    eax, offset i386_partial_list2vector
    mov    [ebx+M_PARTIAL_LIST2VECTOR], eax
    mov    eax, offset i386_bytevector_like_fill
    mov    [ebx+M_BYTEVECTOR_LIKE_FILL], eax
    mov    eax, offset i386_bytevector_like_compare
    mov    [ebx+M_BYTEVECTOR_LIKE_COMPARE], eax
    mov    eax, offset i386_scheme_callout
    mov    [ebx+M_SCHEME_CALLOUT], eax
    mov    eax, offset i386_add
    mov    [ebx+M_ADD], eax
    mov    eax, offset i386_sub
    mov    [ebx+M_SUB], eax
    mov    eax, offset i386_mul
    mov    [ebx+M_MUL], eax
    mov    eax, offset i386_div
    mov    [ebx+M_DIV], eax
    mov    eax, offset i386_quo
    mov    [ebx+M_QUOT], eax
    mov    eax, offset i386_rem
    mov    [ebx+M_REM], eax
    mov    eax, offset i386_neg
    mov    [ebx+M_NEG], eax
    mov    eax, offset i386_abs
    mov    [ebx+M_ABS], eax
    mov    eax, offset i386_equalp
    mov    [ebx+M_NUMEQ], eax
    mov    eax, offset i386_lessp
    mov    [ebx+M_NUMLT], eax
    mov    eax, offset i386_less_or_equalp
    mov    [ebx+M_NUMLE], eax
    mov    eax, offset i386_greaterp
    mov    [ebx+M_NUMGT], eax
    mov    eax, offset i386_greater_or_equalp
    mov    [ebx+M_NUMGE], eax
    mov    eax, offset i386_exact2inexact
    mov    [ebx+M_EXACT2INEXACT], eax
    mov    eax, offset i386_inexact2exact
    mov    [ebx+M_INEXACT2EXACT], eax
    mov    eax, offset i386_real_part
    mov    [ebx+M_REAL_PART], eax
    mov    eax, offset i386_imag_part
    mov    [ebx+M_IMAG_PART], eax
    mov    eax, offset i386_round
    mov    [ebx+M_ROUND], eax
    mov    eax, offset i386_truncate
    mov    [ebx+M_TRUNCATE], eax
    mov    eax, offset i386_zerop
    mov    [ebx+M_ZEROP], eax
    mov    eax, offset i386_complexp
    mov    [ebx+M_COMPLEXP], eax
    mov    eax, offset i386_rationalp
    mov    [ebx+M_RATIONALP], eax
    mov    eax, offset i386_integerp
    mov    [ebx+M_INTEGERP], eax
    mov    eax, offset i386_exactp
    mov    [ebx+M_EXACTP], eax
    mov    eax, offset i386_inexactp
    mov    [ebx+M_INEXACTP], eax
    /* Dummy code to prevent CodeWarrior from optimizing out
       the millicode! */
    cmp    ebx, 0
    jne    L1001
    jmp    L1000
L1001:
  }
}
