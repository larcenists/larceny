! Scheme Run-time System
! Sparc version.
! Scheme initialization file.
!
! $Id: schemestart.s,v 1.5 91/07/12 03:14:26 lth Exp Locker: lth $
!
! The procedure _schemestart is called from the C-language initialization
! code. _schemestart sets up the virtual machine and then calls the
! application specific startup procedure in the globals slot 'SCHEME_ENTRY'.
! If that procedure returns, then _schemestart returns to its caller.
! 
! If the C startup wishes to pass arguments to SCHEME_ENTRY, it should 
! intialize the appropriate register save areas in globals[], as we avoid
! touching the registers here. (If there are no arguments, the startup
! must set %RESULT to 0, at least).
!
! Assemble with '-P' flag

#define ASSEMBLY
#include "registers.s.h"
#include "layouts.s.h"
#include "offsets.h"
#include "millicode.h"

	.global	_schemestart

	.seg	"text"

_schemestart:
	save	%sp, -96, %sp			! Standard stack frame
	st	%i7, [ %sp + 0x44 ]

	call	_restore_scheme_context
	nop

! Call application code: setup a minimal continuation and jump. The
! continuation contains a dummy procedure field; the stack flusher
! had better deal with this.

	set	L1-8, %TMP0		! this is ok since this code won't move
	sub	%STKP, 16, %STKP	! allocate frame
	st	%TMP0, [ %STKP ]	! return address
	mov	12, %TMP1
	st	%TMP1, [ %STKP+4 ]	! size
	st	%g0, [ %STKP+8 ]	! procedure (dummy!)
L2:
	ld	[ %GLOBALS + SCHEME_ENTRY_OFFSET ], %REG0
	and	%REG0, TAGMASK, %TMP0
	cmp	%TMP0, PROC_TAG
	beq	L0
	nop
	jmpl	%MILLICODE + M_PROC_EXCEPTION, %o7
	add	%o7, (L2-(.-4))-8, %o7
L0:
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	jmp	%TMP0 + A_CODEOFFSET
	nop

	! Return to C code
L1:
	call	_save_scheme_context
	nop

	ld	[ %sp + 0x44 ], %i7
	ret
	restore

	! end of file
