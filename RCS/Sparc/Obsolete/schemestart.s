! Scheme Run-time System
! Sparc version.
! Scheme initialization file.
!
! $Id: schemestart.s,v 1.4 91/06/24 01:56:29 lth Exp Locker: lth $
!
! The procedure _schemestart is called from the C-language initialization
! code. _schemestart sets up the virtual machine and then calls the
! application specific startup procedure 'S_ENTRY'. If 'S_ENTRY' returns,
! then _schemestart returns to its caller.
! 
! If the C startup wishes to pass arguments to S_ENTRY, it should intialize
! the appropriate register save areas in globals[].
!
! Assemble with '-P' flag

#include "registers.s.h"

	.global	_schemestart

	.seg	"text"

_schemestart:
	save	%sp, -96, %sp			! Standard stack frame
	st	%i7, [ %fp + 0x44 ]

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

! Do the call. We simply jump to an application-specific entry procedure
! S_ENTRY which must be present in one of the files of the Scheme application.
! The procedure S_ENTRY must setup the global variables and call the
! entry point of the application; both tasks are application specific
! to some degree.

	set	S_ENTRY, %TMP0
	jmp	%TMP0
	nop

	! Return to C code
L1:
	call	_save_scheme_context
	nop

	ld	[ %fp + 0x44 ], %i7
	ret
	restore

	! end of file
