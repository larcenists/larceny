! Scheme Run-time System
! Sparc version.
! Scheme initialization file.
!
! $Id$
!
! The procedure _schemestart is called from the C-language initialization
! code. _schemestart sets up the virtual machine by initializing %GLOBALS
! and then fetching values from the globals table into the VM registers.
! Finally, it calls the application specific startup procedure 'S_ENTRY'.
! If 'S_ENTRY' returns, then _schemestart returns to its caller.
! 
! Assemble with '-P' flag

#include "memory.s.h"

	.globl _schemestart
	.text

_schemestart:
	save	%sp, -96, %sp			! Standard stack frame

	set	_globals, %GLOBALS

	ld	[ %GLOBALS+E_TOP_OFFSET ], %E_TOP
	ld	[ %GLOBALS+E_LIMIT_OFFSET ], %E_LIMIT

	ld	[ %GLOBALS+STK_MAX_OFFSET ], %SP
	ld	[ %GLOBALS+INITIAL_TIMER_OFFSET ], %TIMER

	! All rootable registers must be initialized

	mov	%g0, %RESULT
	mov	%g0, %ARGREG2
	mov	%g0, %ARGREG3

	ld	[ %GLOBALS+R0_OFFSET ], %R0
	ld	[ %GLOBALS+R1_OFFSET ], %R1
	ld	[ %GLOBALS+R2_OFFSET ], %R2
	ld	[ %GLOBALS+R3_OFFSET ], %R3
	ld	[ %GLOBALS+R4_OFFSET ], %R4
	ld	[ %GLOBALS+R5_OFFSET ], %R5
	ld	[ %GLOBALS+R6_OFFSET ], %R6
	ld	[ %GLOBALS+R7_OFFSET ], %R7

	! Initialize temporaries just because.

	mov	%g0, %TMP0
	mov	%g0, %TMP1
	mov	%g0, %TMP2
	mov	%g0, %TMP3
	mov	%g0, %TMP4
	mov	%g0, %TMP5
	mov	%g0, %TMP6

! Call application code: setup a minimal continuation and jump. The
! continuation contains a dummy procedure field; the stack flusher
! had better deal with this.

	set	L1, %TMP0
	st	%TMP0, [ %SP-12 ]	! return address
	mov	16, %TMP1
	st	%TMP1, [ %SP-8 ]	! size
	st	%g0, [ %SP-4 ]		! procedure (dummy!)
	st	%g0, [ %SP ]		! padding
	sub	%SP, 16, %SP		! allocate frame

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
	ret
	restore

	! end of file
