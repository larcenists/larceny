! Memory management test program
! Sparc assembly language procedures
!
! $Id$
!
! All public procedures here are called from C.
	
	.global	_test_alloc_2, _test_alloci_2, _test_gcstart_2
	.global _test_stkoflow_2

	.seg "text"

! Test the alloc() millicode procedure.
! Takes an argument which is the number of words to allocate.
! Returns untagged pointer to allocated space.
!
! We load %REG0 with a dummy '0' procedure (see doc'n; this is a special
! case procedure that is not clobbered by collection), then allocate,
! and then return the allocated pointer.
!
! This procedure is possibly reentrant, but I have to convince myself...
! Anyway it's a good example of C calling Scheme.

_test_alloc_2:

	!---------
	! C-to-Scheme Prologue
	save	%sp, -72, %sp
	st	%i0, [ %fp+8 ]			! save argument
	st	%i7, [ %sp+4 ]			! save return address

	! Initialize virtual machine from "globals" array

	call	_restore_scheme_context
	nop
	! End prologue
	!---------

	mov	0, %REG0			! fake procedure
	ld	[ %MILLICODE + M_ALLOC ], %TMP0
	jmpl	%TMP0, %o7
	ld	[ %fp+8 ], %RESULT		! argument

	!---------
	! C-to-Scheme Epilogue
	! Save virtual machine state in "globals" array

	call	_save_scheme_context
	nop

	! Get ready to return to C

	mov	%RESULT, %i0			! return value
	ld	[ %sp+4 ], %i7			! restore return address
	ret
	restore
	! End epilogue
	!---------


_test_alloci_2:
	retl
	nop


_test_gcstart_2:
	retl
	nop


_test_stkoflow_2:
	retl
	nop


	
