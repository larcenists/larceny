! Scheme Run-time System
! Startup File; Sparc version
!
! $Id$
!
! We call the C-language initialization code, do whatever initialization
! which cannot be done in C, and then call the application.
!
! This file should typically be the first file argument to 'ld'. 
! Assemble with '-P' flag

#include "memory.s.h"

	.text

_start:
	!#PROLOGUE# 0
	!#PROLOGUE# 1

	! Do C-language initialization

	mov	%i0, %o0	! argc
	mov	%i1, %o1	! argv
	mov	%i2, %o2	! envp
	call	_C_init
	nop
	restore

	! Do other initialization

	set	_globals, %GLOBALS		! setup globals pointer

	! Memory space registers

	ld	[ %GLOBALS+E_TOP_OFFSET ], %E_TOP
	ld	[ %GLOBALS+E_LIMIT_OFFSET ], %E_LIMIT
	
	! Stack cache

	ld	[ %GLOBALS+SP_OFFSET ], %SP	! setup stack pointer
	...					! initialize stack bottom

	! Others

	ld	[ %GLOBALS+R0_OFFSET ], %R0
	ld	[ %GLOBALS+R1_OFFSET ], %R1
	ld	[ %GLOBALS+R2_OFFSET ], %R2
	ld	[ %GLOBALS+R3_OFFSET ], %R3
	ld	[ %GLOBALS+R4_OFFSET ], %R4
	ld	[ %GLOBALS+R5_OFFSET ], %R5
	ld	[ %GLOBALS+R6_OFFSET ], %R6
	ld	[ %GLOBALS+R7_OFFSET ], %R7

	! Call application

	call	Smain
	nop

	! Exit if application returned

	mov	0, %o1
	call	_exit

	! end of file
