! Scheme 313 runtime system
! Global table for millicode calls.
!
! $Id: tables.s,v 1.1 91/08/21 14:42:41 lth Exp Locker: lth $

	.global	_millicode

	.seg	"text"

! Each element in the millicode table is two instructions. The first 
! instruction is an unconditional branch to the real millicode routine. The
! second instruction is the delay slot for that branch. That slot can be
! filled with the first instruction from the millicode routine if the branch
! is updated to point to the second instruction of the routine.
!
! The filling must be done by some initializer routine; although possible to
! do it by hand, it seems a waste of effort. That routine must add 1 to the
! branch instruction to get the new target right.

_millicode:
	! #0: M_STKOFLOW
	b	_stkoflow
	nop
	! #1: M_STKUFLOW is weird. It is *not* a branch instruction, rather,
	! the first word is an unimplemented instruction and the second word
	! is the address of the stack underflow routine - 8. This value is
	! put in the dummy stack frame at the bottom of the stack.
	unimp	0
	.word	_stkuflow - 8
	! #2: M_ALLOC
	b	_alloc
	nop
	! #3: M_ALLOCI
	b	_alloci
	nop
	! #4: M_SETCAR
	b	_setcar
	nop
	! #5: M_SETCDR
	b	_setcdr
	nop
	! #6: M_VECTORSET
	b	_vectorset
	nop
	! #7: M_GCSTART
	b	_gcstart
	nop
	! #8: M_SAVE_CONTEXT
	b	_save_scheme_context
	nop
	! #9: M_RESTORE_CONTEXT
	b	_restore_scheme_context
	nop
	! #10: M_TYPE_EXCEPTION
	b	_type_exception
	nop
	! #11: M_ZEROP
	unimp	0
	nop
	! #12: M_ADD
	unimp	0
	nop
	! #13: M_SUB
	unimp	0
	nop
	! #14: M_MUL
	unimp	0
	nop
	! #15: M_QUOT
	unimp	0
	nop
	! #16: M_REM
	unimp	0
	nop
	! #17: M_DIV
	unimp	0
	nop
	! #18: M_NEG
	unimp	0
	nop
	! #19: M_CAPTURE
	b	_capture_continuation
	nop
	! #20: M_RESTORE
	b	_restore_continuation
	nop
	! #21: M_TIMER_EXCEPTION
	b	_timer_exception
	nop
	! #22: M_PROC_EXCEPTION
	b	_proc_exception
	nop
	! #23: M_ARG_EXCEPTION
	b	_arg_exception
	nop
	! #24: M_VARARGS
	unimp	0
	nop
	! #25: M_APPLY
	unimp	0
	nop
	! #26: M_NUMEQ
	unimp	0
	nop
	! #27: M_NUMLT
	unimp	0
	nop
	! #28: M_NUMLE
	unimp	0
	nop
	! #29: M_NUMGT
	unimp	0
	nop
	! #30: M_NUMGE
	unimp	0
	nop
	! #31: M_GARBAGE_COLLECT
	b	_garbage_collect
	nop
	! #32: M_OPEN_FILE
	b	_open_file
	nop
	! #33: M_CLOSE_FILE
	b	_close_file
	nop
	! #34: M_CREATE_FILE
	b	_create_file
	nop
	! #35: M_UNLINK_FILE
	b	_unlink_file
	nop
	! #36: M_READ_FILE
	b	_read_file
	nop
	! #37: M_WRITE_FILE
	b	_write_file
	nop

	! end of millicode table

	
