! Scheme 313 runtime system
! Global table for millicode calls.
!
! $Id: tables.s,v 1.6 92/03/31 12:31:36 lth Exp Locker: lth $
!
! Needs to be sorted into a more logical order, I think.

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
	b	_mem_stkoflow
	nop
	! #1: M_STKUFLOW is weird. It is *not* a branch instruction, rather,
	! the first word is an unimplemented instruction and the second word
	! is the address of the stack underflow routine - 8. This value is
	! put in the dummy stack frame at the bottom of the stack.
	unimp	0
	.word	_mem_stkuflow - 8
	! #2: M_ALLOC
	b	_mem_alloc
	nop
	! #3: M_ALLOCI
	b	_mem_alloci
	nop
	! #4: M_SETCAR
	b	_mem_setcar
	nop
	! #5: M_SETCDR
	b	_mem_setcdr
	nop
	! #6: M_VECTORSET
	b	_mem_vectorset
	nop
	! #7: M_GCSTART
	b	_mem_gcstart
	nop
	! #8: M_SAVE_CONTEXT
	b	_mem_save_scheme_context
	nop
	! #9: M_RESTORE_CONTEXT
	b	_mem_restore_scheme_context
	nop
	! #10: M_TYPE_EXCEPTION
	b	_type_exception
	nop
	! #11: M_ZEROP
	b	_m_generic_zerop
	nop
	! #12: M_ADD
	b	_m_generic_add
	nop
	! #13: M_SUB
	b	_m_generic_sub
	nop
	! #14: M_MUL
	b	_m_generic_mul
	nop
	! #15: M_QUOT
	b	_m_generic_quo
	nop
	! #16: M_REM
	b	_m_generic_rem
	nop
	! #17: M_DIV
	b	_m_generic_div
	nop
	! #18: M_NEG
	b	_m_generic_neg
	nop
	! #19: M_CAPTURE
	b	_mem_capture_continuation
	nop
	! #20: M_RESTORE
	b	_mem_restore_continuation
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
	b	_m_varargs
	nop
	! #25: M_APPLY
	b	_m_apply
	nop
	! #26: M_NUMEQ
	b	_m_generic_equalp
	nop
	! #27: M_NUMLT
	b	_m_generic_lessp
	nop
	! #28: M_NUMLE
	b	_m_generic_less_or_equalp
	nop
	! #29: M_NUMGT
	b	_m_generic_greaterp
	nop
	! #30: M_NUMGE
	b	_m_generic_greater_or_equalp
	nop
	! #31: M_GARBAGE_COLLECT
	b	_mem_garbage_collect
	nop
	! #32: M_OPEN_FILE
	b	_m_open_file
	nop
	! #33: M_CLOSE_FILE
	b	_m_close_file
	nop
	! #34: M_CREATE_FILE		/* OBSOLETE */
	unimp	0
	nop
	! #35: M_UNLINK_FILE
	b	_m_unlink_file
	nop
	! #36: M_READ_FILE
	b	_m_read_file
	nop
	! #37: M_WRITE_FILE
	b	_m_write_file
	nop
	! #38: M_MOD
	b	_m_generic_mod
	nop
	! #39: M_COMPLEXP
	b	_m_generic_complexp
	nop
	! #40: M_REALP
	b	_m_generic_realp
	nop
	! #41: M_RATIONALP
	b	_m_generic_rationalp
	nop
	! #42: M_INTEGERP
	b	_m_generic_integerp
	nop
	! #43: M_EXACTP
	b	_m_generic_exactp
	nop
	! #44: M_INEXACTP
	b	_m_generic_inexactp
	nop
	! #45: M_EXACT2INEXACT
	b	_m_generic_exact2inexact
	nop
	! #46: M_INEXACT2EXACT
	b	_m_generic_inexact2exact
	nop
	! #47: M_MAKE_RECTANGULAR
	b	_m_generic_make_rectangular
	nop
	! #48: M_REAL_PART
	b	_m_generic_real_part
	nop
	! #49: M_IMAG_PART
	b	_m_generic_imag_part
	nop
	! #50: M_SQRT
	b	_m_generic_sqrt
	nop
	! #51: M_ROUND
	b	_m_generic_round
	nop
	! #52: M_TRUNCATE
	b	_m_generic_truncate
	nop
	! #53: M_NOT_SUPPORTED
	b	_not_supported
	nop
	! #54: M_DEBUG
	b	_m_debug
	nop
	! #55: M_RESET
	b	_m_reset
	nop
	! #56: M_EXIT
	b	_m_exit
	nop
	! #57: M_BREAK
	b	_m_break
	nop
	! #58: M_TYPETAG
	b	_m_typetag
	nop
	! #59: M_TYPETAGSET
	b	_m_typetag_set
	nop
	! #60: M_EQV
	b	_m_eqv
	nop
	! #61: M_GETRUSAGE
	b	_m_getrusage
	nop
	! #62: M_RESTORE_FRAME
	b	_mem_restore_frame
	nop
	! #63: M_ARITH_EXCEPTION
	b	_arith_exception
	nop
	! #64: M_UNDEF_EXCEPTION
	b	_undef_exception
	nop
	! #65: M_SINGLESTEP
	b	_m_singlestep
	nop
	! #66: M_GENERIC_EXCEPTION
	b	_m_generic_exception
	nop

	! end of millicode table
