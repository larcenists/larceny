! Rts/Sparc/mcode.s.
! Larceny run-time system (SPARC) -- miscellaneous primitives.
!
! $Id: mcode.s,v 1.4 1997/09/17 15:16:35 lth Exp $

#include "asmdefs.h"
#include "asmmacro.h"

	.seg	"text"

	.global EXTNAME(m_apply)	
	.global EXTNAME(m_varargs)
	.global EXTNAME(m_syscall)
	.global	EXTNAME(m_typetag)
	.global	EXTNAME(m_typetag_set)
	.global	EXTNAME(m_eqv)
	.global EXTNAME(m_partial_list2vector)
	.global	EXTNAME(m_break)
	.global EXTNAME(m_singlestep)
	.global	EXTNAME(m_timer_exception)
	.global EXTNAME(m_enable_interrupts)
	.global EXTNAME(m_disable_interrupts)
	.global EXTNAME(m_exception)
	.global EXTNAME(m_bvlcmp)
	.global EXTNAME(m_fpe_handler)


! _m_apply: millicode for the 'apply' instruction
!
! Call from: Scheme
! Inputs   : RESULT = procedure
!            ARGREG2 = list
!            ARGREG3 = fixnum: length of list in ARGREG2
! Outputs  : Unspecified.
! Destroys : RESULT, ARGREG2, ARGREG3, temporaries
!
! The caller must validate the arguments, compute the length of the
! list, decrement the timer, and fault if the timer is not 0. The
! following code simply executes an APPLY as fast as it can.
!
! Operation:
!  - Map the (head of the) list in REG1 onto registers REG1-REG30, put the
!    tail, if any, into REG31.
!  - Move RESULT to REG0, set RESULT to the length of the list, and invoke
!    the procedure in REG0.

EXTNAME(m_apply):
	mov	30, %TMP0				! counter -- 30 regs
	add	%GLOBALS, G_REG1, %TMP1			! register to store
Lapply3:
	cmp	%ARGREG2, NIL_CONST			! done yet?
	be	Lapply5					!   skip if so
	nop
	ld	[ %ARGREG2 - PAIR_TAG ], %TMP2		! t = (car l)
	st	%TMP2, [ %TMP1 ]			! set a register!
	inc	4, %TMP1				! bump ptr
	deccc	1, %TMP0				! one less reg
	bg	Lapply3					!  loop, if > 0
	ld	[ %ARGREG2 + 4 - PAIR_TAG ], %ARGREG2	! l = (cdr l)
	! store tail in R31
	st	%ARGREG2, [ %GLOBALS + G_REG31 ]	! store tail
Lapply5:
	st	%o7, [ %GLOBALS + G_RETADDR ]
	call	internal_restore_vm_regs
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7

	mov	%RESULT, %REG0
	ld	[ %REG0 + A_CODEVECTOR ], %TMP0
	jmp	%TMP0 + A_CODEOFFSET
	mov	%ARGREG3, %RESULT


! _m_varargs: The ARGS>= instruction.
!
! Call from: Scheme
! Input    : RESULT = fixnum: argument count supplied
!            ARGREG2 = fixnum: minimum argument count wanted
! Output   : Nothing
! Destroys : A VM register, temporaries
!
! Most of the operation has been punted to C code; see comments in that code
! for illumination.

EXTNAME(m_varargs):
	cmp	%RESULT, %ARGREG2
	bge	Lvararg2
	nop
	jmp	%MILLICODE + M_EXCEPTION
	mov	EX_VARGC, %TMP0
Lvararg2:
	set	EXTNAME(C_varargs), %TMP0
	b	callout_to_C
	nop
	

! _m_syscall: implementation of the syscall primitive
!
! Call from: Scheme
! Input:     RESULT has number of arguments.
!            Arguments to C code passed in registers R1-R31; the C 
!            procedure must read the memory register file to get at 
!	     RESULT and the other arguments.
! Output:    C procedure must setup RESULT.
! Destroys:  Temporaries.

EXTNAME(m_syscall):
	set	EXTNAME(C_syscall), %TMP0
	set	0, %TMP1
	set	0, %TMP2
	b	callout_to_C
	nop


! _m_typetag: extract typetag from structured non-pair object.
!
! Call from: Scheme
! Input:     RESULT = object
! Output:    RESULT = fixnum: typetag
! Destroys:  Temporaries, RESULT.

EXTNAME(m_typetag):
	and	%RESULT, 7, %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Ltypetag1
	ld	[ %RESULT - VEC_TAG ], %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltypetag1
	ld	[ %RESULT - BVEC_TAG ], %TMP0
	cmp	%TMP0, PROC_TAG
	be,a	Ltypetag1
	ld	[ %RESULT - PROC_TAG ], %TMP0
	jmp	%MILLICODE + M_EXCEPTION
	mov	EX_TYPETAG, %TMP0
Ltypetag1:
	jmp	%o7+8
	and	%TMP0, TYPETAG_MASK, %RESULT


! _m_typetag_set: set typetag of structured non-pair object.
!
! Call from: Scheme
! Input:     RESULT = object
!            ARGREG2 = fixnum: typetag
! Output:    Nothing.
! Destroys:  Temporaries.
!
! The tag must be a fixnum in the range 0-7, appropriately shifted.

EXTNAME(m_typetag_set):
	and	%RESULT, 7, %TMP0
	cmp	%TMP0, VEC_TAG
	be,a	Ltypetagset1
	xor	%RESULT, VEC_TAG, %TMP0
	cmp	%TMP0, BVEC_TAG
	be,a	Ltypetagset1
	xor	%RESULT, BVEC_TAG, %TMP0
	cmp	%TMP0, PROC_TAG
	be,a	Ltypetagset1
	xor	%RESULT, PROC_TAG, %TMP0
Ltypetagset0:
	jmp	%MILLICODE + M_EXCEPTION
	mov	EX_TYPETAGSET, %TMP0
Ltypetagset1:
	ld	[ %TMP0 ], %TMP1
	andncc	%ARGREG2, TYPETAG_MASK, %g0
	bne	Ltypetagset0
	nop
	andn	%TMP1, TYPETAG_MASK, %TMP1
	or	%TMP1, %ARGREG2, %TMP1
	jmp	%o7 + 8
	st	%TMP1, [ %TMP0 ]


! _m_eqv: the EQV? procedure
!
! Call from: Scheme
! Input:     RESULT = object
!            ARGREG2 = object
! Output:    #t or #f
! Destroys:  RESULT, Temporaries
!
! This procedure is entered only if the two arguments are not eq?.
! Note that fixnums and immediates are always eq? if they are eqv?, so we need
! only concern ourselves with larger structures here.

EXTNAME(m_eqv):
	! Do fixnums first to get them out of the way completely.
	! If operands are fixnums, then they are not eqv?.

	tsubcc	%RESULT, %ARGREG2, %g0
	bvs,a	Leqv_others
	xor	%RESULT, %ARGREG2, %TMP0	! => 0 if they are the same
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_others:
	andcc	%TMP0, TAGMASK, %g0		! get that common tag
	bne,a	Leqv_done
	mov	FALSE_CONST, %RESULT

	! Tags are equal, but addresses are not (they are not eq?). This
	! lets us get rid of all non-numeric types.

	and	%RESULT, TAGMASK, %TMP0
	cmp	%TMP0, PAIR_TAG
	be,a	Leqv_done
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, PROC_TAG
	be,a	Leqv_done
	mov	FALSE_CONST, %RESULT
	cmp	%TMP0, BVEC_TAG
	be	Leqv_bvec
	nop
	cmp	%TMP0, VEC_TAG
	be	Leqv_vec
	nop
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_bvec:
	! Bytevector-like

	ldub	[ %RESULT - BVEC_TAG + 3 ], %TMP0
	ldub	[ %ARGREG2 - BVEC_TAG + 3 ], %TMP1

	cmp	%TMP0, BIGNUM_HDR
	be,a	Leqv_bvec2
	mov	0, %TMP0
	cmp	%TMP0, FLONUM_HDR
	be,a	Leqv_bvec2
	mov	1, %TMP0
	cmp	%TMP0, COMPNUM_HDR
	be,a	Leqv_bvec2
	mov	1,%TMP0
	b	Leqv_done
	mov	FALSE_CONST, %RESULT
Leqv_bvec2:
	cmp	%TMP1, BIGNUM_HDR
	be,a	Leqv_number
	mov	0, %TMP1
	cmp	%TMP1, FLONUM_HDR
	be,a	Leqv_number
	mov	1, %TMP1
	cmp	%TMP1, COMPNUM_HDR
	be,a	Leqv_number
	mov	1, %TMP1
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_vec:
	! We know it has a vector tag here. The header tags must be the same,
	! and both must be either ratnum or rectnum.

	ldub	[ %RESULT - VEC_TAG + 3 ], %TMP0
	ldub	[ %ARGREG2 - VEC_TAG + 3 ], %TMP1

	cmp	%TMP0, %TMP1
	bne,a	Leqv_done
	mov	FALSE_CONST, %RESULT

	mov	0, %TMP1
	cmp	%TMP0, RATNUM_HDR
	be,a	Leqv_number
	mov	0, %TMP0
	cmp	%TMP0, RECTNUM_HDR
	be,a	Leqv_number
	mov	0, %TMP0
	b	Leqv_done
	mov	FALSE_CONST, %RESULT

Leqv_number:
	! Numbers. They are eqv if they are of the same exactness and they
	! test #t with `='. The exactness is encoded in TMP0 and TMP1: 0s
	! mean exact, 1s mean inexact.

	cmp	%TMP0, %TMP1
	bne,a	Leqv_done
	mov	FALSE_CONST, %RESULT

	! Same exactness. Test for equality.

	jmp	%MILLICODE + M_NUMEQ
	nop

Leqv_done:
	jmp	%o7+8
	nop


! _m_partial_list2vector: do the grunge for list->vector.
!
! Call from: Scheme
! Input    : RESULT = proper list
!            ARGREG2 = fixnum: length of list
! Output   : RESULT = vector
! Destroys : RESULT, ARGREG2, ARGREG3, temporaries
!
! list->vector is a partial primop because the Scheme implementation 
! (make the vector, bang the elements) causes a lot of unneccesary side
! effect checking in the generation-scavenging collector. It is not a full
! primop because of the harrowing details of dealing with non-lists etc.
! 
! The correctness of this code depends on the vector being allocated in
! the ephemeral space.

EXTNAME(m_partial_list2vector):
	st	%o7, [ %GLOBALS + G_RETADDR ]	! save return address
	mov	%RESULT, %ARGREG3		! save for later
	call	EXTNAME(mem_internal_alloc)
	add	%ARGREG2, 4, %RESULT		! length of vector
	ld	[ %GLOBALS + G_RETADDR ], %o7	! restore retaddr

	sll	%ARGREG2, 8, %TMP0		! length field
	or	%TMP0, VEC_HDR, %TMP0           !   for header
	st	%TMP0, [ %RESULT ]		! store vector header
	add	%RESULT, 4, %TMP0		! TMP0 = destination pointer
	mov	%ARGREG3, %TMP1			! TMP1 = list pointer
	mov	%ARGREG2, %TMP2			! TMP2 = counter (fixnum)
	b	Ll2v_1
	tst	%TMP2				! done yet?
Ll2v_2:	
	st	%ARGREG3, [ %TMP0 ]		! store in vector
	add	%TMP0, 4, %TMP0			! next element
	ld	[ %TMP1 - PAIR_TAG + 4 ], %TMP1	! get cdr
	subcc	%TMP2, 4, %TMP2			! one less
Ll2v_1:
	bne,a	Ll2v_2				! loop if not done
	ld	[ %TMP1 - PAIR_TAG ], %ARGREG3	! get car

	jmp	%o7+8
	or	%RESULT, VEC_TAG, %RESULT


! _m_bvlcmp: bytevector-like-compare
!
! Call from: Scheme
! Input:     RESULT, ARGREG2 (bytevector-like structures)
! Output:    Fixnum in RESULT
! Destroys:  TMP0, TMP1, TMP2
!
! Takes two bytevector-like structures and performs a lexicographical
! comparison, where the alphabet consists of the integer values 0..255.
! If RESULT orders less than ARGREG2, then a negative number is returned; 
! if they order the same, 0 is returned, and if ARGREG2 orders less 
! than RESULT, then a positive number is returned.
!
! For system use _only_: Does not check argument types.

EXTNAME(m_bvlcmp):
	! Need some extra registers.
	st	%REG0, [ %GLOBALS+G_REG0 ]
	st	%REG1, [ %GLOBALS+G_REG1 ]
	! Load size fields
	ld	[ %RESULT-BVEC_TAG ], %TMP0
	ld	[ %ARGREG2-BVEC_TAG ], %TMP1
	srl	%TMP0, 8, %TMP0
	srl	%TMP1, 8, %TMP1
	! Move smallest of size fields into %TMP2.
	mov	%TMP0, %TMP2
	cmp	%TMP0, %TMP1
	bgt,a	1f
	mov	%TMP1, %TMP2
1:	! Setup pointers in TMP0 and TMP1.
	add	%RESULT, 4-BVEC_TAG, %TMP0
	add	%ARGREG2, 4-BVEC_TAG, %TMP1
	! Now loop over the common segments, comparing bytes.
	! Loop can be scheduled but is already confusing.
	b	2f
	cmp	%TMP2, 0
3:	ldub	[ %TMP1 ], %REG1
	cmp	%REG0, %REG1
	bne	4f
	inc	%TMP0
	inc	%TMP1
	deccc	%TMP2
2: 	bne,a	3b
	ldub	[ %TMP0 ], %REG0

	! Got to here if the loop expired without differences; subtract
	! the size fields to get the result.
	ld	[ %RESULT-BVEC_TAG ], %TMP0
	ld	[ %ARGREG2-BVEC_TAG ], %TMP1
	srl	%TMP0, 8, %TMP0
	sll	%TMP0, 2, %TMP0		! fixnumize
	srl	%TMP1, 8, %TMP1
	sll	%TMP1, 2, %TMP1		! fixnumize
	sub	%TMP0, %TMP1, %RESULT
	ld	[ %GLOBALS+G_REG0 ], %REG0
	jmp	%o7+8
	ld	[ %GLOBALS+G_REG1 ], %REG1

4:	! Got to here if the loop expired with differences; bytes are in
	! REG0 and REG1, so a simple subtract discovers the difference.
	sll	%REG0, 2, %TMP0		! fixnumize
	sll	%REG1, 2, %TMP1		! fixnumize
	sub	%TMP0, %TMP1, %RESULT
	ld	[ %GLOBALS+G_REG0 ], %REG0
	jmp	%o7+8
	ld	[ %GLOBALS+G_REG1 ], %REG1


! _m_break: breakpoint handler.
!
! Call from: Scheme
! Input:     Nothing
! Output:    Nothing
! Destroys:  Temporaries

EXTNAME(m_break):
	ld	[ %GLOBALS + G_BREAKPT_ENABLE ], %TMP0
	cmp	%TMP0, TRUE_CONST
	be,a	Lbreak1
	nop
	jmp	%o7+8
	nop
Lbreak1:
	set	EXTNAME(C_break), %TMP0
	b	callout_to_C
	nop


! _m_singlestep: singlestep handler.
!
! Call from: Scheme
! Input:     ARGREG2 = fixnum: constant vector index
! Output:    Unspecified
! Destroys:  Temporaries
!
! The constant slot has to contain a string, and that string will usually
! be the printable representation of the MacScheme instruction to be executed 
! next.

EXTNAME(m_singlestep):
	ld	[ %GLOBALS + G_SINGLESTEP_ENABLE ], %TMP0
	cmp	%TMP0, TRUE_CONST
	be,a	Lsinglestep1
	nop
	jmp	%o7+8
	nop
Lsinglestep1:
	set	EXTNAME(C_singlestep), %TMP0
	b	callout_to_C
	mov	%ARGREG2, %TMP1


! _m_timer_exception: exception handler for timer expiration.
!
! Call from: Scheme
! Input:     Nothing
! Output:    Nothing
! Destroys:  Temporaries, ARGREG2, ARGREG3

! TMP_FUEL is the number of ticks that the system is given every time the
! timer expires; it is the amount of fuel that the timer handler gets
! to run on or the running code gets to use before another trap is taken.
! It must be > 1.
!
! TIMER_STEP is the ...

#define TMP_FUEL    1000
#define TIMER_STEP  50000

EXTNAME(m_timer_exception):
	st	%o7, [ %GLOBALS + G_RETADDR ]		! save return addr
	set	TMP_FUEL, %TIMER			! in case of interrupt
	call	internal_check_signals
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7		! restore return addr

	ld	[ %GLOBALS + G_TIMER_ENABLE ], %TMP0	! get flag
	cmp	%TMP0, TRUE_CONST			! is it set?
	be,a	handle_timer				! jump if so
	nop
	set	TMP_FUEL, %TIMER			! get to run a while
	jmp	%o7+8					! just return
	nop

	! Handle timer interrupt.
	! First check secondary timer and return quickly if there's
	! more time on it.
handle_timer:
	ld 	[ %GLOBALS + G_TIMER2 ], %TMP0
	cmp	%TMP0, 0
	bne	enable_interrupts_setup			! skip if more time
	nop
	
	! Timer really expired
	! Turn off interrupts and give the timer something to run on
	! so we won't take a detour thru here on every iteration in
	! the scheduler.
	!
	! %RESULT is preserved by the exception mechanism when the global
	! G_SCHCALL_SAVERES is set to #t.
	set	TRUE_CONST, %TMP0
	st	%TMP0, [ %GLOBALS + G_SCHCALL_SAVERES ]
	mov	FALSE_CONST, %TMP0
	st	%TMP0, [ %GLOBALS + G_TIMER_ENABLE ]	! disable interrupts
	set	TMP_FUEL, %TIMER			! get to run a while
	b	EXTNAME(m_exception)
	mov	EX_TIMER, %TMP0


! _m_enable_interrupts: enable interrupts, set countdown timer
!
! Call from: Scheme
! Input:     RESULT = positive fixnum: new countdown
! Output:    Nothing
! Destroys:  Temporaries

EXTNAME(m_enable_interrupts):
	! Check for fixnum
	andcc	%RESULT, 3, %g0
	bne,a	EXTNAME(m_exception)
	mov	EX_EINTR, %TMP0

	! Check that fixnum is > 0
	cmp	%RESULT, 0
	ble,a	EXTNAME(m_exception)
	mov	EX_EINTR, %TMP0

	! Enable interrupts
	mov	TRUE_CONST, %TMP0
	st	%TMP0, [ %GLOBALS + G_TIMER_ENABLE ]

	! Setup timer.  Timer is in native format, so convert.
	srl	%RESULT, 2, %TMP0		! convert to native

enable_interrupts_setup:
	! TMP0 has timer value as nativeint.
	set	TIMER_STEP, %TMP1		! timer step limit
	cmp	%TMP0, %TMP1
	bg	1f				! branch if value > step limit
	nop
	! value <= TIMER_STEP
	mov	%TMP0, %TIMER
	b	2f
	st	%g0, [ %GLOBALS + G_TIMER2 ]
1:	! value > TIMER_STEP
	mov	%TMP1, %TIMER
	sub	%TMP0, %TMP1, %TMP0
	st	%TMP0, [ %GLOBALS + G_TIMER2 ]
2:	
	! check signals
	st	%o7, [ %GLOBALS + G_RETADDR ]
	call	internal_check_signals
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7

	jmp	%o7+8
	nop


! _m_disable_interrupts: disable interrupts, return current timer if it
! was enabled and #f if it was not.
!
! Call from: Scheme
! Input:     Nothing
! Output:    RESULT = current value of countdown timer or #f
! Destroys:  Temporaries

EXTNAME(m_disable_interrupts):
	ld	[ %GLOBALS + G_TIMER_ENABLE ], %TMP0
	cmp	%TMP0, TRUE_CONST
	bne,a	1f
	mov	FALSE_CONST, %RESULT
	mov	FALSE_CONST, %TMP0
	st	%TMP0, [ %GLOBALS + G_TIMER_ENABLE ]
	ld	[ %GLOBALS + G_TIMER2 ], %TMP0
	add	%TIMER, %TMP0, %TMP0
	sll	%TMP0, 2, %RESULT

1:	! check signals
	st	%o7, [ %GLOBALS + G_RETADDR ]
	call	internal_check_signals
	nop
	ld	[ %GLOBALS + G_RETADDR ], %o7

	jmp	%o7+8
	nop


! _m_exception: General exception handler.
!
! Call from: Scheme
! Input:     TMP0 = fixnum: exception code.
!            RESULT, ARGREG2, ARGREG3 = objects: operands to the primitive.
! Output:    Undefined
! Destroys:  Temporaries
!
! The return address must point to the instruction which would have been
! returned to if the operation had succeeded, i.e., the exception handler
! must repair the error if the program is to continue.

EXTNAME(m_exception):
	ld	[ %GLOBALS + G_CALLOUTS ], %TMP1
	ld	[ %TMP1 - GLOBAL_CELL_TAG + CELL_VALUE_OFFSET ], %TMP1
	cmp	%TMP1, UNDEFINED_CONST
	be	Lexception
	nop
	mov	4, %TMP1
	b	internal_scheme_call
	mov	MS_EXCEPTION_HANDLER, %TMP2
	! never returns
Lexception:
	mov	%TMP0, %TMP1			! arg 1: code (fixnum)
	mov	%o7, %TMP2			! arg 2: pc of continuation
	set	EXTNAME(C_exception), %TMP0
	b	callout_to_C
	nop


! _m_fpe_handler: Glue code for arithmetic exceptions.
!
! Call from: is returned to from signal handler when the signal handler
!            determined that an arithmetic exception was taken while the
!            VM was in Scheme mode (or almost).
! Input:     nothing
! Output:    nothing
! Destroys:  temporaries
!
! The following hack deals with a peculiarity in the current implementation
! of integer division in the arithmetic millicode (generic.s):
!
! If globals[ G_IDIV_CODE ] != 0, then the exception code to signal to
! Scheme is in global[ G_IDIV_CODE ], and one _restore_ must be executed
! to bring the machine state back to Scheme mode.
!
! All of this hackery will go away as the arithmetic millicode is cleaned up
! and we switch to using the integer division instructions.

EXTNAME(m_fpe_handler):
	set	EXTNAME(globals), %g1		! %g1 == %TMP0, so this is OK
	ld	[ %g1 + G_IDIV_CODE ], %g1
	tst	%g1
	bz	1f
	nop

	! Integer division special case: code is in globals[ G_IDIV_CODE ],
	! and we must do a restore.

	restore
	ld	[ %GLOBALS + G_IDIV_CODE ], %TMP0
	b	2f
	clr	[ %GLOBALS + G_IDIV_CODE ]

	! All other cases: FPE code is in G_FPE, so raise EX_FPE and
	! hope for the best.  We really need to inspect the faulting
	! instruction here to find out what operation failed, so that
	! we can give a decent error message.  FIXME.
	!
	! In any event, the FPE code is loaded into ARGREG3 so that
	! the error handler can inspect it.

1:	ld	[ %GLOBALS + G_FPE_CODE ], %ARGREG3
	mov	EX_FPE, %TMP0

2:	b	EXTNAME(m_exception)
	clr	[ %GLOBALS + G_FPE_CODE ]	! or recursion will get you.
	
! end-of-file
