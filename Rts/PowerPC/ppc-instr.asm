; -*- mode: m4 -*-
;
; Instruction macros for Larceny on PowerPC (preliminary)
; Using m4 as a macro assembler frontend for the native assembler.
;
; See ppc-machine.ah for full documentation.

; Generic hair

; From GNU m4 manual, but modified to test the condition first, so it
; may execute zero times.

define(`forloop',
	`pushdef(`$1', `$2')_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_forloop',
        `ifelse($1, `$3', ,
	            `$4`'define(`$1', incr($1))_forloop(`$1', `$2', `$3', `$4')')')

; Local label management.  Use DEFLOCn to give a new value to
; a local label, then Ln to use its value.
;

; NOTE!!  The Ln symbols carry global values, a macro does not 
; get its own.  So be very careful when defining utility macros; 
; they must not use the same labels as the macros invoking them.
;
; Labels 0..4 are reserved for top level.  Labels 5..9 for utility
; macros:
;  * timer_check uses L5
;  * set{z,nz,lt,gt,le,ge} use L5
;  * branch_and_link_through_tempx uses L5

define(`LOC',10)	; Exercise for the reader: why not start at 0?  :-)
define(`L0',`xxx')
define(`L1',`xxx')
define(`L2',`xxx')
define(`L3',`xxx')
define(`L4',`xxx')
define(`L5',`xxx')
define(`L6',`xxx')
define(`L7',`xxx')
define(`L8',`xxx')
define(`L9',`xxx')
define(`DEFLOC0',
	`define(`LOC',incr(LOC)) define(`L0', `L'LOC)')
define(`DEFLOC1',
	`define(`LOC',incr(LOC)) define(`L1', `L'LOC)')
define(`DEFLOC2',
	`define(`LOC',incr(LOC)) define(`L2', `L'LOC)')
define(`DEFLOC3',
	`define(`LOC',incr(LOC)) define(`L3', `L'LOC)')
define(`DEFLOC4',
	`define(`LOC',incr(LOC)) define(`L4', `L'LOC)')
define(`DEFLOC5',
	`define(`LOC',incr(LOC)) define(`L5', `L'LOC)')
define(`DEFLOC6',
	`define(`LOC',incr(LOC)) define(`L6', `L'LOC)')
define(`DEFLOC7',
	`define(`LOC',incr(LOC)) define(`L7', `L'LOC)')
define(`DEFLOC8',
	`define(`LOC',incr(LOC)) define(`L8', `L'LOC)')
define(`DEFLOC9',
	`define(`LOC',incr(LOC)) define(`L9', `L'LOC)')

; End hair

define(`fixtag_mask',`3')
define(`tag_mask',`7')
define(`hdr_shift',`8')
define(`char_shift',`16')
define(`is_hwreg', `eval($1>=FIRST_HWREG && $1<=LAST_HWREG)')
define(`fixnum',`eval(($1)<<2)')
define(`char',`eval((($1)<<char_shift)|IMM_CHAR)')
define(`roundup4',`eval((($1)+3)&~3)')
define(`roundup8',`eval((($1)+7)&~7)')
define(`words2bytes',`eval(($1)*4)')
define(`stkslot',`eval(STK_REG0+words2bytes($1))')
define(`framesize',`eval(roundup8(wordsize+STK_OVERHEAD+words2bytes($1)))')
define(`recordedsize',`eval(STK_OVERHEAD+words2bytes($1))')
define(`t_label',`$1')
define(`reg',`REG`$1'')

; begin_codevector name
; 	Define a code vector, just raw code
	
define(`begin_codevector',
`	.text
	.align	code_align
	undefine(`Lbranch_indirect')
$1:')

; end_codevector name
;	Terminate a codevector started by begin_codevector

define(`end_codevector',
	`undefine(`Lbranch_indirect')')

; mcall
;	call millicode.  

define(`mcall',
	`addi	TEMPX, GLOBALS, $1
	branch_and_link_through_tempx')

; branch_and_link_through_tempx
;	Create a common point for jumping to the value in TEMPX, if that
;	has not already been done, and emit an instruction to jump to that
;	common point.  The purpose is to save code size, which it will in
;	all but very small procedures.  Since the jump to the common point
;	is unconditional, performance should not suffer much.

define(`branch_and_link_through_tempx',
	`ifdef(`Lbranch_indirect',
	`bl	Lbranch_indirect',
	`DEFLOC5
	define(`Lbranch_indirect',L5)
	bl	Lbranch_indirect
Lbranch_indirect:
	mtctr	TEMPX
	bcctr	20,31')')

; loadr targetreg, regno
;	Load HW register targetreg from VM register regno

define(`loadr',
`ifelse(is_hwreg($2),1,
	`or	$1, REG$2, REG$2',
	`lwz	$1, G_REG$2(GLOBALS)')')

; storer regno, sourcereg
;     store VM register regno from HW register sourcereg
;     Does not destroy sourcereg

define(`storer',
`ifelse(is_hwreg($1),1,
	`or	REG$1, $2, $2',
	`stw	$2, G_REG$2(GLOBALS)')')

; loadc hwreg, slot
; 	Load constant vector element 'slot' into hwreg

define(`loadc',
	`lwz	$1, -PROC_TAG+PROC_CONSTVECTOR(REG0)
	lwz	$1, -VEC_TAG+words2bytes(($2)+1)($1)')

; write_barrier r1 r2
;	Move values from hardware registers r1 and r2 to RESULT 
; 	and SECOND and perform a write barrier.  r1 and r2 may 
; 	be -1, in which case the value must already be in RESULT 
; 	and SECOND.

define(`write_barrier',
`ifelse($1,-1,
	`',
	`or	RESULT, REG$1, REG$1')
ifelse($2,-1,
	`',
	`or	SECOND, REG$2, REG$2')
	mcall(M_FULL_BARRIER)')

; timer_check
;	decrement timer and take interrupt if zero

define(`timer_check',
	`DEFLOC5
	addi.	TIMER, TIMER, -1
	bc	13,2,L5
	mcall(M_TIMER_EXCEPTION)
L5:')

; setz
;	set RESULT to #t if the zero flag is set, otherwise #f

define(`setz',
	`DEFLOC5
	addi	RESULT, r0, TRUE_CONST
	bc	4, 2, L5
	addi	RESULT, r0, FALSE_CONST
L5:')

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MacScheme Assembly Language instructions

define(`T_ALIGN',
	`.align	$1')

define(`T_CONT', `')
	
define(`T_LABEL',
`t_label($1):')

; CONST is broken up into two instructions depending on the type of
; the constant.  The argument to T_CONST_IMM is a bitpattern or
; symbolic constant name, to T_CONST_CONSTVECTOR it is the constant
; vector index.
	
define(`T_CONST_IMM',
	`addi	RESULT, r0, $1')

define(`T_CONST_CONSTVECTOR', 
	`loadc(RESULT, $1)')

; introduced by peephole optimization
define(`T_CONST_SETREG_IMM',
`ifelse(is_hwreg($2),1,
	`addi	REG$2, r0, $1',
	`addi	TEMP, r0, $1
	storer($2, TEMP)')')

; introduced by peephole optimization
define(`T_CONST_SETREG_CONSTVECTOR',
`ifelse(is_hwreg($2),1,
	`loadc(REG$2, $1)',
	`loadc(TEMP, $1)
	storer($2, TEMP)')')

define(`T_GLOBAL',
	`DEFLOC0
	loadc(TEMP, $1)
	lwz	RESULT, -PAIR_TAG(TEMP)
ifdef(`UNSAFE_GLOBALS', 
	`',
	`cmpi	0, 0, RESULT, UNDEFINED_CONST
	bc	5, 2, L0
	or	RESULT, TEMP, TEMP
	mcall(M_GLOBAL_EX)
L0:')')

define(`T_SETGLBL',
	`or	SECOND, RESULT, RESULT
	loadc(RESULT, $1)
	stw	SECOND, -PAIR_TAG(RESULT)
	write_barrier(-1, -1)')

define(`T_LEXICAL',
`ifelse($1,0,
	`lwz	RESULT, -PROC_TAG+PROC_REG0+words2bytes($2)(REG0)',
	`lwz	TEMP, -PROC_TAG+PROC_REG0(REG0)
forloop(`i',1,$1,
``	lwz	TEMP, -PROC_TAG+PROC_REG0(TEMP)
'')
	lwz	RESULT, -PROC_TAG+PROC_REG0+words2bytes($2)(TEMP)')')

define(`T_SETLEX',
`ifelse($1,0,
	`stw	RESULT, -PROC_TAG+PROC_REG0+words2bytes($2)(REG0)',
	`lwz	TEMP, -PROC_TAG+PROC_REG0(REG0)
forloop(`i',1,$1,
``	lwz	TEMP, -PROC_TAG+PROC_REG0(TEMP)
'')
	stw	RESULT, -PROC_TAG+PROC_REG0+words2bytes($2)(TEMP)')')
	
define(`T_STACK',
	`lwz	RESULT, stkslot($1)(CONT)')

define(`T_SETSTK',
	`stw	RESULT, stkslot($1)(CONT)')

define(`T_LOAD',
`ifelse(is_hwreg($1),1,
	`lwz	REG$1, stkslot($2)(CONT)',
	`lwz	TEMP, stkslot($2)(CONT)
	storer($1, TEMP)')')

define(`T_STORE',
`ifelse(is_hwreg($1),1,
	`stw	REG$1, stkslot($2)(CONT)',
	`loadr(TEMP, $1)
	stw	TEMP, stkslot($2)(CONT)')')

define(`T_REG',
	`loadr(RESULT, $1)')
	
define(`T_SETREG',
	`storer($1, RESULT)')

define(`T_MOVEREG',
`ifelse(is_hwreg($1),1,
	`storer($2, REG$1)',
       is_hwreg($2),1,
	`loadr(REG$2, $1)',
	`loadr(TEMP, $1)
	storer($2, TEMP)')')

define(`T_LAMBDA', ...)     ; FIXME
define(`T_LEXES', ...)      ; FIXME

define(`T_ARGSEQ',
`ifdef(`UNSAFE_CODE',
	`',
	`DEFLOC0
	cmpi	0, 0, RESULT, fixnum($1)
	bc	13, 2, L0
	mcall(M_ARGC_EX)
L0:')')

define(`T_ARGSGE',
	`addi	SECOND, r0, fixnum($1)
ifdef(`UNSAFE_CODE',
	`',
	`ifelse($1 > 0, 1,
	`DEFLOC0
	cmpi	0, 0, RESULT, SECOND
	bc	5,0,L0
	mcall(M_ARGC_EX)
L0:')')
	mcall(M_VARARGS)')

; Note the millicode for M_INVOKE_EX is used to check for timer
; exception as well, and must check the timer first.

define(`T_INVOKE', 
	`DEFLOC0 DEFLOC1
	addi.	TIMER, TIMER, -1
	bc	5,2,L1
L0:	mcall(M_INVOKE_EX)
L1:	addi	TEMP, RESULT, 8-PROC_TAG
	andi.	TEMP, TEMP, tag_mask
	bc	5, 2, L0
	storer(0, RESULT)
	addi	RESULT, r0, fixnum($1)
	lwz	TEMPX, -8+PROC_CODEVECTOR_NATIVE(TEMP)
	branch_and_link_through_tempx')

; T_SAVE0 and T_SAVE1 are introduced by peephole optimization.

; Allocate the frame but initialize only the basic slots
; and any pad words.  Leave RESULT with a non-pointer value at
; the end; this fact is used by T_SAVE1 below to initialize
; a stack slot with non-garbage that is also safe-for-space.
; (We could clear RESULT or give it NULL, at the cost of one
; static instruction.)

define(`T_SAVE0',
	`DEFLOC0 DEFLOC1
L0:	addi	CONT, CONT, -framesize($1)
	cmpl	CONT, HEAP
	bc	5, 0, L1
	addi	CONT, CONT, framesize($1)
	mcall(M_STKOFLOW)
	b	L0
L1:	addi	RESULT, r0, recordedsize($1)
	stw	RESULT, 0(CONT)
	stw	RESULT, STK_RETADDR (CONT)
ifelse(eval(framesize($1)-recordedsize($1)),8, ; Clear pad word at the end
	`stw	RESULT, stkslot($1+1)(CONT)')')

; Initialize one stack slot to zero
define(`T_SAVE1',
	`stw	RESULT, stkslot($1)(CONT)
')

; T_SAVE may is emitted by the assembler when peephole optimization is
; disabled, otherwise T_SAVE0 and T_SAVE1 are emitted directly.
define(`T_SAVE',
	`T_SAVE0($1)
forloop(`i',1,eval($1+1),
	`T_SAVE1(i)')')

define(`T_SETRTN',
	`DEFLOC0
	bc	20,31,$+4
L0:	mflr	TEMP
	addi	TEMP, TEMP, t_label($1)-L0
	stw	TEMP, STK_RETADDR(CONT)')

define(`T_RESTORE',
`forloop(`i',0,eval($1+1),
`ifelse(is_hwreg(i),1,
	`lwz	REG`i', stkslot(i)(CONT)
',
	`lwz	TEMP, stkslot(i)(CONT)
	stw	TEMP, G_REG`i'(GLOBALS)
')')')

define(`T_POP',
	`addi	CONT, CONT, framesize($1)')
	
define(`T_POPSTK',
	`error -- T_POPSTK not implemented -- students only')

define(`T_RETURN',
	`lwz	TEMP, STK_RETADDR(CONT)
	mtlr	TEMP
	bclr	20,31')

define(`T_APPLY',	
	`timer_check
	loadr(THIRD, $2)
	loadr(SECOND, $1)
	mcall(M_APPLY)
	lwz	TEMP, -PROC_TAG+PROC_CODEVECTOR_NATIVE(REG0)
	mtctr	TEMP
	bcctr	20,31')

define(`T_NOP', `')

; JUMP: Arguments are levels, label, and name of the code vector
; into which we are jumping (an artifact of the labelling system
; in this implementation).
	
define(`T_JUMP', ... ) ; FIXME

; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
; as appropriate to avoid timer checks.

define(`T_SKIP',
	`b	t_label($1)')

define(`T_SKIPF',
	`cmpi	0, 0, RESULT, FALSE_CONST
	bc	4, 2, t_label($1)')

define(`T_BRANCH',
	`addi	TIMER, TIMER, -1
	bc	5, 2, t_label($1)
	mcall(M_TIMER_EXCEPTION)
	b	t_label($1)')

define(`T_BRANCHF',
	`timer_check
	cmpi	0, 0, RESULT, FALSE_CONST
	bc	12, 2, t_label($1)')

define(`T_CHECK',
	`cmpi	0, 0, RESULT, FALSE_CONST
	bc	12, 2, t_label($4)')

; Call handler for noncontinuable exception with RESULT, SECOND, and
; THIRD defined.

define(`T_TRAP',
`ifelse($1,0,
	`',
	`loadr(RESULT, $1)')
ifelse($3,0,
	`',
	`loadr(THIRD, $3)')
ifelse($2,0,
	`',
	`loadr(SECOND, $2)')
	error_noncontinuable($4)')


; Primitives

; break
define(`T_OP1_1',
	`mcall(M_BREAK)')

; unspecified
define(`T_OP1_3',
	`addi	RESULT, r0, UNSPECIFIED_CONST')

; undefined
define(`T_OP1_4',
	`addi	RESULT, r0, UNDEFINED_CONST')

; eof-object
define(`T_OP1_5',
	`addi	RESULT, r0, EOF_CONST')

; enable-interrupts
define(`T_OP1_6',
	`mcall(M_ENABLE_INTERRUPTS)')

; disable-interrupts
define(`T_OP1_7',
	`mcall(M_DISABLE_INTERRUPTS)')

; typetag
define(`T_OP1_8',
	`mcall(M_TYPETAG)')

; not
define(`T_OP1_9',
	`cmpi	0, 0, RESULT, FALSE_CONST
	setz')

; null?
define(`T_OP1_10',
	`cmpi	0, 0, RESULT, NIL_CONST
	setz')

; pair?
define(`T_OP1_11',
	`single_tag_test(PAIR_TAG)
	setz')

; eof-object?
define(`T_OP1_12',
	`cmpi	0, 0, RESULT, EOF_CONST
	setz')

; port?
define(`T_OP1_13',
	`double_tag_predicate(VEC_TAG,PORT_HDR)')

; structure?
define(`T_OP1_14',
	`double_tag_predicate(VEC_TAG,STRUCT_HDR)')

; car
define(`T_OP1_15',
	`single_tag_test_ex(PAIR_TAG, EX_CAR)
	lwz	RESULT, -PAIR_TAG(RESULT)')

; cdr
define(`T_OP1_16',
	`single_tag_test_ex(PAIR_TAG, EX_CDR)
	lwz	RESULT, -PAIR_TAG+wordsize(RESULT)')

; symbol?
define(`T_OP1_17',
	`double_tag_predicate(VEC_TAG,SYMBOL_HDR)')

; number? and complex?
define(`T_OP1_18',
	`mcall(M_COMPLEXP)')

; real? and rational?
define(`T_OP1_20',
	`mcall(M_RATIONALP)')

; compnum?
define(`T_OP1_21',
	`double_tag_predicate(BVEC_TAG,COMPNUM_HDR)')

; integer?
define(`T_OP1_22',
	`DEFLOC0 DEFLOC1
	andi.	TEMPX, RESULT, fixtag_mask
	bc	5, 2, L0
	mcall(M_INTEGERP)
	b	L1
L0:	addi	RESULT, r0, TRUE_CONST
L1:')

; fixnum?
define(`T_OP1_23',
	`andi.	TEMPX, RESULT, fixtag_mask
	setz')

; flonum?
define(`T_OP1_24',
	`double_tag_predicate(BVEC_TAG,FLONUM_HDR)')

; exact?
define(`T_OP1_25',
	`mcall(M_EXACTP)')

; inexact?
define(`T_OP1_26',
	`mcall(M_INEXACTP)')

; inexact->exact
define(`T_OP1_27',
	`mcall(M_EXACT2INEXACT)')

; 
; +
define(`T_OP2_61',
	`DEFLOC0
	or	TEMP, RESULT, REG$1
	andi.	TEMP, TEMP, 3
	addo	RESULT, RESULT, REG$1
	mcrxr	1
	crandc	2,2,5
	bc	13, 2, L0
	halt
L0:')

; -
define(`T_OP2IMM_131',
	`DEFLOC0
	andi.	TEMP, RESULT, 3
	addo	RESULT, RESULT, -$1
	mcrxr	1
	crandc	2,2,5
	bc	13, 2, L0
	halt
L0:')

; <
define(`T_OP2IMM_136',
	`DEFLOC0 DEFLOC1
	andi.	TEMP, RESULT, 3
	bc	5, 2, L1
L0:	halt
L1:	cmpi	RESULT, RESULT, $1
	setlt')
