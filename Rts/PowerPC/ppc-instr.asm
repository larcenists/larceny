;;; -*- mode: m4 -*-
;;;
;;; Instruction macros for Larceny on PowerPC (preliminary)

;;; Generic hair

;;; From GNU m4 manual, but modified to test the condition first, so it
;;; may execute zero times.

define(`forloop',
	`pushdef(`$1', `$2')_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_forloop',
        `ifelse($1, `$3', ,
	            `$4`'define(`$1', incr($1))_forloop(`$1', `$2', `$3', `$4')')')

;;; Local label management.  Use DEFLOCn to give a new value to
;;; a local label, then Ln to use its value.

define(`LOC',10)	; Exercise for the reader: why not start at 0?  :-)
define(`L0',`xxx')
define(`L1',`xxx')
define(`L2',`xxx')
define(`L3',`xxx')
define(`DEFLOC0',
	`define(`LOC',incr(LOC)) define(`L0', `L'LOC)')
define(`DEFLOC1',
	`define(`LOC',incr(LOC)) define(`L1', `L'LOC)')
define(`DEFLOC2',
	`define(`LOC',incr(LOC)) define(`L2', `L'LOC)')
define(`DEFLOC3',
	`define(`LOC',incr(LOC)) define(`L3', `L'LOC)')

;;; End hair

define(`fixtag_mask',`3')
define(`tag_mask',`7')
define(`hdr_shift',`8')
define(`char_shift',`16')
define(`is_hwreg', `eval($1>=1 && $1<=3)')
define(`fixnum',`eval(($1)<<2)')
define(`char',`eval((($1)<<char_shift)|IMM_CHAR)')
define(`roundup4',`eval((($1)+3)&~3)')
define(`roundup8',`eval((($1)+7)&~7)')
define(`words2bytes',`eval(($1)*4)')
define(`stkslot',`eval(STK_REG0+words2bytes($1))')
define(`framesize',`eval(roundup8(wordsize+STK_OVERHEAD+words2bytes($1)))')
define(`recordedsize',`eval(STK_OVERHEAD+words2bytes($1))')
define(`t_label',`$1')

;;; loadr targetreg, regno
;;;	Load HW register targetreg from VM register regno

define(`loadr',
`ifelse(is_hwreg($2),1,
	`mr	$1, REG$2',
	`lwz	$1, G_REG$2(GLOBALS)')')

;;; loadc hwreg, slot
;;; 	Load constant vector element 'slot' into hwreg

define(`loadc',
	`lwz	$1, -PROC_TAG+PROC_CONSTVECTOR(REG0)
	lwz	$1, -VEC_TAG+words2bytes(($2)+1)($1)')

;;; MacScheme Assembly Language instructions

define(`T_ALIGN',
	`.align	$1')

define(`T_CONT', `')
	
define(`T_LABEL',
`t_label($1):')

;;; CONST is broken up into two instructions depending on the
;;; type of the constant.  The argument to T_CONST_IMM is a bitpattern
;;; or symbolic constant name, to T_CONST_CONSTVECTOR it is the
;;; constant vector index.
	
define(`T_CONST_IMM',
	`li	RESULT, $1')

define(`T_CONST_CONSTVECTOR', 
	`loadc(RESULT, $1)')

define(`T_CONST_SETREG_IMM',		; peephole optimization
`ifelse(is_hwreg($2),1,
	`li	REG$2, $1',
	`li	TEMP, $1
	storer($2, TEMP)')')

define(`T_CONST_SETREG_CONSTVECTOR',	; peephole optimization
`ifelse(is_hwreg($2),1,
	`loadc(REG$2, $1)',
	`loadc(TEMP, $1)
	storer($2, TEMP)')')

define(`T_GLOBAL',
	`DEFLOC0 DEFLOC1
L0:	loadc(TEMP, $1)
	lwz	RESULT, -PAIR_TAG(TEMP)
ifdef(`UNSAFE_GLOBALS', 
	`',
	`cmpwi	RESULT, UNDEFINED_CONST
	bne	L1
	mr	RESULT, TEMP
	mcall(M_GLOBAL_EX)
	b	L0
L1:')')

define(`T_SETGLBL',
	`mr	SECOND, RESULT
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
	`stw	REG$1, stkslot($2)(CONT)'
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

define(`T_LAMBDA', ...)
define(`T_LEXES', ...)

define(`T_ARGSEQ',
`ifdef(`UNSAFE_CODE',
	`',
	`DEFLOC0 DEFLOC1
L0:	cmpi	RESULT, fixnum($1)
	bz	L1
	mcall(M_ARGC_EX)
	b	L0
L1:')')

define(`T_ARGSGE',
	`li	SECOND, fixnum($1)
ifdef(`UNSAFE_CODE',
	`',
	`ifelse($1 > 0, 1,
	`DEFLOC0 DEFLOC1
L0:	cmp	RESULT, SECOND
	bge	L1
	mcall(M_ARGC_EX)
	b	L0
L1:')')
	mcall(M_VARARGS)')

define(`T_INVOKE', ...)

%macro T_SAVE 1
	T_SAVE0 %1
%assign slotno 1
%rep	65536
  %if slotno > %1
    %exitrep
  %endif
	T_SAVE1 slotno
  %assign slotno slotno+1
%endrep
%endmacro

; FIXME
define(`T_SETRTN',
	`mov	dword [CONT+STK_RETADDR], t_label($1)')

%macro T_RESTORE 1
%assign slotno 0
%rep	65536
  %if slotno > %1
    %exitrep
  %endif
  %if is_hwreg(slotno)
	mov	REG %+ slotno, dword [stkslot(slotno)]
  %else
	mov	TEMP, dword [stkslot(slotno)]
	mov	[GLOBALS+G_REG %+ slotno], TEMP
  %endif
  %assign slotno slotno+1
%endrep
%endmacro

define(`T_POP',
	`addi	CONT, framesize($1)')
	
define(`T_POPSTK',
	`error -- T_POPSTK not implemented -- students only')

define(`T_RETURN',
	`lwz	TEMP, STK_RETADDR(CONT)
	mtlr	TEMP
	blr')

define(`T_APPLY',	
	`timer_check
	loadr(THIRD, $2)
	loadr(SECOND, $1)
	mcall(M_APPLY)
	lwz	TEMP, -PROC_TAG+PROC_CODEVECTOR_NATIVE(REG0)
	mtlr	TEMP
	blr')

define(`T_NOP', `')

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; in this implementation)
	
define(`T_JUMP',
	`timer_check
%if %1 > 0
	loadr	TEMP, 0		; We know R0 is not a HWREG
%assign ribno 0
%rep 65536
  %if ribno == %1
    %exitrep
  %endif
	mov	TEMP, [TEMP-PROC_TAG+PROC_REG0]
  %assign ribno ribno+1
%endrep
	storer	0, TEMP
%endif
	jmp	%3
%endmacro

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

define(`T_SKIP',
	`b	t_label($1)')

define(`T_SKIPF',
	`cmpi	RESULT, FALSE_CONST
	be	t_label($1)')

define(`T_BRANCH',
	`subfic	TIMER
	bnz	t_label($1)
	mcall(M_TIMER_EXCEPTION)
	b	t_label($1)')

define(`T_BRANCHF',
	`timer_check
	cmpi	RESULT, FALSE_CONST
	bz	t_label($1)')

define(`T_CHECK',
	`cmpi	RESULT, FALSE_CONST
	bz	t_label($4)')

;; Call handler for noncontinuable exception with RESULT,
;; SECOND, and THIRD defined.

define(`T_TRAP',
`ifelse($1,0,
	`',
	`loadr(RESULT, $1)')
ifelse($3,0,
	`li	THIRD, 0',
	`loadr(THIRD, $3)')
ifelse($2,0,
	`li	SECOND, 0',
	`loadr(SECOND, $2)')
	exception_noncontinuable($4)')


