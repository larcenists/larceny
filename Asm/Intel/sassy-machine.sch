; -*- mode: asm -*-
;
; Copyright 2003 Lars T Hansen
; Machine description for x86-nasm port of Larceny.
;
; $Id: i386-machine.ah 2543 2005-07-20 21:54:03Z pnkfelix $
;
; Notes on the representations.
;
; EAX is used as a temp for generated code, and to pass SECOND on
; calls to millicode.  EAX is never a root for garbage collection, so
; SECOND must be flushed to memory by millicode.
;
; GLOBALS must be mapped mapped to ESP to make millicode calls
; compact.  The GLOBALS pointer points to element 1 of the globals
; vector; element 0 is normally unused.  To call millicode one
; executes an indirect CALL instruction; the return address will
; be pushed into the first element of GLOBALS.  Millicode
; must pop the value to adjust GLOBALS and save the address.
;
; REG0 is not mapped to a hardware reg because there are few hardware
; regs available and computation is probably more important than access
; to the procedure.  I don't have any data to back this up.
;
; ECX and EDI must be mapped to VM registers to make it easy to write
; code that uses the REP STOS instructions without knowing too much
; about the register layout.
;
; Using the low byte of registers when possible reduces code size.
; The instruction macros know that RESULT and TEMP have a low byte
; register, but otherwise rely on hwreg_has_low() to test the mapping.

(define sassy-machine-directives '())

(define-syntax define-sassy-constant
  (syntax-rules ()
    ((_ NAME VAL)
     (begin
       (define NAME VAL)
       (set! sassy-machine-directives
             (cons '(macro NAME (! VAL))
                   sassy-machine-directives))))))

(define-syntax define-sassy-macro
  (syntax-rules ()
    ((_ (NAME ARGS ...) BODY)
     (begin
       (define (NAME ARGS ...) BODY)
       (set! sassy-machine-directives
             (cons '(macro NAME (lambda (ARGS ...) BODY))
                   sassy-machine-directives))))))

(define-sassy-constant wordsize        4)
(define-sassy-constant object_align    8)
(define-sassy-constant code_align      4)

(define-sassy-constant TEMP    	'eax)	; always same as SECOND
(define-sassy-constant SECOND  	'eax)	; always same as TEMP
(define-sassy-constant RESULT  	'ebx)
(define-sassy-constant REG1    	'ecx)	; ecx must map to a VM register
(define-sassy-constant REG2    	'edx)
(define-sassy-constant REG3    	'edi)	; edi must map to a VM register
(define-sassy-constant REG4    	'esi)
(define-sassy-constant GLOBALS 	'esp)
(define-sassy-constant CONT    	'ebp)

(define-sassy-constant G_REGALIAS_ECX	$g.reg1)	; used by rep stos constructions
(define-sassy-constant G_REGALIAS_EDI	$g.reg3)	; ditto

(define-sassy-constant G_TIMER          $g.timer)

(define-sassy-constant FIRST_HWREG	1)
(define-sassy-constant LAST_HWREG	4)

(define-sassy-constant TEMP_LOW	        'al)
(define-sassy-constant RESULT_LOW	'bl)
(define-sassy-constant SECOND_LOW	'al)
(define-sassy-constant REG1_LOW	        'cl)
(define-sassy-constant REG2_LOW	        'dl)

(define-sassy-macro (hwreg_has_low r) (or (= r 1) (= r 2)))

;; corresponds to settings in features.sch
;; %include "config.ah" 

;; corresponds to constants defined by Rts/Build/*.sh
;; %include "asmdefs.h" 


(define-sassy-macro (EXTNAME x) 
  (if (eq? (nbuild-parameter 'target-os) 'win32) 
      ;; above approach might not actually work
      (string->symbol (string-append "_" (symbol->string x)))
      x))

(define-sassy-constant STK_OVERHEAD 12)
(define-sassy-constant STK_RETADDR 4)
(define-sassy-constant STK_REG0 12)

(define-sassy-constant LASTREG 31)

(define-sassy-constant TRUE_CONST $imm.true)

(define-sassy-constant M_VARARGS  $m.varargs)
(define-sassy-constant M_MORECORE $m.morecore)

(define-sassy-constant PROC_HDR $imm.procedure-header)
(define-sassy-constant PROC_HEADER_WORDS           1)
(define-sassy-constant PROC_OVERHEAD_WORDS         2)             ; code and constants
(define-sassy-constant PROC_CONSTVECTOR            8)   ; byte offset
(define-sassy-constant PROC_CODEVECTOR_NATIVE      4)    ; byte offset
(define-sassy-constant PROC_REG0                   12)            ; byte offset

(define-sassy-constant EX_PROCEDURE_REF	            'EX_PREF)
(define-sassy-constant EX_MAKE_BYTEVECTOR	    'EX_MKBVL)	  ; FIXME
(define-sassy-constant EX_BYTEVECTOR_LENGTH	    'EX_BVLEN)
(define-sassy-constant EX_BYTEVECTOR_LIKE_LENGTH    'EX_BVLLEN)
(define-sassy-constant EX_BYTEVECTOR_LIKE_REF	    'EX_BVLREF)
(define-sassy-constant EX_BYTEVECTOR_LIKE_SET	    'EX_BVLSET)
(define-sassy-constant EX_MAKE_STRING		    'EX_MKBVL)	  ; FIXME
(define-sassy-constant EX_STRING_LENGTH	            'EX_SLEN)
(define-sassy-constant EX_STRING_REF		    'EX_SREF)
(define-sassy-constant EX_STRING_SET		    'EX_SSET)
(define-sassy-constant EX_MAKE_VECTOR		    'EX_MKVL)       ; FIXME
(define-sassy-constant EX_VECTOR_LENGTH	            'EX_VLEN)
(define-sassy-constant EX_VECTOR_REF		    'EX_VREF)
(define-sassy-constant EX_VECTOR_SET		    'EX_VSET)
(define-sassy-constant EX_VECTOR_LIKE_LENGTH	    'EX_VLLEN)
(define-sassy-constant EX_VECTOR_LIKE_REF	    'EX_VLREF)
(define-sassy-constant EX_VECTOR_LIKE_SET	    'EX_VLSET)
(define-sassy-constant EX_BYTEVECTOR_REF	    'EX_BVREF)
(define-sassy-constant EX_BYTEVECTOR_SET	    'EX_BVSET)
(define-sassy-constant EX_MAKE_PROCEDURE	    'EX_MKVL)	  ; FIXME
(define-sassy-constant EX_PROCEDURE_LENGTH	    'EX_PLEN)
(define-sassy-constant EX_PROCEDURE_REF	            'EX_PREF)
(define-sassy-constant EX_PROCEDURE_SET	            'EX_PSET)

