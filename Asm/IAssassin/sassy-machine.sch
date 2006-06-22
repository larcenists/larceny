; Copyright 2006 Felix S Klock
; (Based on Lars T Hansen's i386-machine.asm file)
; Machine description for x86-sassy port of Larceny.
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
  (syntax-rules (quote)
    ((_ NAME (quote VAL))
     (begin
       (define NAME (quote VAL))
       (set! sassy-machine-directives
             (begin 
               (if (not (symbol? (quote VAL)))
                   (error 'define-sassy-constant
                          "Only define symbols or numbers!"))
               (cons '(macro NAME VAL)
                     sassy-machine-directives)))))
    ((_ NAME VAL)
     (begin
       (define NAME VAL)
       (set! sassy-machine-directives
             (begin 
               (if (not (number? VAL))
                   (error 'define-sassy-constant 
                          "Only define symbols or numbers!"))
               (cons '(macro NAME VAL)
                     sassy-machine-directives)))))))

(define-sassy-constant wordsize        4)
(define-sassy-constant object_align    8)
(define-sassy-constant code_align      4)

(define-sassy-constant fixtag_mask	    3)
(define-sassy-constant tag_mask             7)
(define-sassy-constant hdr_shift            8)
(define-sassy-constant char_shift	    16)

(define-sassy-constant TEMP    	'eax)	; always same as SECOND
(define-sassy-constant SECOND  	'eax)	; always same as TEMP
(define-sassy-constant RESULT  	'ebx)
(define-sassy-constant REG1    	'ecx)	; ecx must map to a VM register
(define-sassy-constant REG2    	'edx)
(define-sassy-constant REG3    	'edi)	; edi must map to a VM register
(define-sassy-constant REG4    	'esi)
(define-sassy-constant REG1_LOW 'cl)	; ecx must map to a VM register
(define-sassy-constant REG2_LOW 'dl)
(define-sassy-constant GLOBALS 	'esp)
(define-sassy-constant CONT    	'ebp)

(define BVEC_HEADER_BYTES 4)

(define G_REGALIAS_ECX	$g.reg1)	; used by rep stos constructions
(define G_REGALIAS_EDI	$g.reg3)	; ditto

(define-sassy-constant TEMP_LOW	        'al)
(define-sassy-constant RESULT_LOW	'bl)
(define-sassy-constant SECOND_LOW	'al)
(define-sassy-constant REG1_LOW	        'cl)
(define-sassy-constant REG2_LOW	        'dl)

(define (hwreg_has_low r) (or (= r 1) (= r 2)))

;; corresponds to settings in features.sch
;; %include "config.ah" 

;; corresponds to constants defined by Rts/Build/*.sh
;; %include "asmdefs.h" 

(define STK_OVERHEAD 12)
(define STK_RETADDR 4)
(define STK_REG0 12)

(define LASTREG 31)

(define PROC_HEADER_WORDS           1)
(define PROC_OVERHEAD_WORDS         2)             ; code and constants
(define PROC_CONSTVECTOR            8)   ; byte offset
(define PROC_CODEVECTOR_NATIVE      4)    ; byte offset
(define PROC_REG0                   12)            ; byte offset


