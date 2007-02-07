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
; ECX and EDI must be mapped to VM registers to make it easy to write
; code that uses the REP STOS instructions without knowing too much
; about the register layout.
;
; Using the low byte of registers when possible reduces code size.
; The instruction macros know that RESULT and TEMP have a low byte
; register, but otherwise rely on hwreg_has_low() to test the mapping.

(define wordsize        4)
(define object_align    8)
(define code_align      4)

(define fixtag_mask	    3)
(define tag_mask             7)
(define hdr_shift            8)
(define char_shift	     8)

(define G_REGALIAS_ECX	$g.reg1)	; used by rep stos constructions
(define G_REGALIAS_EDI	$g.reg3)	; ditto
(define G_REGALIAS_EDX  $g.reg2)        ; used by rdrsc
(define G_REGALIAS_EBX  $g.result)


;; corresponds to settings in features.sch
;; %include "config.ah" 

;; corresponds to constants defined by Rts/Build/*.sh
;; %include "asmdefs.h" 

