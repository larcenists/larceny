; Copyright 2006 Felix S Klock
; (Based on Lars T Hansen's i386-machine.asm file)
; Machine description for x86-sassy port of Larceny.
;
; $Id: i386-machine.ah 2543 2005-07-20 21:54:03Z pnkfelix $
;
; Notes on the representations.
; The official register assignments are defined in iasn-regs.cfg
;
; EAX is used as a temp for generated code, and to pass SECOND on
; calls to millicode.  EAX is never a root for garbage collection, so
; SECOND must be flushed to memory by millicode.
;
; ESP is used as the pointer to the GLOBALS array.  
; To make millicode calls compact, ESP needs to point to storage with
; (at least) one word free for modification above it.  To call
; millicode one executes an indirect CALL instruction; the return
; address will be pushed into the first element of GLOBALS.  Millicode
; must pop the value to adjust GLOBALS and save the address.
;
; ECX and EDI must have free saving locations available to enable
; code that uses the REP STOS instructions without knowing too much
; about the register layout.
;
; Using the low byte of registers when possible reduces code size.
; The instruction macros know that RESULT and TEMP have a low byte
; register, but otherwise rely on hwreg_has_low() to test the mapping.

