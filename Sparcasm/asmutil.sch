; -*- scheme -*-
;
; Larceny -- Sparc assembler utility procedures.
;
; lth@cs.uoregon.edu / August 25, 1995 (code is older)
; $id$

; Round up to nearest 8.

(define (roundup8 n)
  (* (quotient (+ n 7) 8) 8))

; Machine parameters

(define maxregs   32)
(define lastreg   (- maxregs 1))

; Given an integer code for a register, return its register label.
; This register label is the register number for a h.w. register and the
; offsets from GLOBALS[ r0 ] for a s.w. register.

(define regname
  (let ((v (vector $r.reg0  $r.reg1  $r.reg2  $r.reg3  $r.reg4  $r.reg5
		   $r.reg6  $r.reg7  $r.reg8  $r.reg9  $r.reg10 $r.reg11
		   $r.reg12 $r.reg13 $r.reg14 $r.reg15 $r.reg16 $r.reg17
		   $r.reg18 $r.reg19 $r.reg20 $r.reg21 $r.reg22 $r.reg23
		   $r.reg24 $r.reg25 $r.reg26 $r.reg27 $r.reg28 $r.reg29
		   $r.reg30 $r.reg31)))
    (lambda (r)
      (vector-ref v r))))

; Is a general-purpose register mapped to a hardware register?
; This is fragile! FIXME.

(define (hardware-mapped? r)
  (or (and (>= r $r.reg0) (<= r $r.reg7))
      (= r $r.argreg2)
      (= r $r.argreg3)
      (= r $r.result)
      (= r $r.g0)
      (= r $r.tmp0)
      (= r $r.tmp1)
      (= r $r.tmp2)))

; Used by peephole optimizer

(define (hwreg? x) (<= 0 x 7))
(define (immediate-int? x) (<= -1024 x 1023))

; Return the offset in the %GLOBALS table of the given memory-mapped 
; register. A memory-mapped register is represented by an integer which 
; is its offet, so just return the value.

(define (offsetof r)
  r)

; eof
