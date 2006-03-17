; operands.scm - Sassy's operand predicates
; Copyright (C) 2005 Jonathan Kraut

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Contact:
; Jonathan Kraut
; 4130 43 ST #C2
; Sunnyside, NY 11104
; jak76@columbia.edu

; see file COPYING in the top of Sassy's distribution directory


; module operands
; import numbers srfi-69
; import-syntax meta-lambda
; export all


; register type operands
(define r8   #f)
(define r16  #f)
(define r32  #f)
(define mm   #f)
(define st   #f)
(define xmm  #f)
(define creg #f)
(define dreg #f)
(define sreg #f)
(define (r32-not-esp x) (and (not (eq? x 'esp)) (r32 x)))
(define (sreg-not-cs x) (and (not (eq? x 'cs))  (sreg x)))

(define symbol #f)
(let
    ((the-registers
      (alist->hash-table
       '((eax . (32 . 0)) (ecx . (32 . 1)) (edx . (32 . 2)) (ebx . (32 . 3))
	 (esp . (32 . 4)) (ebp . (32 . 5)) (esi . (32 . 6)) (edi . (32 . 7))
	 (ax . (16 . 0)) (cx . (16 . 1)) (dx . (16 . 2)) (bx . (16 . 3))
	 (sp . (16 . 4)) (bp . (16 . 5)) (si . (16 . 6)) (di . (16 . 7))
	 (al . (8 . 0)) (cl . (8 . 1)) (dl . (8 . 2)) (bl . (8 . 3))
	 (ah . (8 . 4)) (ch . (8 . 5)) (dh . (8 . 6)) (bh . (8 . 7))
	 (st0 . (80 . 0)) (st1 . (80 . 1)) (st2 . (80 . 2)) (st3 . (80 . 3))
	 (st4 . (80 . 4)) (st5 . (80 . 5)) (st6 . (80 . 6)) (st7 . (80 . 7))
	 (mm0 . (64 . 0)) (mm1 . (64 . 1)) (mm2 . (64 . 2)) (mm3 . (64 . 3))
	 (mm4 . (64 . 4)) (mm5 . (64 . 5)) (mm6 . (64 . 6)) (mm7 . (64 . 7))
	 (xmm0 . (128 . 0)) (xmm1 . (128 . 1)) (xmm2 . (128 . 2))
	 (xmm3 . (128 . 3)) (xmm4 . (128 . 4)) (xmm5 . (128 . 5))
	 (xmm6 . (128 . 6)) (xmm7 . (128 . 7))
	 (es . (1 . 0)) (cs . (1 . 1)) (ss . (1 . 2))
	 (ds . (1 . 3)) (fs . (1 . 4)) (gs . (1 . 5))
	 (cr0 . (2 . 0)) (cr2 . (2 . 2)) (cr3 . (2 . 3)) (cr4 . (2 . 4))
	 (dr0 . (3 . 0)) (dr1 . (3 . 1)) (dr2 . (3 . 2))
	 (dr3 . (3 . 3)) (dr6 . (3 . 6)) (dr7 . (3 . 7))
	 )))) ;sreg type-code = 1      creg type-code = 2    dreg type-code = 3
  (let ((reg-x (lambda (reg-type-code)
		 (memoize
		  (lambda (x)
		    (cond ((hash-table-ref the-registers x (lambda () #f)) =>
			   (lambda (found)
			     (and (= reg-type-code (car found)) (cdr found))))
			  (else #f)))))))
    (set! r8   (reg-x 8))
    (set! r16  (reg-x 16))
    (set! r32  (reg-x 32))
    (set! mm   (reg-x 64))
    (set! st   (reg-x 80))
    (set! xmm  (reg-x 128))
    (set! creg (reg-x 2))
    (set! dreg (reg-x 3))
    (set! sreg (reg-x 1))
    (set! symbol (memoize
		  (lambda (x)
		    (or (and (symbol? x)
			     (not (hash-table-ref the-registers
						  x (lambda () #f)))
			     x)
			(custom-reloc x)))))))

; For the remainder of the following, every operand is either an e_ u_
; or general. The u-types are for unexplicit operand sizes. The
; e-types are for the cases where the operand size if explicit, and
; the general is either of those.

; mem type operands - the actual parsing happens in proc-mem in operands
(define um8
  (memoize
   (let ((segger (lambda (x) (and (memq x '(cs ss ds es fs gs)) x)))
	 (mem (meta-lambda (and '& __))))
     (meta-lambda
      (or ,@mem
	  (and segger mem))))))
(define um16  um8)
(define um32  um8)
(define um64  um8)
(define um80  um8)
(define um128 um8)

(define em8   (memoize (meta-lambda (and 'byte   um8))))
(define em16  (memoize (meta-lambda (and 'word   um16))))
(define em32  (memoize (meta-lambda (and 'dword  um32))))
(define em64  (memoize (meta-lambda (and 'qword  um64))))
(define em80  (memoize (meta-lambda (and 'tword  um80))))
(define em128 (memoize (meta-lambda (and 'dqword um128))))

(define (m8   x) (or (um8   x) (em8 x)))
(define (m16  x) (or (um16  x) (em16 x)))
(define (m32  x) (or (um32  x) (em32 x)))
(define (m64  x) (or (um64  x) (em64 x)))
(define (m80  x) (or (um80  x) (em80 x)))
(define (m128 x) (or (um128 x) (em128 x)))

(define (mem-any x)
  (or (m32 x) (m16 x) (m8 x) (m64 x) (m80 x) (m128 x)))


; NOTE: This needs fixing. The current bit-size should be checked to
; make sure that "target" and "value", if specified, fit within the
; current bit size.
(define custom-reloc
  (meta-lambda
   (and 'reloc
	(or (and 'rel
		 (or symbol? u-dword)
		 (or (lambda (target) (list 'reloc 'rel target 0))
		     (else (lambda x (error "sassy: bad rel reloc" x)))))
	    (and symbol?
		 (or (lambda (type) (list 'reloc type #f 0))
		     (and (or symbol? u-dword)
			  (or (lambda (type target) (list 'reloc type target 0))
			      (and s-dword
				   (lambda (type target value)
				     (list 'reloc type target value)))))))))))

; rel type operands are used by branches

(define urel8  (memoize (meta-lambda (or ,@u-byte ,@symbol))))
(define urel16 (memoize (meta-lambda (or ,@u-word ,@symbol))))
(define urel32 (memoize (meta-lambda (or ,@u-dword ,@symbol))))

(define erel8  (memoize (meta-lambda (and 'byte urel8))))
(define erel16 (memoize (meta-lambda (and 'word urel16))))
(define erel32 (memoize (meta-lambda (and 'dword urel32))))

(define (rel8  x) (or (urel8  x) (erel8  x)))
(define (rel16 x) (or (urel16 x) (erel16 x)))
(define (rel32 x) (or (urel32 x) (erel32 x)))

; mi type operand is used by mov instruction only with eax
(define umi8  #f)
(define umi16 #f)
(define umi32 #f)

(define emi8  (memoize (meta-lambda (and 'byte umi8))))
(define emi16 (memoize (meta-lambda (and 'word umi16))))
(define emi32 (memoize (meta-lambda (and 'dword umi32))))

(define (mi8  x) (or (umi8 x)  (emi8 x)))
(define (mi16 x) (or (umi16 x) (emi16 x)))
(define (mi32 x) (or (umi32 x) (emi32 x)))

(let ((mi (lambda (x)
	    (let ((asym #f)
		  (acc 0))
	      (let ((a-rest
		     (meta-lambda
		      (or (and ,@symbol
			       (lambda (x) (and (not asym) (set! asym x))))
			  (and ,@integer?
			       (lambda (x) (set! acc (+ x acc))))))))
		(let ((go (meta-lambda
			   (and '& (+ a-rest)
				(begin
				  (cond ((pair? asym)
					 (list 'reloc (car asym) (cadr asym)
					       (+ acc (caddr asym))))
					((symbol? asym)
					 (list 'reloc 'abs asym acc))
					((not asym) acc)))))))
;					 (list 'reloc 'abs acc 0))))))))
		  (go x)))))))
  (set! umi8  mi)
  (set! umi16 mi)
  (set! umi32 mi))

; immediate type operands
; unexplicit
(define ui8  #f)
(define ui16 #f)
(define ui32 #f)

; explicit eg (dword N)
(define ei8  (memoize (meta-lambda (and 'byte  ui8))))
(define ei16 (memoize (meta-lambda (and 'word  ui16))))
(define ei32 (memoize (meta-lambda (and 'dword ui32))))

; any
(define (i8 x)  (or (ui8 x)  (ei8 x)))
(define (i16 x) (or (ui16 x) (ei16 x)))
(define (i32 x) (or (ui32 x) (ei32 x)))

(let ((string-able
       (lambda (z)
	 (lambda (x)
	   (and (string? x)
		(<= (string-length x) z)
		(let ((tmp (string-append x (make-string (- z (string-length x))
							 (integer->char 0)))))
		  (do ((i (- z 1) (- i 1))
		       (r 0 (+ (ash r 8) (char->integer (string-ref tmp i)))))
		      ((< i 0) r))))))))
  (let ((str1 (string-able 1))
	(str2 (string-able 2))
	(str4 (string-able 4))
	(u/s-byte  u/s-byte)
	(u/s-word  u/s-word)
	(u/s-dword u/s-dword))
    (let ((imm16/32
	   (lambda (num-pred str-pred)
	     (meta-lambda
	      (or ,@num-pred
		  ,@symbol
		  ,@str-pred
		  (and ,@char? (lambda (x) (char->integer x))))))))
      (set! ui8 (memoize
		 (meta-lambda
		  (or ,@u/s-byte
		      ,@str1
		      (and ,@char? (lambda (x) (char->integer x)))))))
      (set! ui16 (memoize (imm16/32 u/s-word str2)))
      (set! ui32 (memoize (imm16/32 u/s-dword str4))))))
