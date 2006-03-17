; The procedures bit-field and logcount are from slib/the srfi-60
; reference implementation, by Aubrey Jaffer http://srfi.schemers.org

;;;; "logical.scm", bit access and operations for integers for Scheme
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define (bit-field n start end)
  (logand (lognot (ash -1 (- end start)))
	  (arithmetic-shift n (- start))))

; (define logcount
;   (letrec ((logcnt (lambda (n tot)
; 		     (if (zero? n)
; 			 tot
; 			 (logcnt (quotient n 16)
; 				 (+ (vector-ref
; 				     '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
; 				     (modulo n 16))
; 				    tot))))))
;     (lambda (n)
;       (cond ((negative? n) (logcnt (lognot n) 0))
; 	    ((positive? n) (logcnt n 0))
; 	    (else 0)))))
