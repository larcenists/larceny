; -*- Scheme -*-
;
; This file contains architecture-independent procedures which set up
; the Scheme-level support for millicode, like the vector of millicode-
; callable scheme procedures.
;
; $Id: mcode.sch,v 1.3 1997/07/07 20:52:12 lth Exp lth $

; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; FIXME
; The scheme procedure 'make-rectangular' is implemented in Scheme for now.
; Perhaps it always should be; anyway, calling make-rectangular is expensive
; because we fall straight thru millicode into _schemecall and then into
; this procedure.

(define (generic-make-rectangular a b) 
  (cond ((exact? a)
	 (if (exact? b)
	     (if (= b 0)
		 a
		 (make-rectnum a b))
	     (if (= b 0.0)
		 (exact->inexact a)
		 (make-compnum (exact->inexact a) b))))
	((exact? b)
	 (if (= b 0)
	     a
	     (make-compnum a (exact->inexact b))))
	(else
	 (if (= b 0.0)
	     a
	     (make-compnum a b)))))


; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; 'a' is known to be a non-fixnum exact number, or not a number at all.
; FIXME: should ratnum case be handled by Algorithm Bellerophon?

(define (generic-exact->inexact a)
  (cond ((bignum? a) 
	 (bignum->flonum a))
	((ratnum? a)
	 (/ (exact->inexact (numerator a)) (exact->inexact (denominator a))))
	((rectnum? a) 
	 (make-rectangular (exact->inexact (real-part a))
			   (exact->inexact (imag-part a))))
	(else 
	 (error "exact->inexact: " a " is not a number.")
	 #t)))


; THIS PROCEDURE TO BE CALLED ONLY FROM MILLICODE.
;
; Identity operations are handled by the millicode.
; We have to handle the cases flonum->rational and compnum->rectnum.

(define (generic-inexact->exact a)
  (cond ((flonum? a)
	 (if (integer? a)
	     (flonum->integer a)
	     (flonum->ratnum a)))
	((compnum? a)
	 (make-rectangular (inexact->exact (real-part a))
			   (inexact->exact (imag-part a))))
	(else
	 (error "generic-inexact->exact: internal error: " a)
	 #t)))

(define (heavy-quotient a b)
  (cond ((and (bignum? a) (bignum? b))
	 (bignum-quotient a b))
	((and (integer? a) (integer? b))
	 (cond ((and (flonum? a) (flonum? b))
		(truncate (/ a b)))
	       ((and (compnum? a) (compnum? b))
		(truncate (/ (real-part a) (real-part b)))) ; Could be (/ a b)
	       (else
		(contagion a b quotient))))
	(else
	 (error "quotient: arguments must be integers: " a " " b)
	 #t)))

(define (heavy-remainder a b)
  (cond ((and (bignum? a) (bignum? b))
	 (bignum-remainder a b))
	((and (integer? a) (integer? b))
	 (- a (* (quotient a b) b)))
	(else
	 (error "remainder: arguments must be integers: " a " " b)
	 #t)))


; When calling fixnum2ratnum-div by saying '(/ 2 4):
;  the arguments are: 2 and 4
;  they are known to be integers and to be exact  
;
;   -r'm
(define (fixnum2ratnum-div a b)
  (cond ((= b 0) (error "fixnum2ratnum-div: division by zero") #t)
	((< b 0) (make-reduced-ratnum (- a) (- b)))
	(else    (make-reduced-ratnum a b))))


; "install-millicode-support" makes a vector of *all* scheme procedures
; which are callable from millicode and puts this vector in the global
; variable "millicode-support". It also sets up the global procedure
; "contagion-error-code" which returns an error code for certain operations
; supported by contagion.
;
; The order of the procedures in this vector is very important and 
; *must* correspond to the offsets defined in the file "../milliprocs.cfg".
;
; DO NOT CHANGE THE ORDER OF ARGUMENTS TO "vector"!
;
; FIXME: should assign by index, rather than having a large vector
;        expression.

(define millicode-support)
(define contagion-error-code)

(define (install-millicode-support)
  (let ((v (vector #f                   ; obsolete: scheme2scheme-helper
		   #f                   ; obs: undefined-global-exception
		   exception-handler
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   bignum-add		; loc 10
		   bignum-subtract
		   bignum-multiply
		   bignum-divide
		   bignum-negate
		   bignum-abs
		   bignum=?
		   bignum<?
		   bignum<=?
		   bignum>?
		   bignum>=?		; loc 20
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   ratnum-add		; loc 30
		   ratnum-sub
		   ratnum-mul
		   ratnum-div
		   ratnum-neg
		   ratnum-abs
		   ratnum=?
		   ratnum<?
		   ratnum<=?
		   ratnum>?
		   ratnum>=?		; loc 40
		   ratnum-round
		   ratnum-truncate
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   rectnum-add		; loc 50
		   rectnum-sub
		   rectnum-mul
		   rectnum-div
		   rectnum-neg
		   rectnum=?
		   #f
		   #f
		   #f
		   #f
		   #f			; loc 60
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   (lambda (a b) (+ a b))		; loc 70
		   (lambda (a b) (- a b))
		   (lambda (a b) (* a b))
		   (lambda (a b) (/ a b))
		   fixnum2ratnum-div
		   heavy-quotient
		   heavy-remainder
		   #f ; heavy-modulo ; no longer in millicode
		   (lambda (a b) (= a b))
		   (lambda (a b) (< a b))
		   (lambda (a b) (<= a b))		; loc 80
		   (lambda (a b) (> a b))
		   (lambda (a b) (>= a b))
		   contagion
		   pcontagion
		   econtagion
		   #f
		   #f
		   #f
		   #f
		   generic-make-rectangular ; loc 90
		   generic-inexact->exact
		   generic-exact->inexact
		   )))

    ; Return the error code for the given millicode support vector index,
    ; for operations supported by contagion.
    ;
    ; Returns the error code for the given millicode support vector procedure,
    ; for operations supported by contagion.
    ;
    ; Returns #f if no error code can be found.

    (define %contagion-error-code
      (let ((v (vector $ex.add $ex.sub $ex.mul $ex.div #f
		       $ex.quotient $ex.remainder $ex.modulo 
		       $ex.equalp $ex.lessp $ex.lesseqp
		       $ex.greaterp $ex.greatereqp)))
	(lambda (idx)
	  (cond ((procedure? idx)
		 (let loop ((i 70))
		   (cond ((= i (+ 70 (vector-length v)))
			  #f)
			 ((eq? idx (vector-ref millicode-support i))
			  (vector-ref v (- i 70)))
			 (else (loop (+ i 1))))))
		((< idx 70) #f)
		((>= idx (+ 70 (vector-length v))) #f)
		(else (vector-ref v (- idx 70)))))))

    (set! millicode-support v)
    (set! contagion-error-code %contagion-error-code)))

; eof
