; -*- Scheme -*-

; Arithmetic operations. The fixnum/fixnum case which overflows is always taken
; care of by the millicode.
;
; We return a pair consisting of the new values.

; Return the type of a number as one of the above-defined symbolic
; constants, or signal an error if the argument is not a number.

(define fixtype 0)
(define bigtype 1)
(define rattype 2)
(define recttype 3)
(define flotype 4)
(define comptype 5)

(define (typeof x)
  (cond ((fixnum? x) fixtype)
	((bignum? x) bigtype)
	((ratnum? x) rattype)
	((rectnum? x) recttype)
	((flonum? x) flotype)
	((compnum? x) comptype)
	(else
	 (error 'generic-arithmetic
		"Non-numeric argument: ~a is not a number"
		x))))

; contagion for arithmetic

(define (contagion a b)
  (do-contagion cmatrix a b))

; contagion for predicates sans '='

(define (pcontagion a b)
  (do-contagion pmatrix a b))

; contagion for '='

(define (econtagion a b)
  (do-contagion ematrix a b))

; where contagion really is

(define (do-contagion matrix a b)
  (let ((ta (typeof a))
	(tb (typeof b)))
    (let ((v (vector-ref matrix (+ (* ta 6) tb))))
      (cons ((vector-ref v ta) a) ((vector-ref v tb) b)))))

; Coercions galore. Also see the files "bignums.scm", "ratnums.scm", and
; "rectnums.scm".

(define (fixnum->ratnum f)
  (make-unreduced-ratnum f 1))

(define (fixnum->rectnum f)
  '())

(define (fixnum->flonum f)
  '())

(define (fixnum->compnum f)
  (make-compnum (fixnum->flonum f) 0.0))

(define (bignum->ratnum f)
  (make-unreduced-ratnum f 1))

(define (bignum->rectnum f)
  '())

(define (bignum->compnum f)
  (make-compnum (bignum->flonum f) 0.0))

(define (ratnum->rectnum f)
  (make-rectnum f 0))

(define (ratnum->flonum f)
  (/ (exact->inexact (numerator f)) (exact->inexact (denominator f))))

(define (ratnum->compnum f)
  (make-compnum (ratnum->flonum f) 0.0))

(define (rectnum->compnum f)
  '())

(define (flonum->compnum f)
  (make-compnum f 0.0))

(define (identity x) x)

(define (noconv x)
  (error 'generic-arithmetic "INTERNAL ERROR: Impossible coercion"))

(define (oops x)
  (error 'generic-arithmetic
	 "INTERNAL ERROR: same-representation arith. in contagion"))


; Coercion vectors. This may be elegant, but it is possibly much slower than
; nested cond statements. 

(define ->big   (vector fixnum->bignum identity 
		        noconv noconv noconv))
(define ->rat   (vector fixnum->ratnum bignum->ratnum identity 
		        noconv noconv noconv))
(define ->rect  (vector fixnum->rectnum bignum->rectnum ratnum->rectnum 
		        identity noconv noconv))
(define ->float (vector fixnum->flonum bignum->flonum ratnum->flonum
			noconv identity noconv))
(define ->comp  (vector fixnum->compnum bignum->compnum ratnum->compnum
			rectnum->compnum flonum->compnum identity))
(define ->oops  (vector oops oops oops oops oops oops))


; Contagion matrices

(define cmatrix
  (vector ->oops  ->big   ->rat   ->rect  ->float  ->comp
	  ->big   ->oops  ->rat   ->rect  ->float  ->comp
	  ->rat   ->rat   ->oops  ->rect  ->float  ->comp
	  ->rect  ->rect  ->rect  ->oops  ->comp   ->comp
	  ->float ->float ->float ->comp  ->oops   ->comp
	  ->comp  ->comp  ->comp  ->comp  ->comp   ->oops))

; fixme

(define pmatrix
  (vector ->oops  ->big   ->rat   ->rect  ->float  ->comp
	  ->big   ->oops  ->rat   ->rect  ->float  ->comp
	  ->rat   ->rat   ->oops  ->rect  ->float  ->comp
	  ->rect  ->rect  ->rect  ->oops  ->comp   ->comp
	  ->float ->float ->float ->comp  ->oops   ->comp
	  ->comp  ->comp  ->comp  ->comp  ->comp   ->oops))

; fixme

(define ematrix
  (vector ->oops  ->big   ->rat   ->rect  ->float  ->comp
	  ->big   ->oops  ->rat   ->rect  ->float  ->comp
	  ->rat   ->rat   ->oops  ->rect  ->float  ->comp
	  ->rect  ->rect  ->rect  ->oops  ->comp   ->comp
	  ->float ->float ->float ->comp  ->oops   ->comp
	  ->comp  ->comp  ->comp  ->comp  ->comp   ->oops))
