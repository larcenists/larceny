; -*- Scheme -*-
;
; Scheme 313 runtime system.
; Scheme code for various contagion procedures and numeric coercion.
;
; $Id: contagion.sch,v 1.1 1995/08/01 04:45:56 lth Exp lth $
;
; There are three main procedures: contagion, pcontagion, and econtagion.

(define contagion #f)
(define econtagion #f)
(define pcontagion #f)

(let ()

  ; Types

  (define fixtype 0)
  (define bigtype 1)
  (define rattype 2)
  (define recttype 3)
  (define flotype 4)
  (define comptype 5)

  ; Return the type of a number as one of the above-defined symbolic
  ; constants, or signal an error if the argument is not a number.

  (define (number-type x)
    (cond ((fixnum? x)  fixtype)
	  ((bignum? x)  bigtype)
	  ((ratnum? x)  rattype)
	  ((rectnum? x) recttype)
	  ((flonum? x)  flotype)
	  ((compnum? x) comptype)
	  (else (error "Contagion:" x "is not a number."))))

  (define (do-contagion matrix a b retry)
    (let* ((ta (number-type a))
	   (tb (number-type b))
	   (v  (vector-ref (vector-ref matrix ta) tb))
	   (t  (v a b)))
      (retry (car t) (cdr t)))) ; CPS would be better...

  ; Many of the numbers created here violate various invariants in the system;
  ; this is how it should be.

  (define (fixnum->ratnum f)
    (make-unreduced-ratnum f 1))

  (define (fixnum->rectnum f)
    (make-rectnum f 0))

  (define fixnum->flonum (lambda (x) (exact->inexact x)))

  (define (fixnum->compnum f)
    (make-compnum (fixnum->flonum f) 0.0))

  (define (bignum->ratnum f)
    (make-unreduced-ratnum f 1))

  (define (bignum->rectnum f)
    (make-rectnum f 0))

  (define (bignum->compnum f)
    (make-compnum (bignum->flonum f) 0.0))

  (define (ratnum->rectnum f)
    (make-rectnum f 0))

  (define (ratnum->flonum f)
    (/ (exact->inexact (numerator f)) (exact->inexact (denominator f))))

  (define (ratnum->compnum f)
    (make-compnum (ratnum->flonum f) 0.0))

  (define (rectnum->compnum f)
    (make-compnum (exact->inexact (real-part f)) 
		  (exact->inexact (imag-part f))))

  (define (flonum->compnum f)
    (make-compnum f 0.0))

  (define (id x) x)

  (define (oops a b)
    (error "contagion: internal error: same-representation arithmetic."))

  (define (fun f1 f2)
    (lambda (a b)
      (cons (f1 a) (f2 b))))

  (define (algorithm* a b)
    (if (and (integer? a) (integer? b))
	(cons (->int a) (->int b))
	(cons (->flo a) (->flo b))))

  (define (->int a)
    (error "contagion: ->int has not been implemented"))

  (define (->flo a)
    (error "contagion: ->flo has not been implemented"))

  (define cmatrix #f)
  (define ematrix #f)
  (define pmatrix #f)

  ; initialize things.

  ; Contagion matrices. They are completely symmetric with respect to the
  ; final types, although the entries in the matrix are different across the
  ; diagonal.

  (set! cmatrix
    (vector (vector oops (fun fixnum->bignum id) (fun fixnum->ratnum id) 
		    (fun fixnum->rectnum id) (fun fixnum->flonum id) 
		    (fun fixnum->compnum id))
	    (vector (fun id fixnum->bignum) oops (fun bignum->ratnum id)
		    (fun bignum->rectnum id) (fun bignum->flonum id)
		    (fun bignum->compnum id))
	    (vector (fun id fixnum->ratnum) (fun id bignum->ratnum) oops
		    (fun ratnum->rectnum id) (fun ratnum->flonum id)
		    (fun ratnum->compnum id))
	    (vector (fun id fixnum->rectnum) (fun id bignum->rectnum)
		    (fun id ratnum->rectnum) oops
		    (fun rectnum->compnum flonum->compnum)
		    (fun rectnum->compnum id))
	    (vector (fun id fixnum->flonum) (fun id bignum->flonum) 
		    (fun id ratnum->flonum) 
		    (fun flonum->compnum rectnum->compnum)
		    oops (fun flonum->compnum id))
	    (vector (fun id fixnum->compnum) (fun id bignum->compnum) 
		    (fun id ratnum->compnum) (fun id rectnum->compnum)
		    (fun id flonum->compnum) oops)))

  ; FIXME

  (set! pmatrix
    (vector (vector oops (fun fixnum->bignum id) (fun fixnum->ratnum id) 
		    (fun fixnum->rectnum id) (fun fixnum->flonum id) 
		    (fun fixnum->compnum id))
	    (vector (fun id fixnum->bignum) oops (fun bignum->ratnum id)
		    (fun bignum->rectnum id) algorithm*
		    algorithm*)
	    (vector (fun id fixnum->ratnum) (fun id bignum->ratnum) oops
		    (fun ratnum->rectnum id) (fun ratnum->flonum id)
		    (fun ratnum->compnum id))
	    (vector (fun id fixnum->rectnum) (fun id bignum->rectnum)
		    (fun id ratnum->rectnum) oops
		    (fun rectnum->compnum flonum->compnum)
		    (fun rectnum->compnum id))
	    (vector (fun id fixnum->flonum) algorithm*
		    (fun id ratnum->flonum) 
		    (fun flonum->compnum rectnum->compnum)
		    oops (fun flonum->compnum id))
	    (vector (fun id fixnum->compnum) algorithm*
		    (fun id ratnum->compnum) (fun id rectnum->compnum)
		    (fun id flonum->compnum) oops)))

  ; FIXME

  (set! ematrix
    (vector (vector oops (fun fixnum->bignum id) (fun fixnum->ratnum id) 
		    (fun fixnum->rectnum id) (fun fixnum->flonum id) 
		    (fun fixnum->compnum id))
	    (vector (fun id fixnum->bignum) oops (fun bignum->ratnum id)
		    (fun bignum->rectnum id) (fun bignum->flonum id)
		    (fun bignum->compnum id))
	    (vector (fun id fixnum->ratnum) (fun id bignum->ratnum) oops
		    (fun ratnum->rectnum id) (fun ratnum->flonum id)
		    (fun ratnum->compnum id))
	    (vector (fun id fixnum->rectnum) (fun id bignum->rectnum)
		    (fun id ratnum->rectnum) oops
		    (fun rectnum->compnum flonum->compnum)
		    (fun rectnum->compnum id))
	    (vector (fun id fixnum->flonum) (fun id bignum->flonum) 
		    (fun id ratnum->flonum) 
		    (fun flonum->compnum rectnum->compnum)
		    oops (fun flonum->compnum id))
	    (vector (fun id fixnum->compnum) (fun id bignum->compnum) 
		    (fun id ratnum->compnum) (fun id rectnum->compnum)
		    (fun id flonum->compnum) oops)))

  (set! contagion (lambda (a b retry)
		    (do-contagion cmatrix a b retry)))

  (set! econtagion (lambda (a b retry)
		     (do-contagion ematrix a b retry)))

  (set! pcontagion (lambda (a b retry)
		     (do-contagion pmatrix a b retry)))

  #t)
