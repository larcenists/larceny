; -*- Scheme -*-
;
; Scheme 313 runtime system
; Procedures for rectnum arithmetic
;
; $Id: rectnums.sch,v 1.2 1997/07/07 20:52:12 lth Exp lth $
;
; A rectnum is a pair of two exact real numbers (the "real" and "imaginary"
; parts) where the imaginary part is nonzero.
;
;
; A complex number z is represented as: 
;   z = a + bi        where i is (sqrt -1)
;                           a is the real part of z  
;                           b is the imaginary part.
;
;   The complex plane is a coordinate system with the reals as the abscissa
;   and the imaginaries as the ordinate.
;
; If we use polor coordinates in the plane, then 
;
;              it                         2    2
;       z = r e         where r is (sqrt a  + b  )
;                             i is (sqrt -1)
;                             t is the angle, theta.
;
;                                               it
; Thus, z = a + bi = r (cos(t) + i sin(t)) = r e
;
; Equality:
;   Two complex numbers x and y are said to be equal when their real parts
;   are equal and their imaginary parts are equal.
;
; Addition, subtraction, multiplication:
;   (a + bi) + (c + di) = (a + c) + (b + d)i
;   (a + bi) * (c + di) = ac - bd + (ad + bc)i
;
; Conjugate:                         _
;     z = a + bi  ->  conjugate(z) = z = a - bi = C
;
; Magnitude: 
;     z = a + bi  ->  |z| = r
;


; (export rectnum-add
;         rectnum-subtract
;         rectnum-multiply
;         rectnum-divide
;         rectnum-negate
;         rectnum-zero?
;         rectnum-negative?
;         rectnum-positive?
;         rectnum=?
;         rectnum>?
;         rectnum>=?
;         rectnum<?
;         rectnum<=?)


; Low-level stuff.
; A rectnum is a vector-like object with two parts, the first being the
; real and the second being the imaginary part. Both parts are exact real
; numbers (integers or ratnums in this implementation).
;
; These procedures should be recognized by the compiler or at the very
; least coded in assembly language, since we cannot really use vector-ref
; etc. to access these objects.

(define (make-rectnum a b)
  (if (or (inexact? a) (inexact? b))
      (begin (error "Inexact argument(s) to make-rectnum.") #t)
      (let ((v (vector a b)))
	(typetag-set! v sys$tag.rectnum-typetag)
	v)))

(define (rectnum? obj)
  (and (vector-like? obj)
       (= (typetag obj) sys$tag.rectnum-typetag)))

(define (rectnum-real-part r)
  (if (rectnum? r)
      (vector-like-ref r 0)
      (begin (error "Not a rectnum.") #t)))

(define (rectnum-imag-part r)
  (if (rectnum? r)
      (vector-like-ref r 1)
      (begin (error "Not a rectnum.") #t)))

(define (rectnum->string r radix)
  (if (negative? (rectnum-imag-part r))
      (string-append (number->string (rectnum-real-part r))
		     "-"
		     (number->string (- (rectnum-imag-part r)))
		     "i")
      (string-append (number->string (rectnum-real-part r))
		     "+"
		     (number->string (rectnum-imag-part r))
		     "i")))

(define (rectnum-add x y) 
  (make-rectangular
   (+ (rectnum-real-part x) (rectnum-real-part y))
   (+ (rectnum-imag-part x) (rectnum-imag-part y))))

(define (rectnum-sub x y) 
  (make-rectangular
   (- (rectnum-real-part x) (rectnum-real-part y))
   (- (rectnum-imag-part x) (rectnum-imag-part y))))

(define (rectnum-mul x y)
  (let ((a (rectnum-real-part x))
	(b (rectnum-imag-part x))
	(c (rectnum-real-part y))
	(d (rectnum-imag-part y)))
    (make-rectangular
     (- (* a c) (* b d))
     (+ (* a d) (* b c)))))

(define (rectnum-neg x)
  (make-rectangular
   (- (rectnum-real-part x))
   (- (rectnum-imag-part x))))

(define (rectnum-conjugate x)
  (make-rectangular
   (rectnum-real-part x)
   (- (rectnum-imag-part x))))

(define (rectnum-abs x)
  (let ((a (rectnum-real-part x))
	(b (rectnum-real-part x))
	(sqr (lambda (x) (* x x))))
    (cond ((zero? a) b)
	  ((zero? b) a)
	  ((> a b) (* a (sqrt (+ 1.0 (sqr (/ b a))))))
	  (else    (* b (sqrt (+ 1.0 (sqr (/ a b)))))))))


; This is taken from the Numerical Programming in C book.
;
(define (rectnum-div x y)
  (let ((a (rectnum-real-part x))
	(b (rectnum-imag-part x))
	(c (rectnum-real-part y))
	(d (rectnum-imag-part y)))
    (if (>= (abs c) (abs d))
	(let* ((r (/ d c))
	       (denominator (+ c (* r d)))
	       (real (+ a (* r b)))
	       (imag (- b (* r a))))
	  (make-rectangular (/ real denominator) 
			    (/ imag denominator)))
	(let* ((r (/ c d))
	       (denominator (+ d (* r c)))
	       (real (+ b (* r a)))
	       (imag (- a (* r b))))
	  (make-rectangular (/ real denominator) 
			    (/ imag denominator))))))


(define (rectnum=? x y)
  (and (= (rectnum-real-part x) (rectnum-real-part y))
       (= (rectnum-imag-part x) (rectnum-imag-part y))))

; eof
