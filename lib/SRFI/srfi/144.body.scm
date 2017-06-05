;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; References
;;;
;;; Milton Abramowitz and Irene A Stegun [editors].
;;; Handbook of Mathematical Functions With Formulas, Graphs, and
;;; Mathematical Tables.  United States Department of Commerce.
;;; National Bureau of Standards Applied Mathematics Series, 55,
;;; June 1964.  Fifth Printing, August 1966, with corrections.
;;;
;;; R W Hamming.  Numerical Methods for Scientists and Engineers.
;;; McGraw-Hill, 1962.
;;;
;;; Donald E Knuth.  The Art of Computer Programming, Volume 2,
;;; Seminumerical Algorithms, Second Edition.  Addison-Wesley, 1981.

;;; I have deliberately avoided recent references, and have also
;;; avoided looking at any code or pseudocode for these or similar
;;; functions.

;;; Quick-and-dirty implementation of a draft of SRFI 144 (flonums),
;;; as specified at http://vrici.lojban.org/~cowan/temp/srfi-144.html
;;; as of 4 June 2017.
;;;
;;; Spec of flnegative? refers to fl< instead of fl<?
;;;
;;; Spec of flinteger-exponent says it returns "the same as flexponent
;;; as an exact integer", but flexponent usually returns a non-integer.
;;;
;;; FIXME: not as accurate as it should be
;;; FIXME: not as fast as it should be
;;; FIXME: assumes IEEE arithmetic or similar
;;; FIXME: assumes all inexact reals are flonums
;;; FIXME: assumes (scheme inexact)
;;; FIXME: assumes (rnrs flonums)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private, not exported.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: Larceny-specific code for visualization of flonums.
;;; Assumes IEEE double precision, Larceny's usual representation,
;;; and little-endian.

(define (show x)
  (map (lambda (i) (bytevector-like-ref x i))
       '(4 5 6 7 8 9 10 11)))

(define (show-sign x)
  (bitwise-arithmetic-shift (list-ref (show x) 7) -7))

(define (show-exponent x)
  (bitwise-ior
   (bitwise-arithmetic-shift (bitwise-and (list-ref (show x) 7) 127)
                             3)
   (bitwise-arithmetic-shift (bitwise-and (list-ref (show x) 6) #b11100000)
                             -5)))

(define (show-significand x)
  (let ((bytes (show x)))
    (+ (* (list-ref bytes 0) 1)
       (* (list-ref bytes 1) 256)
       (* (list-ref bytes 2) 256 256)
       (* (list-ref bytes 3) 256 256 256)
       (* (list-ref bytes 4) 256 256 256 256)
       (* (list-ref bytes 5) 256 256 256 256 256)
       (* (bitwise-and (list-ref bytes 6) #b00011111)
          256 256 256 256 256 256))))

;;; Private but portable code.

(define FIXME 'FIXME)

(define precision-bits    ; IEEE double has 53 bits of precision
  (let loop ((bits 0)
             (x 1.0))
    (if (= x (+ x 1.0))
        bits
        (loop (+ bits 1)
              (* 2.0 x)))))

(define (check-flonum! name x)
  (if (not (flonum? x))
      (error (string-append "non-flonum argument passed to "
                            (symbol->string name))
             x)))

;;; Given a symbol naming a flonum procedure and a generic operation,
;;; returns a flonum procedure that restricts the generic operation
;;; to flonum arguments and result.

(define (flop1 name op)
  (lambda (x)
    (check-flonum! name x)
    (let ((result (op x)))
      (if (not (flonum? result))
          (error (string-append "non-flonum result from "
                              (symbol->string name))
                              result))
      result)))

(define (flop2 name op)
  (lambda (x y)
    (check-flonum! name x)
    (check-flonum! name y)
    (let ((result (op x y)))
      (if (not (flonum? result))
          (error (string-append "non-flonum result from "
                                (symbol->string name))
                                result))
      result)))

(define (flop3 name op)
  (lambda (x y z)
    (check-flonum! name x)
    (check-flonum! name y)
    (check-flonum! name z)
    (let ((result (op x y z)))
      (if (not (flonum? result))
          (error (string-append "non-flonum result from "
                                (symbol->string name))
                                result))
      result)))

;;; Given a flonum x and a list of flonum coefficients for a polynomial,
;;; in order of increasing degree, returns the value of the polynomial at x.

(define (polynomial-at x coefs)
  (if (null? coefs)
      0.0
      (fl+ (car coefs)
           (fl* x (polynomial-at x (cdr coefs))))))

;;; Given a non-negative integral flonum x, returns its factorial.

(define (factorial x)
  (if (flzero? x)
      1.0
      (* x (factorial (fl- x 1.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mathematical Constants

;;; For portability and ease of implementation, most are calculated.
;;; Some numerical values are copied from Wikipedia or the references.
;;; Inaccuracies will be revealed by testing and then repaired.

(define fl-e (exp 1))
(define fl-1/e (/ fl-e))
(define fl-e-2 (* fl-e fl-e))
(define fl-e-pi/4 (exp (/ (acos -1.0) 4.0)))
(define fl-log2-e (log fl-e 2.0))
(define fl-log10-e (log fl-e 10.0))
(define fl-log-2 (log 2.0))
(define fl-1/log-2 (/ fl-log-2))
(define fl-log-3 (log 3.0))
(define fl-log-pi (log (acos -1.0)))
(define fl-log-10 (log 10.0))
(define fl-1/log-10 (/ fl-log-10))
(define fl-pi (acos -1.0))
(define fl-1/pi (/ fl-pi))
(define fl-2pi (* 2.0 fl-pi))
(define fl-pi/2 (/ fl-pi 2.0))
(define fl-pi/4 (/ fl-pi 4.0))
(define fl-2/sqrt-pi (/ 2.0 (sqrt fl-pi)))
(define fl-pi-squared (* fl-pi fl-pi))
(define fl-degree (/ fl-pi 180.0))
;(define fl-2/pi (expt 2.0 fl-pi))    ; as specified, but I don't believe it
(define fl-2/pi (/ 2.0 fl-pi))        ; this is more likely
;(define fl-2/sqrt-pi fl-2/sqrt-pi)   ; specified twice in draft of SRFI 144
(define fl-sqrt-2 (sqrt 2.0))
(define fl-sqrt-3 (sqrt 3.0))
(define fl-sqrt-5 (sqrt 5.0))
(define fl-sqrt-10 (sqrt 10.0))
(define fl-1/sqrt-2 (/ fl-sqrt-2))
(define fl-cbrt-2 (expt 2.0 (inexact 1/3)))
(define fl-cbrt-3 (expt 3.0 (inexact 1/3)))
(define fl-4thrt-2 (expt 2.0 .25))
(define fl-phi (/ (+ 1.0 (sqrt 5.0)) 2.0))
(define fl-log-phi (log fl-phi))
(define fl-1/log-phi (/ fl-log-phi))
(define fl-euler 0.57721566490153286060651209008240243104215933593992)
(define fl-e-euler (exp fl-euler))
(define fl-sin-1 (sin 1.0))
(define fl-cos-1 (cos 1.0))
(define fl-gamma-1/2 (sqrt fl-pi))
(define fl-gamma-1/3 2.6789385347077476336556929409746776441287)
(define fl-gamma-2/3 1.3541179394264004169452880281545137855193)

;; Implementation Constants

(define fl-greatest
  (let loop ((x (- (expt 2.0 precision-bits) 1.0)))
    (if (finite? (* 2.0 x))
        (loop (* 2.0 x))
        x)))

(define fl-least
  (let loop ((x 1.0))
    (if (> (/ x 2.0) 0.0)
        (loop (/ x 2.0))
        x)))

(define fl-epsilon
  (let loop ((eps 1.0))
    (if (= 1.0 (+ 1.0 eps))
        (* 2.0 eps)
        (loop (/ eps 2.0)))))

(define fl-fast-fl+* #f)

(define fl-integer-exponent-zero                ; arbitrary
  (exact (- (log fl-least 2.0) 1.0)))

(define fl-integer-exponent-nan                 ; arbitrary
  (- fl-integer-exponent-zero 1))

;;; Constructors

(define (flonum x)
  (if (real? x)
      (inexact x)
      (error "bad argument passed to flonum" x)))

;;; FIXME: goes into infinite loop for extreme arguments

(define fladjacent
  (flop2 'fladjacent
         (lambda (x y)
           (cond ((fl=? x y)
                  x)
                 ((or (fl<? x y) (fl>? x y))    ; FIXME: near infinity
                  (let loop ((y y))
                    (let ((y2 (fl/ (fl+ x y) 2.0)))
                      (if (fl=? x y2)
                          y
                          (loop y2)))))
                 (else x)))))

(define flcopysign
  (flop2 'flcopysign
         (lambda (x y)
           (let ((z (fl* x y)))
             (cond ((flpositive? z)
                    x)
                   ((flnegative? z)
                    (fl- x))
                   ((and (flzero? z)
                         (eqv? y -0.0))
                    (fl- x))
                   (else                ; FIXME: ignores sign of NaN
                    x))))))
                 

(define (make-flonum x n)
  (let ((y (expt 2.0 n)))
    (cond ((or (not (flonum? x))
               (not (exact-integer? n)))
           (error "bad arguments to make-flonum" x n))
          ((finite? y)
           (* x y))
          (else
           (inexact (* (exact x) (expt 2 n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Accessors

(define (flinteger-fraction x)
  (check-flonum! 'flinteger-fraction x)
  (let* ((result1 (fltruncate x))
         (result2 (flcopysign (fl- (flabs x) result1) x)))
    (values result1 result2)))

(define (flexponent x)
  (fllog2 (flabs x)))

(define (flinteger-exponent x)    ; spec doesn't make sense
  FIXME)

;;; FIXME: not yet implemented

(define (flnormalized-fraction-exponent x) ; unspecified for infinities, NaNs
  (check-flonum! 'flnormalized-fraction-exponent x)
  (if (not (flfinite? x))    ; unspecified for this case
      (values 0.0 0)
      (let ((result2 (exact (flceiling (flexponent x)))))
        (if (> result2 0)
            (let* ((two^result2 (expt 2.0 result2))
                   (result1 (if (flfinite? two^result2)
                                (fl/ x two^result2)
                                FIXME)))
              FIXME)))))

(define flsign-bit
  (flop1 'flsign-bit
         (lambda (x)
           (cond ((fl<? x 0.0)
                  1)
                 ((eqv? x -0.0)
                  1)
                 (else
                  0)))))


;;; Predicates

;(define flonum? R6RS)               ; defined by (rnrs flonums)
;(define fl=? R6RS)                  ; defined by (rnrs flonums)
;(define fl<? R6RS)                  ; defined by (rnrs flonums)
;(define fl>? R6RS)                  ; defined by (rnrs flonums)
;(define fl<=? R6RS)                 ; defined by (rnrs flonums)
;(define fl>=? R6RS)                 ; defined by (rnrs flonums)

(define (flunordered? x y)
  (or (flnan? x) (flnan? y)))

(define flmax                        ; incompatible with (rnrs flonums)
  (let ((flmax2 (flop2 'flmax max)))
    (lambda args
      (cond ((null? args)
             -inf.0)                 ; spec says fl-greatest, but that's wrong
            ((null? (cdr args))
             (car args))
            ((null? (cddr args))
             (flmax2 (car args) (cadr args)))
            (else
             (flmax2 (flmax2 (car args) (cadr args))
                     (apply flmax (cddr args))))))))

(define flmin                        ; incompatible with (rnrs flonums)
  (let ((flmin2 (flop2 'flmin min)))
    (lambda args
      (cond ((null? args)
             +inf.0)                 ; spec says fl-least, but that's wrong
            ((null? (cdr args))
             (car args))
            ((null? (cddr args))
             (flmin2 (car args) (cadr args)))
            (else
             (flmin2 (flmin2 (car args) (cadr args))
                     (apply flmin (cddr args))))))))

;(define flinteger? R6RS)            ; defined by (rnrs flonums)
;(define flzero? R6RS)               ; defined by (rnrs flonums)
;(define flpositive? R6RS)           ; defined by (rnrs flonums)
;(define flnegative? R6RS)           ; defined by (rnrs flonums)
;(define flodd? R6RS)                ; defined by (rnrs flonums)
;(define fleven? R6RS)               ; defined by (rnrs flonums)
;(define flfinite? R6RS)             ; defined by (rnrs flonums)
;(define flinfinite? R6RS)           ; defined by (rnrs flonums)
;(define flnan? R6RS)                ; defined by (rnrs flonums)

(define flnormalized? FIXME)
(define fldenormalized? FIXME)

;;; Arithmetic

;(define fl+ R6RS)                   ; defined by (rnrs flonums)
;(define fl* R6RS)                   ; defined by (rnrs flonums)

;;; FIXME: not "as if to infinite precision and rounded only once"

(define fl+*
  (flop3 'fl+*
         (lambda (x y z)
           (fl+ (fl* x y) z))))

;(define fl- R6RS)                   ; defined by (rnrs flonums)
;(define fl/ R6RS)                   ; defined by (rnrs flonums)
;(define flabs R6RS)                 ; defined by (rnrs flonums)

(define (flabsdiff x y)
  (flabs (fl- x y)))

(define (flsgn x)
  (flcopysign 1.0 x))

;(define flnumerator R6RS)           ; defined by (rnrs flonums)
;(define fldenominator R6RS)         ; defined by (rnrs flonums)
;(define flfloor R6RS)               ; defined by (rnrs flonums)
;(define flceiling R6RS)             ; defined by (rnrs flonums)
;(define flround R6RS)               ; defined by (rnrs flonums)
;(define fltruncate R6RS)            ; defined by (rnrs flonums)

;;; Exponents and logarithms

;(define flexp R6RS)                 ; defined by (rnrs flonums)

(define flexp2 (flop1 'flexp2 (lambda (x) (flexpt 2.0 x))))

;;; e^x = \sum_n (z^n / (n!))
;;;
;;; FIXME: the number of terms and the constant 0.5 seem reasonable
;;; for IEEE double precision, but the number of terms might need
;;; to be increased for higher precisions.

(define flexp-1
  (flop1 'flexp-1
         (let ((coefs (cons 0.0
                            (map fl/
                                 (map factorial
                                      '(1.0 2.0 3.0 4.0 5.0
                                        6.0 7.0 8.0 9.0 10.0
                                        11.0 12.0 13.0 14.0 15.0))))))
           (lambda (x)
             (cond ((fl<? (flabs x) 0.5)    ; FIXME
                    (polynomial-at x coefs))
                   (else
                    (fl- (flexp x) 1.0)))))))

(define flsquare (flop1 'flsquare (lambda (x) (fl* x x))))

;(define flsqrt R6RS)                ; defined by (rnrs flonums)

(define flcbrt (flop1 'flcbrt (lambda (x) (flexpt x (fl/ 3.0)))))

(define flhypot
  (flop2 'flhypot
         (lambda (x y)
           (cond ((flzero? x) (flabs y))
                 ((flzero? y) (flabs x))
                 ((or (flinfinite? x) (flinfinite? y)) +inf.0)
                 ((flnan? x) x)
                 ((flnan? y) y)
                 ((fl>? y x) (flhypot y x))
                 (else
                  (let* ((y/x (fl/ y x))
                         (root (flsqrt (fl+ 1.0 (fl* y/x y/x)))))
                    (fl* x root)))))))

;(define flexpt R6RS)                ; defined by (rnrs flonums)
;(define fllog R6RS)                 ; defined by (rnrs flonums)

;;; FIXME
;;; I believe this is supposed to return log(x+1), as in C99 log1p,
;;; instead of the (log x) + 1 specified in the draft SRFI.
;;;
;;; log (x + 1) = \sum_{n=1}^\infty - (-1)^n x^n/n

(define fllog1+
  (flop1 'fllog1+
         (let ((coefs (cons 0.0
                            (map fl/
                                 '(1.0 -2.0 3.0 -4.0 5.0
                                   -6.0 7.0 -8.0 9.0 -10.0)))))
           (lambda (x)
             (cond ((fl<? (flabs x) 0.5)    ; FIXME
                    (polynomial-at x coefs))
                   (else
                    (fllog (fl+ 1.0 x))))))))
           

(define fllog2 (flop1 'fllog2 (lambda (x) (log x 2.0))))

(define fllog10 (flop1 'fllog10 (lambda (x) (log x 10.0))))

(define (make-fllog-base k)
  (if (and (exact-integer? k) (> k 1))
      (flop1 'procedure-created-by-make-fllog-base
             (let ((base (inexact k)))
               (lambda (x) (log x base))))
      (error "bad argument passed to make-fllog-base" k)))

;;; Trigonometric functions

;(define flsin R6RS)                 ; defined by (rnrs flonums)
;(define flcos R6RS)                 ; defined by (rnrs flonums)
;(define fltan R6RS)                 ; defined by (rnrs flonums)
;(define flasin R6RS)                ; defined by (rnrs flonums)
;(define flacos R6RS)                ; defined by (rnrs flonums)
;(define flatan R6RS)                ; defined by (rnrs flonums)

(define flsinh
  (flop1 'flsinh
         (lambda (x)
           (cond ((flzero? x) x)
                 ((not (flfinite? x)) x)
                 (else
                  (fl/ (fl- (flexp x) (flexp (fl- x))) 2.0))))))

(define flcosh
  (flop1 'flcosh
         (lambda (x)
           (cond ((flzero? x) x)
                 ((not (flfinite? x)) (flabs x))
                 (else
                  (fl/ (fl+ (flexp x) (flexp (fl- x))) 2.0))))))

(define fltanh
  (flop1 'fltanh
         (lambda (x)
           (cond ((flzero? x) x)
                 ((flinfinite? x) (flcopysign 1.0 x))
                 ((flnan? x) x)
                 (else
                  (fl/ (flsinh x) (flcosh x)))))))

;;; inverse hyperbolic functions

(define flasinh
  (flop1 'flasinh
         (lambda (x)
           (cond ((flzero? x) x)
                 ((not (flfinite? x)) x)
                 (else
                  (fllog (fl+ x (flsqrt (fl+ (fl* x x) 1.0)))))))))

(define flacosh
  (flop1 'flacosh
         (lambda (x)
           (fllog (fl+ x (flsqrt (fl- (fl* x x) 1.0)))))))

(define flatanh
  (flop1 'flatanh
         (lambda (x)
           (cond ((flzero? x) x)
                 (else
                  (fl* 0.5 (fllog (fl/ (fl+ 1.0 x) (fl- 1.0 x)))))))))

;;; Integer division

(define flquotient (flop2 'flquotient quotient))

;;; FIXME: should probably implement the following part of the C spec:
;;; "If the returned value is 0, it will have the same sign as x."

(define flremainder (flop2 'flremainder remainder))

(define (flremquo x y)
  (values (flremainder x y)
          (flquotient x y)))

;; Special functions

(define flgamma FIXME)
(define flloggamma FIXME)
(define flfirst-bessel FIXME)
(define flsecond-bessel FIXME)
(define flerf FIXME)
(define flerfc FIXME)

